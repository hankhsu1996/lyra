#include <memory>
#include <string>
#include <unordered_set>
#include <utility>
#include <vector>

#include <slang/ast/ASTVisitor.h>
#include <slang/ast/Compilation.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/unit_signatures.hpp"
#include "lyra/lowering/ast_to_hir/lower.hpp"
#include "lyra/lowering/ast_to_hir/specialization_name.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// Collects the distinct units reachable from the tops. slang owns the
// structural descent: visiting an instance recurses into its body, and every
// container (generate blocks, instance arrays) is a Scope the visitor walks
// through, so an instance nested in a generate block is reached without this
// code knowing the container taxonomy.
//
// The specialization name keys the dedup, because that is what identifies a
// unit: a definition and the bindings selected for it, which is what a referrer
// can compute about the child it constructs. Two instances agreeing on it are
// one unit however many bodies the frontend chose to build -- it declines to
// share one when a name inside reaches outward, since the resolution differs
// per instance, but that resolution is settled per instance at construction and
// never inside the unit, so the unit itself is the same.
struct UnitCollector
    : slang::ast::ASTVisitor<UnitCollector, slang::ast::VisitFlags::Canonical> {
  std::unordered_set<std::string> seen;
  std::vector<const slang::ast::InstanceBodySymbol*> order;

  void handle(const slang::ast::InstanceSymbol& inst) {
    const auto* canonical = inst.getCanonicalBody();
    const auto& body = canonical != nullptr ? *canonical : inst.body;
    if (seen.insert(SpecializationName(body)).second) {
      order.push_back(&body);
      visitDefault(inst);
    }
  }
};

auto CollectUnitBodies(const LowerCompilationFacts& facts)
    -> std::vector<const slang::ast::InstanceBodySymbol*> {
  const auto& root = facts.Compilation().getRoot();
  UnitCollector collector;
  for (const auto* top : root.topInstances) {
    top->visit(collector);
  }
  return std::move(collector.order);
}

// Indexes the frontend's resolution of every `export "DPI-C"` directive (LRM
// 35.5) by the subroutine it names. The directive is resolved against the scope
// declaring it, so the subroutine it resolves to is the one that scope's walk
// reaches, and a scope elaborated under several specializations contributes the
// subroutine of each.
auto CollectForeignExportNames(const LowerCompilationFacts& facts)
    -> ForeignExportNames {
  ForeignExportNames names;
  for (const auto& exported : facts.Compilation().getDPIExports()) {
    names.emplace(exported.subroutine, exported.cIdentifier);
  }
  return names;
}

auto CollectPackages(const LowerCompilationFacts& facts)
    -> std::vector<const slang::ast::PackageSymbol*> {
  // `getPackages` includes the built-in `std` package (LRM 6.7.1); the runtime
  // provides its contents, so the compiler never lowers or emits it. `std` is a
  // reserved package name, so matching it excludes exactly the built-in. Only
  // user-declared packages are compiled.
  std::vector<const slang::ast::PackageSymbol*> packages;
  for (const auto* package : facts.Compilation().getPackages()) {
    if (package->name != "std") {
      packages.push_back(package);
    }
  }
  return packages;
}

// Whether a compilation-unit scope declares a member that becomes namespace
// content -- a variable, net, subroutine, or type alias. A file whose only
// scope members are design elements (a module or package declaration) and
// imports manifests no `$unit` unit, so it is not collected.
auto HasUnitScopeContent(const slang::ast::CompilationUnitSymbol& cu) -> bool {
  for (const auto& member : cu.members()) {
    switch (member.kind) {
      case slang::ast::SymbolKind::Variable:
      case slang::ast::SymbolKind::Net:
      case slang::ast::SymbolKind::Subroutine:
      case slang::ast::SymbolKind::TypeAlias:
        return true;
      default:
        break;
    }
  }
  return false;
}

auto CollectCompilationUnits(const LowerCompilationFacts& facts)
    -> std::vector<const slang::ast::CompilationUnitSymbol*> {
  // The `$unit` file-set scope (LRM 3.12.1) is modeled as an anonymous
  // namespace unit. slang exposes one `CompilationUnitSymbol` per
  // compilation-unit input (one per file, or one for all files under
  // `--single-unit`); only those declaring namespace-level content manifest a
  // unit, so a file holding only a design element contributes none.
  std::vector<const slang::ast::CompilationUnitSymbol*> units;
  for (const auto* cu : facts.Compilation().getRoot().compilationUnits) {
    if (HasUnitScopeContent(*cu)) {
      units.push_back(cu);
    }
  }
  return units;
}

}  // namespace

auto LowerCompilationToHir(const LowerCompilationFacts& facts)
    -> diag::Result<HirCompilation> {
  const auto packages = CollectPackages(facts);
  const auto compilation_units = CollectCompilationUnits(facts);
  const auto bodies = CollectUnitBodies(facts);
  const auto export_names = CollectForeignExportNames(facts);
  const LoweringFacts unit_facts(
      facts.SourceMapper(), facts.Sensitivity(), export_names,
      facts.DisableAssertions());

  std::vector<std::unique_ptr<UnitLowerer>> lowerers;
  lowerers.reserve(packages.size() + compilation_units.size() + bodies.size());
  for (const auto* package : packages) {
    lowerers.push_back(
        std::make_unique<UnitLowerer>(
            unit_facts, *package, std::string{package->name},
            hir::UnitKind::kPackage));
  }
  for (const auto* cu : compilation_units) {
    // A `$unit` scope is lowered, emitted, and initialized exactly as a package
    // is -- a rootless namespace unit -- so it carries the same unit kind;
    // nothing downstream distinguishes the two, so there is no separate kind.
    lowerers.push_back(
        std::make_unique<UnitLowerer>(
            unit_facts, *cu, CompilationUnitName(*cu),
            hir::UnitKind::kPackage));
  }
  for (const auto* body : bodies) {
    lowerers.push_back(
        std::make_unique<UnitLowerer>(
            unit_facts, *body, SpecializationName(*body),
            hir::UnitKind::kModule));
  }

  // Every unit declares before any unit lowers a body, because a body may
  // reference another unit and cannot reference what has not been declared.
  // This is the design-scope reading of the same ordering a single unit already
  // applies to its own declarations. A declaration reads only its own unit, so
  // nothing orders this pass and no cycle among units can arise.
  hir::UnitSignatures signatures;
  for (const auto& lowerer : lowerers) {
    if (auto r = lowerer->Declare(); !r) {
      return std::unexpected(std::move(r.error()));
    }
    signatures.Publish(lowerer->TakeSignature());
  }

  // Each unit's bodies lower against the signatures of the units it named and
  // no others, so what one unit's emission can depend on is bounded by its own
  // declarations rather than by what the design happens to contain.
  std::vector<hir::CompilationUnit> units;
  units.reserve(lowerers.size());
  for (const auto& lowerer : lowerers) {
    auto unit =
        lowerer->LowerBodies(signatures.Consumed(lowerer->ReferencedUnits()));
    if (!unit) {
      return std::unexpected(std::move(unit.error()));
    }
    units.push_back(*std::move(unit));
  }
  return HirCompilation{
      .units = std::move(units), .signatures = std::move(signatures)};
}

auto TopLevelUnitNames(slang::ast::Compilation& compilation)
    -> std::vector<std::string> {
  const auto& root = compilation.getRoot();
  std::vector<std::string> names;
  names.reserve(root.topInstances.size());
  for (const auto* inst : root.topInstances) {
    names.emplace_back(SpecializationName(*inst));
  }
  return names;
}

}  // namespace lyra::lowering::ast_to_hir
