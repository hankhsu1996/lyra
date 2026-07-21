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
#include "lyra/lowering/ast_to_hir/lower.hpp"
#include "lyra/lowering/ast_to_hir/specialization_name.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// Collects the distinct unit bodies reachable from the tops. slang owns the
// structural descent: visiting an instance recurses into its body, and every
// container (generate blocks, instance arrays) is a Scope the visitor walks
// through, so an instance nested in a generate block is reached without this
// code knowing the container taxonomy. The canonical body keys the dedup so a
// module instantiated many times is lowered once.
struct UnitCollector
    : slang::ast::ASTVisitor<UnitCollector, slang::ast::VisitFlags::Canonical> {
  std::unordered_set<const slang::ast::InstanceBodySymbol*> seen;
  std::vector<const slang::ast::InstanceBodySymbol*> order;

  void handle(const slang::ast::InstanceSymbol& inst) {
    const auto* canonical = inst.getCanonicalBody();
    const auto& body = canonical != nullptr ? *canonical : inst.body;
    if (seen.insert(&body).second) {
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

auto LowerUnit(
    const LowerCompilationFacts& facts,
    const slang::ast::InstanceBodySymbol& body)
    -> diag::Result<hir::CompilationUnit> {
  const LoweringFacts unit_facts(
      facts.SourceMapper(), facts.Sensitivity(), facts.DisableAssertions());
  UnitLowerer lowerer(unit_facts, body, SpecializationName(body));
  auto unit = lowerer.Run();
  if (unit) {
    unit->kind = hir::UnitKind::kModule;
  }
  return unit;
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

auto LowerPackageUnit(
    const LowerCompilationFacts& facts,
    const slang::ast::PackageSymbol& package)
    -> diag::Result<hir::CompilationUnit> {
  const LoweringFacts unit_facts(
      facts.SourceMapper(), facts.Sensitivity(), facts.DisableAssertions());
  UnitLowerer lowerer(unit_facts, package, std::string{package.name});
  auto unit = lowerer.Run();
  if (unit) {
    unit->kind = hir::UnitKind::kPackage;
  }
  return unit;
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

auto LowerCompilationUnitUnit(
    const LowerCompilationFacts& facts,
    const slang::ast::CompilationUnitSymbol& cu)
    -> diag::Result<hir::CompilationUnit> {
  const LoweringFacts unit_facts(
      facts.SourceMapper(), facts.Sensitivity(), facts.DisableAssertions());
  UnitLowerer lowerer(unit_facts, cu, CompilationUnitName(cu));
  auto unit = lowerer.Run();
  if (unit) {
    // A `$unit` scope is lowered, emitted, and initialized exactly as a package
    // is -- a rootless namespace unit -- so it carries the same unit kind;
    // nothing downstream distinguishes the two, so there is no separate kind.
    unit->kind = hir::UnitKind::kPackage;
  }
  return unit;
}

}  // namespace

auto LowerCompilationToHir(const LowerCompilationFacts& facts)
    -> diag::Result<std::vector<hir::CompilationUnit>> {
  std::vector<hir::CompilationUnit> units;
  const auto packages = CollectPackages(facts);
  const auto compilation_units = CollectCompilationUnits(facts);
  const auto bodies = CollectUnitBodies(facts);
  units.reserve(packages.size() + compilation_units.size() + bodies.size());
  for (const auto* package : packages) {
    auto unit = LowerPackageUnit(facts, *package);
    if (!unit) {
      return std::unexpected(std::move(unit.error()));
    }
    units.push_back(*std::move(unit));
  }
  for (const auto* cu : compilation_units) {
    auto unit = LowerCompilationUnitUnit(facts, *cu);
    if (!unit) {
      return std::unexpected(std::move(unit.error()));
    }
    units.push_back(*std::move(unit));
  }
  for (const auto* body : bodies) {
    auto unit = LowerUnit(facts, *body);
    if (!unit) {
      return std::unexpected(std::move(unit.error()));
    }
    units.push_back(*std::move(unit));
  }
  return units;
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
