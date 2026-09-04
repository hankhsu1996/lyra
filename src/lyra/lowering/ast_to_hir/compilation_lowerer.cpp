#include <cstdint>
#include <expected>
#include <format>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>
#include <vector>

#include <slang/ast/ASTVisitor.h>
#include <slang/ast/Compilation.h>
#include <slang/ast/SemanticFacts.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/PortSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/unit_signatures.hpp"
#include "lyra/lowering/ast_to_hir/lower.hpp"
#include "lyra/lowering/ast_to_hir/unit_identity.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// A unit to compile: the body it compiles, and the name its specialization is
// known by. The body belongs to an instance that name was computed from, so
// what the unit compiles against and what it was named for are one application
// of a definition. A body the frontend elaborated for a different application
// states different types at the same positions, which is a unit nothing in the
// design asked for.
struct CollectedUnit {
  const slang::ast::InstanceBodySymbol* body;
  std::string name;
};

// Collects the distinct units reachable from the tops. slang owns the
// structural descent: visiting an instance recurses into its body, and every
// container (generate blocks, instance arrays) is a Scope the visitor walks
// through, so an instance nested in a generate block is reached without this
// code knowing the container taxonomy.
//
// The specialization key decides the dedup, because that is what identifies a
// unit: a definition and everything fixed for it, which is what a referrer can
// compute about the child it constructs. Two instances agreeing on it are one
// unit however many bodies the frontend chose to build -- it declines to share
// one when a name inside reaches outward, since the resolution differs per
// instance, but that resolution is settled per instance at construction and
// never inside the unit, so the unit itself is the same.
//
// Descending reaches each occurrence's own body, so a child is collected under
// what its own parent fixed for it. Descending happens once per key rather than
// once per occurrence, because an occurrence whose key is already held stops
// here, so reaching every body costs nothing beyond what telling the
// specializations apart requires.
//
// Two keys reaching one name would silently make two units into one, so the
// name a unit is known by is checked against the key it came from rather than
// standing in for it.
struct UnitCollector : slang::ast::ASTVisitor<UnitCollector> {
  std::unordered_map<std::string, SpecializationKey> seen;
  std::vector<CollectedUnit> order;

  void handle(const slang::ast::InstanceSymbol& inst) {
    SpecializationKey key = SpecializationKeyOf(inst);
    std::string name = SpecializationName(key);
    const auto [entry, fresh] = seen.try_emplace(name, key);
    if (!fresh) {
      if (entry->second != key) {
        throw InternalError(
            "UnitCollector: two specializations reached one name, so the name "
            "no longer tells the units apart");
      }
      return;
    }
    order.push_back(CollectedUnit{.body = &inst.body, .name = std::move(name)});
    visitDefault(inst);
  }
};

auto CollectUnits(const LowerCompilationFacts& facts)
    -> std::vector<CollectedUnit> {
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
// content -- storage, a subroutine, a type alias, or a class. A file whose only
// scope members are design elements (a module or package declaration) and
// imports manifests no `$unit` unit, so it is not collected.
//
// A class declared here is namespace content like any other: whoever names it
// names it through this scope's unit (LRM 3.12.1), so a scope holding one has
// to become a unit for that name to reach a definition.
auto HasUnitScopeContent(const slang::ast::CompilationUnitSymbol& cu) -> bool {
  for (const auto& member : cu.members()) {
    switch (member.kind) {
      case slang::ast::SymbolKind::Variable:
      case slang::ast::SymbolKind::Net:
      case slang::ast::SymbolKind::Subroutine:
      case slang::ast::SymbolKind::TypeAlias:
      case slang::ast::SymbolKind::ClassType:
      case slang::ast::SymbolKind::GenericClassDef:
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

// The two port kinds IEEE 1800 forbids leaving unconnected. Every other
// direction has a defined meaning with no connection -- an input takes its
// declared default and an output drives nothing -- so only these two decide
// whether a module can stand alone.
enum class PortConnectionRule : std::uint8_t { kInterfacePort, kRefPort };

struct PortRequiringConnection {
  const slang::ast::Symbol* port;
  PortConnectionRule rule;
};

auto FindPortRequiringConnection(const slang::ast::InstanceBodySymbol& body)
    -> std::optional<PortRequiringConnection> {
  for (const auto* port : body.getPortList()) {
    if (port->kind == slang::ast::SymbolKind::InterfacePort) {
      return PortRequiringConnection{
          .port = port, .rule = PortConnectionRule::kInterfacePort};
    }
    const slang::ast::ArgumentDirection direction =
        port->kind == slang::ast::SymbolKind::MultiPort
            ? port->as<slang::ast::MultiPortSymbol>().direction
            : port->as<slang::ast::PortSymbol>().direction;
    if (direction == slang::ast::ArgumentDirection::Ref) {
      return PortRequiringConnection{
          .port = port, .rule = PortConnectionRule::kRefPort};
    }
  }
  return std::nullopt;
}

auto WhyItMustBeConnected(PortConnectionRule rule) -> std::string_view {
  switch (rule) {
    case PortConnectionRule::kInterfacePort:
      return "an interface port cannot be left unconnected (LRM 23.3.3.4)";
    case PortConnectionRule::kRefPort:
      return "a 'ref' port cannot be left unconnected (LRM 23.3.3.2)";
  }
  throw InternalError(
      "WhyItMustBeConnected: a port connection rule the language does not "
      "state");
}

}  // namespace

auto LowerCompilationToHir(const LowerCompilationFacts& facts)
    -> diag::Result<HirCompilation> {
  const auto packages = CollectPackages(facts);
  const auto compilation_units = CollectCompilationUnits(facts);
  const auto units_to_compile = CollectUnits(facts);
  const auto export_names = CollectForeignExportNames(facts);
  const LoweringFacts unit_facts(
      facts.SourceMapper(), facts.Sensitivity(), export_names,
      facts.AssertionPolicy());

  std::vector<std::unique_ptr<UnitLowerer>> lowerers;
  lowerers.reserve(
      packages.size() + compilation_units.size() + units_to_compile.size());
  for (const auto* package : packages) {
    lowerers.push_back(
        std::make_unique<UnitLowerer>(
            unit_facts, *package, std::string{package->name},
            hir::UnitRole::kNamespace));
  }
  for (const auto* cu : compilation_units) {
    // A `$unit` scope is lowered, emitted, and initialized exactly as a package
    // is -- a rootless namespace unit -- so it carries the same unit kind;
    // nothing downstream distinguishes the two, so there is no separate kind.
    lowerers.push_back(
        std::make_unique<UnitLowerer>(
            unit_facts, *cu, CompilationUnitName(*cu),
            hir::UnitRole::kNamespace));
  }
  for (const CollectedUnit& unit : units_to_compile) {
    lowerers.push_back(
        std::make_unique<UnitLowerer>(
            unit_facts, *unit.body, unit.name, hir::UnitRole::kObjectRoot));
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

auto TopLevelUnitNames(const LowerCompilationFacts& facts)
    -> diag::Result<std::vector<std::string>> {
  const auto& root = facts.Compilation().getRoot();
  std::vector<std::string> names;
  names.reserve(root.topInstances.size());
  for (const auto* inst : root.topInstances) {
    if (const auto required = FindPortRequiringConnection(inst->body)) {
      return std::unexpected(
          diag::Make(
              facts.SourceMapper().PointSpanOf(required->port->location),
              diag::DiagCode::kErrorTopLevelPortMustBeConnected,
              std::format(
                  "'{}' cannot be a simulation top because nothing "
                  "instantiates a top to connect its ports, and {}",
                  inst->name, WhyItMustBeConnected(required->rule)))
              .WithNote(
                  std::format(
                      "instantiate '{}' from a module that connects this port, "
                      "and make that module the top",
                      inst->name)));
    }
    names.emplace_back(SpecializationName(*inst));
  }
  return names;
}

}  // namespace lyra::lowering::ast_to_hir
