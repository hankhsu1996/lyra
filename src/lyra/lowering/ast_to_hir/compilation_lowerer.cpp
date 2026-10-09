#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <expected>
#include <format>
#include <memory>
#include <mutex>
#include <optional>
#include <set>
#include <span>
#include <string>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

#include <slang/ast/ASTVisitor.h>
#include <slang/ast/Compilation.h>
#include <slang/ast/SemanticFacts.h>
#include <slang/ast/symbols/ClassSymbols.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/PortSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/AllTypes.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/sink.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/dump.hpp"
#include "lyra/hir/unit_signatures.hpp"
#include "lyra/lowering/ast_to_hir/declaration_scopes.hpp"
#include "lyra/lowering/ast_to_hir/lower.hpp"
#include "lyra/lowering/ast_to_hir/sensitivity.hpp"
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

// One application of a unit: the unit, and the value of each argument its
// constructor is passed, in declaration order. Two instances that are one
// application are alike in every respect, so this is what tells a witness worth
// lowering from one that would repeat work already done.
struct Application {
  std::string unit;
  std::vector<std::string> arguments;

  auto operator<=>(const Application&) const = default;
};

auto ApplicationOf(
    const slang::ast::InstanceSymbol& inst, std::string unit,
    const SpecializationPolicy& policy) -> Application {
  Application application{.unit = std::move(unit), .arguments = {}};
  for (const slang::ast::ParameterSymbol* param :
       policy.SuppliedParametersOf(inst)) {
    application.arguments.push_back(ValueIdentity(param->getValue()));
  }
  return application;
}

// What the collection found: the distinct units, and every other instance,
// each of which shares one of them. A unit compiles one body for all of its
// instances, and that is sound only if each lowers to what the unit lowers to,
// so each is checked against it.
//
// The two kinds differ in what a difference means. A witness was handed values
// none of the unit's earlier instances was, so a difference is those values
// reaching the body, and keeping them in the unit's identity separates the
// two. A repeat is one application with an earlier instance, so nothing a unit
// is told apart by separates them and a difference has no such answer.
struct Collected {
  std::vector<CollectedUnit> units;
  std::vector<CollectedUnit> witnesses;
  std::vector<CollectedUnit> repeats;
};

// Collects the distinct units reachable from the tops. slang owns the
// structural descent: visiting an instance recurses into its body, and every
// container (generate blocks, instance arrays) is a Scope the visitor walks
// through, so an instance nested in a generate block is reached without this
// code knowing the container taxonomy.
//
// What an instance's specialization is called decides the dedup, because the
// name stands for what identifies a unit: a definition and everything fixed for
// it, which is what a referrer can compute about the child it constructs. Two
// instances agreeing on it are one unit however many bodies the frontend chose
// to build -- it declines to share one when a name inside reaches outward,
// since the resolution differs per instance, but that resolution is settled per
// instance at construction and never inside the unit, so the unit itself is the
// same.
//
// Descending reaches each occurrence's own body, so a child is collected under
// what its own parent fixed for it, and every occurrence is reached, because
// each is held to the unit it shares.
//
// A virtual interface's type names an interface together with its parameters
// (LRM 25.9), and code reaching through one compiles against that unit's
// signature whether or not any instance of it exists -- a declaration of a
// parameterization no instance has is legal, only never assignable. So the
// interface a declared type names is collected as an instantiated one is. Only
// declarations are read for it, which is what the walk reaches anyway.
struct UnitCollector : slang::ast::ASTVisitor<UnitCollector> {
  explicit UnitCollector(const SpecializationPolicy& policy) : policy(&policy) {
  }

  const SpecializationPolicy* policy;
  std::unordered_set<std::string> named;
  std::set<Application> applications;
  Collected found;

  // Every declared variable, class property and subroutine argument.
  void handle(const slang::ast::VariableSymbol& variable) {
    CollectInterfacesNamedBy(variable.getType());
  }

  // A function's result is declared by its return type, which no variable
  // carries.
  void handle(const slang::ast::SubroutineSymbol& subroutine) {
    CollectInterfacesNamedBy(subroutine.getReturnType());
    visitDefault(subroutine);
  }

  void CollectInterfacesNamedBy(const slang::ast::Type& type) {
    const slang::ast::Type& canonical = type.getCanonicalType();
    if (const auto* handle_type =
            canonical.as_if<slang::ast::VirtualInterfaceType>()) {
      handle(handle_type->iface);
      return;
    }
    if (canonical.isUnpackedArray()) {
      CollectInterfacesNamedBy(*canonical.getArrayElementType());
      return;
    }
    if (const auto* record =
            canonical.as_if<slang::ast::UnpackedStructType>()) {
      for (const slang::ast::FieldSymbol* field : record->fields) {
        CollectInterfacesNamedBy(field->getType());
      }
    }
  }

  void handle(const slang::ast::InstanceSymbol& inst) {
    std::string name = policy->NameOf(inst);
    const bool fresh = named.insert(name).second;
    const bool repeat =
        !applications.insert(ApplicationOf(inst, name, *policy)).second;
    std::vector<CollectedUnit>* collected_as = &found.witnesses;
    if (fresh) {
      collected_as = &found.units;
    } else if (repeat) {
      collected_as = &found.repeats;
    }
    collected_as->push_back(
        CollectedUnit{.body = &inst.body, .name = std::move(name)});
    visitDefault(inst);
  }
};

auto CollectUnits(
    const LowerCompilationFacts& facts, const SpecializationPolicy& policy)
    -> Collected {
  const auto& root = facts.Compilation().getRoot();
  UnitCollector collector(policy);
  for (const auto* top : root.topInstances) {
    top->visit(collector);
  }
  // A class a package or the `$unit` scope declares is under no instance, and
  // what it declares may name an interface too.
  for (const auto* package : facts.Compilation().getPackages()) {
    package->visit(collector);
  }
  for (const auto* cu : root.compilationUnits) {
    cu->visit(collector);
  }
  return std::move(collector.found);
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

// Where each specialization of a generic class a namespace declares is held.
// Its parameters are written wherever it is named (LRM 8.25), so the namespace
// declaring the generic fixes none of them and holds none of its
// specializations: one specialized on a type an instance replicates is held by
// that instance's design element, and every other one is a unit of its own.
//
// The front end makes a specialization where an expression naming it is first
// bound, and an instance body it judged a duplicate of another is bound only
// when something reads it. So every body a unit is lowered from is bound
// first; otherwise a specialization appears only once its unit is already
// lowering, after the scope replicating it has settled what it holds.
struct SpecializationHomes {
  std::vector<const slang::ast::ClassType*> own_units;
  PlacedSpecializations placed;
};

// Binds what one body holds, an instance inside it as far as the connections
// the body writes for it, since the instance's own body is one of the collected
// ones and is bound in its turn.
struct BindsEveryExpression
    : slang::ast::ASTVisitor<
          BindsEveryExpression, slang::ast::VisitFlags::AllGood> {
  void handle(const slang::ast::InstanceSymbol& child) {
    child.visitExprs(*this);
  }
};

auto CollectSpecializationHomes(
    const LowerCompilationFacts& facts, const Collected& collected,
    const SpecializationPolicy& policy) -> SpecializationHomes {
  BindsEveryExpression binder;
  for (const auto* bodies :
       {&collected.units, &collected.witnesses, &collected.repeats}) {
    for (const CollectedUnit& unit : *bodies) {
      unit.body->visit(binder);
    }
  }
  SpecializationHomes homes;
  const auto visit = [&](const slang::ast::Symbol& member) {
    const auto* cls = member.as_if<slang::ast::ClassType>();
    if (cls == nullptr || cls->genericClass == nullptr) return;
    const slang::ast::Symbol& home = UnitHomeOf(*cls);
    if (&home == cls) {
      homes.own_units.push_back(cls);
    } else if (IsDesignElement(home)) {
      homes.placed[&home].push_back(cls);
    }
  };
  for (const auto* package : CollectPackages(facts)) {
    WalkDeclarationScopes(*package, visit);
  }
  for (const auto* cu : facts.Compilation().getRoot().compilationUnits) {
    WalkDeclarationScopes(*cu, visit);
  }
  // The front end lists a generic's specializations in no order a second
  // compilation repeats, so they are taken in the order of their names.
  const auto by_name = [&](const slang::ast::ClassType* cls) {
    return CompilationUnitName(UnitHomeOf(*cls), policy) +
           SpecializationName(*cls, policy);
  };
  std::ranges::sort(homes.own_units, {}, by_name);
  for (auto& [_, placed] : homes.placed) {
    std::ranges::sort(placed, {}, by_name);
  }
  return homes;
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

// The design's tops. A top is where the design begins, so nothing instantiates
// it and its ports are connected to nothing, which two kinds of port may not
// be.
auto TopLevelUnits(
    const LowerCompilationFacts& facts, const SpecializationPolicy& policy)
    -> diag::Result<std::vector<TopLevelUnit>> {
  const auto& root = facts.Compilation().getRoot();
  std::vector<TopLevelUnit> tops;
  tops.reserve(root.topInstances.size());
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
    tops.emplace_back(
        TopLevelUnit{
            .instance_name = std::string{inst->name},
            .unit_name = policy.NameOf(*inst)});
  }
  return tops;
}

using Definitions = std::unordered_set<const slang::ast::DefinitionSymbol*>;

// The bodies of `unit` lowered exactly as a unit of its own would be, against
// what the design's units published.
auto LowerBodiesOf(
    const LoweringFacts& facts, const CollectedUnit& unit,
    const hir::UnitSignatures& signatures)
    -> diag::Result<hir::CompilationUnit> {
  UnitLowerer lowerer(facts, *unit.body, unit.name, hir::UnitRole::kObjectRoot);
  if (auto declared = lowerer.Declare(); !declared) {
    return std::unexpected(std::move(declared.error()));
  }
  return lowerer.LowerBodies(signatures);
}

// The first line two units' dumps disagree on, as a clause for a report. Two
// units that compare unequal say nothing about where, and whoever reads the
// report has only the two instances' names to start from otherwise.
auto WhereTheyFirstDiffer(
    const hir::CompilationUnit& unit, const hir::CompilationUnit& instance)
    -> std::string {
  const std::string of_unit = hir::DumpHir(unit);
  const std::string of_instance = hir::DumpHir(instance);
  std::string_view left = of_unit;
  std::string_view right = of_instance;
  while (!left.empty() && !right.empty()) {
    const std::string_view left_line = left.substr(0, left.find('\n'));
    const std::string_view right_line = right.substr(0, right.find('\n'));
    if (left_line != right_line) {
      return std::format(
          ": the unit states `{}` where the instance states `{}`", left_line,
          right_line);
    }
    left.remove_prefix(std::min(left.size(), left_line.size() + 1));
    right.remove_prefix(std::min(right.size(), right_line.size() + 1));
  }
  if (left.empty() && right.empty()) {
    return ": the two differ in something the dump of a unit does not state";
  }
  const std::string_view longer = left.empty() ? right : left;
  return std::format(
      ": the {} goes on to state `{}` where the other ends",
      left.empty() ? "instance" : "unit", longer.substr(0, longer.find('\n')));
}

// The definitions a witness of which lowered apart from the unit it shares.
// Each unit with witnesses is lowered once here and each of its witnesses
// beside it, one at a time and dropped after, so only one unit and one witness
// are ever resident; the unit is lowered again in its own turn.
//
// Where a witness does not come out as its unit, something in its body held a
// value it was handed, and the definition is declared again with every
// parameter in its key, each value its own unit -- so the program is the one
// each instance describes and only the sharing is lost, which is said as a
// remark against the definition. A failure to lower is left to the unit's own
// turn to report: a unit that fails has nothing to hold a witness against, and
// a witness that fails where its unit did not has lowered apart, so its
// definition is kept whole and the witness reports in a turn of its own.
//
// A repeat is held to the same comparison and has no such answer. It agrees
// with an earlier instance on everything a unit is told apart by, so one that
// lowers to something else was told apart by nothing: the identity is missing
// what distinguishes them, or the lowering states one text two ways. Either is
// this compiler's defect, and sharing the unit anyway would build one of the
// two instances as the other.
auto DefinitionsLoweredApart(
    const LoweringFacts& facts, const Collected& collected,
    const hir::UnitSignatures& signatures, diag::DiagnosticSink& sink)
    -> Definitions {
  using InstancesByUnit =
      std::unordered_map<std::string_view, std::vector<const CollectedUnit*>>;
  InstancesByUnit witnesses_of;
  for (const CollectedUnit& witness : collected.witnesses) {
    witnesses_of[witness.name].push_back(&witness);
  }
  InstancesByUnit repeats_of;
  for (const CollectedUnit& repeat : collected.repeats) {
    repeats_of[repeat.name].push_back(&repeat);
  }
  Definitions apart;
  for (const CollectedUnit& unit : collected.units) {
    const auto witnesses = witnesses_of.find(unit.name);
    const auto repeats = repeats_of.find(unit.name);
    if (witnesses == witnesses_of.end() && repeats == repeats_of.end()) {
      continue;
    }
    const auto shared = LowerBodiesOf(facts, unit, signatures);
    if (!shared) continue;
    if (repeats != repeats_of.end()) {
      for (const CollectedUnit* repeat : repeats->second) {
        const auto lowered = LowerBodiesOf(facts, *repeat, signatures);
        if (lowered && *lowered == *shared) continue;
        throw InternalError(
            std::format(
                "DefinitionsLoweredApart: instance '{}' shares unit '{}' with "
                "instance '{}', which it agrees with on everything a unit is "
                "told apart by, and lowers to something that unit does not{}",
                repeat->body->getHierarchicalPath(), unit.name,
                unit.body->getHierarchicalPath(),
                lowered ? WhereTheyFirstDiffer(*shared, *lowered)
                        : std::string{", because it does not lower at all"}));
      }
    }
    if (witnesses == witnesses_of.end()) continue;
    for (const CollectedUnit* witness : witnesses->second) {
      const auto lowered = LowerBodiesOf(facts, *witness, signatures);
      if (lowered && *lowered == *shared) continue;
      const auto& definition = witness->body->getDefinition();
      if (apart.insert(&definition).second) {
        sink.Report(
            diag::Make(
                facts.SourceMapper().PointSpanOf(definition.location),
                diag::DiagCode::kRemarkLostSharing,
                std::format(
                    "sharing lost: '{}' is compiled once per parameter value, "
                    "because instances handed different values lowered apart",
                    definition.name)));
      }
      break;
    }
  }
  return apart;
}

}  // namespace

// What the design's units hold between declaring and lowering their bodies.
// Everything a unit reads is here and outlives the unit reading it: the AST is
// declared first, so it is released after every lowerer pointing into it, and
// the sensitivity analysis, the specialization policy and where each
// specialization is held are built beside the facts that point at them rather
// than handed in.
struct DeclaredDesign::Units {
  std::unique_ptr<slang::ast::Compilation> front_end;
  SensitivityAnalyzer sensitivity;
  SpecializationPolicy specialization;
  SpecializationHomes specialization_homes;
  LoweringFacts facts;
  std::vector<TopLevelUnit> tops;
  std::vector<std::unique_ptr<UnitLowerer>> lowerers;
  hir::UnitSignatures signatures;
  // Held while a unit reads the frontend, which elaborates on first read.
  std::mutex reading_front_end;

  Units(
      std::unique_ptr<slang::ast::Compilation> elaborated,
      const frontend::SlangSourceMapper& source_mapper,
      support::AssertionPolicy assertion_policy)
      : front_end(std::move(elaborated)),
        facts(
            source_mapper, sensitivity, assertion_policy, specialization,
            specialization_homes.placed) {
  }

  // Declares every unit the design has under the specialization policy held
  // here, replacing whatever an earlier policy declared. Whether any failed is
  // the sink's answer.
  void DeclareEveryUnit(
      const LowerCompilationFacts& front_end_facts, const Collected& collected,
      diag::DiagnosticSink& sink) {
    lowerers.clear();
    signatures = hir::UnitSignatures{};
    for (const auto* package : CollectPackages(front_end_facts)) {
      lowerers.push_back(
          std::make_unique<UnitLowerer>(
              facts, *package, std::string{package->name},
              hir::UnitRole::kNamespace));
    }
    for (const auto* cu : CollectCompilationUnits(front_end_facts)) {
      // A `$unit` scope is lowered, emitted, and initialized exactly as a
      // package is -- a rootless namespace unit -- so it carries the same unit
      // kind; nothing downstream distinguishes the two, so there is no
      // separate kind.
      lowerers.push_back(
          std::make_unique<UnitLowerer>(
              facts, *cu, CompilationUnitName(*cu, specialization),
              hir::UnitRole::kNamespace));
    }
    for (const slang::ast::ClassType* spec : specialization_homes.own_units) {
      lowerers.push_back(
          std::make_unique<UnitLowerer>(
              facts, *spec, CompilationUnitName(*spec, specialization),
              hir::UnitRole::kNamespace));
    }
    for (const CollectedUnit& unit : collected.units) {
      lowerers.push_back(
          std::make_unique<UnitLowerer>(
              facts, *unit.body, unit.name, hir::UnitRole::kObjectRoot));
    }

    // Every unit declares before any unit lowers a body, because a body may
    // reference another unit and cannot reference what has not been declared.
    // This is the design-scope reading of the same ordering a single unit
    // already applies to its own declarations. A declaration reads only its own
    // unit, so nothing orders this pass and no cycle among units can arise.
    for (const auto& lowerer : lowerers) {
      if (auto declared = lowerer->Declare(); !declared) {
        sink.Report(std::move(declared.error()));
        continue;
      }
      signatures.Publish(lowerer->TakeSignature());
    }
  }
};

// A definition whose instances lowered apart is declared again kept whole. A
// definition kept whole is supplied nothing at construction, so none of its
// instances can lower apart again, and the set grows every time round: at
// worst every definition is kept whole and each parameter value is its own
// unit. Nothing else reads the frontend until this returns, so the check reads
// it without taking turns.
auto DeclaredDesign::Declare(
    std::unique_ptr<slang::ast::Compilation> front_end,
    const frontend::SlangSourceMapper& source_mapper,
    support::AssertionPolicy assertion_policy, diag::DiagnosticSink& sink)
    -> std::optional<DeclaredDesign> {
  auto units = std::make_unique<Units>(
      std::move(front_end), source_mapper, assertion_policy);
  const LowerCompilationFacts facts(
      *units->front_end, source_mapper, assertion_policy);

  Definitions kept_whole;
  for (;;) {
    units->specialization = SpecializationPolicy(kept_whole);
    auto tops = TopLevelUnits(facts, units->specialization);
    if (!tops) {
      sink.Report(std::move(tops.error()));
      return std::nullopt;
    }
    const Collected collected = CollectUnits(facts, units->specialization);
    units->specialization_homes =
        CollectSpecializationHomes(facts, collected, units->specialization);
    units->DeclareEveryUnit(facts, collected, sink);
    if (sink.HasErrors()) {
      return std::nullopt;
    }
    const Definitions apart = DefinitionsLoweredApart(
        units->facts, collected, units->signatures, sink);
    if (apart.empty()) {
      units->tops = *std::move(tops);
      return DeclaredDesign(std::move(units));
    }
    for (const slang::ast::DefinitionSymbol* definition : apart) {
      if (!kept_whole.insert(definition).second) {
        throw InternalError(
            std::format(
                "DeclaredDesign::Declare: '{}' was kept whole and still "
                "lowered apart, so declaring it once more cannot settle it",
                definition->name));
      }
    }
  }
}

DeclaredDesign::DeclaredDesign(std::unique_ptr<Units> units)
    : units_(std::move(units)) {
}
DeclaredDesign::DeclaredDesign(DeclaredDesign&&) noexcept = default;
auto DeclaredDesign::operator=(DeclaredDesign&&) noexcept
    -> DeclaredDesign& = default;
DeclaredDesign::~DeclaredDesign() = default;

auto DeclaredDesign::Tops() const -> std::span<const TopLevelUnit> {
  return units_->tops;
}

auto DeclaredDesign::UnitCount() const -> std::size_t {
  return units_->lowerers.size();
}

// Which of the published signatures a unit depends on is the set its bodies
// read: a name first reached from inside a body is reached after any set fixed
// in advance, and whether a name is on a signature does not depend on who
// asked.
auto DeclaredDesign::LowerUnit(std::size_t index)
    -> diag::Result<hir::CompilationUnit> {
  const std::scoped_lock reading(units_->reading_front_end);
  std::unique_ptr<UnitLowerer> lowerer = std::move(units_->lowerers[index]);
  if (lowerer == nullptr) {
    throw InternalError(
        "DeclaredDesign::LowerUnit: a unit's bodies are lowered once, and "
        "this one has been");
  }
  return lowerer->LowerBodies(units_->signatures);
}

auto DeclaredDesign::Signatures() const -> const hir::UnitSignatures& {
  return units_->signatures;
}

}  // namespace lyra::lowering::ast_to_hir
