#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <expected>
#include <format>
#include <map>
#include <memory>
#include <mutex>
#include <optional>
#include <ranges>
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
#include "lyra/frontend/slang_source_span.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/dump.hpp"
#include "lyra/hir/unit_signatures.hpp"
#include "lyra/lowering/ast_to_hir/declaration_scopes.hpp"
#include "lyra/lowering/ast_to_hir/lower.hpp"
#include "lyra/lowering/ast_to_hir/name_in_a_path.hpp"
#include "lyra/lowering/ast_to_hir/one_body.hpp"
#include "lyra/lowering/ast_to_hir/sensitivity.hpp"
#include "lyra/lowering/ast_to_hir/unit_identity.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

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

// An instance read through a body of its own: the body, and the application of
// its unit it is. The body belongs to an instance the unit's name was computed
// from, so what the unit compiles against and what it was named for are one
// application of a definition. A body the frontend elaborated for a different
// definition's application states different types at the same positions, which
// is a unit nothing in the design asked for.
struct CollectedUnit {
  const slang::ast::InstanceBodySymbol* body;
  Application application;

  [[nodiscard]] auto Name() const -> const std::string& {
    return application.unit;
  }
};

// What the collection found: the distinct units, and every other instance
// read through a body of its own, each of which shares one of them. A unit is
// one text compiled for all of its instances, and that is sound only if each
// is one body with what the unit holds, so each of these is held against it.
// An instance the front end left sharing another's body is not among them: it
// is one application with an instance that is.
//
// The two kinds differ in what a difference means. A witness was handed values
// none of the unit's earlier instances was. What it builds may differ, since
// those values decide which blocks its construction builds, and the unit then
// holds what it built as well; a difference in anything else is those values
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
// Descending reaches an occurrence's own body, so a child is collected under
// what its own parent fixed for it. An occurrence the front end left sharing
// another's body is not descended into where it is one application with an
// instance already found: what is below it was collected under that one.
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
    if (!policy->HasBodyOfItsOwn(inst)) {
      // One application with an instance already found: nothing of it is
      // read, not its body and so nothing below it either.
      if (named.contains(name) &&
          applications.contains(ApplicationOf(inst, name, *policy))) {
        return;
      }
      // The first instance its unit is met through, or the first handed
      // these values: either is read through its own body after all, and
      // named by what that body says. Which values are the same is this
      // compiler's answer and not the front end's.
      policy->ReadBodyOf(inst);
      name = policy->NameOf(inst);
    }
    const bool fresh = named.insert(name).second;
    Application application = ApplicationOf(inst, std::move(name), *policy);
    const bool repeat = !applications.insert(application).second;
    std::vector<CollectedUnit>* collected_as = &found.witnesses;
    if (fresh) {
      collected_as = &found.units;
    } else if (repeat) {
      collected_as = &found.repeats;
    }
    collected_as->push_back(
        CollectedUnit{
            .body = &inst.body, .application = std::move(application)});
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
  const auto name_of = [&](const slang::ast::ClassType& cls) {
    return SpecializationName(cls, policy);
  };
  for (const auto* package : CollectPackages(facts)) {
    WalkDeclarationScopes(*package, visit, name_of);
  }
  for (const auto* cu : facts.Compilation().getRoot().compilationUnits) {
    WalkDeclarationScopes(*cu, visit, name_of);
  }
  // Specializations of several generics meet in one list, so each list is
  // taken in the order of its names.
  const auto by_name = [&](const slang::ast::ClassType* cls) {
    return CompilationUnitName(UnitHomeOf(*cls), policy) + name_of(*cls);
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
              frontend::PointSpanOf(required->port->location),
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
            .name_in_a_path = NameInAPath(*inst),
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
  UnitLowerer lowerer(
      facts, *unit.body, unit.Name(), hir::UnitRole::kObjectRoot, {});
  if (auto declared = lowerer.Declare(); !declared) {
    return std::unexpected(std::move(declared.error()));
  }
  return lowerer.LowerBodies(signatures);
}

// The bodies a unit is lowered from beside the one it is named for, by unit.
using AlsoLoweredFrom = std::unordered_map<
    std::string, std::vector<const slang::ast::InstanceBodySymbol*>>;

// Whether the unit of `body` holds a class: one the design element declares
// (LRM 8.3), or a specialization of a generic placed in it. Such a class is
// declared once per unit, from one body, so what two bodies holding one build
// cannot be lowered into one unit and held scope against scope; they are held
// to each other only as two units compared whole.
auto HoldsAClass(
    const slang::ast::InstanceBodySymbol& body,
    const PlacedSpecializations& placed) -> bool {
  return DeclaresAClass(body) || placed.contains(&body);
}

// Every witness of each unit, where the unit and all of them can be lowered
// into one unit.
auto WitnessesLoweredWithTheirUnits(
    const Collected& collected, const PlacedSpecializations& placed)
    -> AlsoLoweredFrom {
  AlsoLoweredFrom also;
  std::unordered_set<std::string_view> compared_whole;
  for (const CollectedUnit& unit : collected.units) {
    if (HoldsAClass(*unit.body, placed)) compared_whole.insert(unit.Name());
  }
  for (const CollectedUnit& witness : collected.witnesses) {
    if (HoldsAClass(*witness.body, placed)) {
      compared_whole.insert(witness.Name());
    }
  }
  for (const CollectedUnit& witness : collected.witnesses) {
    if (!compared_whole.contains(witness.Name())) {
      also[witness.Name()].push_back(witness.body);
    }
  }
  return also;
}

// Where two units' dumps first part, as a clause for a report: the line they
// disagree on, the line one goes on with, or that neither shows the difference.
// Two units that compare unequal say nothing about where, and whoever reads
// the report has only the two instances' names to start from otherwise.
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

// Says that `definition` is compiled once per parameter value, once however
// many of its instances were found apart.
void RemarkOnInstancesApart(
    const slang::ast::DefinitionSymbol& definition, std::string_view found,
    Definitions& apart, diag::DiagnosticSink& sink) {
  if (!apart.insert(&definition).second) return;
  sink.Report(
      diag::Make(
          frontend::PointSpanOf(definition.location),
          diag::DiagCode::kRemarkLostSharing,
          std::format(
              "sharing lost: '{}' is compiled once per parameter value, "
              "because instances handed different values {} apart",
              definition.name, found)));
}

// What holding every instance against the unit it shares found: the
// definitions a witness of which is not one body with its unit, and for each
// unit the witnesses that built something the unit did not yet hold, which are
// the ones it is lowered from beside the instance it is named for.
//
// A unit whose witnesses could not be declared into it is compared with each
// of them whole, and holds what the one instance it is named for built. A
// witness that differs from it there may only have built other blocks, which
// such a unit cannot hold, so its definition is first declared again as
// `built_by_one_instance`, where what decides the blocks an instance builds
// names its unit. One that still differs after that is apart.
struct InstancesHeldToTheirUnits {
  Definitions apart;
  Definitions built_by_one_instance;
  AlsoLoweredFrom adding;
};

// The bodies among `others` that built something `one_body` and the bodies
// before them did not hold, each lowered into `lowerer`'s unit and held
// against what is held so far. Nothing where one of them is not one body with
// it, or does not lower.
auto BodiesThatAdd(
    UnitLowerer& lowerer, hir::StructuralScope one_body,
    std::span<const slang::ast::InstanceBodySymbol* const> others,
    const hir::UnitSignatures& signatures)
    -> std::optional<std::vector<const slang::ast::InstanceBodySymbol*>> {
  std::vector<const slang::ast::InstanceBodySymbol*> adding;
  for (const slang::ast::InstanceBodySymbol* other : others) {
    const auto built = lowerer.LowerScopeBuiltBy(*other, signatures);
    if (!built) return std::nullopt;
    auto both = OneBodyOf(one_body, *built);
    if (!both.has_value()) return std::nullopt;
    if (*both == one_body) continue;
    adding.push_back(other);
    one_body = *std::move(both);
  }
  return adding;
}

// Each unit with witnesses is lowered here and each of its witnesses with it,
// one at a time and dropped after, so only one unit and one witness are ever
// resident; the unit is lowered again in its own turn.
//
// A witness was handed values its unit's earlier instances were not, and what
// those values decide when an object is built is no part of the unit: which
// alternative of a conditional generate stands, and how many blocks a loop
// counts out (LRM 27.4, 27.5). So a witness is one body with its unit where
// the two differ in nothing else, and the unit then holds what either built.
// `lowered_with` holds the witnesses of each unit that were declared into it,
// which is what lets the two be held against each other scope by scope; a unit
// with none of its witnesses there is compared with each of them whole.
//
// Where a witness is not one body with its unit, something in its body held a
// value it was handed, and the definition is declared again with every
// parameter in its key, each value its own unit -- so the program is the one
// each instance describes and only the sharing is lost, which is said as a
// remark against the definition. A failure to lower is left to the unit's own
// turn to report: a unit that fails has nothing to hold a witness against, and
// a witness that fails where its unit did not has lowered apart, so its
// definition is kept whole and the witness reports in a turn of its own.
//
// A repeat is held to the instance it is one application with, exactly, and
// has no such answer. It agrees with that instance on everything a unit is
// told apart by and on every value it is handed, so one that lowers to
// something else was told apart by nothing: the identity is missing what
// distinguishes them, or the lowering states one text two ways. Either is this
// compiler's defect, and sharing the unit anyway would build one of the two
// instances as the other.
auto HoldInstancesToTheirUnits(
    const LoweringFacts& facts, const Collected& collected,
    const AlsoLoweredFrom& lowered_with,
    const Definitions& already_built_by_one_instance,
    const hir::UnitSignatures& signatures, diag::DiagnosticSink& sink)
    -> InstancesHeldToTheirUnits {
  std::unordered_map<std::string_view, std::vector<const CollectedUnit*>>
      witnesses_of;
  for (const CollectedUnit& witness : collected.witnesses) {
    witnesses_of[witness.Name()].push_back(&witness);
  }
  std::map<Application, std::vector<const CollectedUnit*>> repeats_of;
  for (const CollectedUnit& repeat : collected.repeats) {
    repeats_of[repeat.application].push_back(&repeat);
  }
  const auto hold_repeats_to = [&](const CollectedUnit& first) {
    const auto repeats = repeats_of.find(first.application);
    if (repeats == repeats_of.end()) return;
    const auto own = LowerBodiesOf(facts, first, signatures);
    if (!own) return;
    for (const CollectedUnit* repeat : repeats->second) {
      const auto lowered = LowerBodiesOf(facts, *repeat, signatures);
      if (lowered && *lowered == *own) continue;
      throw InternalError(
          std::format(
              "HoldInstancesToTheirUnits: instance '{}' shares unit '{}' with "
              "instance '{}', which it agrees with on everything a unit is "
              "told apart by and on every value it is handed, and lowers to "
              "something that instance does not{}",
              repeat->body->getHierarchicalPath(), first.Name(),
              first.body->getHierarchicalPath(),
              lowered ? WhereTheyFirstDiffer(*own, *lowered)
                      : std::string{", because it does not lower at all"}));
    }
  };

  InstancesHeldToTheirUnits held;
  for (const CollectedUnit& unit : collected.units) {
    hold_repeats_to(unit);
    const auto witnesses = witnesses_of.find(unit.Name());
    if (witnesses == witnesses_of.end()) continue;
    for (const CollectedUnit* witness : witnesses->second) {
      hold_repeats_to(*witness);
    }
    const auto& definition = unit.body->getDefinition();

    const auto with = lowered_with.find(unit.Name());
    if (with == lowered_with.end()) {
      const auto whole = LowerBodiesOf(facts, unit, signatures);
      if (!whole) continue;
      for (const CollectedUnit* witness : witnesses->second) {
        const auto lowered = LowerBodiesOf(facts, *witness, signatures);
        if (lowered && *lowered == *whole) continue;
        if (already_built_by_one_instance.contains(&definition)) {
          RemarkOnInstancesApart(definition, "lowered", held.apart, sink);
        } else {
          held.built_by_one_instance.insert(&definition);
        }
        break;
      }
      continue;
    }

    UnitLowerer lowerer(
        facts, *unit.body, unit.Name(), hir::UnitRole::kObjectRoot,
        with->second);
    if (!lowerer.Declare()) continue;
    auto one_body = lowerer.LowerScopeBuiltBy(*unit.body, signatures);
    if (!one_body) continue;
    auto adding =
        BodiesThatAdd(lowerer, *std::move(one_body), with->second, signatures);
    if (!adding.has_value()) {
      RemarkOnInstancesApart(definition, "lowered", held.apart, sink);
    } else if (!adding->empty()) {
      held.adding.emplace(unit.Name(), *std::move(adding));
    }
  }
  return held;
}

// Whether `body` holds, anywhere below it, an instance whose unit is named
// through the unit of `body`'s own definition: an instance of that definition
// (LRM 27.5 lets a conditional generate end such a recursion), or of a design
// element declared inside another (LRM 23.4), whose name starts with the unit
// of the instance declaring it.
auto HoldsAnInstanceNamedThroughItsUnit(
    const slang::ast::InstanceBodySymbol& body) -> bool {
  struct Finder : slang::ast::ASTVisitor<Finder> {
    const slang::ast::DefinitionSymbol* definition = nullptr;
    bool found = false;
    void handle(const slang::ast::InstanceSymbol& child) {
      const slang::ast::DefinitionSymbol& of = child.getDefinition();
      if (&of == definition) found = true;
      for (const slang::ast::Scope* level = of.getParentScope();
           level != nullptr && !found;
           level = level->asSymbol().getParentScope()) {
        found = level->asSymbol().kind == slang::ast::SymbolKind::InstanceBody;
      }
      if (!found) visitDefault(child);
    }
  };
  Finder finder;
  finder.definition = &body.getDefinition();
  body.visit(finder);
  return finder.found;
}

// The definitions a unit of which found, declaring, two scopes it took for
// objects of one class publishing different ones: two block instances taken
// for one application of their generate block, or two instances taken for one
// unit. Such a unit states a signature no body can be held against, and its
// definition is declared again with every parameter and every index of its
// loops deciding by its own value -- so the program is the one each scope
// describes and only the sharing is lost, which is said as a remark against
// the definition.
auto DefinitionsPublishedApart(
    std::span<const std::unique_ptr<UnitLowerer>> lowerers,
    diag::DiagnosticSink& sink) -> Definitions {
  Definitions apart;
  for (const std::unique_ptr<UnitLowerer>& lowerer : lowerers) {
    if (!lowerer->BlocksPublishedApart() &&
        !lowerer->ApplicationsPublishedApart()) {
      continue;
    }
    const auto& definition = lowerer->SourceScope()
                                 .asSymbol()
                                 .as<slang::ast::InstanceBodySymbol>()
                                 .getDefinition();
    if (lowerer->ApplicationsPublishedApart()) {
      RemarkOnInstancesApart(definition, "published", apart, sink);
    } else if (apart.insert(&definition).second) {
      sink.Report(
          diag::Make(
              frontend::PointSpanOf(definition.location),
              diag::DiagCode::kRemarkLostSharing,
              std::format(
                  "sharing lost: every generate block of '{}' is compiled "
                  "once per index, because blocks built at different indices "
                  "published apart",
                  definition.name)));
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
  // Where the design elements' units start among the lowerers, which follow
  // in the order the collection found them.
  std::size_t first_design_element = 0;
  hir::UnitSignatures signatures;
  // Held while a unit reads the frontend, which elaborates on first read.
  std::mutex reading_front_end;

  Units(
      std::unique_ptr<slang::ast::Compilation> elaborated,
      support::AssertionPolicy assertion_policy)
      : front_end(std::move(elaborated)),
        facts(
            sensitivity, assertion_policy, specialization,
            specialization_homes.placed) {
  }

  // The lowerer of a design element's unit, over the instance it is named for
  // and the others `also_lowered_from` holds for it.
  [[nodiscard]] auto LowererOf(
      const CollectedUnit& unit, const AlsoLoweredFrom& also_lowered_from) const
      -> std::unique_ptr<UnitLowerer> {
    const auto others = also_lowered_from.find(unit.Name());
    return std::make_unique<UnitLowerer>(
        facts, *unit.body, unit.Name(), hir::UnitRole::kObjectRoot,
        others == also_lowered_from.end()
            ? std::span<const slang::ast::InstanceBodySymbol* const>{}
            : std::span<const slang::ast::InstanceBodySymbol* const>{
                  others->second});
  }

  // Declares every unit the design has under the specialization policy held
  // here, replacing whatever an earlier policy declared. A design element's
  // unit is declared from every instance `also_lowered_from` holds for it, so
  // what it publishes states what each of them built and a name reaching into
  // any of them finds it. Whether any failed is the sink's answer.
  void DeclareEveryUnit(
      const LowerCompilationFacts& front_end_facts, const Collected& collected,
      const AlsoLoweredFrom& also_lowered_from, diag::DiagnosticSink& sink) {
    lowerers.clear();
    signatures = hir::UnitSignatures{};
    for (const auto* package : CollectPackages(front_end_facts)) {
      lowerers.push_back(
          std::make_unique<UnitLowerer>(
              facts, *package, std::string{package->name},
              hir::UnitRole::kNamespace,
              std::span<const slang::ast::InstanceBodySymbol* const>{}));
    }
    for (const auto* cu : CollectCompilationUnits(front_end_facts)) {
      // A `$unit` scope is lowered, emitted, and initialized exactly as a
      // package is -- a rootless namespace unit -- so it carries the same unit
      // kind; nothing downstream distinguishes the two, so there is no
      // separate kind.
      lowerers.push_back(
          std::make_unique<UnitLowerer>(
              facts, *cu, CompilationUnitName(*cu, specialization),
              hir::UnitRole::kNamespace,
              std::span<const slang::ast::InstanceBodySymbol* const>{}));
    }
    for (const slang::ast::ClassType* spec : specialization_homes.own_units) {
      lowerers.push_back(
          std::make_unique<UnitLowerer>(
              facts, *spec, CompilationUnitName(*spec, specialization),
              hir::UnitRole::kNamespace,
              std::span<const slang::ast::InstanceBodySymbol* const>{}));
    }
    first_design_element = lowerers.size();
    for (const CollectedUnit& unit : collected.units) {
      lowerers.push_back(LowererOf(unit, also_lowered_from));
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

  // Leaves each design element's unit lowered from the instances that built
  // something of it and no others. Every witness was declared into its unit so
  // that what the unit published is complete before any body is read; one that
  // built nothing the unit did not already hold has nothing more to give, and
  // a unit is held until its turn, so what it is held with is kept to what its
  // lowering reads. The unit publishes again from the instances it keeps, so
  // what every other unit reads of it is what its own lowering states.
  void KeepTheInstancesThatAdd(
      const Collected& collected, const AlsoLoweredFrom& declared_with,
      const AlsoLoweredFrom& adding, diag::DiagnosticSink& sink) {
    for (std::size_t at = 0; at < collected.units.size(); ++at) {
      const CollectedUnit& unit = collected.units[at];
      const auto declared = declared_with.find(unit.Name());
      if (declared == declared_with.end()) continue;
      const auto kept = adding.find(unit.Name());
      if (kept != adding.end() &&
          kept->second.size() == declared->second.size()) {
        continue;
      }
      std::unique_ptr<UnitLowerer> lowerer = LowererOf(unit, adding);
      if (auto redeclared = lowerer->Declare(); !redeclared) {
        sink.Report(std::move(redeclared.error()));
        continue;
      }
      signatures.Replace(lowerer->TakeSignature());
      lowerers[first_design_element + at] = std::move(lowerer);
    }
  }

  // Says which definitions were compiled into more units than their instances
  // need. The units of one definition are lowered into one unit under a policy
  // that hands that definition every value it is given, and held against each
  // other as the instances of one unit are. Where they come out one body,
  // whatever told them apart decided nothing that was compiled.
  //
  // It is a check and never an answer: a definition's units stay as they were
  // named, because naming a unit by how the design's other instances of it
  // lowered would make a parent's text depend on instances it does not hold.
  // Only what the trial cannot be run on is left out, and none of it says
  // anything was missed: a definition kept whole, which an earlier comparison
  // already found apart; one holding a class, whose bodies cannot be lowered
  // into one unit; and one holding an instance named through its own unit,
  // which the trial would name as nothing the design declared.
  void RemarkOnSharingMissed(
      const Collected& collected, diag::DiagnosticSink& sink) {
    std::vector<const slang::ast::DefinitionSymbol*> in_order;
    std::unordered_map<
        const slang::ast::DefinitionSymbol*, std::vector<const CollectedUnit*>>
        units_of;
    for (const CollectedUnit& unit : collected.units) {
      const auto& definition = unit.body->getDefinition();
      std::vector<const CollectedUnit*>& its = units_of[&definition];
      if (its.empty()) in_order.push_back(&definition);
      its.push_back(&unit);
    }
    for (const slang::ast::DefinitionSymbol* definition : in_order) {
      const std::vector<const CollectedUnit*>& its = units_of.at(definition);
      if (its.size() < 2 || specialization.KeepsWhole(*definition)) continue;
      if (std::ranges::any_of(its, [&](const CollectedUnit* unit) {
            return HoldsAClass(*unit->body, specialization_homes.placed) ||
                   HoldsAnInstanceNamedThroughItsUnit(*unit->body);
          })) {
        continue;
      }
      const SpecializationPolicy handed_every_value =
          specialization.HandingEveryValueTo(*definition);
      const LoweringFacts trial(
          sensitivity, facts.AssertionPolicy(), handed_every_value,
          specialization_homes.placed);
      std::vector<const slang::ast::InstanceBodySymbol*> others;
      for (const CollectedUnit* unit : its | std::views::drop(1)) {
        others.push_back(unit->body);
      }
      UnitLowerer lowerer(
          trial, *its.front()->body, its.front()->Name(),
          hir::UnitRole::kObjectRoot, others);
      if (!lowerer.Declare() || lowerer.BlocksPublishedApart() ||
          lowerer.ApplicationsPublishedApart()) {
        continue;
      }
      auto one_body = lowerer.LowerScopeBuiltBy(*its.front()->body, signatures);
      if (!one_body ||
          !BodiesThatAdd(lowerer, *std::move(one_body), others, signatures)
               .has_value()) {
        continue;
      }
      sink.Report(
          diag::Make(
              frontend::PointSpanOf(definition->location),
              diag::DiagCode::kRemarkMissedSharing,
              std::format(
                  "sharing missed: '{}' is compiled {} times, and its "
                  "instances are one body where each is built with the values "
                  "it is given",
                  definition->name, its.size())));
    }
  }
};

// A definition whose instances lowered apart, or whose block instances
// published apart, is declared again kept whole. A definition kept whole is
// supplied nothing at construction and tells every index of its loops apart,
// so neither can happen to it again. One whose unit holds what a single
// instance built is declared again as that before it is kept whole. Both sets
// grow every time round and a definition enters each once: at worst every
// definition is kept whole and each parameter value is its own unit. Nothing
// else reads the frontend until this returns, so the check reads it without
// taking turns.
auto DeclaredDesign::Declare(
    std::unique_ptr<slang::ast::Compilation> front_end,
    support::AssertionPolicy assertion_policy, support::BodiesRead bodies_read,
    MissedSharing missed_sharing, diag::DiagnosticSink& sink)
    -> std::optional<DeclaredDesign> {
  auto units = std::make_unique<Units>(std::move(front_end), assertion_policy);
  const LowerCompilationFacts facts(*units->front_end, assertion_policy);

  Definitions kept_whole;
  Definitions built_by_one_instance;
  for (;;) {
    units->specialization =
        SpecializationPolicy(kept_whole, bodies_read, built_by_one_instance);
    auto tops = TopLevelUnits(facts, units->specialization);
    if (!tops) {
      sink.Report(std::move(tops.error()));
      return std::nullopt;
    }
    const Collected collected = CollectUnits(facts, units->specialization);
    units->specialization_homes =
        CollectSpecializationHomes(facts, collected, units->specialization);
    const AlsoLoweredFrom declared_with = WitnessesLoweredWithTheirUnits(
        collected, units->specialization_homes.placed);
    units->DeclareEveryUnit(facts, collected, declared_with, sink);
    if (sink.HasErrors()) {
      return std::nullopt;
    }
    // What the signatures already show is settled before any body is lowered
    // against them.
    Definitions apart = DefinitionsPublishedApart(units->lowerers, sink);
    if (apart.empty()) {
      InstancesHeldToTheirUnits held = HoldInstancesToTheirUnits(
          units->facts, collected, declared_with, built_by_one_instance,
          units->signatures, sink);
      apart = std::move(held.apart);
      const bool settled = apart.empty() && held.built_by_one_instance.empty();
      built_by_one_instance.merge(held.built_by_one_instance);
      if (settled) {
        units->KeepTheInstancesThatAdd(
            collected, declared_with, held.adding, sink);
        if (sink.HasErrors()) {
          return std::nullopt;
        }
        switch (missed_sharing) {
          case MissedSharing::kNotLookedFor:
            break;
          case MissedSharing::kLookedFor:
            units->RemarkOnSharingMissed(collected, sink);
            break;
        }
        units->tops = *std::move(tops);
        return DeclaredDesign(std::move(units));
      }
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
