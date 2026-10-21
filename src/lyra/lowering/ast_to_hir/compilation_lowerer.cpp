#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <expected>
#include <format>
#include <memory>
#include <mutex>
#include <optional>
#include <span>
#include <string>
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
#include <slang/ast/symbols/MemberSymbols.h>
#include <slang/ast/symbols/PortSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/AllTypes.h>

#include "lyra/base/diag_message.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/sink.hpp"
#include "lyra/frontend/slang_source_span.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/unit_body_ref.hpp"
#include "lyra/hir/unit_signatures.hpp"
#include "lyra/lowering/ast_to_hir/declaration_scopes.hpp"
#include "lyra/lowering/ast_to_hir/lower.hpp"
#include "lyra/lowering/ast_to_hir/name_in_a_path.hpp"
#include "lyra/lowering/ast_to_hir/sensitivity.hpp"
#include "lyra/lowering/ast_to_hir/std_classes.hpp"
#include "lyra/lowering/ast_to_hir/unit_identity.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// The role of a unit compiled from the design element `body` was elaborated
// from.
auto RoleOf(const slang::ast::InstanceBodySymbol& body) -> hir::UnitRole {
  switch (body.getDefinition().definitionKind) {
    case slang::ast::DefinitionKind::Module:
    case slang::ast::DefinitionKind::Interface:
      return hir::UnitRole::kModuleOrInterface;
    case slang::ast::DefinitionKind::Program:
      return hir::UnitRole::kProgram;
  }
  throw InternalError("RoleOf: a design element of no kind");
}

// An instance read through a body of its own: what the front end elaborated
// for it, the unit it is an object of, and the name of the body it is compiled
// to. What was elaborated belongs to an instance the names were computed from,
// so what the unit compiles against and what it was named for are one
// application of a definition. A body the frontend elaborated for a different
// definition's application states different types at the same positions, which
// is a unit nothing in the design asked for.
struct CollectedUnit {
  const slang::ast::InstanceBodySymbol* elaborated;
  std::string unit;
  std::string body;
};

// What the collection found: the distinct bodies, and, where the compile
// checks itself, every other instance read through a body of its own, each of
// which is compiled to one of them. An instance the front end left sharing
// another's body is not among them.
//
// A repeat is one body with an instance found before it: the body's name holds
// everything that changes what is compiled, the blocks its construction built
// included, so it builds what that instance does and is read only to check the
// compile against itself.
struct Collected {
  std::vector<CollectedUnit> units;
  std::vector<CollectedUnit> repeats;
};

// Collects the distinct units reachable from the tops. slang owns the
// structural descent: visiting an instance recurses into its body, and every
// container (generate blocks, instance arrays) is a Scope the visitor walks
// through, so an instance nested in a generate block is reached without this
// code knowing the container taxonomy.
//
// Two instances are compiled to one body exactly when they agree on that
// body's name, because the name stands for what identifies it: a definition,
// everything fixed for it that the unit declares, and the values its code
// uses. Two instances agreeing on it are one body however many bodies the
// frontend chose to elaborate -- it declines to share one when a name inside
// reaches outward, since the resolution differs per instance, but that
// resolution is settled per instance at construction and never inside the unit,
// so what is compiled is the same.
//
// Descending reaches an occurrence's own body, so a child is collected under
// what its own parent fixed for it. An occurrence whose body is one already
// found is not descended into, whether or not the front end left it sharing
// that body: what is below it was collected under the instance found first,
// and only the compile checking itself reads it again.
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
  // An interface every virtual interface of one type names is one instance
  // however many declarations name it, and is collected once.
  std::unordered_set<const slang::ast::InstanceSymbol*> met;
  std::unordered_set<std::string> named;
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
    if (!met.insert(&inst).second) return;
    std::string body = policy->BodyNameOf(inst);
    if (!policy->HasBodyOfItsOwn(inst)) {
      // One body with an instance already found: nothing of it is read, not
      // its body and so nothing below it either.
      if (named.contains(body)) return;
      // The first instance its body is met through is read through its own
      // body after all, and named by what that body says.
      policy->ReadBodyOf(inst);
      body = policy->BodyNameOf(inst);
    }
    const bool fresh = named.insert(body).second;
    if (!fresh && !policy->ChecksItself()) return;
    (fresh ? found.units : found.repeats)
        .push_back(
            CollectedUnit{
                .elaborated = &inst.body,
                .unit = policy->NameOf(inst),
                .body = std::move(body)});
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
  // `getPackages` includes the built-in `std` package (LRM 26.7), which the
  // front end declares with no body; what its classes do is either the
  // runtime's or Lyra's own definitions compiled in its name, so the built-in
  // is never lowered. `std` can not be declared in source, so matching the
  // name excludes exactly the built-in.
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
// to become a unit for that name to reach a definition. So is what an
// anonymous program declares, since it opens no scope of its own (LRM 24.6).
auto HasUnitScopeContent(const slang::ast::Scope& scope) -> bool {
  for (const auto& member : scope.members()) {
    switch (member.kind) {
      case slang::ast::SymbolKind::Variable:
      case slang::ast::SymbolKind::Net:
      case slang::ast::SymbolKind::Subroutine:
      case slang::ast::SymbolKind::TypeAlias:
      case slang::ast::SymbolKind::ClassType:
      case slang::ast::SymbolKind::GenericClassDef:
        return true;
      case slang::ast::SymbolKind::AnonymousProgram:
        if (HasUnitScopeContent(
                member.as<slang::ast::AnonymousProgramSymbol>())) {
          return true;
        }
        break;
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

// The front end makes a specialization where an expression naming it is first
// bound, and an instance body it judged a duplicate of another is bound only
// when something reads it. So every body a unit is lowered from is bound before
// specializations are collected; otherwise one appears only once its unit is
// already lowering, after the scope replicating it has settled what it holds.
void BindEveryBody(const Collected& collected) {
  BindsEveryExpression binder;
  for (const auto* bodies : {&collected.units, &collected.repeats}) {
    for (const CollectedUnit& unit : *bodies) {
      unit.elaborated->visit(binder);
    }
  }
}

auto CollectSpecializationHomes(
    const LowerCompilationFacts& facts, const SpecializationPolicy& policy)
    -> SpecializationHomes {
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

auto WhyItMustBeConnected(PortConnectionRule rule) -> base::DiagMessage {
  switch (rule) {
    // LRM 23.3.3.4.
    case PortConnectionRule::kInterfacePort:
      return base::DiagMessage("an interface port cannot be left unconnected");
    // LRM 23.3.3.2.
    case PortConnectionRule::kRefPort:
      return base::DiagMessage("a 'ref' port cannot be left unconnected");
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
              "'{}' cannot be a simulation top because nothing "
              "instantiates a top to connect its ports, and {}",
              inst->name, WhyItMustBeConnected(required->rule).Text())
              .WithNote(
                  "instantiate '{}' from a module that connects this port, "
                  "and make that module the top",
                  inst->name));
    }
    tops.emplace_back(
        TopLevelUnit{
            .instance_name = std::string{inst->name},
            .name_in_a_path = NameInAPath(*inst),
            .unit_name = policy.NameOf(*inst)});
  }
  return tops;
}

// The instances each compiled body is checked against, by the body's name.
using BodiesCheckedAgainst = std::unordered_map<
    std::string, std::vector<const slang::ast::InstanceBodySymbol*>>;

// The instances each compiled body is lowered from beside the one it is named
// for, which come out the same as that one: every repeat, which is where one
// that lowers to anything else is found.
auto CheckedAgainstOf(const Collected& collected) -> BodiesCheckedAgainst {
  BodiesCheckedAgainst checked;
  for (const CollectedUnit& repeat : collected.repeats) {
    checked[repeat.body].push_back(repeat.elaborated);
  }
  return checked;
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
  StdDefinitions std_classes;
  SpecializationHomes specialization_homes;
  LoweringFacts facts;
  std::vector<TopLevelUnit> tops;
  std::vector<std::unique_ptr<UnitLowerer>> lowerers;
  hir::UnitSignatures signatures;
  // Held while a unit reads the frontend, which elaborates on first read.
  std::mutex reading_front_end;

  Units(
      std::unique_ptr<slang::ast::Compilation> elaborated,
      support::AssertionPolicy assertion_policy)
      : front_end(std::move(elaborated)),
        facts(
            sensitivity, assertion_policy, specialization,
            specialization_homes.placed, std_classes) {
  }

  // The lowerer of one body of a design element's unit, over the instance it
  // is named for, checked against the others `checked_against` holds for it.
  [[nodiscard]] auto LowererOf(
      const CollectedUnit& collected,
      const BodiesCheckedAgainst& checked_against) const
      -> std::unique_ptr<UnitLowerer> {
    const auto others = checked_against.find(collected.body);
    return std::make_unique<UnitLowerer>(
        facts, *collected.elaborated,
        hir::UnitBodyRef{.unit = collected.unit, .body = collected.body},
        RoleOf(*collected.elaborated),
        others == checked_against.end()
            ? std::span<const slang::ast::InstanceBodySymbol* const>{}
            : std::span<const slang::ast::InstanceBodySymbol* const>{
                  others->second});
  }

  // Declares every unit the design has under the specialization policy held
  // here. Every body of a design element's unit declares what the unit
  // publishes, from the instance it is named for: the unit's name holds
  // everything the declarations depend on, the blocks its instances built
  // included, so every body states it alike and the design keeps the first.
  // One stating anything else is this compiler's defect, which the compile
  // looks for where it checks itself. Whether any failed is the sink's answer.
  void DeclareEveryUnit(
      const LowerCompilationFacts& front_end_facts, const Collected& collected,
      const BodiesCheckedAgainst& checked_against, diag::DiagnosticSink& sink) {
    for (const auto* package : CollectPackages(front_end_facts)) {
      lowerers.push_back(
          std::make_unique<UnitLowerer>(
              facts, *package, hir::UnitBodyRef::Sole(package->name),
              hir::UnitRole::kNamespace));
    }
    for (const auto* cu : CollectCompilationUnits(front_end_facts)) {
      // A `$unit` scope is lowered, emitted, and initialized exactly as a
      // package is -- a rootless namespace unit -- so it carries the same unit
      // kind; nothing downstream distinguishes the two, so there is no
      // separate kind.
      lowerers.push_back(
          std::make_unique<UnitLowerer>(
              facts, *cu,
              hir::UnitBodyRef::Sole(CompilationUnitName(*cu, specialization)),
              hir::UnitRole::kNamespace));
    }
    for (const slang::ast::ClassType* spec : specialization_homes.own_units) {
      lowerers.push_back(
          std::make_unique<UnitLowerer>(
              facts, *spec,
              hir::UnitBodyRef::Sole(
                  CompilationUnitName(*spec, specialization)),
              hir::UnitRole::kNamespace));
    }
    for (const CollectedUnit& body : collected.units) {
      lowerers.push_back(LowererOf(body, checked_against));
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
      hir::UnitSignature signature = lowerer->TakeSignature();
      const hir::UnitSignature* kept = signatures.Find(signature.unit_name);
      if (kept == nullptr) {
        signatures.Publish(std::move(signature));
        continue;
      }
      if (specialization.ChecksItself() && *kept != signature) {
        throw InternalError(
            std::format(
                "DeclaredDesign: two bodies of unit '{}' publish different "
                "declarations, though its name holds everything they depend on",
                signature.unit_name));
      }
    }
  }
};

// Nothing else reads the frontend until this returns, so it is read without
// taking turns.
auto DeclaredDesign::Declare(
    std::unique_ptr<slang::ast::Compilation> front_end,
    support::AssertionPolicy assertion_policy, support::BodiesRead bodies_read,
    diag::DiagnosticSink& sink) -> std::optional<DeclaredDesign> {
  auto units = std::make_unique<Units>(std::move(front_end), assertion_policy);
  const LowerCompilationFacts facts(*units->front_end, assertion_policy);

  units->specialization = SpecializationPolicy(bodies_read);
  auto tops = TopLevelUnits(facts, units->specialization);
  if (!tops) {
    sink.Report(std::move(tops.error()));
    return std::nullopt;
  }
  const Collected collected = CollectUnits(facts, units->specialization);
  BindEveryBody(collected);
  units->std_classes = DefineStdClasses(facts.Compilation());
  units->specialization_homes =
      CollectSpecializationHomes(facts, units->specialization);
  units->DeclareEveryUnit(facts, collected, CheckedAgainstOf(collected), sink);
  if (sink.HasErrors()) {
    return std::nullopt;
  }
  units->tops = *std::move(tops);
  return DeclaredDesign(std::move(units));
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
