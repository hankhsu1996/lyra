#include "lyra/lowering/ast_to_hir/structural_scope_lowerer.hpp"

#include <algorithm>
#include <array>
#include <cstdint>
#include <expected>
#include <iterator>
#include <optional>
#include <span>
#include <string>
#include <unordered_set>
#include <utility>
#include <variant>
#include <vector>

#include <slang/ast/Compilation.h>
#include <slang/ast/HierarchicalReference.h>
#include <slang/ast/Scope.h>
#include <slang/ast/SemanticFacts.h>
#include <slang/ast/Statement.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/TimingControl.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/MemberSymbols.h>
#include <slang/ast/symbols/ParameterSymbols.h>
#include <slang/ast/symbols/PortSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/ValueSymbol.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/AllTypes.h>
#include <slang/ast/types/NetType.h>
#include <slang/numeric/Time.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/failure_context.hpp"
#include "lyra/frontend/slang_source_span.hpp"
#include "lyra/hir/clocking_block.hpp"
#include "lyra/hir/continuous_assign.hpp"
#include "lyra/hir/dumped_declaration.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/expr_builders.hpp"
#include "lyra/hir/structural_data_object.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/unit_definition.hpp"
#include "lyra/lowering/ast_to_hir/canonical_body.hpp"
#include "lyra/lowering/ast_to_hir/clocking_block.hpp"
#include "lyra/lowering/ast_to_hir/constant_value.hpp"
#include "lyra/lowering/ast_to_hir/generate_construct.hpp"
#include "lyra/lowering/ast_to_hir/hierarchy_override.hpp"
#include "lyra/lowering/ast_to_hir/inferred_clocking.hpp"
#include "lyra/lowering/ast_to_hir/instance_array_shape.hpp"
#include "lyra/lowering/ast_to_hir/integral_constant.hpp"
#include "lyra/lowering/ast_to_hir/name_in_a_path.hpp"
#include "lyra/lowering/ast_to_hir/net_overlay.hpp"
#include "lyra/lowering/ast_to_hir/net_type.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/reads.hpp"
#include "lyra/lowering/ast_to_hir/sampled_reads.hpp"
#include "lyra/lowering/ast_to_hir/statement/assertions.hpp"
#include "lyra/lowering/ast_to_hir/statement/timing.hpp"
#include "lyra/lowering/ast_to_hir/strength.hpp"
#include "lyra/lowering/ast_to_hir/subroutine_decl.hpp"
#include "lyra/lowering/ast_to_hir/time_resolution.hpp"
#include "lyra/lowering/ast_to_hir/unit_definition.hpp"
#include "lyra/lowering/ast_to_hir/unit_identity.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"
#include "lyra/profiling/time_trace.hpp"
#include "lyra/support/dump_kind.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// What a variable of `declared` type is recorded as in a value dump, by the
// type mapping (LRM 21.7.5, Table 21-11): an int and an enumeration are
// integers, an enumeration whose base type is stated is recorded as that type,
// and every other integral type is a reg.
auto DumpedKindOfVariableType(const slang::ast::Type& declared)
    -> std::optional<support::DumpVarKind> {
  const slang::ast::Type& type = declared.getCanonicalType();
  if (type.isEvent()) {
    return support::DumpVarKind::kEvent;
  }
  if (const auto* floating = type.as_if<slang::ast::FloatingType>()) {
    switch (floating->floatKind) {
      case slang::ast::FloatingType::Real:
      case slang::ast::FloatingType::ShortReal:
        return support::DumpVarKind::kReal;
      case slang::ast::FloatingType::RealTime:
        return support::DumpVarKind::kRealtime;
    }
    return std::nullopt;
  }
  if (const auto* enumeration = type.as_if<slang::ast::EnumType>()) {
    return DumpedKindOfVariableType(enumeration->baseType);
  }
  if (const auto* predefined =
          type.as_if<slang::ast::PredefinedIntegerType>()) {
    switch (predefined->integerKind) {
      case slang::ast::PredefinedIntegerType::Integer:
      case slang::ast::PredefinedIntegerType::Int:
        return support::DumpVarKind::kInteger;
      case slang::ast::PredefinedIntegerType::Time:
        return support::DumpVarKind::kTime;
      case slang::ast::PredefinedIntegerType::ShortInt:
      case slang::ast::PredefinedIntegerType::LongInt:
      case slang::ast::PredefinedIntegerType::Byte:
        return support::DumpVarKind::kReg;
    }
    return std::nullopt;
  }
  if (type.isIntegral()) {
    return support::DumpVarKind::kReg;
  }
  return std::nullopt;
}

// What a net or a variable is recorded as in a value dump (LRM 21.7.2.1
// `var_type`), none for one a dump records nothing of: a variable holding no
// integral value, real or event -- an unpacked array or structure, a string, a
// handle, a container (LRM 21.7.2.1) -- one of automatic lifetime, which no
// dump can reach, a net of a type the language gives no keyword there, and a
// net holding neither an integral value nor a real. A net holding a real is an
// input port of that type written with no port kind (LRM 23.2.2.3), and is
// recorded by its value, as a variable of its type is, since no net type
// keyword names it.
auto DumpedKindOf(const slang::ast::ValueSymbol& value)
    -> std::optional<support::DumpVarKind> {
  if (const auto* net = value.as_if<slang::ast::NetSymbol>()) {
    if (value.getType().isFloating()) {
      return DumpedKindOfVariableType(value.getType());
    }
    if (!value.getType().isIntegral()) {
      return std::nullopt;
    }
    switch (net->netType.netKind) {
      case slang::ast::NetType::Wire:
      case slang::ast::NetType::UWire:
        // LRM 21.7.2.3: a `uwire` is recorded as a `wire`.
        return support::DumpVarKind::kWire;
      case slang::ast::NetType::Tri:
        return support::DumpVarKind::kTri;
      case slang::ast::NetType::WAnd:
        return support::DumpVarKind::kWand;
      case slang::ast::NetType::TriAnd:
        return support::DumpVarKind::kTriand;
      case slang::ast::NetType::WOr:
        return support::DumpVarKind::kWor;
      case slang::ast::NetType::TriOr:
        return support::DumpVarKind::kTrior;
      case slang::ast::NetType::Tri0:
        return support::DumpVarKind::kTri0;
      case slang::ast::NetType::Tri1:
        return support::DumpVarKind::kTri1;
      case slang::ast::NetType::Supply0:
        return support::DumpVarKind::kSupply0;
      case slang::ast::NetType::Supply1:
        return support::DumpVarKind::kSupply1;
      case slang::ast::NetType::TriReg:
        return support::DumpVarKind::kTrireg;
      case slang::ast::NetType::Interconnect:
      case slang::ast::NetType::UserDefined:
      case slang::ast::NetType::Unknown:
        return std::nullopt;
    }
    return std::nullopt;
  }
  const auto* variable = value.as_if<slang::ast::VariableSymbol>();
  if (variable == nullptr || !HasStaticStorage(*variable)) return std::nullopt;
  return DumpedKindOfVariableType(value.getType());
}

// How many bits a dump declares a thing of `kind` and type `type` with: an
// event the one bit the format gives it, a real a double's.
auto DumpedWidthOf(support::DumpVarKind kind, const slang::ast::Type& type)
    -> std::uint32_t {
  switch (support::DumpValueFormOf(kind)) {
    case support::DumpValueForm::kTriggered:
      return 1;
    case support::DumpValueForm::kReal:
      return 64;
    case support::DumpValueForm::kBits:
      return static_cast<std::uint32_t>(type.getBitWidth());
  }
  throw InternalError("DumpedWidthOf: unknown value");
}

// The range a vector declares, which a dump writes beside its name (LRM
// 21.7.2.3 `$var`): the outermost of a packed array's, none for anything else.
auto DumpedRangeOf(const slang::ast::Type& type)
    -> std::optional<hir::DumpedDeclaration::Range> {
  const auto* packed =
      type.getCanonicalType().as_if<slang::ast::PackedArrayType>();
  if (packed == nullptr) {
    return std::nullopt;
  }
  return hir::DumpedDeclaration::Range{
      .left = packed->range.left, .right = packed->range.right};
}

// Each port `scope` declares, by the net or variable inside the instance it
// connects, with its direction and its position among the ports in the order
// the declaration gives them -- a port written as a concatenation being one
// position for each part (LRM 21.7.4.2). A port connecting nothing inside, or
// one connecting storage by reference, is no port an extended dump records.
auto DumpedPortsOf(const slang::ast::Scope& scope) -> DumpedPorts {
  DumpedPorts ports;
  const auto* body = scope.asSymbol().as_if<slang::ast::InstanceBodySymbol>();
  if (body == nullptr) {
    return ports;
  }
  std::uint32_t position = 0;
  const auto take = [&](const slang::ast::PortSymbol& port) {
    const std::uint32_t at = position++;
    std::optional<support::DumpPortDirection> direction;
    switch (port.direction) {
      case slang::ast::ArgumentDirection::In:
        direction = support::DumpPortDirection::kInput;
        break;
      case slang::ast::ArgumentDirection::Out:
        direction = support::DumpPortDirection::kOutput;
        break;
      case slang::ast::ArgumentDirection::InOut:
        direction = support::DumpPortDirection::kInOut;
        break;
      case slang::ast::ArgumentDirection::Ref:
        break;
    }
    if (direction.has_value() && port.internalSymbol != nullptr) {
      ports.try_emplace(
          port.internalSymbol, hir::DumpedDeclaration::Port{
                                   .direction = *direction, .position = at});
    }
  };
  for (const slang::ast::Symbol* port : body->getPortList()) {
    if (const auto* single = port->as_if<slang::ast::PortSymbol>()) {
      take(*single);
    } else if (const auto* multi = port->as_if<slang::ast::MultiPortSymbol>()) {
      for (const slang::ast::PortSymbol* part : multi->ports) {
        take(*part);
      }
    }
  }
  return ports;
}

// Whether `type`, or the type of any member or element a value of it holds,
// satisfies `leaf`.
template <typename Leaf>
auto HoldsType(const slang::ast::Type& type, const Leaf& leaf) -> bool {
  const slang::ast::Type& canonical = type.getCanonicalType();
  if (leaf(canonical)) {
    return true;
  }
  const auto any_field =
      [&](std::span<const slang::ast::FieldSymbol* const> fields) {
        return std::ranges::any_of(
            fields, [&](const slang::ast::FieldSymbol* field) {
              return HoldsType(field->getType(), leaf);
            });
      };
  if (const auto* overlay = canonical.as_if<slang::ast::UnpackedUnionType>()) {
    return any_field(overlay->fields);
  }
  if (const auto* product = canonical.as_if<slang::ast::UnpackedStructType>()) {
    return any_field(product->fields);
  }
  if (const auto* array =
          canonical.as_if<slang::ast::FixedSizeUnpackedArrayType>()) {
    return HoldsType(array->elementType, leaf);
  }
  return false;
}

// An unpacked tagged union, which LRM 6.7.1 admits in a net's data type, and
// whose drivers no resolution is carried out for.
auto IsTaggedUnion(const slang::ast::Type& type) -> bool {
  const auto* overlay = type.as_if<slang::ast::UnpackedUnionType>();
  return overlay != nullptr && overlay->isTagged;
}

// A value whose size the running program decides, or an event: a net of one
// is what an input port written with no port kind is (LRM 23.2.2.3), and a
// net holding one is not yet carried out.
auto IsDynamicOrEvent(const slang::ast::Type& type) -> bool {
  return type.isQueue() || type.isAssociativeArray() ||
         type.as_if<slang::ast::DynamicArrayType>() != nullptr ||
         type.isEvent();
}

// Where one parameter an instance takes at construction gets its value: the
// expression its instantiation wrote (LRM 23.10.2); a value given elsewhere --
// a defparam, a configuration (LRM 23.10.1, 33.4.3) -- which is handed as the
// constant it settled to; or none at all, which leaves it to the default its
// declaration states (LRM 23.10.2.2).
//
// Two sources are equal where they build elements of one instantiation alike.
// The instantiation writes one assignment for every element (LRM 23.3.2), so
// two written arguments are the same expression and two left to the default
// are the same default, while a value given elsewhere is told apart by the
// constant it settled to.
struct WrittenAtInstantiation {
  auto operator==(const WrittenAtInstantiation&) const -> bool = default;
};
struct GivenElsewhere {
  std::string settled;

  auto operator==(const GivenElsewhere&) const -> bool = default;
};
struct LeftToDefault {
  auto operator==(const LeftToDefault&) const -> bool = default;
};
struct ArgumentSource {
  const slang::ast::ParameterSymbol* parameter;
  std::variant<WrittenAtInstantiation, GivenElsewhere, LeftToDefault> from;
};

// How one instance is built: the unit and the lowering it is made as, and
// where each parameter it takes at construction gets its value, in the unit's
// own order.
struct InstanceConstruction {
  hir::LoweringRef made;
  std::vector<ArgumentSource> arguments;
};

auto ConstructionOf(
    const slang::ast::InstanceSymbol& instance, const InstanceNames& names,
    const ElaboratedInstances& instances) -> InstanceConstruction {
  InstanceConstruction built{
      .made =
          hir::LoweringRef{
              .unit = names.NameOf(instance),
              .lowering = LoweringNameOf(instance)},
      .arguments = {}};
  for (const slang::ast::ParameterSymbol* param :
       instances.SuppliedParametersOf(instance)) {
    ArgumentSource source{.parameter = param, .from = LeftToDefault{}};
    if (ValueWrittenAtInstantiation(instance, *param) != nullptr) {
      source.from = WrittenAtInstantiation{};
    } else if (GivenAValue(instance.body, *param)) {
      source.from = GivenElsewhere{.settled = ValueIdentity(param->getValue())};
    }
    built.arguments.push_back(std::move(source));
  }
  return built;
}

// Whether the code building two elements of one instantiation is the same: an
// object of one unit, handed its values from the same places. Which
// specialization each makes is the object's construction and no part of that
// code. Each element has parameters of its own, so the arguments are compared
// by where each gets its value.
auto BuiltAlike(const InstanceConstruction& a, const InstanceConstruction& b)
    -> bool {
  return a.made.unit == b.made.unit &&
         std::ranges::equal(
             a.arguments, b.arguments, {}, &ArgumentSource::from,
             &ArgumentSource::from);
}

// What `child`'s instantiation says of each parameter it takes at
// construction, as `built` says. The front end binds the instantiation's own
// assignment where it is written (LRM 23.10.2), so that is lowered here,
// against this scope's names. A value given elsewhere was written in another
// scope and is part of what this unit is, so it is handed as the constant it
// settled to. A default is the child's own expression, so for one this scope
// states only that no value was assigned.
auto LowerParameterAssignments(
    StructuralScopeLowerer& lowerer, const InstanceConstruction& built,
    const slang::ast::InstanceSymbol& child, WalkFrame frame)
    -> diag::Result<std::vector<hir::ParameterValueAssignment>> {
  UnitLowerer& owner = lowerer.Owner();
  const auto span = frontend::PointSpanOf(child.location);
  // Each value is handed at the type the child's unit published for it, which
  // is what its object entry is declared with.
  const hir::UnitSignature& signature =
      owner.Signatures().Instantiated(built.made.unit);
  const std::vector<hir::TypeId>& handed =
      hir::DesignElementOf(signature).handed;
  if (handed.size() != built.arguments.size()) {
    throw InternalError(
        std::format(
            "ast_to_hir: '{}' is built with {} values where its unit publishes "
            "{} -- please report this as a bug",
            child.name, built.arguments.size(), handed.size()));
  }
  std::vector<hir::ParameterValueAssignment> assignments;
  for (std::size_t i = 0; i < built.arguments.size(); ++i) {
    const ArgumentSource& source = built.arguments[i];
    const hir::TypeId type = owner.ImportSignatureType(signature, handed[i]);
    auto value = std::visit(
        Overloaded{
            [&](const WrittenAtInstantiation&)
                -> diag::Result<std::optional<hir::ExprId>> {
              auto lowered = lowerer.LowerExpr(
                  *ValueWrittenAtInstantiation(child, *source.parameter),
                  frame);
              if (!lowered) return std::unexpected(std::move(lowered.error()));
              return frame.Exprs().Add(*std::move(lowered));
            },
            [&](const GivenElsewhere&)
                -> diag::Result<std::optional<hir::ExprId>> {
              auto constant = MakeConstantValueExpr(
                  owner, frame, source.parameter->getValue(), type, span);
              if (!constant) {
                return std::unexpected(std::move(constant.error()));
              }
              return frame.Exprs().Add(*std::move(constant));
            },
            [](const LeftToDefault&)
                -> diag::Result<std::optional<hir::ExprId>> {
              return std::nullopt;
            }},
        source.from);
    if (!value) return std::unexpected(std::move(value.error()));
    assignments.push_back(
        hir::ParameterValueAssignment{.type = type, .value = *value});
  }
  return assignments;
}

}  // namespace

// The declaration of the child objects `elements` are, in row-major order of
// their positions, under the name `declared` was given; `dims` carries the
// range each dimension of an array declares, a scalar instance being the empty
// case rather than a shape of its own. What each is comes off this unit's
// record of the class its unit's instances are, never from the unit's own
// name.
//
// Every element is described by how it is built, and elements built alike are
// one alternative. Each alternative's assignments are lowered once and stored
// among this scope's expressions, which is where the construction reads them;
// which specialization each element makes is this object's construction.
auto StructuralScopeLowerer::BuildInstanceMember(
    const slang::ast::Symbol& declared,
    std::span<const slang::ast::InstanceSymbol* const> elements,
    std::vector<hir::UnpackedRange> dims, WalkFrame frame)
    -> diag::Result<BuiltInstanceMember> {
  BuiltInstanceMember built_member{
      .member =
          hir::InstanceMemberDecl{
              .instance_name = NameInAPath(declared),
              .instantiated = DesignElementKindOf(
                  elements.front()->getDefinition().definitionKind),
              .array_dims = std::move(dims),
              .alternatives = {},
              .taken = {}},
      .made = {}};
  hir::InstanceMemberDecl& member = built_member.member;
  member.taken.reserve(elements.size());
  std::vector<InstanceConstruction> distinct;
  for (const slang::ast::InstanceSymbol* element : elements) {
    InstanceConstruction built =
        ConstructionOf(*element, owner_->Names(), owner_->Instances());
    built_member.made.push_back(built.made);
    const auto known =
        std::ranges::find_if(distinct, [&](const InstanceConstruction& other) {
          return BuiltAlike(other, built);
        });
    if (known != distinct.end()) {
      member.taken.push_back(
          static_cast<std::uint32_t>(known - distinct.begin()));
      continue;
    }
    auto assignments = LowerParameterAssignments(*this, built, *element, frame);
    if (!assignments) return std::unexpected(std::move(assignments.error()));
    member.taken.push_back(
        static_cast<std::uint32_t>(member.alternatives.size()));
    member.alternatives.push_back(
        hir::InstanceAlternative{
            .scope_class = owner_->ScopeClassOf(built.made.unit),
            .assignments = *std::move(assignments)});
    distinct.push_back(std::move(built));
  }
  return built_member;
}

auto StructuralScopeLowerer::Run(WalkFrame parent_frame)
    -> diag::Result<BuiltScope> {
  const profiling::TimeTraceScope span("lower scope", [this] {
    return slang_scope_->asSymbol().getHierarchicalPath();
  });
  hir::StructuralScope scope;
  owner_->BeginScope(frame_, *this, scope);
  // Filling a declaration is defining the identity a peer may already hold,
  // which is why this scope takes the pools rather than growing its own.
  ScopeDeclarations declarations = owner_->TakeScopeDeclarations(*slang_scope_);
  scope.structural_data_objects =
      std::move(declarations.structural_data_objects);
  scope.structural_subroutines = std::move(declarations.structural_subroutines);
  scope.processes = std::move(declarations.processes);
  scope.generates = std::move(declarations.generates);
  construction_.generates.resize(scope.generates.size());
  scope.instance_members = std::move(declarations.instance_members);
  construction_.instances.resize(scope.instance_members.size());
  scope.procedural_scopes = std::move(declarations.procedural_scopes);
  scope.clocking_blocks = std::move(declarations.clocking_blocks);
  const WalkFrame frame =
      parent_frame.WithStructuralFrame(frame_, slang_scope_, &scope)
          .WithProceduralScopeOwner(slang_scope_, &scope.procedural_scopes);
  const std::optional<slang::TimeScale> time_scale =
      slang_scope_->getTimeScale();
  scope.time_resolution = ResolveTimeResolution(time_scale);
  scope.time_resolution_stated = time_scale.has_value();

  // Declared before anything else so that a name reaching one during the walk
  // below resolves to the declaration rather than folding to what one
  // elaboration gave it.
  auto parameters = DeclareDifferingParameters(scope, frame);
  if (!parameters) return std::unexpected(std::move(parameters.error()));

  // Structural members (variables, instances, generates, subroutine bodies)
  // are lowered before the members that name them (processes, continuous
  // assigns, and the aliases that state which of this scope's nets are one
  // physical net), so such a member resolves a reference to a declaration it
  // textually precedes -- declarations are scope-wide (LRM 27).
  for (const auto& member : slang_scope_->members()) {
    if (!owner_->Owns(member) ||
        member.kind == slang::ast::SymbolKind::ProceduralBlock ||
        member.kind == slang::ast::SymbolKind::ContinuousAssign ||
        member.kind == slang::ast::SymbolKind::NetAlias) {
      continue;
    }
    auto r = PopulateMember(member, frame);
    if (!r) return std::unexpected(std::move(r.error()));
  }

  // A definition reaches the interface through a port or an instance, either
  // of which the source may declare after it, so it is recorded once both are.
  for (const auto& member : slang_scope_->members()) {
    const auto* subroutine = member.as_if<slang::ast::SubroutineSymbol>();
    if (subroutine == nullptr || !owner_->Owns(member) ||
        !ExternDefinitionPrefix(*subroutine).has_value()) {
      continue;
    }
    auto r = RecordExternDefinition(*subroutine, frame);
    if (!r) return std::unexpected(std::move(r.error()));
  }

  // The classes this scope replicates, lowered where a process of the scope
  // is: every declaration a class body may name is bound by now, and the
  // references it records against this scope are recorded before the scope
  // takes them.
  auto classes = owner_->PopulateClassBodiesReplicatedBy(*slang_scope_);
  if (!classes) return std::unexpected(std::move(classes.error()));
  scope.replicated_classes = *std::move(classes);
  auto covergroups = owner_->PopulateCovergroupsReplicatedBy(*slang_scope_);
  if (!covergroups) return std::unexpected(std::move(covergroups.error()));
  scope.replicated_covergroups = *std::move(covergroups);

  for (const auto& member : slang_scope_->members()) {
    if (!owner_->Owns(member) ||
        (member.kind != slang::ast::SymbolKind::ProceduralBlock &&
         member.kind != slang::ast::SymbolKind::ContinuousAssign &&
         member.kind != slang::ast::SymbolKind::NetAlias)) {
      continue;
    }
    auto r = PopulateMember(member, frame);
    if (!r) return std::unexpected(std::move(r.error()));
  }

  // A variable port connection is an implied continuous assignment
  // (LRM 23.3.3), synthesized after every variable and instance binding
  // exists so its source and child-side endpoint resolve regardless of source
  // order.
  auto pc = PopulatePortConnections(*slang_scope_, frame);
  if (!pc) return std::unexpected(std::move(pc.error()));

  auto dumped = CollectDumped(
      *slang_scope_, DumpedPortsOf(*slang_scope_), frame, std::nullopt);
  if (!dumped) return std::unexpected(std::move(dumped.error()));
  scope.dumped = *std::move(dumped);

  scope.routes = owner_->TakeRoutesForFrame(frame_);
  scope.published = owner_->TakePublication(*slang_scope_);
  return BuiltScope{
      .scope = std::move(scope), .construction = std::move(construction_)};
}

// The parameters of this scope whose values differ between the objects built
// from it, each a declaration of the scope. One supplied at construction -- a
// parameter the code reads and an instantiation may assign, or the index a
// loop's block was built at -- is a value the construction fills, and the scope
// declares them in the order a construction supplies them. One computed from
// those holds the expression the source wrote for it. Every other parameter is
// read as the constant this specialization settled it to.
//
// An expression naming the index resolves to the declaration the construction
// fills, so every block a loop counts out states the same thing whatever index
// it was built at.
auto StructuralScopeLowerer::DeclareDifferingParameters(
    hir::StructuralScope& scope, WalkFrame frame) -> diag::Result<void> {
  for (const auto& member : slang_scope_->members()) {
    const auto* parameter = member.as_if<slang::ast::ParameterSymbol>();
    if (parameter == nullptr) continue;
    switch (owner_->ValueSourceOf(*parameter)) {
      case ParameterValueSource::kSettledAtElaboration:
        break;
      case ParameterValueSource::kSuppliedAtConstruction: {
        auto kind = SuppliedDeclarationOf(*parameter, frame);
        if (!kind) return std::unexpected(std::move(kind.error()));
        auto declared =
            DeclareSettledValue(scope, *parameter, *std::move(kind));
        if (!declared) return std::unexpected(std::move(declared.error()));
        break;
      }
      case ParameterValueSource::kComputedAtConstruction: {
        // Only a parameter a declaration writes is computed, so there is
        // always the expression it was written with.
        const slang::ast::Expression* initializer = parameter->getInitializer();
        if (initializer == nullptr) {
          throw InternalError(
              "StructuralScopeLowerer::DeclareDifferingParameters: a parameter "
              "its declaration writes states no expression");
        }
        auto lowered = LowerExpr(*initializer, frame);
        if (!lowered) return std::unexpected(std::move(lowered.error()));
        auto declared = DeclareSettledValue(
            scope, *parameter,
            hir::StructuralLocalparamDecl{
                .initializer = frame.Exprs().Add(*std::move(lowered))});
        if (!declared) return std::unexpected(std::move(declared.error()));
        break;
      }
    }
  }
  return {};
}

// What a parameter the construction supplies is declared as: a loop's index,
// which the loop always hands (LRM 27.4), or a parameter an instantiation
// assigns or leaves to the default it is declared with (LRM 6.20.1, 23.10.2).
// The default is the declaration's whichever value this body's instantiation
// gave, lowered here against this scope's names, so one written from an
// earlier parameter reads the value this object holds. One that is not valid
// here is one no instance of this unit is left to, since what makes it invalid
// -- a type, a value something compiled holds -- is part of which unit it is.
auto StructuralScopeLowerer::SuppliedDeclarationOf(
    const slang::ast::ParameterSymbol& parameter, WalkFrame frame)
    -> diag::Result<hir::StructuralDataObjectKind> {
  if (parameter.isFromGenvar()) return hir::StructuralConstructionValueDecl{};
  const slang::ast::Expression* declared = parameter.getDefaultValue();
  if (declared == nullptr || declared->bad()) {
    return hir::StructuralParameterDecl{.default_value = std::nullopt};
  }
  auto lowered = LowerExpr(*declared, frame);
  if (!lowered) return std::unexpected(std::move(lowered.error()));
  return hir::StructuralParameterDecl{
      .default_value = frame.Exprs().Add(*std::move(lowered))};
}

// Fills the declaration of a value the scope settles before its walk begins,
// which every name reaching the symbol resolves to rather than to what one
// elaboration gave it.
auto StructuralScopeLowerer::DeclareSettledValue(
    hir::StructuralScope& scope, const slang::ast::ValueSymbol& value,
    hir::StructuralDataObjectKind kind) -> diag::Result<void> {
  auto type_or = owner_->InternType(
      value.getType(), frontend::PointSpanOf(value.location));
  if (!type_or) return std::unexpected(std::move(type_or.error()));
  scope.structural_data_objects.Define(
      owner_->ReservedDataObject(value), hir::StructuralDataObjectDecl{
                                             .name = std::string{value.name},
                                             .type = *type_or,
                                             .kind = std::move(kind)});
  return {};
}

// Total over slang's symbol kinds with no `default`: a member that carries
// behavior must not vanish into a catch-all -- the failure mode that let a
// checker's entire body disappear without a word. Listing every kind forces a
// deliberate classification of each -- lowered here, deliberately nothing, or
// reported -- and a kind added by a future slang release fails to compile
// until it is classified.
auto StructuralScopeLowerer::PopulateMember(
    const slang::ast::Symbol& member, WalkFrame frame) -> diag::Result<void> {
  using slang::ast::SymbolKind;
  const diag::FailureContext at(frontend::PointSpanOf(member.location));
  switch (member.kind) {
    case SymbolKind::Variable:
      return PopulateVariableMember(
          member.as<slang::ast::VariableSymbol>(), frame);
    case SymbolKind::Net:
      return PopulateNetMember(member.as<slang::ast::NetSymbol>(), frame);
    case SymbolKind::Subroutine: {
      const auto& sub = member.as<slang::ast::SubroutineSymbol>();
      if (sub.flags.has(slang::ast::MethodFlags::DPIImport)) {
        return PopulateForeignImportMember(sub, frame);
      }
      return PopulateSubroutineMember(sub, frame);
    }
    case SymbolKind::Modport:
      return PopulateModportMember(
          member.as<slang::ast::ModportSymbol>(), frame);
    case SymbolKind::ProceduralBlock:
      return PopulateProceduralBlockMember(
          member.as<slang::ast::ProceduralBlockSymbol>(), frame);
    case SymbolKind::ContinuousAssign:
      return PopulateContinuousAssignMember(
          member.as<slang::ast::ContinuousAssignSymbol>(), frame);
    case SymbolKind::NetAlias:
      return PopulateNetAliasMember(
          member.as<slang::ast::NetAliasSymbol>(), frame);
    case SymbolKind::GenerateBlockArray:
      return PopulateGenerateArrayMember(
          member.as<slang::ast::GenerateBlockArraySymbol>(), frame);
    case SymbolKind::GenerateBlock:
      return PopulateGenerateBlockMember(
          member.as<slang::ast::GenerateBlockSymbol>(), frame);

    case SymbolKind::Instance:
      return PopulateInstanceMember(
          member.as<slang::ast::InstanceSymbol>(), frame);
    case SymbolKind::InstanceArray:
      return PopulateInstanceArrayMember(
          member.as<slang::ast::InstanceArraySymbol>(), frame);

    // An anonymous program opens no scope: what it declares are members of
    // this one (LRM 24.6). It holds only subroutines and classes, so all of
    // them belong to the walk that lays out declarations.
    case SymbolKind::AnonymousProgram:
      for (const auto& item :
           member.as<slang::ast::AnonymousProgramSymbol>().members()) {
        if (!owner_->Owns(item)) continue;
        auto r = PopulateMember(item, frame);
        if (!r) return std::unexpected(std::move(r.error()));
      }
      return {};

    // A named sequence or property declares no storage and no behavior: what
    // an instance of one stands for is its body with the actual arguments
    // substituted, and the front end hands over that expansion at the instance
    // (LRM 16.8, 16.12). So the declaration itself needs nothing here, and its
    // ports and local variables are members of its own scope rather than of
    // this one.
    case SymbolKind::Sequence:
    case SymbolKind::Property:
      return {};

    // LRM 17 checkers observe the design and never drive it, so a design with
    // them removed behaves identically and the policy may drop them whole.
    // Without it they are reported, so no design is quietly reduced to one that
    // checks nothing.
    case SymbolKind::AssertionPort:
    case SymbolKind::LocalAssertionVar:
    case SymbolKind::Checker:
    case SymbolKind::CheckerInstance:
    case SymbolKind::CheckerInstanceBody:
      if (support::ElidesAssertions(owner_->AssertionPolicy())) {
        return {};
      }
      return std::unexpected(
          diag::Make(
              frontend::PointSpanOf(member.location),
              diag::DiagCode::kUnsupportedStructuralMember,
              "an assertion or checker declaration is not yet supported")
              .WithNote("pass '--assertions skip' to leave it out"));

    // Behavior the design depends on: skipping one would hand the backend a
    // different design than the source describes.
    // LRM 28, 29.
    case SymbolKind::PrimitiveInstance:
      return diag::Fail(
          frontend::PointSpanOf(member.location),
          diag::DiagCode::kUnsupportedStructuralMember,
          "a gate or user-defined primitive instance is not yet supported");
    case SymbolKind::RandSeqProduction:
    case SymbolKind::UninstantiatedDef:
      return diag::Fail(
          frontend::PointSpanOf(member.location),
          diag::DiagCode::kUnsupportedStructuralMember,
          "this kind of declaration is not yet supported");

    // Naming a type creates no structure. Whatever declares an object of it
    // interns the type at its own declaration.
    case SymbolKind::PredefinedIntegerType:
    case SymbolKind::ScalarType:
    case SymbolKind::FloatingType:
    case SymbolKind::EnumType:
    case SymbolKind::EnumValue:
    case SymbolKind::PackedArrayType:
    case SymbolKind::FixedSizeUnpackedArrayType:
    case SymbolKind::DynamicArrayType:
    case SymbolKind::DPIOpenArrayType:
    case SymbolKind::AssociativeArrayType:
    case SymbolKind::QueueType:
    case SymbolKind::PackedStructType:
    case SymbolKind::UnpackedStructType:
    case SymbolKind::PackedUnionType:
    case SymbolKind::UnpackedUnionType:
    case SymbolKind::CovergroupType:
    case SymbolKind::VoidType:
    case SymbolKind::NullType:
    case SymbolKind::CHandleType:
    case SymbolKind::StringType:
    case SymbolKind::EventType:
    case SymbolKind::UnboundedType:
    case SymbolKind::TypeRefType:
    case SymbolKind::UntypedType:
    case SymbolKind::SequenceType:
    case SymbolKind::PropertyType:
    case SymbolKind::VirtualInterfaceType:
    case SymbolKind::TypeAlias:
    case SymbolKind::ErrorType:
    case SymbolKind::ForwardingTypedef:
    case SymbolKind::NetType:
    case SymbolKind::TypeParameter:
      return {};

    // A class the scope declares is one of the scopes it declares (LRM 37.12),
    // a parameterized one as much as any; its code is built where the design
    // uses it.
    case SymbolKind::ClassType:
    case SymbolKind::GenericClassDef:
      frame.current_structural_scope->declared_classes.push_back(
          NameInAPath(member));
      return {};

    // Nothing to build at the walk. A parameter that varies per block or per
    // instance is already a declaration, settled before this walk began so that
    // the members below reach it; any other is part of the scope's own
    // specialization, which makes reading its value cost nothing. A genvar and
    // an import contribute to the members that read them and nothing of their
    // own.
    case SymbolKind::Parameter:
    case SymbolKind::Specparam:
    case SymbolKind::DefParam:
    case SymbolKind::Genvar:
    case SymbolKind::ExplicitImport:
    case SymbolKind::WildcardImport:
    case SymbolKind::Attribute:
    case SymbolKind::ConfigBlock:
    case SymbolKind::ElabSystemTask:
      return {};

    // An interface port is its own declaration: no separate internal name
    // stands behind it the way one stands behind a data port, so the member
    // the scope holds for it is built here (LRM 25.3).
    case SymbolKind::InterfacePort:
      return PopulateInterfacePortMember(
          member.as<slang::ast::InterfacePortSymbol>(), frame);

    // A subroutine an interface leaves to a connected module is one of the
    // interface's subroutines with no body (LRM 25.7).
    case SymbolKind::MethodPrototype:
      return PopulateSubroutineMember(
          *member.as<slang::ast::MethodPrototypeSymbol>().getSubroutine(),
          frame);

    case SymbolKind::Port:
      return PopulatePortMember(member.as<slang::ast::PortSymbol>(), frame);

    // The scope's own boundary and its enclosing containers, reached as
    // members but describing where this scope sits rather than what it does.
    case SymbolKind::MultiPort:
    case SymbolKind::ModportPort:
    case SymbolKind::ModportClocking:
    case SymbolKind::InstanceBody:
    case SymbolKind::Package:
    case SymbolKind::CompilationUnit:
    case SymbolKind::Root:
    case SymbolKind::Definition:
      return {};

    // Owned by a different scope -- a subroutine's arguments, a class's
    // fields, a covergroup's bins -- and lowered with that scope if at all.
    case SymbolKind::Unknown:
    case SymbolKind::DeferredMember:
    case SymbolKind::TransparentMember:
    case SymbolKind::EmptyMember:
    case SymbolKind::StatementBlock:
    case SymbolKind::FormalArgument:
    case SymbolKind::Field:
    case SymbolKind::ClassProperty:
    case SymbolKind::Iterator:
    case SymbolKind::PatternVar:
    case SymbolKind::ConstraintBlock:
    case SymbolKind::CovergroupBody:
    case SymbolKind::Coverpoint:
    case SymbolKind::CoverCross:
    case SymbolKind::CoverCrossBody:
    case SymbolKind::CoverageBin:
      return {};

    // These annotate or define; no behavior arises at the point of
    // declaration.
    case SymbolKind::Primitive:
    case SymbolKind::PrimitivePort:
    case SymbolKind::SpecifyBlock:
    case SymbolKind::TimingPath:
    case SymbolKind::PulseStyle:
    case SymbolKind::SystemTimingCheck:
      return {};

    case SymbolKind::ClockingBlock:
      return PopulateClockingMember(
          member.as<slang::ast::ClockingBlockSymbol>(), frame);

    // Declarations whose effect happens where they are used, not where they
    // are written, so the use site is what has to support them.
    case SymbolKind::ClockVar:
    case SymbolKind::LetDecl:
      return {};
  }
  throw InternalError(
      "StructuralScopeLowerer::PopulateMember: unknown slang "
      "SymbolKind");
}

auto StructuralScopeLowerer::PopulateVariableMember(
    const slang::ast::VariableSymbol& var, WalkFrame frame)
    -> diag::Result<void> {
  if (var.lifetime != slang::ast::VariableLifetime::Static) {
    return diag::Fail(
        frontend::PointSpanOf(var.location),
        diag::DiagCode::kUnsupportedNonStaticVariableLifetime,
        "an automatic variable is not yet supported here");
  }
  auto type_id_or =
      owner_->InternType(var.getType(), frontend::PointSpanOf(var.location));
  if (!type_id_or) return std::unexpected(std::move(type_id_or.error()));
  // Slang rejects `void` in any variable-declaration position before
  // elaboration, so a void-typed VariableSymbol can only reach this path
  // via a slang/Lyra integration bug.
  if (owner_->Unit().types.Get(*type_id_or).Is<hir::VoidType>()) {
    throw InternalError(
        "StructuralScopeLowerer::PopulateVariableMember: variable declaration "
        "produced "
        "void type");
  }
  hir::StructuralDataObjectKind kind = hir::StructuralVariableDecl{};
  if (const auto binding = owner_->ReferenceBindingOf(var)) {
    kind = hir::StructuralReferenceDecl{.binding = *binding};
  } else if (const auto* init = var.getInitializer(); init != nullptr) {
    auto init_or = LowerExpr(*init, frame);
    if (!init_or) return std::unexpected(std::move(init_or.error()));
    kind = hir::StructuralVariableDecl{
        .initializer = frame.Exprs().Add(*std::move(init_or))};
  }
  frame.current_structural_scope->structural_data_objects.Define(
      owner_->ReservedDataObject(var), hir::StructuralDataObjectDecl{
                                           .name = std::string{var.name},
                                           .type = *type_id_or,
                                           .kind = std::move(kind)});
  return {};
}

auto StructuralScopeLowerer::PopulateInterfacePortMember(
    const slang::ast::InterfacePortSymbol& port, WalkFrame frame)
    -> diag::Result<void> {
  // How many instances the port stands for is the range it declares (LRM 25.3),
  // outermost first; a port standing for one declares none, which is the same
  // answer with nothing in it.
  const auto declared = port.getDeclaredRange();
  if (!declared.has_value()) {
    throw InternalError(
        "PopulateInterfacePortMember: the port's range evaluated where the "
        "signature was published, so it evaluates here too");
  }
  std::vector<hir::UnpackedRange> array_dims;
  array_dims.reserve(declared->size());
  for (const slang::ConstantRange& dim : *declared) {
    array_dims.push_back(
        hir::UnpackedRange{.left = dim.left, .right = dim.right});
  }
  const hir::InterfacePortId local =
      frame.current_structural_scope->interface_ports.Add(
          hir::InterfacePortDecl{
              .name = std::string{port.name},
              .array_dims = std::move(array_dims)});
  owner_->MapInterfacePortBinding(port, frame_, local);
  return {};
}

auto StructuralScopeLowerer::PopulateNetMember(
    const slang::ast::NetSymbol& net, WalkFrame frame) -> diag::Result<void> {
  const auto span = frontend::PointSpanOf(net.location);
  auto type_id_or = owner_->InternType(net.getType(), span);
  if (!type_id_or) return std::unexpected(std::move(type_id_or.error()));
  auto net_type = TranslateNetType(net.netType, span);
  if (!net_type) return std::unexpected(std::move(net_type.error()));

  // A delay on a net declaration that does not assign is a net delay: the time
  // a change any driver makes takes to reach the net (LRM 10.3.3, 28.16), and
  // on a `trireg` its third component is how long a stored charge lasts (LRM
  // 6.6.4.2). Neither is modelled, and dropping one answers at the wrong time
  // rather than refusing. On a declaration that assigns, the delay is that
  // assignment's own.
  const auto* init = net.getInitializer();
  if (net.getDelay() != nullptr && init == nullptr) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedTypeKind,
        "a net delay is not yet supported");
  }
  if (HoldsType(net.getType(), IsTaggedUnion)) {
    // LRM 6.7.1, 7.3.2.
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedTypeKind,
        "a net holding a tagged union is not yet supported");
  }
  if (HoldsType(net.getType(), IsDynamicOrEvent)) {
    // LRM 23.2.2.3, 6.6.7.
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedTypeKind,
        "an input port with no port kind holding a dynamic array, a queue, an "
        "associative array or an event is a net, which is not yet supported; "
        "'input var' declares the variable port");
  }

  frame.current_structural_scope->structural_data_objects.Define(
      owner_->ReservedDataObject(net),
      hir::StructuralDataObjectDecl{
          .name = std::string{net.name},
          .type = *type_id_or,
          .kind = hir::StructuralNetDecl{
              .net_type = *net_type,
              .charge_strength =
                  TranslateChargeStrength(net.getChargeStrength())}});

  // A net-declaration assignment (`wire w = expr;`, LRM 6.5) is a single
  // continuous driver of the net. slang carries it as the net's initializer
  // rather than a separate continuous-assignment item, so synthesize the
  // equivalent continuous assignment here; it lands on the same driver path an
  // explicit `assign` does, with the net as the symbol holding it.
  if (init != nullptr) {
    auto strength = TranslateDriveStrength(net.getDriveStrength(), span);
    if (!strength) return std::unexpected(std::move(strength.error()));
    auto rhs_or = LowerExpr(*init, frame);
    if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
    // The net is this scope's own, so the driver reaches it over the route
    // that climbs no edges.
    auto net_ref =
        owner_->MakeRoutedValueRef(net, frame_, ScopeRoute::Enclosing({}));
    if (!net_ref) return std::unexpected(std::move(net_ref.error()));
    auto assign = BuildContinuousAssign(
        span, hir::MakeRefExpr(*net_ref, *type_id_or, span), *std::move(rhs_or),
        *init, net, *strength, net.getDelay(), frame);
    if (!assign) return std::unexpected(std::move(assign.error()));
    frame.current_structural_scope->continuous_assigns.Add(*std::move(assign));
  }
  return {};
}

// The subroutine this scope evaluates `expr`, which `holder` holds, in as
// `result_type`, for a caller that asks for the value and hands it
// `parameters`.
auto StructuralScopeLowerer::DefineEvaluator(
    const slang::ast::Symbol& holder, std::string name,
    const slang::ast::Expression& expr, hir::TypeId result_type,
    hir::ParamDirection result_direction,
    std::span<const slang::ast::FormalArgumentSymbol* const> parameters,
    WalkFrame frame) -> diag::Result<void> {
  auto evaluator = LowerEvaluatorDecl(
      *owner_, holder, std::move(name), expr, result_type, result_direction,
      parameters, frame);
  if (!evaluator) return std::unexpected(std::move(evaluator.error()));
  frame.current_structural_scope->structural_subroutines.Define(
      owner_->EvaluatorOf(expr), *std::move(evaluator));
  return {};
}

// A name a view offers only for reading (LRM 25.5.4): nothing bounds it to an
// lvalue, so a module written against the view asks this interface for its
// value. Every other name a view offers designates storage the referrer
// reaches, so it has nothing to build here.
auto StructuralScopeLowerer::PopulateModportMember(
    const slang::ast::ModportSymbol& modport, WalkFrame frame)
    -> diag::Result<void> {
  for (const auto& item : modport.members()) {
    const auto* port = item.as_if<slang::ast::ModportPortSymbol>();
    if (port == nullptr || !ViewDefinesTheName(*port)) continue;
    if (ViewNameDesignatesStorage(*port)) continue;
    auto type = owner_->InternDeclaredType(*port);
    if (!type) return std::unexpected(std::move(type.error()));
    auto defined = DefineEvaluator(
        *port, ModportReadName(modport.name, port->name),
        ViewExpressionOf(*port), *type, hir::ParamDirection::kInput, {}, frame);
    if (!defined) return std::unexpected(std::move(defined.error()));
  }
  return {};
}

namespace {

// Every bit 'z, at `type`, the 4-state packed type `declared` interns as:
// what a driver a clocking block creates holds until a drive sets it (LRM
// 14.16).
auto HighImpedanceOf(
    const slang::ast::Type& declared, hir::TypeId type, diag::SourceSpan span)
    -> hir::Expr {
  return MakeIntegralLiteralExpr(
      slang::SVInt::createFillZ(
          static_cast<slang::bitwidth_t>(declared.getBitWidth()),
          declared.isSigned()),
      type, span);
}

// A variable a clocking block holds (LRM 14.10, 14.13, 14.16), declared in the
// scope declaring the block.
void DefineBlockVariable(
    hir::StructuralScope& scope, hir::StructuralDataObjectId id,
    std::string name, hir::TypeId type,
    std::optional<hir::ExprId> initializer) {
  scope.structural_data_objects.Define(
      id, hir::StructuralDataObjectDecl{
              .name = std::move(name),
              .type = type,
              .kind = hir::StructuralVariableDecl{.initializer = initializer}});
}

// A clockvar, the type its value interns as, and where it is written.
struct TypedClockvar {
  const slang::ast::ClockVarSymbol* symbol = nullptr;
  hir::TypeId type;
  diag::SourceSpan at;
};

// The delay a skew writes, as the source wrote it (LRM 14.4).
auto SkewDelayOf(
    StructuralScopeLowerer& lowerer, WalkFrame frame,
    const slang::ast::TimingControl& control)
    -> diag::Result<hir::DelayControl> {
  auto amount =
      lowerer.LowerExpr(control.as<slang::ast::DelayControl>().expr, frame);
  if (!amount) return std::unexpected(std::move(amount.error()));
  return hir::DelayControl{.duration = frame.Exprs().Add(*std::move(amount))};
}

// The input half of `clockvar`, which samples into `sample` (LRM 14.13).
auto ClockvarInputOf(
    StructuralScopeLowerer& lowerer, WalkFrame frame,
    const TypedClockvar& clockvar, hir::StructuralDataObjectId sample)
    -> diag::Result<hir::ClockingInput> {
  DefineBlockVariable(
      *frame.current_structural_scope, sample,
      std::string{clockvar.symbol->name}, clockvar.type, {});
  auto skew = std::visit(
      Overloaded{
          // `#1step`, written or taken for want of any skew (LRM 14.3), reads
          // the signal's sampled value (LRM 16.5.1).
          [&](const OneStepSkewed&) -> diag::Result<hir::ClockingInputSkew> {
            auto armed = ArmSampledReads(
                lowerer.Owner(), frame, *clockvar.symbol->getInitializer(),
                clockvar.at);
            if (!armed) return std::unexpected(std::move(armed.error()));
            return hir::OneStepSkew{};
          },
          [&](const DelaySkewed& delayed)
              -> diag::Result<hir::ClockingInputSkew> {
            auto delay = SkewDelayOf(lowerer, frame, *delayed.delay);
            if (!delay) return std::unexpected(std::move(delay.error()));
            return hir::DelaySkew{.delay = *delay};
          },
          [](const EdgeSkewed&) -> diag::Result<hir::ClockingInputSkew> {
            throw InternalError(
                "ClockvarInputOf: an input not carried holds no variable of "
                "the block");
          }},
      InputSkewOf(*clockvar.symbol));
  if (!skew) return std::unexpected(std::move(skew.error()));
  return hir::ClockingInput{.sample = sample, .skew = *std::move(skew)};
}

// The output half of `clockvar`, which every drive of it writes into `driving`
// (LRM 14.16).
auto ClockvarOutputOf(
    StructuralScopeLowerer& lowerer, WalkFrame frame,
    const TypedClockvar& clockvar, hir::StructuralDataObjectId driving)
    -> diag::Result<hir::ClockingOutput> {
  auto skew = std::visit(
      Overloaded{
          [](const Unskewed&) -> diag::Result<hir::ClockingOutputSkew> {
            return hir::NoSkew{};
          },
          [&](const DelaySkewed& delayed)
              -> diag::Result<hir::ClockingOutputSkew> {
            auto delay = SkewDelayOf(lowerer, frame, *delayed.delay);
            if (!delay) return std::unexpected(std::move(delay.error()));
            return hir::DelaySkew{.delay = *delay};
          },
          [&](const EdgeSkewed& edge) -> diag::Result<hir::ClockingOutputSkew> {
            std::optional<hir::DelayControl> after;
            if (edge.delay != nullptr) {
              auto delay = SkewDelayOf(lowerer, frame, *edge.delay);
              if (!delay) return std::unexpected(std::move(delay.error()));
              after = *delay;
            }
            return hir::EdgeSkew{
                .edge = LowerEventEdge(edge.edge), .delay = after};
          },
          [](const OneStepSkewed&) -> diag::Result<hir::ClockingOutputSkew> {
            throw InternalError(
                "ClockvarOutputOf: an output not carried is landed by "
                "nothing");
          }},
      OutputSkewOf(*clockvar.symbol));
  if (!skew) return std::unexpected(std::move(skew.error()));
  hir::StructuralScope& scope = *frame.current_structural_scope;
  const std::string name = DrivingVariableNamingOf(*clockvar.symbol).name;
  hir::ClockingOutputSignal signal = std::visit(
      Overloaded{
          // LRM 14.16: the net's driver is updated as if by a continuous
          // assignment from the variable inside the block, at (strong1,
          // strong0), and is 'z until a drive sets it.
          [&](const NetSignal&) -> hir::ClockingOutputSignal {
            DefineBlockVariable(
                scope, driving, name, clockvar.type,
                frame.Exprs().Add(HighImpedanceOf(
                    clockvar.symbol->getType(), clockvar.type, clockvar.at)));
            const hir::Expr read = lowerer.Owner().MakeRoutedMemberRef(
                frame.Current(),
                hir::ValueRoute{
                    .base = hir::InUnitBase{.hops = {}},
                    .steps = {},
                    .leaf =
                        hir::StructuralDataObjectLeaf{
                            .object = driving,
                            .storage = hir::VariableStorage{},
                            .type = clockvar.type}},
                clockvar.at);
            const auto& read_ref = std::get<hir::RoutedValueRef>(
                std::get<hir::PrimaryExpr>(read.data).data);
            return hir::NetSignal{
                .driving = frame.Exprs().Add(read),
                .reads = {hir::SensitivityEntry{
                    .cell = hir::ValueTarget{read_ref},
                    .part = hir::WatchedWhole{}}}};
          },
          // LRM 14.16.2: what the outputs onto a variable put onto it is taken
          // from the variable itself as each pass driving it ends, so this one
          // never shows a value of its own. Every output onto the variable
          // shares it, so the first of them declares it.
          [&](const VariableSignal&) -> hir::ClockingOutputSignal {
            if (!scope.structural_data_objects.IsDefined(driving)) {
              DefineBlockVariable(scope, driving, name, clockvar.type, {});
            }
            return hir::VariableSignal{};
          },
          [](const UncarriedSignal&) -> hir::ClockingOutputSignal {
            throw InternalError(
                "ClockvarOutputOf: an output not carried holds no variable of "
                "the block");
          }},
      OutputSignalOf(*clockvar.symbol));
  return hir::ClockingOutput{
      .skew = *std::move(skew),
      .driving = driving,
      .signal = std::move(signal)};
}

}  // namespace

// A named clocking block's clocking event clocks a sequence or property it
// declares wherever the declaration is instantiated (LRM 14.13, 16.16), so a
// unit instantiating it from elsewhere asks this scope for the value of each
// event's operand and qualifier. Each answers its own expression's type, which
// is what the event is taken of.
auto StructuralScopeLowerer::PopulateClockingEvaluators(
    const slang::ast::ClockingBlockSymbol& block, WalkFrame frame)
    -> diag::Result<void> {
  if (block.name.empty()) return {};
  const auto span = frontend::PointSpanOf(block.location);
  const auto define = [&](const slang::ast::Expression& expression,
                          std::string name) -> diag::Result<void> {
    auto type = owner_->InternExpressionType(expression, span);
    if (!type) return std::unexpected(std::move(type.error()));
    return DefineEvaluator(
        block, std::move(name), expression, *type, hir::ParamDirection::kInput,
        {}, frame);
  };
  const std::vector<const slang::ast::SignalEventControl*> events =
      ClockingEventsOf(block);
  for (std::size_t at = 0; at < events.size(); ++at) {
    const slang::ast::SignalEventControl& event = *events[at];
    if (auto r = define(event.expr, ClockingEventName(block.name, at)); !r) {
      return r;
    }
    if (event.iffCondition == nullptr) continue;
    if (auto r =
            define(*event.iffCondition, ClockingConditionName(block.name, at));
        !r) {
      return r;
    }
  }
  return {};
}

// A clocking block holds each clockvar's signal as the scope declaring the
// block writes it (LRM 14.3), each half's skew as written or as LRM 14.3
// defaults it, and the variables the block holds: what each input last
// sampled (LRM 14.13), the event its name stands for (LRM 14.10), and the
// variable inside the block every drive of an output writes, which drives a
// net output's nets and is 'z until a drive sets it (LRM 14.16, 14.16.2). What
// samples the inputs, keeps a skewed input's copy, puts that variable onto the
// signal and lands a drive is no statement the source wrote, so it is built a
// layer down from these.
auto StructuralScopeLowerer::PopulateClockingMember(
    const slang::ast::ClockingBlockSymbol& block, WalkFrame frame)
    -> diag::Result<void> {
  if (auto r = PopulateClockingEvaluators(block, frame); !r) return r;
  if (block.isGlobal) return {};
  const auto span = frontend::PointSpanOf(block.location);
  const DeclaredClocking& declared = owner_->DeclaredClockingOf(block);
  hir::StructuralScope& scope = *frame.current_structural_scope;
  DefineBlockVariable(
      scope, declared.block_event, ClockingBlockEventName(),
      owner_->Unit().types.Intern(hir::Type{hir::EventType{}}), {});
  auto event =
      LowerEventControl(*this, frame, ClockingEvent{&block.getEvent()}, span);
  if (!event) return std::unexpected(std::move(event.error()));
  std::vector<hir::Clockvar> clockvars;
  clockvars.reserve(declared.clockvars.size());
  for (const auto& clockvar :
       block.membersOfType<slang::ast::ClockVarSymbol>()) {
    const auto at = frontend::PointSpanOf(clockvar.location);
    const DeclaredClockvar& parts = declared.clockvars.at(&clockvar);
    auto type = owner_->InternType(clockvar.getType(), at);
    if (!type) return std::unexpected(std::move(type.error()));
    const slang::ast::Expression* signal = clockvar.getInitializer();
    if (signal == nullptr) {
      throw InternalError(
          "PopulateClockingMember: the front end binds every clockvar to the "
          "signal it names or refuses the block");
    }
    auto lowered = LowerExpr(*signal, frame);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    auto signal_reads = CellsOfWaitedExpression(*this, frame, *signal);
    if (!signal_reads) return std::unexpected(std::move(signal_reads.error()));
    const TypedClockvar typed{.symbol = &clockvar, .type = *type, .at = at};
    std::optional<hir::ClockingInput> input;
    if (parts.sample.has_value()) {
      auto half = ClockvarInputOf(*this, frame, typed, *parts.sample);
      if (!half) return std::unexpected(std::move(half.error()));
      input = *std::move(half);
    }
    std::optional<hir::ClockingOutput> output;
    if (parts.driving.has_value()) {
      auto half = ClockvarOutputOf(*this, frame, typed, *parts.driving);
      if (!half) return std::unexpected(std::move(half.error()));
      output = *std::move(half);
    }
    clockvars.push_back(
        hir::Clockvar{
            .name = std::string{clockvar.name},
            .span = at,
            .signal = frame.Exprs().Add(*std::move(lowered)),
            .signal_reads = *std::move(signal_reads),
            .input = std::move(input),
            .output = std::move(output)});
  }
  // The positions the declaration pass gave the clockvars are the order the
  // block declares them in, which is the order they were taken in here.
  const std::size_t count = clockvars.size();
  scope.clocking_blocks.Define(
      declared.id, hir::ClockingBlockDecl{
                       .name = NameInAPath(block),
                       .span = span,
                       .event = *std::move(event),
                       .block_event = declared.block_event,
                       .clockvars = {count, std::move(clockvars)}});
  return {};
}

// A port's default (LRM 23.2.2.4) is evaluated in this scope, not in the
// instantiator's, so an instance leaving the port unconnected asks this unit
// for it. A port with no default has nothing to build here.
auto StructuralScopeLowerer::PopulatePortMember(
    const slang::ast::PortSymbol& port, WalkFrame frame) -> diag::Result<void> {
  const slang::ast::Expression* initializer = port.getInitializer();
  if (initializer == nullptr) return {};
  auto type = owner_->InternDeclaredType(port);
  if (!type) return std::unexpected(std::move(type.error()));
  return DefineEvaluator(
      port, PortDefaultName(port.name), *initializer, *type,
      hir::ParamDirection::kInput, {}, frame);
}

// A subroutine this scope defines for an interface (LRM 25.7) is handed to that
// interface's object: the route to it is the one a call written through the
// same name takes, and what it defines is the subroutine that object's class
// leaves to a connected module.
auto StructuralScopeLowerer::RecordExternDefinition(
    const slang::ast::SubroutineSymbol& definition, WalkFrame frame)
    -> diag::Result<void> {
  const auto span = frontend::PointSpanOf(definition.location);
  const slang::ast::SubroutineSymbol& defined =
      *definition.getPrototype()->getSubroutine();
  const slang::ast::Symbol* prefix =
      definition.getParentScope()->find(*ExternDefinitionPrefix(definition));
  const std::array<slang::ast::HierarchicalReference::Element, 2> path{
      slang::ast::HierarchicalReference::Element{*prefix},
      slang::ast::HierarchicalReference::Element{defined}};
  slang::ast::HierarchicalReference written;
  written.target = &defined;
  written.path = path;
  // An instance the module holds is reached from the module, as any name
  // staying inside it is, and a port is reached as whatever it was bound to.
  const RouteOrigin origin =
      written.isViaIfacePort()
          ? owner_->StartOf(frame, defined, written)
          : RouteOrigin{.start = FromReader{}, .written = written.path};
  auto walked = owner_->RouteToScopeOrRefuse(
      frame, *defined.getParentScope(), origin, span);
  if (!walked) return std::unexpected(std::move(walked.error()));
  const auto* on = std::get_if<InPublishedScope>(&walked->place);
  if (on == nullptr || !on->within.empty()) {
    throw InternalError(
        "RecordExternDefinition: the interface a definition is written through "
        "is no object of a published class -- please report this as a bug");
  }
  const hir::ScopeClassId scope_class = on->scope_class;
  const hir::ScopeClassSignature& published =
      owner_->Unit().scope_classes.Get(scope_class);
  const std::optional<hir::PublishedCallableId> callable =
      published.FindCallable(defined.name);
  const std::optional<hir::PublishedExternId> extern_id =
      callable.has_value() ? published.FindExtern(*callable) : std::nullopt;
  if (!extern_id.has_value()) {
    throw InternalError(
        "RecordExternDefinition: the interface publishes no subroutine it "
        "leaves to a connected module under the defined name -- please report "
        "this as a bug");
  }
  const auto binding = owner_->LookupSubroutineBinding(definition);
  if (!binding.has_value()) {
    throw InternalError(
        "RecordExternDefinition: the definition was not minted by the "
        "declaration pass -- please report this as a bug");
  }
  const hir::RoutedObjectRef target = owner_->MakeRoutedObjectRef(
      frame.Current(), *std::move(walked),
      owner_->ScopeClassTypeOf(scope_class));
  frame.current_structural_scope->extern_definitions.push_back(
      hir::ExternDefinition{
          .definition = binding->subroutine_id,
          .target = target,
          .scope_class = scope_class,
          .of = *extern_id});
  return {};
}

// Each formal's default is evaluated here, in the scope declaring the
// subroutine, for every call that omits it (LRM 13.5.3).
auto StructuralScopeLowerer::PopulateArgumentDefaults(
    const slang::ast::SubroutineSymbol& sym, WalkFrame frame)
    -> diag::Result<void> {
  for (const slang::ast::FormalArgumentSymbol* formal : sym.getArguments()) {
    const slang::ast::Expression* default_value = formal->getDefaultValue();
    if (default_value == nullptr) continue;
    auto type = owner_->InternDefaultType(
        *formal, frontend::PointSpanOf(formal->location));
    if (!type) return std::unexpected(std::move(type.error()));
    auto defined = DefineEvaluator(
        *formal, ArgumentDefaultName(SubroutineNameOf(sym), formal->name),
        *default_value, *type, ParamDirectionOf(*formal),
        EvaluatorParametersOf(sym, *formal), frame);
    if (!defined) return std::unexpected(std::move(defined.error()));
  }
  return {};
}

auto StructuralScopeLowerer::PopulateSubroutineMember(
    const slang::ast::SubroutineSymbol& sym, WalkFrame frame)
    -> diag::Result<void> {
  auto decl_or = LowerSubroutineDecl(*owner_, sym, frame);
  if (!decl_or) return std::unexpected(std::move(decl_or.error()));

  const auto binding = owner_->LookupSubroutineBinding(sym);
  if (!binding.has_value()) {
    throw InternalError(
        "StructuralScopeLowerer::PopulateSubroutineMember: the subroutine was "
        "not minted by the declaration pass");
  }
  frame.current_structural_scope->structural_subroutines.Define(
      binding->subroutine_id, *std::move(decl_or));

  // A static variable of a namespace unit's subroutine is one of the
  // namespace's variables (LRM 6.21, 26.2), defined here with its initializer,
  // which runs with theirs before any process starts (LRM 10.5).
  for (const KeptStatic& local : StaticsKeptBy(sym)) {
    std::optional<hir::ExprId> initializer;
    if (const slang::ast::Expression* init = local.var->getInitializer()) {
      auto lowered = LowerExpr(*init, frame);
      if (!lowered) return std::unexpected(std::move(lowered.error()));
      initializer = frame.Exprs().Add(*std::move(lowered));
    }
    owner_->DefineNamespaceVariable(
        local.name, hir::NamespaceStatic{
                        .initializer = initializer,
                        .written_in = owner_->LookupProceduralScope(
                            NamedScopeAround(*local.var))});
  }

  if (auto defaults = PopulateArgumentDefaults(sym, frame); !defaults) {
    return defaults;
  }

  // An `export "DPI-C"` (LRM 35.5) names a subroutine of its own scope, so the
  // exported subroutine reaches this ordinary body path and the export is
  // additionally recorded here to drive a foreign-linkage entry.
  if (sym.isDPIExport()) {
    auto export_or = LowerForeignExport(
        *owner_, sym, binding->subroutine_id, sym.getDPICIdentifier());
    if (!export_or) return std::unexpected(std::move(export_or.error()));
    frame.current_structural_scope->foreign_exports.push_back(
        *std::move(export_or));
  }
  return {};
}

auto StructuralScopeLowerer::PopulateForeignImportMember(
    const slang::ast::SubroutineSymbol& sym, WalkFrame frame)
    -> diag::Result<void> {
  // The declaration is classified here even when nothing in this unit calls it,
  // so a signature outside the DPI-C type mapping (LRM 35.5.6) is reported
  // against the declaration that wrote it rather than against a call site far
  // away, or nowhere at all.
  auto id_or = owner_->EnsureForeignImport(sym);
  if (!id_or) return std::unexpected(std::move(id_or.error()));
  frame.current_structural_scope->declared_imports.push_back(*id_or);
  return PopulateArgumentDefaults(sym, frame);
}

auto StructuralScopeLowerer::PopulateProceduralBlockMember(
    const slang::ast::ProceduralBlockSymbol& proc, WalkFrame frame)
    -> diag::Result<void> {
  if (!owner_->Contains(proc)) {
    return {};
  }
  if (const StaticConcurrentAssertion found = StaticConcurrentAssertionOf(proc);
      found.assertion != nullptr) {
    ProcessLowerer assertion_lowerer(*owner_, proc);
    auto decl = assertion_lowerer.RunConcurrentAssertion(
        proc, *found.assertion, found.named_block, frame);
    if (!decl) return std::unexpected(std::move(decl.error()));
    frame.current_structural_scope->concurrent_assertions.Add(*std::move(decl));
    return {};
  }
  ProcessLowerer proc_lowerer(*owner_, proc);
  auto p = proc_lowerer.Run(proc, frame);
  if (!p) return std::unexpected(std::move(p.error()));
  const auto reserved = owner_->LookupProcessBinding(proc);
  if (!reserved.has_value()) {
    throw InternalError(
        "StructuralScopeLowerer::PopulateProceduralBlockMember: the process "
        "was not minted by the declaration pass");
  }
  frame.current_structural_scope->processes.Define(*reserved, *std::move(p));
  return {};
}

auto StructuralScopeLowerer::PopulateContinuousAssignMember(
    const slang::ast::ContinuousAssignSymbol& sym, WalkFrame frame)
    -> diag::Result<void> {
  auto ca = LowerContinuousAssign(sym, frame);
  if (!ca) return std::unexpected(std::move(ca.error()));
  frame.current_structural_scope->continuous_assigns.Add(*std::move(ca));
  return {};
}

// An alias states that the bits of the signals it lists are the same physical
// nets (LRM 10.11). Each member is one side of the overlay, and the list is one
// statement about all of them.
//
// What the standard demands of the members is decided over the elaborated
// design and reported there: one net type across the list, sides of equal
// width, no variable and no hierarchical reference, and no pair stated twice.
auto StructuralScopeLowerer::PopulateNetAliasMember(
    const slang::ast::NetAliasSymbol& alias, WalkFrame frame)
    -> diag::Result<void> {
  const diag::SourceSpan span = frontend::PointSpanOf(alias.location);
  std::vector<hir::NetSide> sides;
  for (const slang::ast::Expression* member : alias.getNetReferences()) {
    auto side = NetPositionsOfLvalue(
        *this, slang_scope_->asSymbol(), *member, span,
        diag::DiagCode::kUnsupportedStructuralMember, frame);
    if (!side) return std::unexpected(std::move(side.error()));
    sides.push_back(*std::move(side));
  }
  frame.current_structural_scope->net_joins.push_back(
      hir::NetJoin{.span = span, .sides = std::move(sides)});
  return {};
}

auto StructuralScopeLowerer::CollectDumped(
    const slang::ast::Scope& members, const DumpedPorts& ports, WalkFrame frame,
    std::optional<hir::ProceduralScopeId> enclosing)
    -> diag::Result<std::vector<hir::DumpedDeclaration>> {
  std::vector<hir::DumpedDeclaration> dumped;
  // What a block, task or function declares is enclosed by its scope. One
  // the build lowers no body of -- a DPI-C import, an extern another module
  // defines, an assertion's action the build leaves out -- has no scope here
  // and holds nothing a dump records. A block the source gave no name holds
  // what it declares as a named one does; a dump shows it under the named
  // scopes around it, and no name reaches it (LRM 9.3.4).
  const auto collect_enclosed =
      [&](const slang::ast::Scope& scope,
          const slang::ast::Symbol& symbol) -> diag::Result<void> {
    const auto minted = owner_->LookupMintedProceduralScope(symbol);
    if (!minted.has_value()) return {};
    auto inner = CollectDumped(scope, {}, frame, minted->scope);
    if (!inner) return std::unexpected(std::move(inner.error()));
    std::ranges::move(*inner, std::back_inserter(dumped));
    return {};
  };
  // A concurrent assertion is no procedure the design runs and takes no body
  // (LRM 16.14.5), so what its action declares is storage no route reaches
  // yet, and a dump does not yet record it.
  std::unordered_set<const slang::ast::Symbol*> assertion_actions;
  for (const slang::ast::Symbol& member : members.members()) {
    const auto* proc = member.as_if<slang::ast::ProceduralBlockSymbol>();
    if (proc == nullptr ||
        StaticConcurrentAssertionOf(*proc).assertion == nullptr) {
      continue;
    }
    for (const auto* block : proc->getBlocks()) assertion_actions.insert(block);
  }
  for (const slang::ast::Symbol& member : members.members()) {
    if (!owner_->Owns(member) || assertion_actions.contains(&member)) {
      continue;
    }
    if (const auto* block = member.as_if<slang::ast::StatementBlockSymbol>()) {
      auto collected = collect_enclosed(*block, *block);
      if (!collected) return std::unexpected(std::move(collected.error()));
      continue;
    }
    if (const auto* subroutine = member.as_if<slang::ast::SubroutineSymbol>()) {
      auto collected = collect_enclosed(*subroutine, *subroutine);
      if (!collected) return std::unexpected(std::move(collected.error()));
      continue;
    }
    const auto* value = member.as_if<slang::ast::ValueSymbol>();
    if (value == nullptr) {
      continue;
    }
    const std::optional<support::DumpVarKind> kind = DumpedKindOf(*value);
    if (!kind.has_value()) {
      continue;
    }
    const auto span = frontend::PointSpanOf(value->location);
    auto type = owner_->InternType(value->getType(), span);
    if (!type) return std::unexpected(std::move(type.error()));
    // A namespace's variables, its subroutines' statics among them, are
    // named by the namespace wherever they are read (LRM 26.2, 6.21); a
    // design element's are storage of this scope's object.
    diag::Result<hir::Expr> named =
        !hir::RootsAnObject(owner_->Unit().definition)
            ? owner_
                  ->ResolveValueTarget(
                      frame, *value,
                      RouteOrigin{.start = FromReader{}, .written = {}}, span)
                  .transform([&](const hir::ValueTarget& target) {
                    return hir::MakeValueTargetRefExpr(target, *type, span);
                  })
            : owner_
                  ->MakeRoutedValueRef(
                      *value, frame_, ScopeRoute::Enclosing({}))
                  .transform([&](hir::RoutedValueRef ref) {
                    return hir::MakeRefExpr(std::move(ref), *type, span);
                  });
    if (!named) return std::unexpected(std::move(named.error()));
    const auto port = ports.find(value);
    dumped.push_back(
        hir::DumpedDeclaration{
            .identifier = std::string{value->name},
            .kind = *kind,
            .width = DumpedWidthOf(*kind, value->getType().getCanonicalType()),
            .range = DumpedRangeOf(value->getType()),
            .port = port != ports.end() ? std::optional(port->second)
                                        : std::nullopt,
            .named = frame.Exprs().Add(*std::move(named)),
            .enclosing = enclosing});
  }
  return dumped;
}

auto StructuralScopeLowerer::PopulateGenerateArrayMember(
    const slang::ast::GenerateBlockArraySymbol& array, WalkFrame frame)
    -> diag::Result<void> {
  auto g = BuildGenerateFromArray(array, frame);
  if (!g) return std::unexpected(std::move(g.error()));
  const hir::GenerateId id = owner_->GenerateIdOf(array);
  frame.current_structural_scope->generates.Define(id, std::move(g->generate));
  construction_.generates[id.value] = std::move(g->blocks);
  return {};
}

auto StructuralScopeLowerer::PopulateGenerateBlockMember(
    const slang::ast::GenerateBlockSymbol& block, WalkFrame frame)
    -> diag::Result<void> {
  // A conditional generate is one construct however many alternatives it holds
  // (LRM 27.5), so it is built once, where the first of them stands; a block
  // no conditional produced is its own construct and the same rule reaches it.
  if (!OpensItsConstruct(block)) return {};
  auto g = BuildGenerateFromBlock(block, frame);
  if (!g) return std::unexpected(std::move(g.error()));
  const hir::GenerateId id = owner_->GenerateIdOf(block);
  frame.current_structural_scope->generates.Define(id, std::move(g->generate));
  construction_.generates[id.value] = std::move(g->blocks);
  return {};
}

auto StructuralScopeLowerer::PopulateInstanceMember(
    const slang::ast::InstanceSymbol& inst, WalkFrame frame)
    -> diag::Result<void> {
  const std::array<const slang::ast::InstanceSymbol*, 1> one{&inst};
  auto built = BuildInstanceMember(inst, one, {}, frame);
  if (!built) return std::unexpected(std::move(built.error()));
  const hir::InstanceMemberId id = owner_->InstanceMemberIdOf(inst);
  frame.current_structural_scope->instance_members.Define(
      id, std::move(built->member));
  construction_.instances[id.value] = std::move(built->made);
  return {};
}

auto StructuralScopeLowerer::PopulateInstanceArrayMember(
    const slang::ast::InstanceArraySymbol& array, WalkFrame frame)
    -> diag::Result<void> {
  auto shape = ResolveInstanceArrayShape(array);
  if (!shape) {
    return {};
  }
  std::vector<hir::UnpackedRange> dims;
  dims.reserve(shape->ranges.size());
  for (const slang::ConstantRange& dim : shape->ranges) {
    dims.push_back(hir::UnpackedRange{.left = dim.left, .right = dim.right});
  }
  auto built =
      BuildInstanceMember(array, shape->elements, std::move(dims), frame);
  if (!built) return std::unexpected(std::move(built.error()));
  const hir::InstanceMemberId id = owner_->InstanceMemberIdOf(array);
  frame.current_structural_scope->instance_members.Define(
      id, std::move(built->member));
  construction_.instances[id.value] = std::move(built->made);
  return {};
}

}  // namespace lyra::lowering::ast_to_hir
