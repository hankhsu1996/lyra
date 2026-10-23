#include "lyra/lowering/ast_to_hir/structural_scope_lowerer.hpp"

#include <algorithm>
#include <array>
#include <cstdint>
#include <expected>
#include <optional>
#include <span>
#include <string>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

#include <slang/ast/Compilation.h>
#include <slang/ast/Scope.h>
#include <slang/ast/SemanticFacts.h>
#include <slang/ast/Statement.h>
#include <slang/ast/Symbol.h>
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

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/failure_context.hpp"
#include "lyra/frontend/slang_source_span.hpp"
#include "lyra/hir/continuous_assign.hpp"
#include "lyra/hir/dumped_declaration.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/expr_builders.hpp"
#include "lyra/hir/structural_data_object.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/lowering/ast_to_hir/constant_value.hpp"
#include "lyra/lowering/ast_to_hir/generate_construct.hpp"
#include "lyra/lowering/ast_to_hir/hierarchy_override.hpp"
#include "lyra/lowering/ast_to_hir/instance_array_shape.hpp"
#include "lyra/lowering/ast_to_hir/name_in_a_path.hpp"
#include "lyra/lowering/ast_to_hir/net_overlay.hpp"
#include "lyra/lowering/ast_to_hir/net_type.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/reads.hpp"
#include "lyra/lowering/ast_to_hir/statement/assertions.hpp"
#include "lyra/lowering/ast_to_hir/strength.hpp"
#include "lyra/lowering/ast_to_hir/subroutine_decl.hpp"
#include "lyra/lowering/ast_to_hir/time_resolution.hpp"
#include "lyra/lowering/ast_to_hir/unit_identity.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"
#include "lyra/profiling/time_trace.hpp"
#include "lyra/support/dump_kind.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// What a net or a variable is recorded as in a value dump (LRM 21.7.2.1
// `var_type`), none for one a dump records nothing of: a variable holding no
// integral value, real or event -- an unpacked array or structure, a string, a
// handle, a container (LRM 21.7.2.1) -- one of automatic lifetime, which no
// dump can reach, and a net of a type the language gives no keyword there or
// holding no integral value.
auto DumpedKindOf(const slang::ast::ValueSymbol& value)
    -> std::optional<support::DumpVarKind> {
  if (const auto* net = value.as_if<slang::ast::NetSymbol>()) {
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
  if (variable == nullptr ||
      variable->lifetime != slang::ast::VariableLifetime::Static) {
    return std::nullopt;
  }
  const slang::ast::Type& type = value.getType().getCanonicalType();
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
  if (const auto* predefined =
          type.as_if<slang::ast::PredefinedIntegerType>()) {
    switch (predefined->integerKind) {
      case slang::ast::PredefinedIntegerType::Integer:
        return support::DumpVarKind::kInteger;
      case slang::ast::PredefinedIntegerType::Time:
        return support::DumpVarKind::kTime;
      case slang::ast::PredefinedIntegerType::ShortInt:
      case slang::ast::PredefinedIntegerType::Int:
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

// Whether a value of `type` holds an unpacked tagged union anywhere in it. LRM
// 6.7.1 admits one in a net's data type, and no resolution of its drivers is
// carried out.
auto HoldsTaggedUnion(const slang::ast::Type& type) -> bool {
  const slang::ast::Type& canonical = type.getCanonicalType();
  const auto any_field =
      [](std::span<const slang::ast::FieldSymbol* const> fields) {
        return std::ranges::any_of(
            fields, [](const slang::ast::FieldSymbol* field) {
              return HoldsTaggedUnion(field->getType());
            });
      };
  if (const auto* overlay = canonical.as_if<slang::ast::UnpackedUnionType>()) {
    return overlay->isTagged || any_field(overlay->fields);
  }
  if (const auto* product = canonical.as_if<slang::ast::UnpackedStructType>()) {
    return any_field(product->fields);
  }
  if (const auto* array =
          canonical.as_if<slang::ast::FixedSizeUnpackedArrayType>()) {
    return HoldsTaggedUnion(array->elementType);
  }
  return false;
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

// How one instance is built: the unit it is an instance of, and where each
// parameter it takes at construction gets its value, in the unit's own order.
struct InstanceConstruction {
  std::string unit;
  std::string body;
  std::vector<ArgumentSource> arguments;
};

auto ConstructionOf(
    const slang::ast::InstanceSymbol& instance,
    const SpecializationPolicy& policy) -> InstanceConstruction {
  InstanceConstruction built{
      .unit = policy.NameOf(instance),
      .body = policy.BodyNameOf(instance),
      .arguments = {}};
  for (const slang::ast::ParameterSymbol* param :
       policy.SuppliedParametersOf(instance)) {
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

// Whether two elements of one instantiation are built alike. Each element
// has parameters of its own, so the arguments are compared by where each gets
// its value.
auto BuiltAlike(const InstanceConstruction& a, const InstanceConstruction& b)
    -> bool {
  return a.unit == b.unit && a.body == b.body &&
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
      owner.Signatures().Instantiated(built.unit);
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
                  owner.Unit(), frame, source.parameter->getValue(), type,
                  span);
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
// among this scope's expressions, which is where the construction reads them.
auto StructuralScopeLowerer::BuildInstanceMember(
    const slang::ast::Symbol& declared,
    std::span<const slang::ast::InstanceSymbol* const> elements,
    std::vector<hir::UnpackedRange> dims, WalkFrame frame)
    -> diag::Result<hir::InstanceMemberDecl> {
  hir::InstanceMemberDecl member{
      .instance_name = NameInAPath(declared),
      .array_dims = std::move(dims),
      .alternatives = {},
      .taken = {}};
  member.taken.reserve(elements.size());
  std::vector<InstanceConstruction> distinct;
  for (const slang::ast::InstanceSymbol* element : elements) {
    InstanceConstruction built =
        ConstructionOf(*element, owner_->Specialization());
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
            .scope_class = owner_->ExternalScopeClassOf(built.unit),
            .body = built.body,
            .assignments = *std::move(assignments)});
    distinct.push_back(std::move(built));
  }
  return member;
}

auto StructuralScopeLowerer::Run(WalkFrame parent_frame)
    -> diag::Result<hir::StructuralScope> {
  const profiling::TimeTraceScope span("lower scope", [this] {
    return slang_scope_->asSymbol().getHierarchicalPath();
  });
  hir::StructuralScope scope;
  // Filling a declaration is defining the identity a peer may already hold,
  // which is why this scope takes the pools rather than growing its own.
  ScopeDeclarations declarations = owner_->TakeScopeDeclarations(*slang_scope_);
  scope.structural_data_objects =
      std::move(declarations.structural_data_objects);
  scope.structural_subroutines = std::move(declarations.structural_subroutines);
  scope.processes = std::move(declarations.processes);
  scope.generates = std::move(declarations.generates);
  scope.instance_members = std::move(declarations.instance_members);
  scope.procedural_scopes = std::move(declarations.procedural_scopes);
  scope.replicated_classes = owner_->TakeReplicatedClasses(*slang_scope_);
  scope.replicated_covergroups =
      owner_->TakeReplicatedCovergroups(*slang_scope_);
  const WalkFrame frame =
      parent_frame.WithStructuralFrame(frame_, slang_scope_, &scope)
          .WithProceduralScopeOwner(slang_scope_, &scope.procedural_scopes);
  scope.time_resolution = ResolveTimeResolution(slang_scope_->getTimeScale());

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

  // The classes this scope replicates, lowered where a process of the scope
  // is: every declaration a class body may name is bound by now, and the
  // references it records against this scope are recorded before the scope
  // takes them.
  auto classes = owner_->PopulateClassBodiesReplicatedBy(*slang_scope_);
  if (!classes) return std::unexpected(std::move(classes.error()));
  auto covergroups = owner_->PopulateCovergroupsReplicatedBy(*slang_scope_);
  if (!covergroups) return std::unexpected(std::move(covergroups.error()));

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

  auto dumped =
      CollectDumped(*slang_scope_, DumpedPortsOf(*slang_scope_), frame);
  if (!dumped) return std::unexpected(std::move(dumped.error()));
  scope.dumped = *std::move(dumped);

  scope.routes = owner_->TakeRoutesForFrame(frame_);
  scope.published = owner_->TakePublication(*slang_scope_);
  return scope;
}

// The parameters of this scope whose values differ between the objects built
// from it, each a declaration of the scope. One supplied at construction -- a
// unit's overridden parameter, or the index a loop's block was built at -- is a
// value the construction fills, and the scope declares them in the order a
// construction supplies them. One computed from those holds the expression the
// source wrote for it. Every other parameter's value is fixed by the scope's
// specialization, so reading that value costs no second artifact.
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
      case ParameterValueSource::kFixedBySpecialization:
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
        return PopulateForeignImportMember(sub);
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
    case SymbolKind::ClassType:
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
    case SymbolKind::GenericClassDef:
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
    case SymbolKind::MethodPrototype:
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

    // Declarations whose effect happens where they are used, not where they
    // are written, so the use site is what has to support them.
    case SymbolKind::ClockingBlock:
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
  if (HoldsTaggedUnion(net.getType())) {
    // LRM 6.7.1, 7.3.2.
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedTypeKind,
        "a net holding a tagged union is not yet supported");
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

// The subroutine this scope evaluates `holder`'s expression in, for another
// unit that asks for the value: the expression names declarations that would
// mean nothing where that unit's signature is read, so what crosses is the call
// and the expression is lowered here, in the scope that wrote it. It is one
// return of the expression, as the type `holder` declares, and takes no
// formals.
auto StructuralScopeLowerer::DefineEvaluator(
    const slang::ast::Symbol& holder, std::string name,
    const slang::ast::Expression& expr, WalkFrame frame) -> diag::Result<void> {
  const auto span = frontend::PointSpanOf(holder.location);
  auto result_type = owner_->InternDeclaredPortType(holder);
  if (!result_type) return std::unexpected(std::move(result_type.error()));

  hir::ProceduralBody body;
  OpenProceduralScope root{
      frame.ProceduralScopes().Declare(),
      hir::ProceduralScopeKind::kSubroutineRoot, name};
  const hir::ProceduralVarId result_var = body.procedural_vars.Declare();
  body.procedural_vars.Define(
      result_var,
      hir::ProceduralVarDecl{.name = std::nullopt, .type = *result_type});
  root.declarations.push_back(result_var);
  ProcessLowerer lowerer(*owner_, holder);
  const WalkFrame body_frame =
      frame.WithProceduralBody(&body).WithOpenScope(&root);
  auto value = lowerer.LowerExpr(expr, body_frame);
  if (!value) return std::unexpected(std::move(value.error()));
  // A wait on the value asks the evaluator what it reads (LRM 9.4.2), and an
  // expression a scope evaluates reads that scope's own storage.
  auto cells = CellsOfWaitedExpression(lowerer, body_frame, expr);
  if (!cells) return std::unexpected(std::move(cells.error()));
  hir::Reads reads{
      .cells = *std::move(cells),
      .writes = {},
      .calls = {},
      .unreportable = std::nullopt};
  const hir::StmtId root_stmt = body.stmts.Add(
      hir::Stmt{
          .label = std::nullopt,
          .data = hir::ReturnStmt{.value = body.exprs.Add(*std::move(value))},
          .span = span});
  body.root_scope = frame.SealScope(std::move(root));
  frame.current_structural_scope->structural_subroutines.Define(
      owner_->EvaluatorOf(holder), hir::SubroutineDecl{
                                       .name = std::move(name),
                                       .kind = hir::SubroutineKind::kFunction,
                                       .result_type = *result_type,
                                       .params = {},
                                       .result_var = result_var,
                                       .body = std::move(body),
                                       .root_stmt = root_stmt,
                                       .is_virtual = false,
                                       .is_prototype = false,
                                       .is_static = false,
                                       .overrides = std::nullopt,
                                       .reads = std::move(reads)});
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
    if (port->direction != slang::ast::ArgumentDirection::In) continue;
    const auto* connection = port->getConnectionExpr();
    if (connection == nullptr) {
      throw InternalError(
          "PopulateModportMember: a name the view defines is the expression it "
          "was written with");
    }
    auto defined = DefineEvaluator(
        *port, ModportReadName(modport.name, port->name), *connection, frame);
    if (!defined) return std::unexpected(std::move(defined.error()));
  }
  return {};
}

// A port's default (LRM 23.2.2.4) is evaluated in this scope, not in the
// instantiator's, so an instance leaving the port unconnected asks this unit
// for it. A port with no default has nothing to build here.
auto StructuralScopeLowerer::PopulatePortMember(
    const slang::ast::PortSymbol& port, WalkFrame frame) -> diag::Result<void> {
  const slang::ast::Expression* initializer = port.getInitializer();
  if (initializer == nullptr) return {};
  return DefineEvaluator(port, PortDefaultName(port.name), *initializer, frame);
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
    const slang::ast::SubroutineSymbol& sym) -> diag::Result<void> {
  // The declaration is classified here even when nothing in this unit calls it,
  // so a signature outside the DPI-C type mapping (LRM 35.5.6) is reported
  // against the declaration that wrote it rather than against a call site far
  // away, or nowhere at all.
  auto id_or = owner_->EnsureForeignImport(sym);
  if (!id_or) return std::unexpected(std::move(id_or.error()));
  return {};
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
    const slang::ast::Scope& members, const DumpedPorts& ports, WalkFrame frame)
    -> diag::Result<hir::DumpedMembers> {
  hir::DumpedMembers dumped;
  // A named block, task or function, recorded where it declares something a
  // dump records.
  const auto collect_block =
      [&](const slang::ast::Scope& scope, std::string_view name,
          support::DumpScopeKind kind) -> diag::Result<void> {
    auto inner = CollectDumped(scope, {}, frame);
    if (!inner) return std::unexpected(std::move(inner.error()));
    if (!inner->declarations.empty() || !inner->blocks.empty()) {
      dumped.blocks.push_back(
          hir::DumpedBlock{
              .identifier = std::string{name},
              .kind = kind,
              .members = *std::move(inner)});
    }
    return {};
  };
  for (const slang::ast::Symbol& member : members.members()) {
    if (!owner_->Owns(member)) {
      continue;
    }
    if (const auto* block = member.as_if<slang::ast::StatementBlockSymbol>();
        block != nullptr && !block->name.empty()) {
      auto collected = collect_block(
          *block, block->name,
          block->blockKind == slang::ast::StatementBlockKind::Sequential
              ? support::DumpScopeKind::kBegin
              : support::DumpScopeKind::kFork);
      if (!collected) return std::unexpected(std::move(collected.error()));
      continue;
    }
    if (const auto* subroutine = member.as_if<slang::ast::SubroutineSymbol>()) {
      auto collected = collect_block(
          *subroutine, subroutine->name,
          subroutine->subroutineKind == slang::ast::SubroutineKind::Task
              ? support::DumpScopeKind::kTask
              : support::DumpScopeKind::kFunction);
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
    auto named =
        owner_->MakeRoutedValueRef(*value, frame_, ScopeRoute::Enclosing({}));
    if (!named) return std::unexpected(std::move(named.error()));
    const auto port = ports.find(value);
    dumped.declarations.push_back(
        hir::DumpedDeclaration{
            .identifier = std::string{value->name},
            .kind = *kind,
            .width = DumpedWidthOf(*kind, value->getType().getCanonicalType()),
            .range = DumpedRangeOf(value->getType()),
            .port = port != ports.end() ? std::optional(port->second)
                                        : std::nullopt,
            .named = frame.Exprs().Add(
                hir::MakeRefExpr(*std::move(named), *type, span))});
  }
  return dumped;
}

auto StructuralScopeLowerer::PopulateGenerateArrayMember(
    const slang::ast::GenerateBlockArraySymbol& array, WalkFrame frame)
    -> diag::Result<void> {
  auto g = BuildGenerateFromArray(array, frame);
  if (!g) return std::unexpected(std::move(g.error()));
  frame.current_structural_scope->generates.Define(
      owner_->GenerateIdOf(array), *std::move(g));
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
  frame.current_structural_scope->generates.Define(
      owner_->GenerateIdOf(block), *std::move(g));
  return {};
}

auto StructuralScopeLowerer::PopulateInstanceMember(
    const slang::ast::InstanceSymbol& inst, WalkFrame frame)
    -> diag::Result<void> {
  const std::array<const slang::ast::InstanceSymbol*, 1> one{&inst};
  auto member = BuildInstanceMember(inst, one, {}, frame);
  if (!member) return std::unexpected(std::move(member.error()));
  frame.current_structural_scope->instance_members.Define(
      owner_->InstanceMemberIdOf(inst), *std::move(member));
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
  auto member =
      BuildInstanceMember(array, shape->elements, std::move(dims), frame);
  if (!member) return std::unexpected(std::move(member.error()));
  frame.current_structural_scope->instance_members.Define(
      owner_->InstanceMemberIdOf(array), *std::move(member));
  return {};
}

}  // namespace lyra::lowering::ast_to_hir
