#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"

#include <algorithm>
#include <cstdint>
#include <expected>
#include <format>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/id_allocator.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/procedural_scope.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/hir_to_mir/binding_origin.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/class_shape.hpp"
#include "lyra/lowering/hir_to_mir/concurrent_assertion.hpp"
#include "lyra/lowering/hir_to_mir/continuous_assign.hpp"
#include "lyra/lowering/hir_to_mir/declaration_initializer.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/expression/dpi_call.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/lowering/hir_to_mir/lhs_store.hpp"
#include "lyra/lowering/hir_to_mir/namespace_storage_initialization.hpp"
#include "lyra/lowering/hir_to_mir/net_declaration.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/sampled_history.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/sensitivity_wait.hpp"
#include "lyra/lowering/hir_to_mir/static_var_binding.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/runtime_record.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/strength_level.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// Adds the runtime scope base's construction prefix (parent, hierarchy
// segment) as ordinary ctor params, in the order the base
// constructor consumes them.
void AttachRuntimeScopeCtorPrefix(
    const mir::CompilationUnit& unit, ClassShape& shape) {
  const auto& builtins = unit.builtins;
  shape.ctor_prefix_params.Add(
      mir::ParamDecl{.name = "parent", .type = builtins.scope_ptr});
  shape.ctor_prefix_params.Add(
      mir::ParamDecl{.name = "segment", .type = builtins.hierarchy_segment});
}

// The callable name for a body SV leaves unnamed: a process (LRM 9.2), a
// scope-level continuous assign (LRM 10.3), and the implicit assign a port
// connection carries (LRM 23.3.3). Each is named after what it is and where it
// stands among its own kind, both facts of the HIR entity itself. Nothing here
// consults how many callables the class already holds, which is what lets the
// shape phase mangle a process's static storage against a name the body phase
// produces later, and what keeps one body added to a scope from renaming every
// other body in it.
auto ProcessCallableName(hir::ProcessId id) -> std::string {
  return std::format("process_{}", id.value);
}

auto ContinuousAssignCallableName(hir::ContinuousAssignId id) -> std::string {
  return std::format("continuous_assign_{}", id.value);
}

auto PortConnectionCallableName(hir::PortConnectionId id) -> std::string {
  return std::format("port_connection_{}", id.value);
}

auto MakeUniqueObjectPointer(UnitLowerer& unit_lowerer, mir::ClassId class_id)
    -> mir::TypeId {
  const mir::TypeId object_type = unit_lowerer.Unit().types.Intern(
      mir::Type{mir::ObjectType{.class_id = class_id}});
  return unit_lowerer.Unit().types.Intern(
      mir::Type{mir::PointerType{
          .pointee = object_type,
          .ownership = mir::PointerOwnership::kUnique}});
}

// The pointer type a handle to one of `member`'s objects has. The object is one
// the declaring unit publishes, so the type names this unit's record of it.
auto MakeExternalUnitPointer(
    UnitLowerer& unit_lowerer, const hir::InstanceMemberDecl& member,
    mir::PointerOwnership ownership) -> mir::TypeId {
  const mir::TypeId object_type = unit_lowerer.Unit().types.Intern(
      mir::Type{mir::ExternalUnitObjectType{
          .object = unit_lowerer.TranslateExternalUnitObject(member.object)}});
  return unit_lowerer.Unit().types.Intern(
      mir::Type{
          mir::PointerType{.pointee = object_type, .ownership = ownership}});
}

// A type wrapped once per dimension still to be fixed: what a declaration
// standing for several objects covers where `depth` of its coordinates are
// open. At depth zero it is the type itself, which is why one object needs no
// case of its own.
auto SequenceOver(
    UnitLowerer& unit_lowerer, mir::TypeId type, std::size_t depth)
    -> mir::TypeId {
  for (std::size_t i = 0; i < depth; ++i) {
    type = unit_lowerer.Unit().types.Intern(
        mir::Type{mir::VectorType{.element = type}});
  }
  return type;
}

// The type of the member one instance declaration becomes. Multiplicity is this
// type and nothing else, so an array and a single instance are one declaration
// shape differing in how many wrappers stand over the handle (LRM 23.3.2).
auto MakeInstanceMemberType(
    UnitLowerer& unit_lowerer, const hir::InstanceMemberDecl& member,
    mir::PointerOwnership ownership) -> mir::TypeId {
  return SequenceOver(
      unit_lowerer, MakeExternalUnitPointer(unit_lowerer, member, ownership),
      member.array_dims.size());
}

// The position a coordinate names, as the machine integer a sequence is indexed
// by. Which element a route reaches is settled during elaboration, so it
// crosses as a constant rather than as a value the design computes.
auto BuildSequenceIndex(
    UnitLowerer& unit_lowerer, mir::Block& block, std::uint32_t coord)
    -> mir::ExprId {
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::MachineIntLiteral{.value = static_cast<std::int64_t>(coord)},
          .type = unit_lowerer.Unit().builtins.machine_int64});
}

// Builds one object an external-unit instance member declares, at `coords`, and
// hands back the borrowed pointer the runtime tree returns. The object is built
// and given to the tree to own; its Segment -- the label plus these coordinates
// -- is the key a by-name descent matches it on. A scalar instance is the
// coordinate-free case, built by the same expression.
auto BuildOwnedInstance(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, mir::ExprId parent_self,
    const std::string& runtime_label, mir::TypeId owning_pointer_type,
    mir::TypeId borrowed_pointer_type, std::span<const std::uint32_t> coords)
    -> mir::ExprId {
  mir::Block& block = *frame.current_block;
  const auto& builtins = unit_lowerer.Unit().builtins;

  std::vector<mir::ExprId> indices;
  indices.reserve(coords.size());
  for (const std::uint32_t coord : coords) {
    indices.push_back(BuildIntLiteral(
        unit_lowerer.Unit(), block, static_cast<std::int64_t>(coord)));
  }
  const mir::TypeId indices_type = mir::MachineArrayOf(
      unit_lowerer.Unit().types, builtins.int_type, indices.size());
  const mir::ExprId indices_id = block.exprs.Add(
      mir::Expr{
          .data = mir::CompositeExpr{.parts = std::move(indices)},
          .type = indices_type});
  const mir::ExprId segment_id = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Construct{},
                  .arguments =
                      {block.exprs.Add(
                           mir::MakeStringLiteral(
                               builtins.string, runtime_label)),
                       indices_id}},
          .type = builtins.hierarchy_segment});

  const mir::ExprId ctor_call_id = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Construct{},
                  .arguments = {parent_self, segment_id}},
          .type = owning_pointer_type});

  // The runtime tree owns the instance (AddOwnedChild consumes the freshly
  // built owning pointer) and hands back a borrowed handle, which is what
  // the parent keeps and what a layout-visible route step projects through.
  const mir::ExprId add_id = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = support::BuiltinFn::kAddOwnedChild,
                          .receiver = parent_self},
                  .arguments = {ctor_call_id}},
          .type = builtins.scope_ptr});
  return block.exprs.Add(
      mir::Expr{
          .data = mir::CastExpr{.operand = add_id},
          .type = borrowed_pointer_type});
}

// Builds what an instance declaration's member holds once `coords` are fixed as
// far as they go: the handle to the object those coordinates name when they are
// complete, and the sequence of what the next dimension holds while they are
// not. A sequence is composed where it is built rather than filled afterwards,
// so a dimension's elements are built inside the value that holds them.
auto BuildInstanceMemberValue(
    UnitLowerer& unit_lowerer, const WalkFrame& frame,
    const hir::InstanceMemberDecl& member, mir::TypeId owning,
    mir::TypeId borrowed, std::vector<std::uint32_t>& coords) -> mir::ExprId {
  mir::Block& block = *frame.current_block;
  if (coords.size() == member.array_dims.size()) {
    const mir::ExprId parent_self = block.exprs.Add(
        MakeSelfRefExpr(frame, frame.current_class->self_pointer_type));
    return BuildOwnedInstance(
        unit_lowerer, frame, parent_self, member.instance_name, owning,
        borrowed, coords);
  }
  const std::uint32_t count = member.array_dims[coords.size()];
  std::vector<mir::ExprId> elements;
  elements.reserve(count);
  for (std::uint32_t i = 0; i < count; ++i) {
    coords.push_back(i);
    elements.push_back(BuildInstanceMemberValue(
        unit_lowerer, frame, member, owning, borrowed, coords));
    coords.pop_back();
  }
  const mir::TypeId type = SequenceOver(
      unit_lowerer, borrowed, member.array_dims.size() - coords.size());
  return block.exprs.Add(BuildSequenceConstructionCall(
      unit_lowerer.Unit(), block, type, std::move(elements)));
}

// Emits the constructor-body construction for every object the scope's instance
// members declare: one value per declaration, holding every object that
// declaration builds, stored into the one field it was given. The runtime tree
// owns every built instance; the field keeps the borrowed handles a
// layout-visible route projects through.
void EmitInstanceMemberConstruction(
    StructuralScopeLowerer& lowerer, WalkFrame frame) {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  mir::Block& block = *frame.current_block;
  const hir::StructuralScope& hir_scope = lowerer.HirScope();
  for (const hir::InstanceMemberId id : hir_scope.instance_members.Ids()) {
    const hir::InstanceMemberDecl& im = hir_scope.instance_members.Get(id);
    const mir::TypeId owning = MakeExternalUnitPointer(
        unit_lowerer, im, mir::PointerOwnership::kUnique);
    const mir::TypeId borrowed = MakeExternalUnitPointer(
        unit_lowerer, im, mir::PointerOwnership::kBorrowed);
    std::vector<std::uint32_t> coords;
    const mir::ExprId value = BuildInstanceMemberValue(
        unit_lowerer, frame, im, owning, borrowed, coords);
    const mir::TypeId member_type =
        SequenceOver(unit_lowerer, borrowed, im.array_dims.size());
    const mir::ExprId member = block.exprs.Add(
        mir::MakeFieldAccessExpr(
            block.exprs.Add(
                MakeSelfRefExpr(frame, frame.current_class->self_pointer_type)),
            mir::ClassFieldTarget{
                .owner = frame.current_class_id,
                .slot = lowerer.InstanceMemberField(id)},
            member_type));
    block.AppendStmt(
        mir::ExprStmt{
            .expr = block.exprs.Add(
                mir::MakeAssignExpr(member, value, member_type))});
  }
}

// Allocates one MIR member per cross-unit reference. Every reference -- upward
// or downward, `$root`-anchored or named -- takes one slot, typed by what the
// route it seals ends at, so a body reaching through it meets the target's own
// access protocol and no other. The route that fills each slot runs in the
// resolve phase, after the whole object tree exists.
auto DeclareRoutedRefSlots(StructuralScopeLowerer& lowerer, ClassShape& shape)
    -> base::Translation<hir::RoutedRefId, RoutedRefMeta> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  const hir::StructuralScope& hir_scope = lowerer.HirScope();
  std::vector<RoutedRefMeta> slots;
  slots.reserve(hir_scope.routed_refs.size());
  for (const auto& cu : hir_scope.routed_refs) {
    std::string member_name = "ep" + std::to_string(slots.size());
    // What the slot is typed by is what the endpoint holds. A cell, an object,
    // and a disable target are each reached by a pointer to them; an entry is a
    // code address, which is one already.
    mir::TypePool& types = unit_lowerer.Unit().types;
    const auto borrowed = [&](mir::TypeId pointee) {
      return types.Intern(
          mir::Type{mir::PointerType{
              .pointee = pointee,
              .ownership = mir::PointerOwnership::kBorrowed}});
    };
    const mir::TypeId slot_type = std::visit(
        Overloaded{
            [&](const hir::EndpointCell& cell) {
              return borrowed(unit_lowerer.MemberCellType(
                  unit_lowerer.TranslateType(cell.type), cell.storage));
            },
            [&](const hir::EndpointObject& object) {
              return borrowed(unit_lowerer.TranslateType(object.type));
            },
            [&](const hir::EndpointEntry&) {
              return mir::ErasedFunction(types);
            },
            [&](const hir::EndpointDisableTarget&) {
              return borrowed(types.Intern(
                  mir::Type{mir::RuntimeLibraryType{
                      .kind = mir::RuntimeLibraryKind::kCancellationTarget}}));
            }},
        hir::EndpointOf(cu.recipe.leaf));
    slots.push_back(
        RoutedRefMeta{
            .target = shape.fields.Add(
                mir::FieldDecl{
                    .name = std::move(member_name), .type = slot_type}),
            .slot_type = slot_type});
  }
  return {hir_scope.routed_refs.size(), std::move(slots)};
}

// Builds one `PackedArray[]` value carrying every per-axis index for a
// single hop; the runtime SDK's `GetChild` / `ResolveVisibleChild` accept
// it as a `std::span<PackedArray>`.
auto BuildIndicesLiteral(
    UnitLowerer& unit_lowerer, mir::Block& block,
    std::span<const std::uint32_t> indices) -> mir::ExprId {
  const auto& builtins = unit_lowerer.Unit().builtins;
  std::vector<mir::ExprId> ids;
  ids.reserve(indices.size());
  for (const std::uint32_t idx : indices) {
    ids.push_back(BuildIntLiteral(
        unit_lowerer.Unit(), block, static_cast<std::int64_t>(idx)));
  }
  const mir::TypeId indices_type = mir::MachineArrayOf(
      unit_lowerer.Unit().types, builtins.int_type, indices.size());
  return block.exprs.Add(
      mir::Expr{
          .data = mir::CompositeExpr{.parts = std::move(ids)},
          .type = indices_type});
}

auto BuildStringLiteral(
    UnitLowerer& unit_lowerer, mir::Block& block, const std::string& s)
    -> mir::ExprId {
  return block.exprs.Add(
      mir::MakeStringLiteral(unit_lowerer.Unit().builtins.string, s));
}

// A route runs from its origin (the referrer's `self`) to the referenced leaf,
// and each step reaches through whatever the step before it landed on. What
// that is decides how the next one may reach: a scope this artifact lowers
// admits a typed member access onto anything it declares, an object another
// unit defines is a typed pointer whose names resolve against the signature
// that unit published, and the base every object on the tree is one of admits
// only a name the runtime answers.
struct OwnScope {
  const StructuralScopeLowerer* scope;
};
struct ExternalObject {};
struct ScopeBase {};

using ReceiverTarget = std::variant<OwnScope, ExternalObject, ScopeBase>;

struct RouteReceiver {
  mir::ExprId expr{};
  ReceiverTarget target;
};

// The scope a step or leaf naming one of this artifact's own declarations is
// standing on. Reaching one is only possible while the route is still inside
// the artifact, so a receiver that has left it is a route built against a
// different design than the one it reached.
auto OwnScopeOf(const RouteReceiver& receiver, std::string_view site)
    -> const StructuralScopeLowerer& {
  const auto* own = std::get_if<OwnScope>(&receiver.target);
  if (own == nullptr) {
    throw InternalError(
        std::format(
            "{}: the route names a declaration of a scope this artifact "
            "lowers, so it cannot have left the artifact before reaching it",
            site));
  }
  return *own->scope;
}

// Reaches a child by name+indices as an opaque `Scope*` -- the realization of
// an opaque step (one crossing into another unit's body).
auto StepToChildByName(
    UnitLowerer& unit_lowerer, mir::Block& block, mir::ExprId receiver,
    const std::string& name, std::span<const std::uint32_t> indices)
    -> RouteReceiver {
  const mir::ExprId step = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = support::BuiltinFn::kFindChild,
                          .receiver = receiver},
                  .arguments =
                      {BuildStringLiteral(unit_lowerer, block, name),
                       BuildIndicesLiteral(unit_lowerer, block, indices)}},
          .type = unit_lowerer.Unit().builtins.scope_ptr});
  return RouteReceiver{.expr = step, .target = ScopeBase{}};
}

// Establishes the route's starting receiver from the head. An in-unit head
// climbs `hops` typed parent edges to an ancestor scope of this unit, which
// keeps the receiver typed. `$root` and the visible-child climb name a scope
// this unit does not declare, so both are opaque runtime-SDK reaches.
auto BuildRouteAnchor(
    StructuralScopeLowerer& lowerer, const WalkFrame& frame,
    const hir::RouteHead& head) -> RouteReceiver {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  auto& unit = unit_lowerer.Unit();
  mir::Block& block = *frame.current_block;
  const mir::TypeId scope_ptr_type = unit.builtins.scope_ptr;

  if (const auto* ih = std::get_if<hir::InUnitHead>(&head)) {
    return RouteReceiver{
        .expr = BuildEnclosingScopeReceiver(
            frame, unit, mir::EnclosingHops{.value = ih->hops.value}),
        .target = OwnScope{&lowerer.EnclosingScopeAtHops(ih->hops)}};
  }

  const mir::ExprId self_ref = block.exprs.Add(
      MakeSelfRefExpr(frame, frame.current_class->self_pointer_type));

  if (std::holds_alternative<hir::RootHead>(head)) {
    const mir::ExprId root = block.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee =
                        mir::Direct{
                            .target = support::BuiltinFn::kResolveRoot,
                            .receiver = self_ref},
                    .arguments = {}},
            .type = scope_ptr_type});
    return RouteReceiver{.expr = root, .target = ScopeBase{}};
  }

  // The visible-child climb walks the parent chain by name (LRM 23.8).
  const auto& vc = std::get<hir::VisibleChildHead>(head);
  const mir::ExprId matched = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = support::BuiltinFn::kResolveVisibleChild,
                          .receiver = self_ref},
                  .arguments =
                      {BuildStringLiteral(unit_lowerer, block, vc.head_name),
                       BuildIndicesLiteral(
                           unit_lowerer, block, vc.head_indices)}},
          .type = scope_ptr_type});
  return RouteReceiver{.expr = matched, .target = ScopeBase{}};
}

// Descends one step into a child the receiver's scope declares: the typed
// member access that projects the parent's handle on that child, then one index
// per coordinate the step names. The coordinates are settled during
// elaboration, and each takes one dimension off what the member holds, so a
// child with no declared dimensions indexes nothing and is the same step with
// no coordinates. A child whose body is another compilation unit is still
// reached by a typed pointer, but what it declares is that unit's to state, so
// the route stops resolving names against a scope of this one.
auto StepToOwnedChild(
    UnitLowerer& unit_lowerer, mir::Block& block, const RouteReceiver& receiver,
    const hir::OwnedChildStep& step) -> RouteReceiver {
  const StructuralScopeLowerer& scope =
      OwnScopeOf(receiver, "StepToOwnedChild");
  const OwnedChildAnchor anchor =
      scope.TranslateOwnedChild(hir::StructuralHops{0}, step.child);
  const mir::ClassId receiver_class = scope.ClassId();
  mir::TypeId reached = unit_lowerer.GetClassShape(receiver_class)
                            .fields.Get(anchor.borrowed_handle)
                            .type;
  mir::ExprId access = block.exprs.Add(
      mir::MakeFieldAccessExpr(
          receiver.expr,
          mir::ClassFieldTarget{
              .owner = receiver_class, .slot = anchor.borrowed_handle},
          reached));
  for (const std::uint32_t coord : step.indices) {
    reached =
        unit_lowerer.Unit().types.Get(reached).Get<mir::VectorType>().element;
    access = block.exprs.Add(
        mir::Expr{
            .data =
                mir::VectorGetExpr{
                    .vector = access,
                    .index = BuildSequenceIndex(unit_lowerer, block, coord)},
            .type = reached});
  }
  // A child whose body is another compilation unit leaves the artifact here;
  // one this artifact lowers keeps the route inside it.
  return RouteReceiver{
      .expr = access,
      .target = anchor.target_scope == nullptr
                    ? ReceiverTarget{ExternalObject{}}
                    : ReceiverTarget{OwnScope{anchor.target_scope}}};
}

// Descends one step through an interface port of the receiver's scope: the
// typed member access that projects the borrowed reference the parent bound
// there (LRM 25.3), then one index per coordinate the step names, since a port
// carrying a range is one member standing for every instance bound to it.
// Everything past the step belongs to the unit the port names, which this
// artifact does not lower, so the receiver stops being one of its own scopes --
// the same place an owned child whose body is another unit leaves it.
auto StepThroughInterfacePort(
    UnitLowerer& unit_lowerer, mir::Block& block, const RouteReceiver& receiver,
    const hir::InterfacePortStep& step) -> RouteReceiver {
  const StructuralScopeLowerer& scope =
      OwnScopeOf(receiver, "StepThroughInterfacePort");
  const mir::ClassId receiver_class = scope.ClassId();
  const mir::FieldId field =
      scope.TranslateInterfacePort(hir::StructuralHops{0}, step.port);
  mir::TypeId reached =
      unit_lowerer.GetClassShape(receiver_class).fields.Get(field).type;
  mir::ExprId access = block.exprs.Add(
      mir::MakeFieldAccessExpr(
          receiver.expr,
          mir::ClassFieldTarget{.owner = receiver_class, .slot = field},
          reached));
  for (const std::uint32_t coord : step.indices) {
    reached =
        unit_lowerer.Unit().types.Get(reached).Get<mir::VectorType>().element;
    access = block.exprs.Add(
        mir::Expr{
            .data =
                mir::VectorGetExpr{
                    .vector = access,
                    .index = BuildSequenceIndex(unit_lowerer, block, coord)},
            .type = reached});
  }
  return RouteReceiver{.expr = access, .target = ExternalObject{}};
}

// A member another unit published, as the field it is: the object recorded
// from that unit's signature declares it, and the receiver the route has
// descended to is an instance of exactly that object.
auto PublishedMemberTarget(
    const mir::CompilationUnit& unit, const mir::Block& block,
    const RouteReceiver& receiver, hir::PublishedMemberId member)
    -> mir::ExternalUnitObjectFieldTarget {
  const mir::TypeId pointee =
      unit.types.Get(block.exprs.Get(receiver.expr).type)
          .Get<mir::PointerType>()
          .pointee;
  return mir::ExternalUnitObjectFieldTarget{
      .owner =
          unit.types.Get(pointee).Get<mir::ExternalUnitObjectType>().object,
      .slot = UnitLowerer::TranslatePublishedMember(member)};
}

// Descends one step onto a member another unit published whose type makes it an
// object of a third unit (LRM 25.3, 25.10): the member access at the position
// that unit's signature gave it, then one index per coordinate the step names.
auto StepToSignatureMember(
    UnitLowerer& unit_lowerer, mir::Block& block, const RouteReceiver& receiver,
    const hir::SignatureMemberStep& step) -> RouteReceiver {
  const mir::ExternalUnitObjectFieldTarget target =
      PublishedMemberTarget(unit_lowerer.Unit(), block, receiver, step.member);
  mir::TypeId reached = unit_lowerer.Unit()
                            .external_unit_objects.Get(target.owner)
                            .fields.Get(target.slot)
                            .type;
  mir::ExprId access =
      block.exprs.Add(mir::MakeFieldAccessExpr(receiver.expr, target, reached));
  for (const std::uint32_t coord : step.indices) {
    reached =
        unit_lowerer.Unit().types.Get(reached).Get<mir::VectorType>().element;
    access = block.exprs.Add(
        mir::Expr{
            .data =
                mir::VectorGetExpr{
                    .vector = access,
                    .index = BuildSequenceIndex(unit_lowerer, block, coord)},
            .type = reached});
  }
  return RouteReceiver{.expr = access, .target = ExternalObject{}};
}

// Projects the borrowed-pointer value the slot takes out of a typed receiver:
// the field access, addressed. Everything a scope's bodies declare with a
// lifetime longer than an activation is a field of the scope's own class, so
// the receiver is already standing where the field is.
auto AddressTypedLeaf(
    UnitLowerer& unit_lowerer, mir::Block& block, const RouteReceiver& receiver,
    mir::ClassId owner_class, mir::FieldId field, mir::TypeId slot_type)
    -> mir::ExprId {
  const mir::TypeId field_type =
      unit_lowerer.GetClassShape(owner_class).fields.Get(field).type;
  const mir::ExprId access = block.exprs.Add(
      mir::MakeFieldAccessExpr(
          receiver.expr,
          mir::ClassFieldTarget{.owner = owner_class, .slot = field},
          field_type));
  return block.exprs.Add(
      mir::Expr{
          .data = mir::AddressOfExpr{.operand = access}, .type = slot_type});
}

// Materializes the leaf reach as the borrowed-pointer value the slot takes:
// the addressed member access when the leaf is one this artifact declares or
// one the target unit published, or a cast of the untyped address a by-name
// signal query answers with when it reaches past a signature, where nothing
// was promised for this one to compile against.
auto MaterializeLeaf(
    UnitLowerer& unit_lowerer, mir::Block& block, const RouteReceiver& receiver,
    const hir::RouteLeaf& leaf, mir::TypeId slot_type) -> mir::ExprId {
  auto& unit = unit_lowerer.Unit();

  // A published member is reached through the target unit's own object, whose
  // pointer the step before it produced.
  if (const auto* member = std::get_if<hir::SignatureMemberLeaf>(&leaf)) {
    const auto& slot = unit.types.Get(slot_type).Get<mir::PointerType>();
    const mir::ExprId access = block.exprs.Add(
        mir::MakeFieldAccessExpr(
            receiver.expr,
            PublishedMemberTarget(unit, block, receiver, member->member),
            slot.pointee));
    return block.exprs.Add(
        mir::Expr{
            .data = mir::AddressOfExpr{.operand = access}, .type = slot_type});
  }

  // A route ending at a scope names the object the steps landed on, which the
  // last step already produced as a borrowed pointer. What the slot holds that
  // object as is a separate fact -- a receiver a call passes takes the scope
  // every body is entered through, where a member read takes the object's own
  // type -- so the value states the slot's type rather than staying whatever
  // the step happened to reach.
  if (std::holds_alternative<hir::ScopeLeaf>(leaf)) {
    return block.exprs.Add(
        mir::Expr{
            .data = mir::CastExpr{.operand = receiver.expr},
            .type = slot_type});
  }

  // A callable reached past a signature is answered the same way a cell is,
  // in the same phase, from the scope's own record of what it declares -- what
  // differs is only which of the two namespaces the name is looked up in and
  // that the answer is already a code address rather than something to cast.
  if (const auto* callable = std::get_if<hir::OpaqueCallableLeaf>(&leaf)) {
    return block.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee =
                        mir::Direct{
                            .target = support::BuiltinFn::kFindSubroutine,
                            .receiver = receiver.expr},
                    .arguments = {BuildStringLiteral(
                        unit_lowerer, block, callable->name)}},
            .type = slot_type});
  }

  // What a `disable` terminates is answered by the scope the steps reached,
  // unnamed (LRM 9.6.2, 23.9).
  if (std::holds_alternative<hir::OpaqueDisableTargetLeaf>(leaf)) {
    return block.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee =
                        mir::Direct{
                            .target = support::BuiltinFn::kFindDisableTarget,
                            .receiver = receiver.expr},
                    .arguments = {}},
            .type = slot_type});
  }

  if (const auto* opaque = std::get_if<hir::OpaqueLeaf>(&leaf)) {
    const mir::TypeId void_ptr_type = unit.types.Intern(
        mir::Type{mir::PointerType{
            .pointee = unit.builtins.void_type,
            .ownership = mir::PointerOwnership::kBorrowed}});
    const mir::ExprId raw = block.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee =
                        mir::Direct{
                            .target = support::BuiltinFn::kFindSignal,
                            .receiver = receiver.expr},
                    .arguments = {BuildStringLiteral(
                        unit_lowerer, block, opaque->name)}},
            .type = void_ptr_type});
    return block.exprs.Add(
        mir::Expr{.data = mir::CastExpr{.operand = raw}, .type = slot_type});
  }

  const StructuralScopeLowerer& scope = OwnScopeOf(receiver, "MaterializeLeaf");

  if (const auto* object = std::get_if<hir::StructuralDataObjectLeaf>(&leaf)) {
    return AddressTypedLeaf(
        unit_lowerer, block, receiver, scope.ClassId(),
        scope.TranslateStructuralDataObject(
            hir::StructuralHops{0}, object->object),
        slot_type);
  }

  if (const auto* target = std::get_if<hir::DisableTargetLeaf>(&leaf)) {
    return AddressTypedLeaf(
        unit_lowerer, block, receiver, scope.ClassId(),
        scope.DisableTargetField(target->scope), slot_type);
  }

  const auto& static_leaf = std::get<hir::ProceduralStaticLeaf>(leaf);
  return AddressTypedLeaf(
      unit_lowerer, block, receiver, scope.ClassId(),
      scope.ProceduralStaticField(static_leaf.body, static_leaf.var),
      slot_type);
}

// Composes the resolve-phase pointer value that fills a routed reference
// slot: anchor from the head, walk the descent steps, and materialize the
// leaf. The result flows into the ordinary assignment the caller emits into
// the resolve block.
auto BuildRouteValue(
    StructuralScopeLowerer& lowerer, const WalkFrame& frame,
    const hir::RoutedPathRecipe& recipe, mir::TypeId slot_type) -> mir::ExprId {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  mir::Block& block = *frame.current_block;
  RouteReceiver receiver = BuildRouteAnchor(lowerer, frame, recipe.head);
  for (const auto& step : recipe.steps) {
    receiver = std::visit(
        Overloaded{
            [&](const hir::OwnedChildStep& owned) {
              return StepToOwnedChild(unit_lowerer, block, receiver, owned);
            },
            [&](const hir::InterfacePortStep& port) {
              return StepThroughInterfacePort(
                  unit_lowerer, block, receiver, port);
            },
            [&](const hir::SignatureMemberStep& member) {
              return StepToSignatureMember(
                  unit_lowerer, block, receiver, member);
            },
            [&](const hir::OpaqueStep& opaque) {
              return StepToChildByName(
                  unit_lowerer, block, receiver.expr, opaque.name,
                  opaque.indices);
            }},
        step);
  }
  return MaterializeLeaf(unit_lowerer, block, receiver, recipe.leaf, slot_type);
}

// Each routed reference resolves in the resolve phase: the top-down walk over
// the fully-constructed object tree runs each route, filling the scope's `ep_N`
// endpoint slot with a borrowed pointer to the target's observable cell. Every
// route lands as an ordinary `AssignExpr` on the slot member.
void InstallRoutedRefs(
    StructuralScopeLowerer& lowerer, const WalkFrame& resolve_frame) {
  mir::Class& mir_class = *resolve_frame.current_class;
  mir::Block& resolve_block = *resolve_frame.current_block;
  const hir::StructuralScope& hir_scope = lowerer.HirScope();
  for (const hir::RoutedRefId hir_id : hir_scope.routed_refs.Ids()) {
    const auto& cu = hir_scope.routed_refs.Get(hir_id);
    const mir::FieldId slot = lowerer.RoutedRefTarget(hir_id).target;
    const mir::TypeId slot_type = mir_class.fields.Get(slot).type;
    const mir::ExprId nav =
        BuildRouteValue(lowerer, resolve_frame, cu.recipe, slot_type);
    const mir::ExprId self_for_target = resolve_block.exprs.Add(
        MakeSelfRefExpr(resolve_frame, mir_class.self_pointer_type));
    const mir::ExprId target = resolve_block.exprs.Add(
        mir::Expr{
            .data =
                mir::FieldAccessExpr{
                    .receiver = self_for_target,
                    .field =
                        mir::ClassFieldTarget{
                            .owner = resolve_frame.current_class_id,
                            .slot = slot}},
            .type = slot_type});
    const mir::ExprId assign = resolve_block.exprs.Add(
        mir::Expr{
            .data = mir::AssignExpr{.target = target, .value = nav},
            .type = slot_type});
    resolve_block.AppendStmt(mir::ExprStmt{.expr = assign});
  }
}

// Appends one process activation registration to the scope's `activate` body:
// invokes `body` over the activate frame's `self` to produce the coroutine,
// then registers it for the scope's startup (`is_final == false`) or shutdown
// (`is_final == true`) lifecycle (LRM 9.2). Startup and shutdown are distinct
// registration callees, not one tagged call.
//
// The registration also names the unit instance the process belongs to, which
// is where LRM 18.14.1 keeps the seeds a static process starts from. That
// instance is the scope the artifact's own class tree is rooted at, a fixed
// number of steps out from wherever the process is declared, so the call
// reaches it by typed navigation over a distance this walk already knows.
void AppendProcessRegistration(
    UnitLowerer& unit_lowerer, const WalkFrame& activate_frame,
    mir::CallableId body, bool is_final) {
  mir::Block& block = *activate_frame.current_block;
  const mir::TypeId self_ptr_type =
      activate_frame.current_class->self_pointer_type;
  const mir::ExprId body_self =
      block.exprs.Add(MakeSelfRefExpr(activate_frame, self_ptr_type));
  const mir::ExprId body_call = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target =
                              mir::CallableTarget{
                                  .owner = activate_frame.current_class_id,
                                  .slot = body},
                          .receiver = body_self},
                  .arguments = {}},
          .type = unit_lowerer.Unit().builtins.coroutine_void});
  const mir::ExprId reg_self =
      block.exprs.Add(MakeSelfRefExpr(activate_frame, self_ptr_type));
  const mir::ExprId unit_instance = BuildEnclosingScopeReceiver(
      activate_frame, unit_lowerer.Unit(), activate_frame.HopsToUnitRoot());
  const mir::ExprId reg_call = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = is_final
                                        ? support::BuiltinFn::kRegisterFinal
                                        : support::BuiltinFn::kRegisterInitial},
                  .arguments = {reg_self, unit_instance, body_call}},
          .type = unit_lowerer.Unit().builtins.void_type});
  block.AppendStmt(mir::ExprStmt{.expr = reg_call});
}

// Composes the value a port member takes from the handles its connection
// supplies: the handle itself where the member stands for one object, and the
// sequence of what the dimension below holds where it stands for several. The
// shape is read off the child's own declared type, so how many the parent
// supplies per dimension is the child's promise rather than a second count.
// `next` walks the handles in the order the port's coordinates count them.
auto ComposeBoundObjects(
    UnitLowerer& unit_lowerer, mir::Block& block, hir::TypeId member_type,
    std::span<const mir::ExprId> handles, std::size_t& next) -> mir::ExprId {
  const auto* array =
      unit_lowerer.Hir().types.Get(member_type).As<hir::UnpackedArrayType>();
  if (array == nullptr) {
    if (next >= handles.size()) {
      throw InternalError(
          "ComposeBoundObjects: the connection supplies one instance per "
          "object the port stands for, which is checked where it is recorded");
    }
    return handles[next++];
  }
  const std::uint64_t count = array->dim.ElementCount();
  std::vector<mir::ExprId> elements;
  elements.reserve(count);
  for (std::uint64_t i = 0; i < count; ++i) {
    elements.push_back(ComposeBoundObjects(
        unit_lowerer, block, array->element_type, handles, next));
  }
  return block.exprs.Add(BuildSequenceConstructionCall(
      unit_lowerer.Unit(), block,
      unit_lowerer.MemberCellType(
          unit_lowerer.TranslateType(member_type),
          hir::BorrowedObjectStorage{}),
      std::move(elements)));
}

// Binds a child's interface port to the interface instances the connection
// names (LRM 25.3), in the resolve phase where the object tree is complete: the
// route to the child's member yields a pointer to the slot the child holds, the
// routes to the interfaces yield the objects, and one store fills the one with
// the other. The child owns no storage on either side, so nothing else happens
// here -- the same shape a `ref` port's alias bind takes, over objects rather
// than a cell.
void InstallInterfacePortConnection(
    StructuralScopeLowerer& lowerer, const WalkFrame& resolve_frame,
    const hir::InterfacePortConnection& conn) {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  mir::Block& block = *resolve_frame.current_block;
  auto& types = unit_lowerer.Unit().types;
  // What the member holds is a handle on each object it stands for, which is
  // the same function of its declared type the declaring unit built it from.
  const hir::TypeId port_type =
      std::get<hir::EndpointCell>(hir::EndpointOf(conn.endpoint.leaf)).type;
  const mir::TypeId member_type = unit_lowerer.MemberCellType(
      unit_lowerer.TranslateType(port_type), hir::BorrowedObjectStorage{});
  const mir::TypeId slot_type = types.Intern(
      mir::Type{mir::PointerType{
          .pointee = member_type,
          .ownership = mir::PointerOwnership::kBorrowed}});
  const mir::ExprId nav =
      BuildRouteValue(lowerer, resolve_frame, conn.endpoint, slot_type);
  const mir::ExprId target = block.exprs.Add(
      mir::Expr{.data = mir::DerefExpr{.pointer = nav}, .type = member_type});

  std::vector<mir::ExprId> handles;
  handles.reserve(conn.peers.size());
  for (const hir::RoutedPathRecipe& peer : conn.peers) {
    const mir::TypeId handle_type = types.Intern(
        mir::Type{mir::PointerType{
            .pointee = unit_lowerer.TranslateType(
                std::get<hir::EndpointObject>(hir::EndpointOf(peer.leaf)).type),
            .ownership = mir::PointerOwnership::kBorrowed}});
    handles.push_back(
        BuildRouteValue(lowerer, resolve_frame, peer, handle_type));
  }
  std::size_t next = 0;
  const mir::ExprId value =
      ComposeBoundObjects(unit_lowerer, block, port_type, handles, next);
  block.AppendStmt(
      mir::ExprStmt{
          .expr = block.exprs.Add(
              mir::Expr{
                  .data = mir::AssignExpr{.target = target, .value = value},
                  .type = member_type})});
}

// Realizes each port connection (LRM 23.3.3). An input or output port is the
// implied continuous assignment between the two cells, materialized as the same
// synthesized process a scope-level `assign` produces, registered as a process;
// when the driven side is a net the edge attaches a driver rather than writing
// the cell. A `ref` port instead binds the child's reference member --
// navigated by name from the owned child -- to the connected variable's cell,
// emitted into the resolve block: one assignment of a reference, with no second
// cell and no continuous assignment.
auto InstallPortConnections(
    StructuralScopeLowerer& lowerer, WalkFrame frame, WalkFrame resolve_frame,
    WalkFrame init_frame, WalkFrame activate_frame) -> diag::Result<void> {
  mir::Class& mir_class = *frame.current_class;
  mir::Block& resolve_block = *resolve_frame.current_block;
  const hir::StructuralScope& hir_scope = lowerer.HirScope();
  UnitLowerer& unit_lowerer = lowerer.Owner();
  for (const hir::PortConnectionId id : hir_scope.port_connections.Ids()) {
    const hir::PortConnection& pc = hir_scope.port_connections.Get(id);
    if (const auto* iface =
            std::get_if<hir::InterfacePortConnection>(&pc.kind)) {
      InstallInterfacePortConnection(lowerer, resolve_frame, *iface);
      continue;
    }
    const auto& data = std::get<hir::DataPortConnection>(pc.kind);
    // A `ref` port binds once and is done; the two value directions share the
    // reactive edge built below and differ only in which end of it drives
    // (LRM 23.3.3).
    switch (data.direction) {
      case hir::PortDirection::kInput:
      case hir::PortDirection::kOutput:
        break;
      // A unit publishes every direction the language admits, and AST-to-HIR
      // refuses these two, so a recorded connection never carries one.
      case hir::PortDirection::kInOut:
      case hir::PortDirection::kConstRef:
        throw InternalError(
            "InstallPortConnections: a refused port direction reached the "
            "connection switch");
      case hir::PortDirection::kRef: {
        // A `ref` port reaches the child's reference member by the same route
        // navigation a routed reference uses, then binds it to the peer's cell
        // through the one canonical reference-store primitive. It holds no
        // persistent slot -- a `ref` needs no simulation-time reach, so the
        // member is reached once here in the resolve phase (LRM 23.3.3.2).
        const auto& recipe = std::get<hir::RoutedPathRecipe>(data.endpoint);
        if (!std::holds_alternative<hir::InUnitHead>(recipe.head)) {
          throw InternalError(
              "InstallPortConnections: a ref port reaches its child downward");
        }
        const mir::TypeId value_type = unit_lowerer.TranslateType(
            std::get<hir::EndpointCell>(hir::EndpointOf(recipe.leaf)).type);
        const mir::TypeId ref_type = unit_lowerer.Unit().types.Intern(
            mir::Type{mir::RefType{
                .pointee = value_type,
                .mutability = mir::Mutability::kMutable}});
        const mir::TypeId slot_type = unit_lowerer.Unit().types.Intern(
            mir::Type{mir::PointerType{
                .pointee = ref_type,
                .ownership = mir::PointerOwnership::kBorrowed}});
        const mir::ExprId nav =
            BuildRouteValue(lowerer, resolve_frame, recipe, slot_type);
        const mir::ExprId target = resolve_block.exprs.Add(
            mir::Expr{
                .data = mir::DerefExpr{.pointer = nav}, .type = ref_type});

        auto peer_or =
            lowerer.LowerLhsExpr(hir_scope.exprs.Get(data.peer), resolve_frame);
        if (!peer_or) return std::unexpected(std::move(peer_or.error()));
        const mir::ExprId peer_cell = peer_or->owner;

        const mir::ExprId bind = BindReferenceSlot(
            unit_lowerer.Unit(), resolve_block, target, peer_cell);
        resolve_block.AppendStmt(mir::ExprStmt{.expr = bind});
        continue;
      }
    }
    const auto& cell = std::get<hir::PortCellEndpoint>(data.endpoint);
    const bool is_input = data.direction == hir::PortDirection::kInput;
    // A port connection is a reactive edge: the source is read, the sink is
    // driven. An input port's source is the parent expression and its sink is
    // the child cell; an output port's source is the child cell and its sink
    // is the parent target. The edge is the same continuous assignment either
    // way, written with no strength of its own and so driving at strong like
    // any other (LRM 23.3.3, 10.3.1); the sink's own MIR type -- resolved-net
    // cell or observable cell -- picks the write protocol.
    const hir::ContinuousAssign assign{
        .span = pc.span,
        .lhs = is_input ? cell.cell : data.peer,
        .rhs = is_input ? data.peer : cell.cell,
        .strength = support::StrengthLevel::kStrong,
        .sensitivity_list = data.sensitivity};
    auto method_or = LowerContinuousAssign(
        lowerer, frame, resolve_frame, init_frame,
        PortConnectionCallableName(id), assign);
    if (!method_or) return std::unexpected(std::move(method_or.error()));
    const mir::CallableId body = mir_class.callables.Add(std::move(*method_or));
    AppendProcessRegistration(unit_lowerer, activate_frame, body, false);
  }
  return {};
}

void ValidateOwnedChildConstruction(
    const mir::Class& owner_class, mir::ClassId child_scope_id) {
  if (std::ranges::find(owner_class.contained, child_scope_id) ==
      owner_class.contained.end()) {
    throw InternalError(
        "owned-child construction: child scope is not a direct child of the "
        "enclosing class");
  }
}

// Lowers an owned-child construction site to the MIR call shape
// `AddOwnedChild(parent, make_unique<Child>(parent, HierarchySegment{label,
// indices}, ctor_args...))`: the child instance is built carrying
// its complete hierarchy identity, then handed to the parent to own. The
// runtime tree owns the child; the parent keeps no member, and a later
// reference reaches it by name through GetChild. `runtime_label` is the
// SV-visible identifier; an anonymous scope gets an empty label, which the
// runtime treats as non-addressable so a peer by-name lookup walks past it to
// the addressable descendants underneath. `arm_frame` must point at the block
// where the stmts land and carry the constructor's bindings so a `self` read
// resolves to the receiver binding.
//
// Where the child hangs in the runtime tree and who keeps the borrowed handle
// to it are separate: `runtime_parent_handle` names an object this one already
// holds a handle to, and the handle to the new child lands in `handle_field` of
// this class regardless. So one object can build a whole nested tree and still
// reach every node of it in one step. Absent means the child hangs directly
// under this object.
void AppendOwnedChildConstruction(
    UnitLowerer& unit_lowerer, const WalkFrame& arm_frame,
    std::optional<mir::FieldId> runtime_parent_handle,
    const std::string& runtime_label, mir::ClassId child_scope_id,
    std::optional<mir::ExprId> array_index, mir::FieldId handle_field) {
  mir::Block& arm_block = *arm_frame.current_block;
  const mir::Class& owner_class = *arm_frame.current_class;
  ValidateOwnedChildConstruction(owner_class, child_scope_id);

  const auto& builtins = unit_lowerer.Unit().builtins;
  const mir::TypeId self_ptr_type = owner_class.self_pointer_type;
  const mir::TypeId child_ptr_type =
      MakeUniqueObjectPointer(unit_lowerer, child_scope_id);

  const auto string_literal = [&](const std::string& s) -> mir::ExprId {
    return arm_block.exprs.Add(
        mir::Expr{
            .data = mir::StringLiteral{.value = s}, .type = builtins.string});
  };
  const auto self_read = [&]() -> mir::ExprId {
    return arm_block.exprs.Add(MakeSelfRefExpr(arm_frame, self_ptr_type));
  };
  const auto parent_read = [&]() -> mir::ExprId {
    if (!runtime_parent_handle.has_value()) {
      return self_read();
    }
    return arm_block.exprs.Add(
        mir::MakeFieldAccessExpr(
            self_read(),
            mir::ClassFieldTarget{
                .owner = arm_frame.current_class_id,
                .slot = *runtime_parent_handle},
            owner_class.fields.Get(*runtime_parent_handle).type));
  };

  // Build the child's structural identity once and pass it as the child's
  // own ctor argument. The child holds onto it from the moment its
  // constructor returns; %m, by-name lookup, and debug traces all read
  // from that single source. The index list carries the caller-provided
  // hierarchy index when there is one -- a generated scope's constant index --
  // and is empty otherwise.
  std::vector<mir::ExprId> index_elems;
  if (array_index.has_value()) {
    index_elems.push_back(*array_index);
  }
  const mir::TypeId indices_type = mir::MachineArrayOf(
      unit_lowerer.Unit().types, builtins.int_type, index_elems.size());
  const mir::ExprId indices_id = arm_block.exprs.Add(
      mir::Expr{
          .data = mir::CompositeExpr{.parts = std::move(index_elems)},
          .type = indices_type});
  const mir::ExprId segment_id = arm_block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Construct{},
                  .arguments = {string_literal(runtime_label), indices_id}},
          .type = builtins.hierarchy_segment});

  std::vector<mir::ExprId> ctor_call_args;
  ctor_call_args.reserve(2);
  ctor_call_args.push_back(parent_read());
  ctor_call_args.push_back(segment_id);
  const mir::ExprId ctor_call_id = arm_block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Construct{},
                  .arguments = std::move(ctor_call_args)},
          .type = child_ptr_type});

  // The runtime tree owns the child (AddOwnedChild consumes the freshly built
  // unique pointer); ownership transfers after the child's constructor commits,
  // so a thrown subobject ctor leaves no half-attached scope. The borrowed
  // pointer it hands back is downcast and stored in the parent's handle
  // member, which is what a typed intra-unit route navigates through.
  const mir::ExprId add_call_id = arm_block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = support::BuiltinFn::kAddOwnedChild,
                          .receiver = parent_read()},
                  .arguments = {ctor_call_id}},
          .type = builtins.scope_ptr});
  const mir::TypeId handle_type = owner_class.fields.Get(handle_field).type;
  const mir::ExprId typed_handle = arm_block.exprs.Add(
      mir::Expr{
          .data = mir::CastExpr{.operand = add_call_id}, .type = handle_type});
  const mir::ExprId member = arm_block.exprs.Add(
      mir::MakeFieldAccessExpr(
          self_read(),
          mir::ClassFieldTarget{
              .owner = arm_frame.current_class_id, .slot = handle_field},
          handle_type));
  const mir::ExprId assign = arm_block.exprs.Add(
      mir::Expr{
          .data = mir::AssignExpr{.target = member, .value = typed_handle},
          .type = handle_type});
  arm_block.AppendStmt(mir::ExprStmt{.expr = assign});
}

// The correctness baseline for every generate construct: construct each
// instantiated block's own concrete scalar child directly (no runtime branch or
// loop), each carrying any constant hierarchy index it has. The genvar is
// folded into each body, so no induction-variable argument is threaded.
auto LowerGenerateAsStmt(
    StructuralScopeLowerer& lowerer, WalkFrame frame, const hir::Generate& gen,
    const GenerateBindings& gen_bindings) -> diag::Result<mir::Stmt> {
  mir::Block& block = *frame.current_block;

  mir::Block body;
  const WalkFrame body_frame = frame.WithBlock(&body);
  for (const hir::StructuralScopeId scope_id : gen.child_scopes.Ids()) {
    const auto& child_scope = gen.child_scopes.Get(scope_id);
    const auto& binding = gen_bindings.Get(scope_id);
    std::optional<mir::ExprId> index_id;
    if (child_scope.index.has_value()) {
      index_id =
          BuildIntLiteral(lowerer.Owner().Unit(), body, *child_scope.index);
    }
    AppendOwnedChildConstruction(
        lowerer.Owner(), body_frame, std::nullopt, binding.label,
        binding.lowerer->ClassId(), index_id, binding.borrowed_handle);
  }
  const mir::BlockId body_id = block.child_scopes.Add(std::move(body));
  return mir::Stmt{
      .label = std::nullopt, .data = mir::BlockStmt{.scope = body_id}};
}

}  // namespace

auto StructuralScopeLowerer::DeclareShape() -> diag::Result<mir::ClassId> {
  UnitLowerer& unit_lowerer = *owner_;
  const hir::StructuralScope& hir_scope = *hir_scope_;

  // The identity is minted before the shape is populated so the class's own
  // `self_pointer_type` can name it.
  class_id_ = unit_lowerer.Unit().DeclareClass();
  const mir::TypeId self_object_type = unit_lowerer.Unit().types.Intern(
      mir::Type{mir::ObjectType{.class_id = class_id_}});
  const mir::TypeId self_pointer_type = unit_lowerer.Unit().types.Intern(
      mir::Type{mir::PointerType{
          .pointee = self_object_type,
          .ownership = mir::PointerOwnership::kBorrowed}});

  ClassShape shape;
  shape.name = name_;
  shape.is_final = true;
  shape.self_pointer_type = self_pointer_type;
  shape.time_resolution = hir_scope.time_resolution;

  AttachRuntimeScopeCtorPrefix(unit_lowerer.Unit(), shape);

  // A member this unit published sits in a fixed prefix of the object, in the
  // order its signature states, so a unit reading that signature counts the
  // same position; what it did not publish follows and can move none of it.
  std::vector<hir::PublishedDecl> member_order = hir_scope.published_members;
  const auto append_unpublished = [&](const auto& id) {
    if (!std::ranges::contains(
            hir_scope.published_members, hir::PublishedDecl{id})) {
      member_order.emplace_back(id);
    }
  };
  for (const hir::StructuralDataObjectId id :
       hir_scope.structural_data_objects.Ids()) {
    append_unpublished(id);
  }
  for (const hir::InstanceMemberId id : hir_scope.instance_members.Ids()) {
    append_unpublished(id);
  }
  for (const hir::InterfacePortId id : hir_scope.interface_ports.Ids()) {
    append_unpublished(id);
  }

  std::vector<mir::FieldId> data_object_fields(
      hir_scope.structural_data_objects.size());
  std::vector<mir::FieldId> instance_fields(hir_scope.instance_members.size());
  std::vector<mir::FieldId> interface_port_fields(
      hir_scope.interface_ports.size());
  for (const hir::PublishedDecl& decl : member_order) {
    std::visit(
        Overloaded{
            [&](const hir::StructuralDataObjectId& id) {
              const auto& d = hir_scope.structural_data_objects.Get(id);
              data_object_fields[id.value] = shape.fields.Add(
                  mir::FieldDecl{
                      .name = d.name,
                      .type = unit_lowerer.MemberCellType(
                          unit_lowerer.TranslateType(d.type),
                          hir::StorageOf(d))});
            },
            [&](const hir::InstanceMemberId& id) {
              // Every instance member keeps one borrowed typed handle on this
              // class, and that handle's type states the member's cardinality
              // -- the bare handle for a single instance, one sequence wrapper
              // per declared dimension for an array (LRM 23.3.2). A route step
              // projects the handle and indexes it once per dimension, so
              // reaching an element never has to name the member a second
              // time.
              const auto& im = hir_scope.instance_members.Get(id);
              instance_fields[id.value] = shape.fields.Add(
                  mir::FieldDecl{
                      .name = im.instance_name,
                      .type = MakeInstanceMemberType(
                          unit_lowerer, im, mir::PointerOwnership::kBorrowed)});
            },
            [&](const hir::InterfacePortId& id) {
              const auto& port = hir_scope.interface_ports.Get(id);
              // The port stands for instances of the unit its record names, so
              // that record is the one source of both the object's type and the
              // positions a name reached through it is counted out of. How many
              // instances is the port's own multiplicity, which stands over
              // that type the way an instance member's stands over its handle.
              const mir::TypeId object_type = unit_lowerer.Unit().types.Intern(
                  mir::Type{mir::ExternalUnitObjectType{
                      .object = unit_lowerer.TranslateExternalUnitObject(
                          port.object)}});
              interface_port_fields[id.value] = shape.fields.Add(
                  mir::FieldDecl{
                      .name = port.name,
                      .type = unit_lowerer.MemberCellType(
                          SequenceOver(
                              unit_lowerer, object_type,
                              port.array_dims.size()),
                          hir::BorrowedObjectStorage{})});
            }},
        decl);
  }
  data_object_fields_ = {
      hir_scope.structural_data_objects.size(), std::move(data_object_fields)};

  // A history is storage nothing outside this scope names, so it takes no
  // place in the published member order: what reaches it is the scope's own
  // activation, its sampler, and the reads that asked for it (LRM 16.9.3). Its
  // value type is the subject's own, because what a tick keeps is what that
  // expression settled.
  std::vector<mir::FieldId> sampled_history_fields;
  sampled_history_fields.reserve(hir_scope.sampled_histories.size());
  for (const hir::SampledHistoryId id : hir_scope.sampled_histories.Ids()) {
    const hir::SampledHistoryDecl& history =
        hir_scope.sampled_histories.Get(id);
    const mir::TypeId value_type =
        unit_lowerer.TranslateType(hir_scope.exprs.Get(history.subject).type);
    sampled_history_fields.push_back(shape.fields.Add(
        mir::FieldDecl{
            .name = std::format("sampled_history_{}", id.value),
            .type = unit_lowerer.Unit().types.Intern(
                mir::Type{mir::SampledHistoryType{.value = value_type}})}));
  }
  sampled_history_fields_ = {
      hir_scope.sampled_histories.size(), std::move(sampled_history_fields)};

  // What an assertion has in flight is storage nothing outside this scope
  // names either: an attempt is started by the clock and read by the tick that
  // advances it, both of which are this scope's own (LRM 16.14.1). The type
  // carries nothing, because how wide a position set is and what a pending
  // attempt is owed are fixed by filling the storage rather than by naming it.
  std::vector<mir::FieldId> concurrent_assertion_fields;
  concurrent_assertion_fields.reserve(hir_scope.concurrent_assertions.size());
  for (const hir::ConcurrentAssertionId id :
       hir_scope.concurrent_assertions.Ids()) {
    concurrent_assertion_fields.push_back(shape.fields.Add(
        mir::FieldDecl{
            .name = std::format("concurrent_assertion_{}", id.value),
            .type = unit_lowerer.Unit().types.Intern(
                mir::Type{mir::EvaluationAttemptsType{}})}));
  }
  concurrent_assertion_fields_ = {
      hir_scope.concurrent_assertions.size(),
      std::move(concurrent_assertion_fields)};
  instance_member_fields_ = {
      hir_scope.instance_members.size(), std::move(instance_fields)};
  interface_port_fields_ = {
      hir_scope.interface_ports.size(), std::move(interface_port_fields)};

  routed_ref_targets_ = DeclareRoutedRefSlots(*this, shape);

  // Recursively declare every owned generate child's class shape; each child
  // lowerer is retained for the body sweep.
  std::vector<GenerateBindings> generates;
  generates.reserve(hir_scope.generates.size());
  for (const hir::GenerateId gen_id : hir_scope.generates.Ids()) {
    const auto& gen = hir_scope.generates.Get(gen_id);
    // Each elaborated block is its own concrete scalar child (its own class),
    // distinguished on the hierarchy only by any index it carries; the genvar
    // is folded into the body, so there is no runtime structural-param
    // binding.
    std::vector<ChildStructuralScopeBinding> gen_bindings;
    gen_bindings.reserve(gen.child_scopes.size());
    for (const auto& child_scope : gen.child_scopes) {
      // Every elaborated block of a loop generate shares one source label
      // (LRM 27.4), so the child's own unique scope name is what keeps their
      // borrowed handles apart on the parent.
      std::string scope_name = unit_lowerer.NextGenerateScopeName("gen");
      std::string handle_name = std::format("{}_borrowed_handle", scope_name);
      auto child = std::make_unique<StructuralScopeLowerer>(
          unit_lowerer, this, std::move(scope_name), child_scope);
      auto child_r = child->DeclareShape();
      if (!child_r) return std::unexpected(std::move(child_r.error()));

      const mir::ClassId child_id = *child_r;
      shape.contained.push_back(child_id);
      // Every elaborated block is a distinct child of this scope, whether it
      // is an if/case arm or one iteration of a loop, so each keeps its own
      // borrowed typed handle for a layout-visible route step to project
      // through.
      const mir::TypeId handle_type = unit_lowerer.Unit().types.Intern(
          mir::Type{mir::PointerType{
              .pointee = unit_lowerer.Unit().types.Intern(
                  mir::Type{mir::ObjectType{.class_id = child_id}}),
              .ownership = mir::PointerOwnership::kBorrowed}});
      const mir::FieldId borrowed_handle = shape.fields.Add(
          mir::FieldDecl{.name = std::move(handle_name), .type = handle_type});
      gen_bindings.push_back(
          ChildStructuralScopeBinding{
              .label = child_scope.source_name,
              .borrowed_handle = borrowed_handle,
              .lowerer = child.get()});
      children_.push_back(std::move(child));
    }
    generates.emplace_back(gen.child_scopes.size(), std::move(gen_bindings));
  }
  generate_bindings_ = {hir_scope.generates.size(), std::move(generates)};

  // Every procedural scope becomes a name node -- an object carrying the
  // identity a hierarchical path matches -- whatever the source called it and
  // whether or not anything was declared there, so one shape lowers every
  // scope. Whether a name reaches it decides only what it exposes: a scope the
  // source named carries its segment and one it did not carries none, which
  // keeps the latter off every hierarchical path (LRM 23.6) while it still
  // holds the nodes below it together.
  //
  // This scope keeps a borrowed handle to every one of them, however deeply
  // they nest, so a body reaches its own name node in one step and nothing has
  // to know what stands between. The nodes' own nesting is the HIR scope tree,
  // read where the objects are built.
  const mir::TypeId cancellation_target_type = unit_lowerer.Unit().types.Intern(
      mir::Type{mir::RuntimeLibraryType{
          .kind = mir::RuntimeLibraryKind::kCancellationTarget}});
  std::vector<DeclaredScope> scopes;
  scopes.reserve(hir_scope.procedural_scopes.size());
  for (const hir::ProceduralScopeId scope_id :
       hir_scope.procedural_scopes.Ids()) {
    const auto& scope = hir_scope.procedural_scopes.Get(scope_id);
    const std::string segment = hir::SegmentName(scope, scope_id);

    const mir::ClassId node_class = unit_lowerer.Unit().DeclareClass();
    ClassShape node_shape;
    node_shape.name = std::format("{}__{}", name_, segment);
    node_shape.is_final = true;
    node_shape.self_pointer_type = unit_lowerer.Unit().types.Intern(
        mir::Type{mir::PointerType{
            .pointee = unit_lowerer.Unit().types.Intern(
                mir::Type{mir::ObjectType{.class_id = node_class}}),
            .ownership = mir::PointerOwnership::kBorrowed}});
    node_shape.time_resolution = hir_scope.time_resolution;
    AttachRuntimeScopeCtorPrefix(unit_lowerer.Unit(), node_shape);
    unit_lowerer.DefineClassShape(node_class, std::move(node_shape));
    shape.contained.push_back(node_class);

    // The handle is a borrowed pointer to the node's own class, the same shape
    // an owned child instance or generate block keeps. That every owned child
    // is reachable through a typed member is what makes a class's layout state
    // which objects the runtime builds under it, so the naming handle carries
    // the node's type although it only ever asks for a name.
    DeclaredScope node{
        .name_node =
            ScopeNameNode{
                .class_id = node_class,
                .borrowed_handle = shape.fields.Add(
                    mir::FieldDecl{
                        .name = std::format("{}_borrowed_handle", segment),
                        .type = unit_lowerer.Unit().types.Intern(
                            mir::Type{mir::PointerType{
                                .pointee = unit_lowerer.Unit().types.Intern(
                                    mir::Type{mir::ObjectType{
                                        .class_id = node_class}}),
                                .ownership =
                                    mir::PointerOwnership::kBorrowed}})})},
        .disable_target = std::nullopt};

    // What a `disable` of this scope invalidates (LRM 9.6.2). Its targets are
    // the blocks and tasks a name reaches, so a scope the source named owns one
    // for that reason alone and one it did not owns none -- no pass has to
    // first find out which scopes some `disable` names. A scope of this
    // hierarchy is replicated with its instance, so the cell is one per
    // instance, shared by every activation of the scope.
    if (scope.source_name.has_value()) {
      node.disable_target = DeclareStaticCell(
          InstanceStorage{.fields = &shape.fields},
          std::format("{}__cancel_{}", segment, scope_id.value),
          cancellation_target_type);
    }
    scopes.push_back(node);
  }
  scopes_ = {hir_scope.procedural_scopes.size(), std::move(scopes)};

  // Everything a peer may need about a subroutine before its body exists is
  // settled here, in one pass over the subroutines. Its callable identity comes
  // from the shape's own pool, so a call in one body resolves a forward or
  // mutual reference to a peer (LRM 13.7) whatever order the two lower in; the
  // body pass fills the identity it was handed rather than working out where
  // the other side will put things.
  // A structural scope has no inheritance, so no declaration names another
  // scope's callable and the scope is the authority for its own identity space.
  base::IdAllocator<mir::CallableId> subroutine_ids;
  std::vector<DeclaredCallable> declared_subroutines;
  std::vector<CallableSignature> signatures;
  declared_subroutines.reserve(hir_scope.structural_subroutines.size());
  signatures.reserve(hir_scope.structural_subroutines.size());
  for (const auto& s : hir_scope.structural_subroutines) {
    signatures.push_back(CallableSignature{.virtual_dispatch = std::nullopt});
    declared_subroutines.push_back(
        DeclaredCallable{
            .callable = subroutine_ids.Take(),
            .statics = BindBodyStatics(
                unit_lowerer, hir_scope.procedural_scopes,
                InstanceStorage{.fields = &shape.fields}, s.body,
                SignatureBoundVars(s), s.name)});
  }
  shape.callable_signatures = {
      hir_scope.structural_subroutines.size(), std::move(signatures)};
  declared_subroutines_ = {
      hir_scope.structural_subroutines.size(), std::move(declared_subroutines)};

  std::vector<StaticVarBindings> process_statics;
  process_statics.reserve(hir_scope.processes.size());
  for (const hir::ProcessId id : hir_scope.processes.Ids()) {
    process_statics.push_back(BindBodyStatics(
        unit_lowerer, hir_scope.procedural_scopes,
        InstanceStorage{.fields = &shape.fields},
        hir_scope.processes.Get(id).body, {}, ProcessCallableName(id)));
  }
  process_static_bindings_ = {
      hir_scope.processes.size(), std::move(process_statics)};

  // The classes this scope declares settle their shapes against this one, and
  // before it is published: a class this scope replicates keeps its cells here,
  // as fields of the instance, so what it places has to land while the shape is
  // still open.
  class_lowerers_.reserve(hir_scope.declared_classes.size());
  for (const hir::ClassId hir_class : hir_scope.declared_classes) {
    class_lowerers_.emplace_back(
        unit_lowerer, hir_class, unit_lowerer.TranslateClass(hir_class),
        unit_lowerer.ClassObjectType(hir_class),
        unit_lowerer.Hir().classes.Get(hir_class), this);
  }
  for (ClassDeclLowerer& class_lowerer : class_lowerers_) {
    if (auto r = class_lowerer.DeclareShape(&shape); !r) {
      return std::unexpected(std::move(r.error()));
    }
  }

  unit_lowerer.DefineClassShape(class_id_, std::move(shape));
  return class_id_;
}

namespace {

// Builds the entry a scope answers an SV name with: a free function taking the
// generic scope as its receiver, downcasting it to the declaring class, and
// forwarding the formals to the subroutine. A caller reaching one has no
// declaration to compile against, so the entry's prototype is erased in the
// table and restored at the call site from the same declaration this is built
// from -- which is what makes a caller and this agree without a promise
// between them.
auto SynthesizeSubroutineEntry(
    mir::CompilationUnit& unit, const mir::Class& cls, mir::ClassId cls_id,
    mir::CallableId subroutine) -> mir::CallableCode {
  const mir::CallableCode& target = cls.callables.Get(subroutine).code;
  mir::CallableCode code = mir::CallableCode::Defined();
  const mir::LocalId self = code.locals.Add(
      mir::LocalDecl{.name = "self", .type = unit.builtins.scope_ptr});
  code.params.push_back(self);
  // The subroutine's own receiver leads its params, and the entry supplies it
  // from the scope it was handed rather than forwarding one; what the entry
  // takes beyond that are the formals the source wrote.
  const std::span<const mir::LocalId> formals =
      std::span{target.params}.subspan(
          target.HasReceiver(cls.self_pointer_type) ? 1 : 0);
  std::vector<mir::ExprId> arguments;
  arguments.reserve(formals.size());
  for (const mir::LocalId formal : formals) {
    const mir::LocalDecl& decl = target.locals.Get(formal);
    const mir::LocalId param =
        code.locals.Add(mir::LocalDecl{.name = decl.name, .type = decl.type});
    code.params.push_back(param);
    arguments.push_back(
        code.Body().exprs.Add(mir::MakeLocalRefExpr(param, decl.type)));
  }
  code.result_type = target.result_type;

  const mir::ExprId self_ref = code.Body().exprs.Add(
      mir::MakeLocalRefExpr(self, unit.builtins.scope_ptr));
  const mir::ExprId typed = code.Body().exprs.Add(
      mir::Expr{
          .data = mir::CastExpr{.operand = self_ref},
          .type = cls.self_pointer_type});
  const mir::ExprId call = code.Body().exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target =
                              mir::CallableTarget{
                                  .owner = cls_id, .slot = subroutine},
                          .receiver = typed},
                  .arguments = std::move(arguments)},
          .type = target.result_type});
  // A task suspends its caller until it completes (LRM 13.3), so the entry
  // suspends too: it awaits the body and hands back the completion, which is
  // what the enabling process awaits in turn. Anything else completes where it
  // is called and its result is the entry's.
  const mir::Type& result = unit.types.Get(target.result_type);
  if (const auto* coroutine = result.As<mir::CoroutineType>()) {
    const mir::LocalId completion = code.locals.Add(
        mir::LocalDecl{.name = "completion", .type = coroutine->payload});
    code.Body().AppendStmt(
        mir::LocalDeclStmt{
            .target = completion,
            .init = code.Body().exprs.Add(
                mir::Expr{
                    .data = mir::AwaitExpr{.awaitable = call},
                    .type = coroutine->payload})});
    code.Body().AppendStmt(
        mir::ReturnStmt{
            .value = code.Body().exprs.Add(
                mir::MakeLocalRefExpr(completion, coroutine->payload))});
  } else if (result.Is<mir::VoidType>()) {
    code.Body().AppendStmt(mir::ExprStmt{.expr = call});
    code.Body().AppendStmt(mir::ReturnStmt{.value = std::nullopt});
  } else {
    code.Body().AppendStmt(mir::ReturnStmt{.value = call});
  }
  return code;
}

// Builds a runtime scope class's definition as an ordinary constructed value
// and installs it on `cls`: a per-phase ABI adapter that downcasts the generic
// scope receiver to `cls` and forwards to the phase body (empty when the phase
// has none), wrapped in a ScopeProgram, wrapped in turn in the definition that
// adds the construct entry. Every scope class publishes the same record, so a
// site constructing an instance of one reads the definition the same way
// wherever the class came from. A class that is not a runtime tree node gets
// none.
auto InstallGeneratedDefinition(
    mir::CompilationUnit& unit, mir::Class& cls, mir::ClassId cls_id,
    mir::CallableCode& ctor_code, mir::CallableId resolve_body,
    mir::CallableId init_body, mir::CallableId create_body)
    -> std::vector<mir::ExprId> {
  const mir::TypeId scope_ptr = unit.builtins.scope_ptr;
  const mir::TypeId self_ptr = cls.self_pointer_type;
  const mir::TypeId void_type = unit.builtins.void_type;
  // An adapter over the generic scope receiver, with nothing in it. The
  // construction entry takes this shape because what it runs is the class's
  // constructor, which is a body block on the protocol rather than a callable
  // of the arena, so the backend supplies it.
  const auto empty_adapter = [&]() -> mir::AbiAdapterId {
    mir::CallableCode code = mir::CallableCode::Defined();
    code.params = {
        code.locals.Add(mir::LocalDecl{.name = "self", .type = scope_ptr})};
    code.result_type = void_type;
    return cls.abi_adapters.Add(
        mir::AbiAdapter{
            .code = std::move(code), .published = mir::UnpublishedEntry{}});
  };
  const auto make_adapter = [&](mir::CallableId body) -> mir::AbiAdapterId {
    mir::CallableCode code = mir::CallableCode::Defined();
    const mir::LocalId self =
        code.locals.Add(mir::LocalDecl{.name = "self", .type = scope_ptr});
    code.params = {self};
    code.result_type = void_type;
    const mir::ExprId self_ref =
        code.Body().exprs.Add(mir::MakeLocalRefExpr(self, scope_ptr));
    const mir::ExprId typed = code.Body().exprs.Add(
        mir::Expr{
            .data = mir::CastExpr{.operand = self_ref}, .type = self_ptr});
    const mir::ExprId call = code.Body().exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee =
                        mir::Direct{
                            .target =
                                mir::CallableTarget{
                                    .owner = cls_id, .slot = body},
                            .receiver = typed},
                    .arguments = {}},
            .type = void_type});
    code.Body().AppendStmt(mir::ExprStmt{.expr = call});
    return cls.abi_adapters.Add(
        mir::AbiAdapter{
            .code = std::move(code), .published = mir::UnpublishedEntry{}});
  };
  // Extending the runtime's scope is what puts this class on the object tree,
  // and the three bodies are what it supplies to be driven through, so the two
  // are one statement. The record below is a second reading of it, for a target
  // whose runtime is entered through a function pointer; a target that reaches
  // the bodies directly reads the statement and composes no record at all.
  cls.base = mir::ClassRef{mir::RuntimeClassRef{
      .symbol = "lyra::runtime::Scope",
      .resolve_state = resolve_body,
      .initialize_state = init_body,
      .create_processes = create_body}};
  const mir::AbiAdapterId resolve_abi = make_adapter(resolve_body);
  const mir::AbiAdapterId init_abi = make_adapter(init_body);
  const mir::AbiAdapterId create_abi = make_adapter(create_body);

  // The names this scope answers, one constant per namespace: the table the
  // runtime holds is a pointer into contiguous storage, so the records must
  // outlive the definition that points at them rather than sit in its
  // initializer. A scope answering no name in a namespace contributes an empty
  // array, which the same construction covers.
  struct NameTable {
    mir::TypeId records_type;
    mir::StaticConstantId records;
    std::uint32_t count = 0;
  };
  const auto publish = [&](auto&& name_of) -> NameTable {
    mir::StaticConstantDecl decl;
    mir::RuntimeRecordBuilder records(unit, decl.body.exprs);
    std::vector<mir::ExprId> entries;
    for (const mir::AbiAdapterId adapter_id : cls.abi_adapters.Ids()) {
      const mir::AbiAdapter& adapter = cls.abi_adapters.Get(adapter_id);
      const std::optional<std::string> name = name_of(adapter.published);
      if (!name.has_value()) {
        continue;
      }
      entries.push_back(records.Construct(
          mir::RuntimeLibraryKind::kScopeCallable,
          {records.StringRef(*name),
           records.ErasedFunctionRef(cls, adapter_id)}));
    }
    const auto count = static_cast<std::uint32_t>(entries.size());
    decl.value = records.MachineArray(
        records.Type(mir::RuntimeLibraryKind::kScopeCallable),
        std::move(entries));
    decl.type = records.TypeOf(decl.value);
    const mir::TypeId records_type = decl.type;
    return NameTable{
        .records_type = records_type,
        .records = cls.static_constants.Add(std::move(decl)),
        .count = count};
  };
  const NameTable exports = publish(
      [](const mir::AbiAdapterPublication& p) -> std::optional<std::string> {
        const auto* linkage = std::get_if<mir::ForeignLinkage>(&p);
        return linkage == nullptr ? std::nullopt
                                  : std::optional{linkage->foreign_name};
      });
  const NameTable subroutines = publish(
      [](const mir::AbiAdapterPublication& p) -> std::optional<std::string> {
        const auto* entry = std::get_if<mir::SubroutineEntry>(&p);
        return entry == nullptr ? std::nullopt : std::optional{entry->name};
      });

  mir::StaticConstantDecl def;
  mir::RuntimeRecordBuilder definition(unit, def.body.exprs);
  const auto build_table = [&](const NameTable& table) -> mir::ExprId {
    const mir::ExprId records_ref = definition.Add(
        mir::Expr{
            .data =
                mir::ReferenceExpr{
                    .target =
                        mir::StaticConstantRef{.constant = table.records}},
            .type = table.records_type});
    const mir::ExprId data = definition.Add(
        mir::Expr{
            .data = mir::MachineArrayDataExpr{.array = records_ref},
            .type = unit.types.Intern(
                mir::Type{mir::PointerType{
                    .pointee = definition.Type(
                        mir::RuntimeLibraryKind::kScopeCallable),
                    .ownership = mir::PointerOwnership::kBorrowed,
                    .mutability = mir::Mutability::kReadOnly}})});
    return definition.Construct(
        mir::RuntimeLibraryKind::kScopeCallableTable,
        {data, definition.MachineInt(table.count)});
  };
  const mir::ExprId export_table = build_table(exports);
  const mir::ExprId subroutine_table = build_table(subroutines);

  const mir::ExprId metadata = definition.Construct(
      mir::RuntimeLibraryKind::kScopeMetadata,
      {definition.MachineInt(cls.time_resolution.unit_power),
       definition.MachineInt(cls.time_resolution.precision_power)});
  const mir::ExprId program = definition.Construct(
      mir::RuntimeLibraryKind::kScopeProgram,
      {metadata, definition.FunctionRef(cls, resolve_abi),
       definition.FunctionRef(cls, init_abi),
       definition.FunctionRef(cls, create_abi), export_table,
       subroutine_table});
  const mir::AbiAdapterId construct_abi = empty_adapter();
  def.value = definition.Construct(
      mir::RuntimeLibraryKind::kScopeDefinition,
      {program, definition.FunctionRef(cls, construct_abi)});
  def.type = definition.TypeOf(def.value);
  const mir::TypeId const_type = def.type;
  const mir::StaticConstantId def_id = cls.static_constants.Add(std::move(def));

  // The constructor hands the base the address of the constant just installed.
  auto& cex = ctor_code.Body().exprs;
  const mir::ExprId ref = cex.Add(
      mir::Expr{
          .data =
              mir::ReferenceExpr{
                  .target = mir::StaticConstantRef{.constant = def_id}},
          .type = const_type});
  const mir::ExprId addr = cex.Add(
      mir::Expr{
          .data = mir::AddressOfExpr{.operand = ref},
          .type = unit.types.Intern(
              mir::Type{mir::PointerType{
                  .pointee = const_type,
                  .ownership = mir::PointerOwnership::kBorrowed}})});
  return {addr};
}

// Settles the class's construction protocol: the constructor body, and the
// arguments its base is entered with -- each prefix forwarded as a consuming
// use, then the trailing ones. `ctor_code` arrives finalized, its params and
// result type set, and this is what installs it.
void FinalizeConstructor(
    mir::CompilationUnit& unit, mir::Class& cls, mir::CallableCode ctor_code,
    const std::vector<mir::LocalId>& prefix_local_ids,
    const std::vector<mir::ExprId>& base_trailing_args) {
  std::vector<mir::ExprId> base_args;
  if (cls.base.has_value()) {
    base_args.reserve(prefix_local_ids.size() + base_trailing_args.size());
    for (const mir::LocalId id : prefix_local_ids) {
      const mir::TypeId ty = ctor_code.locals.Get(id).type;
      const mir::ExprId local_ref =
          ctor_code.Body().exprs.Add(mir::MakeLocalRefExpr(id, ty));
      if (unit.types.Get(ty).IsAliasHandle()) {
        base_args.push_back(local_ref);
      } else {
        base_args.push_back(ctor_code.Body().exprs.Add(
            mir::Expr{
                .data = mir::MoveExpr{.operand = local_ref}, .type = ty}));
      }
    }
    for (const mir::ExprId e : base_trailing_args) {
      base_args.push_back(e);
    }
  }
  cls.constructor = mir::ConstructorDecl{
      .code = std::move(ctor_code), .base_args = std::move(base_args)};
}

}  // namespace

auto StructuralScopeLowerer::PopulateBodies(WalkFrame parent_frame)
    -> diag::Result<void> {
  UnitLowerer& unit_lowerer = *owner_;
  const hir::StructuralScope& hir_scope = *hir_scope_;

  const ClassShape& shape = unit_lowerer.GetClassShape(class_id_);
  mir::Class mir_class = shape.OpenClass();

  const mir::TypeId void_type = unit_lowerer.Unit().builtins.void_type;
  const mir::TypeId self_ptr_type = mir_class.self_pointer_type;
  ScopeChainNode outer_scope_link{};
  const auto seed_self = [&](CallableBindings& bindings) -> mir::LocalId {
    return bindings.Declare(
        BindingOriginId::Receiver(),
        mir::LocalDecl{.name = "self", .type = self_ptr_type});
  };

  // Each lifecycle phase is a callable like any other: `self` is the receiver
  // binding seeded into its `locals`, and every nested block's `self` read
  // resolves through that one binding.
  mir::CallableCode ctor_code = mir::CallableCode::Defined();
  CallableBindings ctor_bindings(unit_lowerer.Unit(), ctor_code);
  const mir::LocalId self_id = seed_self(ctor_bindings);
  // Each prefix param the base contract demands lands as an ordinary local
  // after `self`, so a base call reads it as a plain LocalRef and the ctor
  // signature exposes it as a regular parameter.
  std::vector<mir::LocalId> ctor_prefix_local_ids;
  ctor_prefix_local_ids.reserve(shape.ctor_prefix_params.size());
  for (const mir::ParamId param : shape.ctor_prefix_params.Ids()) {
    const auto& p = shape.ctor_prefix_params.Get(param);
    ctor_prefix_local_ids.push_back(ctor_bindings.DeclareAnonymous(
        mir::LocalDecl{.name = p.name, .type = p.type}));
  }
  mir::Block& ctor_block = ctor_code.Body();
  const WalkFrame ctor_frame =
      parent_frame.WithClass(&mir_class, class_id_, outer_scope_link)
          .WithBlock(&ctor_block)
          .WithBindings(&ctor_bindings);

  mir::CallableCode initialize_code = mir::CallableCode::Defined();
  CallableBindings init_bindings(unit_lowerer.Unit(), initialize_code);
  const mir::LocalId init_self_id = seed_self(init_bindings);
  mir::Block& initialize_block = initialize_code.Body();
  const WalkFrame init_frame =
      parent_frame.WithClass(&mir_class, class_id_, outer_scope_link)
          .WithBlock(&initialize_block)
          .WithBindings(&init_bindings);

  mir::CallableCode resolve_code = mir::CallableCode::Defined();
  CallableBindings resolve_bindings(unit_lowerer.Unit(), resolve_code);
  const mir::LocalId resolve_self_id = seed_self(resolve_bindings);
  mir::Block& resolve_block = resolve_code.Body();
  const WalkFrame resolve_frame =
      parent_frame.WithClass(&mir_class, class_id_, outer_scope_link)
          .WithBlock(&resolve_block)
          .WithBindings(&resolve_bindings);

  mir::CallableCode activate_code = mir::CallableCode::Defined();
  CallableBindings activate_bindings(unit_lowerer.Unit(), activate_code);
  const mir::LocalId activate_self_id = seed_self(activate_bindings);
  mir::Block& activate_block = activate_code.Body();
  const WalkFrame activate_frame =
      parent_frame.WithClass(&mir_class, class_id_, outer_scope_link)
          .WithBlock(&activate_block)
          .WithBindings(&activate_bindings);
  const auto self_read = [&]() -> mir::ExprId {
    return ctor_block.exprs.Add(MakeSelfRefExpr(ctor_frame, self_ptr_type));
  };
  const auto init_self_read = [&]() -> mir::ExprId {
    return initialize_block.exprs.Add(
        MakeSelfRefExpr(init_frame, self_ptr_type));
  };

  std::vector<mir::FieldId> data_object_fields;
  data_object_fields.reserve(hir_scope.structural_data_objects.size());
  for (const hir::StructuralDataObjectId hir_id :
       hir_scope.structural_data_objects.Ids()) {
    const auto& d = hir_scope.structural_data_objects.Get(hir_id);
    const mir::FieldId mir_id =
        TranslateStructuralDataObject(hir::StructuralHops{0}, hir_id);
    const mir::TypeId mir_field_type = mir_class.fields.Get(mir_id).type;
    const mir::TypeId mir_value_type = unit_lowerer.TranslateType(d.type);
    const auto* net = std::get_if<hir::StructuralNetDecl>(&d.kind);
    const auto* var = std::get_if<hir::StructuralVariableDecl>(&d.kind);
    const mir::Type& var_type = unit_lowerer.Unit().types.Get(mir_value_type);
    // Owned children (pointer / vector / object), cross-instance reference
    // slots (borrowed pointers filled in the resolve phase), and named events
    // have no "value assignment" -- their declaration shape itself fixes the
    // field at construction. A net takes none either: its value is produced by
    // its drivers, seeded when each driver updates in the initialize phase.
    // Value-typed variables (integral, string, real, unpacked / dynamic array)
    // receive an LRM 10.5 initialization statement, run in the initialize
    // phase after the tree's references resolve, not in the constructor.
    const bool is_assignable_value =
        var != nullptr && !var_type.Is<mir::PointerType>() &&
        !var_type.Is<mir::VectorType>() && !var_type.Is<mir::ObjectType>() &&
        !var_type.Is<mir::ExternalUnitObjectType>() &&
        !var_type.Is<mir::EventType>();
    if (is_assignable_value) {
      const mir::ExprId init_target = initialize_block.exprs.Add(
          mir::MakeFieldAccessExpr(
              init_self_read(),
              mir::ClassFieldTarget{.owner = class_id_, .slot = mir_id},
              mir_field_type));
      const auto append_stmt = [&](mir::Expr expr) {
        initialize_block.AppendStmt(
            mir::Stmt{
                .label = std::nullopt,
                .data = mir::ExprStmt{
                    .expr = initialize_block.exprs.Add(std::move(expr))}});
      };
      const auto emit_value_store = [&](mir::ExprId value_id) {
        append_stmt(BuildStoreExpr(
            unit_lowerer.Unit(), initialize_block,
            WriteTarget{.owner = init_target, .descent = {}}, value_id,
            std::nullopt, mir_value_type));
      };

      // Every observable value cell installs its declared representation and
      // default at construction (LRM 10.5), so its type is fixed by
      // construction and a later store -- including a user initializer -- is
      // verified against it rather than discovered from whichever store runs
      // first. A non-observable value member carries no cell wrapper, so it
      // installs its representation through an ordinary store of the default.
      if (unit_lowerer.Unit().types.Get(mir_field_type).IsCapabilityWrapper()) {
        const mir::ExprId prototype = initialize_block.exprs.Add(
            BuildDefaultValueFromHir(unit_lowerer, initialize_block, d.type));
        append_stmt(
            mir::MakeCapabilityInstallCallExpr(
                init_target, prototype, support::BuiltinFn::kInitialize,
                unit_lowerer.Unit().builtins.void_type));
        if (var->initializer.has_value()) {
          auto value_or =
              LowerExpr(hir_scope.exprs.Get(*var->initializer), init_frame);
          if (!value_or) return std::unexpected(std::move(value_or.error()));
          emit_value_store(initialize_block.exprs.Add(*std::move(value_or)));
        }
      } else {
        mir::ExprId value_id{};
        if (var->initializer.has_value()) {
          auto value_or =
              LowerExpr(hir_scope.exprs.Get(*var->initializer), init_frame);
          if (!value_or) return std::unexpected(std::move(value_or.error()));
          value_id = initialize_block.exprs.Add(*std::move(value_or));
        } else {
          value_id = initialize_block.exprs.Add(
              BuildDefaultValueFromHir(unit_lowerer, initialize_block, d.type));
        }
        emit_value_store(value_id);
      }
    }

    // A net cell fixes what its declaration gives it -- the declared type, and
    // what its net type states about how contributions resolve and what the net
    // shows where nothing drives them -- at construction (LRM 6.6.1, 6.7.1), in
    // the constructor rather than the initialize phase: a net is a readable,
    // well-typed observable before any driver attaches, and before a cross-unit
    // reader seeds from it during the parent-first initialize phase, so a read
    // that early sees what the net type contributes, never an uninitialized
    // cell. Drivers, attached at Resolve, update it from there.
    if (net != nullptr) {
      const mir::ExprId net_target = ctor_block.exprs.Add(
          mir::MakeFieldAccessExpr(
              self_read(),
              mir::ClassFieldTarget{.owner = class_id_, .slot = mir_id},
              mir_field_type));
      const mir::ExprId prototype = ctor_block.exprs.Add(
          BuildDefaultValueFromHir(unit_lowerer, ctor_block, d.type));
      const NetInstall install =
          BuildNetInstall(unit_lowerer.Unit(), ctor_block, *net);
      ctor_block.AppendStmt(
          mir::Stmt{
              .label = std::nullopt,
              .data = mir::ExprStmt{
                  .expr = ctor_block.exprs.Add(
                      mir::MakeNetInstallCallExpr(
                          net_target, prototype, install.fill, install.strength,
                          install.entry, void_type))}});
    }

    // A value signal, or a named event, records its address under its name so a
    // cross-unit referrer resolves it by name at construction. The excluded
    // members -- owned children and cross-unit reference slots -- are not
    // signals.
    const bool is_signal = !var_type.Is<mir::PointerType>() &&
                           !var_type.Is<mir::VectorType>() &&
                           !var_type.Is<mir::ObjectType>() &&
                           !var_type.Is<mir::ExternalUnitObjectType>();
    if (is_signal) {
      const mir::ExprId var_ref = ctor_block.exprs.Add(
          mir::MakeFieldAccessExpr(
              self_read(),
              mir::ClassFieldTarget{.owner = class_id_, .slot = mir_id},
              mir_field_type));
      const mir::TypeId var_ptr_type = unit_lowerer.Unit().types.Intern(
          mir::Type{mir::PointerType{
              .pointee = mir_field_type,
              .ownership = mir::PointerOwnership::kBorrowed}});
      const mir::ExprId addr_id =
          ctor_block.exprs.Add(mir::MakeAddressOfExpr(var_ref, var_ptr_type));
      const mir::ExprId name_id = ctor_block.exprs.Add(
          mir::Expr{
              .data = mir::StringLiteral{.value = d.name},
              .type = unit_lowerer.Unit().builtins.string});
      const mir::ExprId call = ctor_block.exprs.Add(
          mir::Expr{
              .data =
                  mir::CallExpr{
                      .callee =
                          mir::Direct{
                              .target = support::BuiltinFn::kRegisterSignal,
                              .receiver = self_read()},
                      .arguments = {name_id, addr_id}},
              .type = void_type});
      ctor_block.AppendStmt(mir::ExprStmt{.expr = call});
    }
  }

  // The design root's Initialize phase brings up what every namespace unit owns
  // (LRM 26.2 / 8.9 / 10.5). Such a unit owns no runtime tree node, so the root
  // calls its receiver-less callables here, before the top modules initialize
  // -- this scope's Initialize runs parent-first, and the design root is every
  // module's ancestor. It runs design-wide in two passes: install every unit's
  // cells (their declared type and default), then run every unit's value
  // initializers, so a value initializer that reads another unit's cell always
  // reaches installed storage. The plan is resolved by the whole-design
  // assembly and realized here; this scope carries it only for the design root,
  // so a source unit's scope and a nested scope both leave it empty.
  const auto call_namespace_unit = [&](const std::string& unit_name,
                                       mir::NamespaceStoragePhase phase) {
    unit_lowerer.Unit().AddExternalReferencedUnit(unit_name);
    const mir::ExprId call = initialize_block.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee =
                        mir::Direct{
                            .target =
                                mir::ExternalUnitStorageTarget{
                                    .unit_name = unit_name, .phase = phase}},
                    .arguments = {}},
            .type = void_type});
    initialize_block.AppendStmt(mir::ExprStmt{.expr = call});
  };
  for (const std::string& unit : namespace_storage_plan_.units) {
    call_namespace_unit(unit, mir::NamespaceStoragePhase::kInstall);
  }
  for (const std::string& unit : namespace_storage_plan_.units) {
    call_namespace_unit(unit, mir::NamespaceStoragePhase::kInitialize);
  }

  // Commit the class of every procedural scope's name node. A name node is
  // reached by name and answers with a name, so what it carries is what the
  // runtime scope base already gives it and its constructor takes only the
  // identity every scope is built with.
  for (const hir::ProceduralScopeId scope : scopes_.Ids()) {
    const ScopeNameNode& name_node = *scopes_.Get(scope).name_node;
    const ClassShape& node_shape =
        unit_lowerer.GetClassShape(name_node.class_id);
    mir::Class node_class;
    node_class.name = node_shape.name;
    node_class.is_final = node_shape.is_final;
    node_class.self_pointer_type = node_shape.self_pointer_type;
    node_class.time_resolution = node_shape.time_resolution;

    mir::CallableCode node_ctor_code = mir::CallableCode::Defined();
    CallableBindings node_ctor_bindings(unit_lowerer.Unit(), node_ctor_code);
    node_ctor_code.params.push_back(node_ctor_bindings.Declare(
        BindingOriginId::Receiver(),
        mir::LocalDecl{.name = "self", .type = node_shape.self_pointer_type}));
    std::vector<mir::LocalId> node_ctor_prefix_local_ids;
    node_ctor_prefix_local_ids.reserve(node_shape.ctor_prefix_params.size());
    for (const mir::ParamId param : node_shape.ctor_prefix_params.Ids()) {
      const auto& p = node_shape.ctor_prefix_params.Get(param);
      node_ctor_prefix_local_ids.push_back(node_ctor_bindings.DeclareAnonymous(
          mir::LocalDecl{.name = p.name, .type = p.type}));
      node_ctor_code.params.push_back(node_ctor_prefix_local_ids.back());
    }
    node_ctor_code.result_type = void_type;
    // A name node has no work in any phase, which is three bodies with no
    // statements rather than three phases it does not have -- the same shape a
    // scope with work carries, so nothing downstream tells the two apart.
    const auto empty_phase = [&]() -> mir::CallableId {
      mir::CallableCode code = mir::CallableCode::Defined();
      CallableBindings bindings(unit_lowerer.Unit(), code);
      code.params = {bindings.Declare(
          BindingOriginId::Receiver(),
          mir::LocalDecl{
              .name = "self", .type = node_shape.self_pointer_type})};
      code.result_type = void_type;
      return node_class.callables.Add(
          mir::CallableDecl{
              .code = std::move(code),
              .foreign = std::nullopt,
              .virtual_dispatch = std::nullopt});
    };
    const std::vector<mir::ExprId> node_base_trailing_args =
        InstallGeneratedDefinition(
            unit_lowerer.Unit(), node_class, name_node.class_id, node_ctor_code,
            empty_phase(), empty_phase(), empty_phase());
    FinalizeConstructor(
        unit_lowerer.Unit(), node_class, std::move(node_ctor_code),
        node_ctor_prefix_local_ids, node_base_trailing_args);
    unit_lowerer.Unit().DefineClass(name_node.class_id, std::move(node_class));
  }

  // What a `disable` naming a block or task terminates is a cell on this
  // object, placed by the rule that places every other piece of static-lifetime
  // state, while what a name reaches is the scope itself (LRM 9.6.2). So the
  // scope's node keeps the address, and a route that walked to that node asks
  // it for the target the way it asks for a static's cell.
  const mir::TypeId disable_target_ptr_type = unit_lowerer.Unit().types.Intern(
      mir::Type{mir::PointerType{
          .pointee = unit_lowerer.Unit().types.Intern(
              mir::Type{mir::RuntimeLibraryType{
                  .kind = mir::RuntimeLibraryKind::kCancellationTarget}}),
          .ownership = mir::PointerOwnership::kBorrowed}});

  // Build the whole name tree here, in this scope's own constructor: each node
  // hangs under the node of the scope around it, which is what the source
  // nesting means, while the borrowed handle to it lands on this class -- so
  // the objects nest and every one of them is still one step from a body.
  // Construction precedes every resolve, so the names registered below are in
  // place before anything asks for one.
  //
  // What each node answers for registers against the handle the construction
  // fills, so a node registers exactly when it is built and which scopes stand
  // at run time is one answer rather than two. A scope declared without being
  // built therefore registers nothing.
  const auto build_name_tree =
      [&](const auto& self_ref, hir::ProceduralScopeId scope_id,
          std::optional<mir::FieldId> parent_handle) -> void {
    const auto& scope = hir_scope.procedural_scopes.Get(scope_id);
    const DeclaredScope& declared = scopes_.Get(scope_id);
    const ScopeNameNode& name_node = *declared.name_node;
    AppendOwnedChildConstruction(
        unit_lowerer, ctor_frame, parent_handle, scope.source_name.value_or(""),
        name_node.class_id, std::nullopt, name_node.borrowed_handle);
    if (declared.disable_target.has_value()) {
      const mir::FieldId field = DisableTargetField(scope_id);
      const mir::ExprId cell = ctor_block.exprs.Add(
          mir::MakeFieldAccessExpr(
              self_read(),
              mir::ClassFieldTarget{.owner = class_id_, .slot = field},
              mir_class.fields.Get(field).type));
      const mir::ExprId addr = ctor_block.exprs.Add(
          mir::MakeAddressOfExpr(cell, disable_target_ptr_type));
      const mir::ExprId node = ctor_block.exprs.Add(
          mir::MakeFieldAccessExpr(
              self_read(),
              mir::ClassFieldTarget{
                  .owner = class_id_, .slot = name_node.borrowed_handle},
              mir_class.fields.Get(name_node.borrowed_handle).type));
      ctor_block.AppendStmt(
          mir::ExprStmt{
              .expr = ctor_block.exprs.Add(
                  mir::Expr{
                      .data =
                          mir::CallExpr{
                              .callee =
                                  mir::Direct{
                                      .target = support::BuiltinFn::
                                          kRegisterDisableTarget,
                                      .receiver = node},
                              .arguments = {addr}},
                      .type = void_type})});
    }
    for (const hir::ProceduralScopeId child : scope.child_scopes) {
      self_ref(self_ref, child, name_node.borrowed_handle);
    }
  };
  for (const auto& s : hir_scope.structural_subroutines) {
    build_name_tree(build_name_tree, s.body.root_scope, std::nullopt);
  }
  for (const auto& p : hir_scope.processes) {
    build_name_tree(build_name_tree, p.body.root_scope, std::nullopt);
  }

  // A static-lifetime local is a cell on this object, but the name reaching it
  // belongs to the block that wrote it: LRM 6.21 lets a hierarchical reference
  // name any static variable except one declared inside an unnamed block. So it
  // registers under its source spelling on that block's node, and a descent
  // (`Top.outer.x`, intra- or cross-unit) walks the object tree to that node by
  // name and asks it for the cell's address.
  const auto register_named_statics = [&](const StaticVarBindings& statics,
                                          const hir::ProceduralBody& body) {
    for (const StaticVarBinding& binding : statics) {
      const auto& scope = hir_scope.procedural_scopes.Get(binding.scope);
      if (!scope.source_name.has_value()) continue;
      const mir::FieldId field = InstanceFieldOf(binding);
      const mir::ExprId cell = ctor_block.exprs.Add(
          mir::MakeFieldAccessExpr(
              self_read(),
              mir::ClassFieldTarget{.owner = class_id_, .slot = field},
              binding.cell_type));
      const mir::ExprId addr = ctor_block.exprs.Add(
          mir::MakeAddressOfExpr(
              cell, unit_lowerer.Unit().types.Intern(
                        mir::Type{mir::PointerType{
                            .pointee = binding.cell_type,
                            .ownership = mir::PointerOwnership::kBorrowed}})));
      const mir::FieldId borrowed_handle =
          scopes_.Get(binding.scope).name_node->borrowed_handle;
      const mir::ExprId node = ctor_block.exprs.Add(
          mir::MakeFieldAccessExpr(
              self_read(),
              mir::ClassFieldTarget{
                  .owner = class_id_, .slot = borrowed_handle},
              mir_class.fields.Get(borrowed_handle).type));
      const mir::ExprId name_lit = ctor_block.exprs.Add(
          mir::Expr{
              .data =
                  mir::StringLiteral{
                      .value = body.procedural_vars.Get(binding.var).name},
              .type = unit_lowerer.Unit().builtins.string});
      ctor_block.AppendStmt(
          mir::ExprStmt{
              .expr = ctor_block.exprs.Add(
                  mir::Expr{
                      .data =
                          mir::CallExpr{
                              .callee =
                                  mir::Direct{
                                      .target =
                                          support::BuiltinFn::kRegisterSignal,
                                      .receiver = node},
                              .arguments = {name_lit, addr}},
                      .type = void_type})});
    }
  };
  for (const hir::StructuralSubroutineId id :
       hir_scope.structural_subroutines.Ids()) {
    register_named_statics(
        declared_subroutines_.Get(id).statics,
        hir_scope.structural_subroutines.Get(id).body);
  }
  for (const hir::ProcessId id : hir_scope.processes.Ids()) {
    register_named_statics(
        process_static_bindings_.Get(id), hir_scope.processes.Get(id).body);
  }

  // The callable each subroutine lowered to, recorded where it is created so an
  // export below names its own by identity, indexed by that subroutine's id.
  std::vector<mir::CallableId> subroutine_callables;
  subroutine_callables.reserve(hir_scope.structural_subroutines.size());
  for (const hir::StructuralSubroutineId sub_id :
       hir_scope.structural_subroutines.Ids()) {
    const auto& src = hir_scope.structural_subroutines.Get(sub_id);
    const DeclaredCallable& declared = declared_subroutines_.Get(sub_id);
    // A subroutine the source declared is what a hierarchical name and a call
    // from another unit both spell, so the scope's class records the name.
    mir_class.named_callables.push_back(
        mir::NamedCallable{.name = src.name, .body = declared.callable});
    ProcessLowerer subroutine_lowerer(
        unit_lowerer, this, hir_scope.time_resolution, src.body, src.root_stmt,
        src.name, ctor_frame, scopes_, declared.statics);
    auto code_or = subroutine_lowerer.Run(src);
    if (!code_or) return std::unexpected(std::move(code_or.error()));
    mir_class.callables.Define(
        declared.callable, mir::CallableDecl{
                               .code = *std::move(code_or),
                               .foreign = std::nullopt,
                               .virtual_dispatch = std::nullopt});
    subroutine_callables.push_back(declared.callable);
    for (const StaticVarBinding& binding : declared.statics) {
      auto integ = IntegrateStaticInitializer(
          subroutine_lowerer, src.body,
          StorageBringUp{.install = init_frame, .value = init_frame}, binding);
      if (!integ) return std::unexpected(std::move(integ.error()));
    }
  }

  // An exported subroutine's C entry point calls that method on the receiver
  // recovered from the current DPI scope (LRM 35.5.3) -- the instance the
  // foreign call chain targets, which svSetScope may have redirected -- and the
  // unit owns the entry point, since a DPI-C name is program-global and never a
  // class member (LRM 35.4, 35.7).
  for (const hir::ForeignExportDecl& export_decl : hir_scope.foreign_exports) {
    const mir::CallableId method_id =
        subroutine_callables[export_decl.subroutine.value];
    const mir::TypeId method_result_type =
        mir_class.callables.Get(method_id).code.result_type;
    // The subroutine is compiled once per specialization of this scope while
    // the DPI-C name is one program-global symbol, so the scope publishes the
    // entry and the symbol resolves against whichever scope the foreign call
    // chain established. The entry takes the scope receiver, so it is the same
    // species as a lifecycle entry, not a callable the unit's namespace owns.
    ForeignExportEntry entry = SynthesizeForeignExportEntry(
        unit_lowerer, ctor_frame,
        mir::CallableTarget{.owner = class_id_, .slot = method_id},
        method_result_type, export_decl);
    unit_lowerer.Unit().foreign_surface.push_back(
        mir::ForeignSymbol{
            .linkage = entry.linkage,
            .definition = std::move(entry.definition)});
    mir_class.abi_adapters.Add(
        mir::AbiAdapter{
            .code = std::move(entry.code),
            .published = std::move(entry.linkage)});
  }

  // Every subroutine this scope declares answers to its own SV name, so a
  // hierarchical enable that reaches the instance finds it there (LRM 23.6).
  // The declaring unit cannot know which of them anyone will name -- a module
  // promises its parameters and ports and nothing else -- so it publishes them
  // all rather than the ones some referrer happened to compile against.
  for (const hir::StructuralSubroutineId sub_id :
       hir_scope.structural_subroutines.Ids()) {
    const std::string& name = hir_scope.structural_subroutines.Get(sub_id).name;
    const mir::CallableId method_id = subroutine_callables[sub_id.value];
    mir_class.abi_adapters.Add(
        mir::AbiAdapter{
            .code = SynthesizeSubroutineEntry(
                unit_lowerer.Unit(), mir_class, class_id_, method_id),
            .published = mir::SubroutineEntry{.name = name}});
  }

  for (const hir::ProcessId id : hir_scope.processes.Ids()) {
    const auto& p = hir_scope.processes.Get(id);
    const StaticVarBindings& statics = process_static_bindings_.Get(id);
    ProcessLowerer process_lowerer(
        unit_lowerer, this, hir_scope.time_resolution, p.body, p.root_stmt,
        ProcessCallableName(id), ctor_frame, scopes_, statics);
    auto code_or = process_lowerer.Run(p);
    if (!code_or) return std::unexpected(std::move(code_or.error()));
    const mir::CallableId body = mir_class.callables.Add(
        mir::CallableDecl{
            .code = *std::move(code_or),
            .foreign = std::nullopt,
            .virtual_dispatch = std::nullopt});
    AppendProcessRegistration(
        unit_lowerer, activate_frame, body, p.kind == hir::ProcessKind::kFinal);
    for (const StaticVarBinding& binding : statics) {
      auto integ = IntegrateStaticInitializer(
          process_lowerer, p.body,
          StorageBringUp{.install = init_frame, .value = init_frame}, binding);
      if (!integ) return std::unexpected(std::move(integ.error()));
    }
  }

  // Fill every routed-reference endpoint slot first in the resolve phase, so a
  // later resolve-phase consumer that reaches a target through a sealed
  // endpoint -- a continuous-assign driver attached to an enclosing or
  // cross-unit net, a port-cell connection -- dereferences a slot that is
  // already bound.
  InstallRoutedRefs(*this, resolve_frame);

  for (const hir::ContinuousAssignId id : hir_scope.continuous_assigns.Ids()) {
    auto method_or = LowerContinuousAssign(
        *this, ctor_frame, resolve_frame, init_frame,
        ContinuousAssignCallableName(id), hir_scope.continuous_assigns.Get(id));
    if (!method_or) return std::unexpected(std::move(method_or.error()));
    const mir::CallableId body = mir_class.callables.Add(std::move(*method_or));
    AppendProcessRegistration(unit_lowerer, activate_frame, body, false);
  }

  // One sampler per history, not one per clocking event. Two histories under
  // one event could share the wait, but deciding that two clocks are the same
  // event means comparing lowered expressions for equality -- and a wrong
  // answer there records one expression's ticks against another's clock, which
  // no test would obviously catch. What sharing would save is a wait.
  for (const hir::SampledHistoryId id : hir_scope.sampled_histories.Ids()) {
    auto sampler_or = LowerSampledHistorySampler(
        *this, ctor_frame, id, hir_scope.sampled_histories.Get(id));
    if (!sampler_or) return std::unexpected(std::move(sampler_or.error()));
    const mir::CallableId body =
        mir_class.callables.Add(std::move(*sampler_or));
    AppendProcessRegistration(unit_lowerer, activate_frame, body, false);
  }

  // The classes this scope declares, lowered against it: their bodies reach
  // this scope's declarations the way a process of it does, and the frame they
  // stand on is this scope's own.
  for (ClassDeclLowerer& class_lowerer : class_lowerers_) {
    auto class_r = class_lowerer.PopulateBodies(
        ctor_frame, StorageBringUp{.install = init_frame, .value = init_frame});
    if (!class_r) return std::unexpected(std::move(class_r.error()));
  }

  // One process per assertion and clocking event, for the same reason a
  // sampler is one per history: two assertions under one event could share the
  // wait, and deciding that two clocks are the same event means comparing
  // lowered expressions for equality.
  std::vector<
      std::pair<hir::ConcurrentAssertionId, InstalledConcurrentAssertion>>
      installed_assertions;
  installed_assertions.reserve(hir_scope.concurrent_assertions.size());
  for (const hir::ConcurrentAssertionId id :
       hir_scope.concurrent_assertions.Ids()) {
    auto installed = LowerConcurrentAssertion(
        *this, mir_class, ctor_frame, scopes_, id,
        hir_scope.concurrent_assertions.Get(id));
    if (!installed) return std::unexpected(std::move(installed.error()));
    for (const mir::CallableId process : installed->processes) {
      AppendProcessRegistration(unit_lowerer, activate_frame, process, false);
    }
    installed_assertions.emplace_back(id, *installed);
  }

  // Recurse into descendants. Every class's shape is already published, so a
  // body that names a peer's member resolves through the existing identity
  // model regardless of which sibling lowers next.
  for (auto& child : children_) {
    auto child_r = child->PopulateBodies(ctor_frame);
    if (!child_r) return std::unexpected(std::move(child_r.error()));
  }

  for (const hir::GenerateId gen : hir_scope.generates.Ids()) {
    auto stmt = LowerGenerateAsStmt(
        *this, ctor_frame, hir_scope.generates.Get(gen),
        generate_bindings_.Get(gen));
    if (!stmt) return std::unexpected(std::move(stmt.error()));
    ctor_block.AppendStmt(*std::move(stmt));
  }

  EmitInstanceMemberConstruction(*this, ctor_frame);
  auto port_conn_r = InstallPortConnections(
      *this, ctor_frame, resolve_frame, init_frame, activate_frame);
  if (!port_conn_r) return std::unexpected(std::move(port_conn_r.error()));

  // A cell answers for a sampled value only once armed, and what arming
  // installs is the value every read answers with until a later time slot
  // first changes the cell -- for a static variable, the value its declaration
  // assigns (LRM 16.5.1). That value is in the cell once every initializer in
  // the design has run, which is what puts this here rather than beside the
  // initializer itself: a cell reached across an instance boundary is one this
  // scope cannot order itself against.
  for (const hir::SensitivityEntry& sampled : hir_scope.sampled_cells) {
    const mir::ExprId cell = BuildObservableCellExpr(
        activate_block, activate_frame, unit_lowerer.Unit(), *this, sampled);
    activate_block.AppendStmt(
        mir::ExprStmt{
            .expr = activate_block.exprs.Add(
                mir::MakeCellArmSamplingCallExpr(cell, void_type))});
  }

  // Every history is filled here, once its subject's cells are armed above.
  // What the subject evaluates to now is the expression's default sampled value
  // (LRM 16.5.1), which is the answer the standard requires of a read that
  // reaches further back than the ticks that have happened -- so a history that
  // starts full has no empty case and keeps no count of them.
  for (const hir::SampledHistoryId id : hir_scope.sampled_histories.Ids()) {
    const hir::SampledHistoryDecl& history =
        hir_scope.sampled_histories.Get(id);
    auto subject_or = LowerExpr(
        hir_scope.exprs.Get(history.subject),
        activate_frame.WithReadsAsOf(ReadsAsOf::kPreponed));
    if (!subject_or) return std::unexpected(std::move(subject_or.error()));
    const mir::ExprId value = activate_block.exprs.Add(*std::move(subject_or));
    const mir::ExprId depth = BuildIntLiteral(
        unit_lowerer.Unit(), activate_block,
        static_cast<std::int64_t>(history.depth));
    activate_block.AppendStmt(
        mir::ExprStmt{
            .expr = activate_block.exprs.Add(
                mir::MakeSampledHistoryInstallCallExpr(
                    BuildSampledHistoryExpr(
                        activate_block, activate_frame, *this, id),
                    value, depth, void_type))});
  }

  // Every assertion's storage is filled here too. Nothing about one needs an
  // earlier phase -- what it reads it reaches through cells sealed since Seal
  // -- and the sampling those cells answer from is armed just above.
  for (const auto& [id, installed] : installed_assertions) {
    AppendConcurrentAssertionInstall(*this, activate_frame, id, installed);
  }

  ctor_code.params.clear();
  ctor_code.params.reserve(1 + ctor_prefix_local_ids.size());
  ctor_code.params.push_back(self_id);
  for (const mir::LocalId id : ctor_prefix_local_ids) {
    ctor_code.params.push_back(id);
  }
  ctor_code.result_type = void_type;
  // Ctor code stays local so subsequent lowering can still append exprs into
  // its body; once complete, it is moved into the class's method storage and
  // referenced by the construction protocol.

  auto& unit = unit_lowerer.Unit();

  // The three bodies the runtime drives this scope through after construction
  // (LRM 23.3.3.2 / 6.8 / 9.2). `self` is the body's own receiver, typed as
  // this class. Which body is which is said once, where the class states that
  // it stands on the object tree; none of them answers to a name, because a
  // name would sit in the same name space as the scope's own subroutines and a
  // method spelled the same way would then share it.
  // A phase with no work is a body with no statements, not an absent body:
  // whether emitting one is worth avoiding is a question for the layer that
  // removes dead work, and answering it here would cost every consumer a case.
  const auto add_body = [&](mir::CallableCode& code,
                            mir::LocalId self) -> mir::CallableId {
    code.params = {self};
    code.result_type = void_type;
    return mir_class.callables.Add(
        mir::CallableDecl{
            .code = std::move(code),
            .foreign = std::nullopt,
            .virtual_dispatch = std::nullopt});
  };
  const mir::CallableId resolve_body = add_body(resolve_code, resolve_self_id);
  const mir::CallableId init_body = add_body(initialize_code, init_self_id);
  const mir::CallableId create_body = add_body(activate_code, activate_self_id);

  const std::vector<mir::ExprId> base_trailing_args =
      InstallGeneratedDefinition(
          unit, mir_class, class_id_, ctor_code, resolve_body, init_body,
          create_body);

  FinalizeConstructor(
      unit, mir_class, std::move(ctor_code), ctor_prefix_local_ids,
      base_trailing_args);

  unit.DefineClass(class_id_, std::move(mir_class));
  return {};
}

}  // namespace lyra::lowering::hir_to_mir
