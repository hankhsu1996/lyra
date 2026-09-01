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
#include "lyra/lowering/hir_to_mir/continuous_assign.hpp"
#include "lyra/lowering/hir_to_mir/declaration_initializer.hpp"
#include "lyra/lowering/hir_to_mir/declared_instances.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/expression/dpi_call.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/lowering/hir_to_mir/lhs_store.hpp"
#include "lyra/lowering/hir_to_mir/package_initialization.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
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
#include "lyra/support/builtin_fn.hpp"

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
  const mir::TypeId object_type =
      unit_lowerer.Unit().types.Intern(mir::ObjectType{.class_id = class_id});
  return unit_lowerer.Unit().types.PointerTo(
      object_type, mir::PointerOwnership::kUnique);
}

// The pointer type a handle to one of `member`'s objects has. The object is one
// the declaring unit publishes, so the type names this unit's record of it.
auto MakeExternalUnitPointer(
    UnitLowerer& unit_lowerer, const hir::InstanceMemberDecl& member,
    mir::PointerOwnership ownership) -> mir::TypeId {
  const mir::TypeId object_type = unit_lowerer.Unit().types.Intern(
      mir::ExternalUnitObjectType{
          .object = unit_lowerer.TranslateExternalUnitObject(member.object)});
  return unit_lowerer.Unit().types.PointerTo(object_type, ownership);
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
  const mir::TypeId indices_type = unit_lowerer.Unit().types.MachineArrayOf(
      builtins.int_type, indices.size());
  const mir::ExprId indices_id = block.exprs.Add(
      mir::Expr{
          .data = mir::ArrayLiteralExpr{.elements = std::move(indices)},
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
                      mir::Direct{.target = support::BuiltinFn::kAddOwnedChild},
                  .arguments = {parent_self, ctor_call_id}},
          .type = builtins.scope_ptr});
  return block.exprs.Add(
      mir::Expr{
          .data = mir::PointerCastExpr{.operand = add_id},
          .type = borrowed_pointer_type});
}

// Emits the constructor-body construction for every object the scope's instance
// members declare: one built object per element, each stored into the field
// that element was given. The runtime tree owns every built instance; the field
// keeps the borrowed handle a layout-visible route projects through.
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
    for (const DeclaredInstance& object : lowerer.Instances(id)) {
      const mir::ExprId parent_self = block.exprs.Add(
          MakeSelfRefExpr(frame, frame.current_class->self_pointer_type));
      const mir::ExprId value = BuildOwnedInstance(
          unit_lowerer, frame, parent_self, im.instance_name, owning, borrowed,
          object.coordinates);
      const mir::ExprId member = block.exprs.Add(
          mir::MakeFieldAccessExpr(
              parent_self,
              mir::FieldTarget{
                  .owner = frame.current_class_id, .slot = object.handle},
              borrowed));
      block.AppendStmt(
          mir::ExprStmt{
              .expr = block.exprs.Add(
                  mir::MakeAssignExpr(member, value, borrowed))});
    }
  }
}

// Allocates one MIR member per cross-unit reference. Every reference -- upward
// or downward, `$root`-anchored or named -- takes the same borrowed-pointer
// slot, and its pointee is the cell the target's own storage says it holds, so
// a read or a drive reaches the right access protocol. The route that fills
// each slot runs in the resolve phase, after the whole object tree exists.
auto DeclareRoutedRefSlots(StructuralScopeLowerer& lowerer, ClassShape& shape)
    -> base::Translation<hir::RoutedRefId, RoutedRefMeta> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  const hir::StructuralScope& hir_scope = lowerer.HirScope();
  std::vector<RoutedRefMeta> slots;
  slots.reserve(hir_scope.routed_refs.size());
  for (const auto& cu : hir_scope.routed_refs) {
    std::string member_name = "ep" + std::to_string(slots.size());
    if (std::holds_alternative<hir::NetStorage>(cu.target_storage) &&
        !std::holds_alternative<hir::InUnitHead>(cu.recipe.head)) {
      throw InternalError(
          "DeclareRoutedRefSlots: an upward routed reference to a net is not "
          "yet supported");
    }
    const mir::TypeId leaf = unit_lowerer.MemberCellType(
        unit_lowerer.TranslateType(cu.recipe.type), cu.target_storage);
    const mir::TypeId slot_type = unit_lowerer.Unit().types.PointerTo(
        leaf, mir::PointerOwnership::kBorrowed);
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
  const mir::TypeId indices_type = unit_lowerer.Unit().types.MachineArrayOf(
      builtins.int_type, indices.size());
  return block.exprs.Add(
      mir::Expr{
          .data = mir::ArrayLiteralExpr{.elements = std::move(ids)},
          .type = indices_type});
}

auto BuildStringLiteral(
    UnitLowerer& unit_lowerer, mir::Block& block, const std::string& s)
    -> mir::ExprId {
  return block.exprs.Add(
      mir::MakeStringLiteral(unit_lowerer.Unit().builtins.string, s));
}

// A route runs from its origin (the referrer's `self`) to the referenced leaf.
// Its receiver at each step is either **typed** -- layout-visible, known to
// point at a class this artifact owns, so the step takes a typed
// `FieldAccessExpr` on the receiver's class shape -- or **opaque**, a runtime
// `Scope*` reached through the SDK by name (a step crossing into another
// unit's body). An indexed hop on a layout-visible step uses the SDK
// `GetChild(name, indices)` fallback but downcasts back to typed, since MIR
// carries no typed vector-index primitive.
struct RouteReceiver {
  mir::ExprId expr{};
  // The scope the receiver points at, when the receiver is a typed pointer to
  // a class this artifact owns; whatever the route names next resolves against
  // it. Null when the receiver is a runtime `Scope*` and every following step
  // is opaque.
  const StructuralScopeLowerer* scope = nullptr;
};

// Reaches a child by name+indices as an opaque `Scope*` -- the realization of
// an opaque step (one crossing into another unit's body).
auto SdkChildOpaque(
    UnitLowerer& unit_lowerer, mir::Block& block, mir::ExprId receiver,
    const std::string& name, std::span<const std::uint32_t> indices)
    -> RouteReceiver {
  const mir::ExprId step = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{.target = support::BuiltinFn::kGetChild},
                  .arguments =
                      {receiver, BuildStringLiteral(unit_lowerer, block, name),
                       BuildIndicesLiteral(unit_lowerer, block, indices)}},
          .type = unit_lowerer.Unit().builtins.scope_ptr});
  return RouteReceiver{.expr = step, .scope = nullptr};
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
        .scope = &lowerer.EnclosingScopeAtHops(ih->hops)};
  }

  const mir::ExprId self_ref = block.exprs.Add(
      MakeSelfRefExpr(frame, frame.current_class->self_pointer_type));

  if (std::holds_alternative<hir::RootHead>(head)) {
    const mir::ExprId root = block.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee =
                        mir::Direct{.target = support::BuiltinFn::kResolveRoot},
                    .arguments = {self_ref}},
            .type = scope_ptr_type});
    return RouteReceiver{.expr = root, .scope = nullptr};
  }

  // The visible-child climb walks the parent chain by name (LRM 23.8).
  const auto& vc = std::get<hir::VisibleChildHead>(head);
  const mir::ExprId matched = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = support::BuiltinFn::kResolveVisibleChild},
                  .arguments =
                      {self_ref,
                       BuildStringLiteral(unit_lowerer, block, vc.head_name),
                       BuildIndicesLiteral(
                           unit_lowerer, block, vc.head_indices)}},
          .type = scope_ptr_type});
  return RouteReceiver{.expr = matched, .scope = nullptr};
}

// Descends one step into a child the receiver's scope declares: the typed
// member access that projects the parent's handle on that child. The step's
// coordinates are settled during elaboration, so they name one of the objects
// the child declares rather than indexing anything here; a child with no
// declared dimensions is the one-object case and carries none. A child whose
// body is another compilation unit leaves the receiver opaque from there.
auto AppendOwnedChildStep(
    UnitLowerer& unit_lowerer, mir::Block& block, const RouteReceiver& receiver,
    const hir::OwnedChildStep& step) -> RouteReceiver {
  if (receiver.scope == nullptr) {
    throw InternalError(
        "AppendOwnedChildStep: the step names a child of a scope this artifact "
        "lowers, so the route cannot have left the artifact before it");
  }
  const OwnedChildAnchor anchor = receiver.scope->TranslateOwnedChild(
      hir::StructuralHops{}, step.child, step.indices);
  const mir::ClassId receiver_class = receiver.scope->ClassId();
  const mir::TypeId type = unit_lowerer.GetClassShape(receiver_class)
                               .fields.Get(anchor.borrowed_handle)
                               .type;
  const mir::ExprId access = block.exprs.Add(
      mir::MakeFieldAccessExpr(
          receiver.expr,
          mir::FieldTarget{
              .owner = receiver_class, .slot = anchor.borrowed_handle},
          type));
  return RouteReceiver{.expr = access, .scope = anchor.target_scope};
}

// Descends one step through an interface port of the receiver's scope: the
// typed member access that projects the borrowed reference the parent bound
// there (LRM 25.3). Everything past the step belongs to the unit the port
// names, which this artifact does not lower, so the receiver stops being one of
// its own scopes -- the same place an owned child whose body is another unit
// leaves it.
auto AppendInterfacePortStep(
    UnitLowerer& unit_lowerer, mir::Block& block, const RouteReceiver& receiver,
    const hir::InterfacePortStep& step) -> RouteReceiver {
  if (receiver.scope == nullptr) {
    throw InternalError(
        "AppendInterfacePortStep: the step names a port of a scope this "
        "artifact lowers, so the route cannot have left the artifact before "
        "it");
  }
  const mir::ClassId receiver_class = receiver.scope->ClassId();
  const mir::FieldId field =
      receiver.scope->TranslateInterfacePort(hir::StructuralHops{}, step.port);
  const mir::TypeId type =
      unit_lowerer.GetClassShape(receiver_class).fields.Get(field).type;
  const mir::ExprId access = block.exprs.Add(
      mir::MakeFieldAccessExpr(
          receiver.expr,
          mir::FieldTarget{.owner = receiver_class, .slot = field}, type));
  return RouteReceiver{.expr = access, .scope = nullptr};
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
          receiver.expr, mir::FieldTarget{.owner = owner_class, .slot = field},
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
  // pointer the step before it produced. The access states only the position:
  // which object those positions index is already on the receiver's type.
  if (const auto* member = std::get_if<hir::SignatureMemberLeaf>(&leaf)) {
    const auto& slot =
        std::get<mir::PointerType>(unit.types.Get(slot_type).data);
    const mir::ExprId access = block.exprs.Add(
        mir::MakeFieldAccessExpr(
            receiver.expr,
            UnitLowerer::TranslatePublishedMember(member->member),
            slot.pointee));
    return block.exprs.Add(
        mir::Expr{
            .data = mir::AddressOfExpr{.operand = access}, .type = slot_type});
  }

  // A route ending at a scope names the object the steps landed on, and every
  // step already yields a borrowed pointer to what it reached, so the last one
  // is the value.
  if (std::holds_alternative<hir::ScopeLeaf>(leaf)) {
    return receiver.expr;
  }

  if (const auto* opaque = std::get_if<hir::OpaqueLeaf>(&leaf)) {
    const mir::TypeId void_ptr_type = unit.types.PointerTo(
        unit.builtins.void_type, mir::PointerOwnership::kBorrowed);
    const mir::ExprId raw = block.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee =
                        mir::Direct{.target = support::BuiltinFn::kGetSignal},
                    .arguments =
                        {receiver.expr,
                         BuildStringLiteral(
                             unit_lowerer, block, opaque->name)}},
            .type = void_ptr_type});
    return block.exprs.Add(
        mir::Expr{
            .data = mir::PointerCastExpr{.operand = raw}, .type = slot_type});
  }

  if (receiver.scope == nullptr) {
    throw InternalError(
        "MaterializeLeaf: the leaf names a declaration of a scope this "
        "artifact lowers, so the route cannot have left the artifact");
  }

  if (const auto* object = std::get_if<hir::StructuralDataObjectLeaf>(&leaf)) {
    return AddressTypedLeaf(
        unit_lowerer, block, receiver, receiver.scope->ClassId(),
        receiver.scope->TranslateStructuralDataObject(
            hir::StructuralHops{}, object->object),
        slot_type);
  }

  const auto& static_leaf = std::get<hir::ProceduralStaticLeaf>(leaf);
  return AddressTypedLeaf(
      unit_lowerer, block, receiver, receiver.scope->ClassId(),
      receiver.scope->ProceduralStaticBinding(static_leaf.body, static_leaf.var)
          .field,
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
              return AppendOwnedChildStep(unit_lowerer, block, receiver, owned);
            },
            [&](const hir::InterfacePortStep& port) {
              return AppendInterfacePortStep(
                  unit_lowerer, block, receiver, port);
            },
            [&](const hir::OpaqueStep& opaque) {
              return SdkChildOpaque(
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
                        mir::FieldTarget{
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
                                  .slot = body}},
                  .arguments = {body_self}},
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

// Binds a child's interface port to the interface instance the connection
// names (LRM 25.3), in the resolve phase where the object tree is complete: the
// route to the child's member yields a pointer to the slot the child holds, the
// route to the interface yields the object, and one store fills the one with
// the other. The child owns no storage on either side, so nothing else happens
// here -- the same shape a `ref` port's alias bind takes, over an object rather
// than a cell.
void InstallInterfacePortConnection(
    StructuralScopeLowerer& lowerer, const WalkFrame& resolve_frame,
    const hir::InterfacePortConnection& conn) {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  mir::Block& block = *resolve_frame.current_block;
  auto& types = unit_lowerer.Unit().types;
  const mir::TypeId member_type = types.PointerTo(
      unit_lowerer.TranslateType(conn.endpoint.type),
      mir::PointerOwnership::kBorrowed);
  const mir::TypeId slot_type =
      types.PointerTo(member_type, mir::PointerOwnership::kBorrowed);
  const mir::ExprId nav =
      BuildRouteValue(lowerer, resolve_frame, conn.endpoint, slot_type);
  const mir::ExprId target = block.exprs.Add(
      mir::Expr{.data = mir::DerefExpr{.pointer = nav}, .type = member_type});
  const mir::ExprId peer =
      BuildRouteValue(lowerer, resolve_frame, conn.peer, member_type);
  block.AppendStmt(
      mir::ExprStmt{
          .expr = block.exprs.Add(
              mir::Expr{
                  .data = mir::AssignExpr{.target = target, .value = peer},
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
        const mir::TypeId value_type = unit_lowerer.TranslateType(recipe.type);
        const mir::TypeId ref_type = unit_lowerer.Unit().types.Intern(
            mir::RefType{
                .pointee = value_type,
                .mutability = mir::Mutability::kMutable});
        const mir::TypeId slot_type = unit_lowerer.Unit().types.PointerTo(
            ref_type, mir::PointerOwnership::kBorrowed);
        const mir::ExprId nav =
            BuildRouteValue(lowerer, resolve_frame, recipe, slot_type);
        const mir::ExprId target = resolve_block.exprs.Add(
            mir::Expr{
                .data = mir::DerefExpr{.pointer = nav}, .type = ref_type});

        auto peer_or =
            lowerer.LowerLhsExpr(hir_scope.exprs.Get(data.peer), resolve_frame);
        if (!peer_or) return std::unexpected(std::move(peer_or.error()));
        const mir::ExprId peer_cell =
            resolve_block.exprs.Add(*std::move(peer_or));

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
    // way; the sink's own MIR type -- resolved-net cell or observable cell --
    // picks the write protocol (LRM 23.3.3).
    const hir::ContinuousAssign assign{
        .span = pc.span,
        .lhs = is_input ? cell.cell : data.peer,
        .rhs = is_input ? data.peer : cell.cell,
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
            mir::FieldTarget{
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
  const mir::TypeId indices_type = unit_lowerer.Unit().types.MachineArrayOf(
      builtins.int_type, index_elems.size());
  const mir::ExprId indices_id = arm_block.exprs.Add(
      mir::Expr{
          .data = mir::ArrayLiteralExpr{.elements = std::move(index_elems)},
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
                      mir::Direct{.target = support::BuiltinFn::kAddOwnedChild},
                  .arguments = {parent_read(), ctor_call_id}},
          .type = builtins.scope_ptr});
  const mir::TypeId handle_type = owner_class.fields.Get(handle_field).type;
  const mir::ExprId typed_handle = arm_block.exprs.Add(
      mir::Expr{
          .data = mir::PointerCastExpr{.operand = add_call_id},
          .type = handle_type});
  const mir::ExprId member = arm_block.exprs.Add(
      mir::MakeFieldAccessExpr(
          self_read(),
          mir::FieldTarget{
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
  const mir::TypeId self_object_type =
      unit_lowerer.Unit().types.Intern(mir::ObjectType{.class_id = class_id_});
  const mir::TypeId self_pointer_type = unit_lowerer.Unit().types.PointerTo(
      self_object_type, mir::PointerOwnership::kBorrowed);

  ClassShape shape;
  shape.name = name_;
  shape.base =
      mir::ClassRef{mir::RuntimeClassRef{.symbol = "lyra::runtime::Scope"}};
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
  for (const hir::InterfacePortId id : hir_scope.interface_ports.Ids()) {
    append_unpublished(id);
  }

  std::vector<mir::FieldId> data_object_fields(
      hir_scope.structural_data_objects.size());
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
            [&](const hir::InterfacePortId& id) {
              const auto& port = hir_scope.interface_ports.Get(id);
              // The port stands for an instance of the unit its record names,
              // so that record is the one source of both the object's type and
              // the positions a name reached through it is counted out of.
              const mir::TypeId object_type = unit_lowerer.Unit().types.Intern(
                  mir::ExternalUnitObjectType{
                      .object = unit_lowerer.TranslateExternalUnitObject(
                          port.object)});
              interface_port_fields[id.value] = shape.fields.Add(
                  mir::FieldDecl{
                      .name = port.name,
                      .type = unit_lowerer.MemberCellType(
                          object_type, hir::BorrowedObjectStorage{})});
            }},
        decl);
  }
  data_object_fields_ = {
      hir_scope.structural_data_objects.size(), std::move(data_object_fields)};
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
      const mir::TypeId handle_type = unit_lowerer.Unit().types.PointerTo(
          unit_lowerer.Unit().types.Intern(
              mir::ObjectType{.class_id = child_id}),
          mir::PointerOwnership::kBorrowed);
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

  // Every instance member keeps a borrowed typed handle on this class, and that
  // handle's type states the member's cardinality -- the bare handle for a
  // single instance, one sequence wrapper per declared dimension for an array
  // (LRM 23.3.2). A layout-visible route step projects the handle and indexes
  // it once per dimension, so reaching an element never has to name the member
  // a second time.
  {
    std::vector<DeclaredInstances> declared;
    declared.reserve(hir_scope.instance_members.size());
    for (const auto& im : hir_scope.instance_members) {
      const mir::TypeId handle_type = MakeExternalUnitPointer(
          unit_lowerer, im, mir::PointerOwnership::kBorrowed);
      declared.push_back(
          DeclaredInstances::Declare(
              im.array_dims, [&](std::span<const std::uint32_t> coords) {
                std::string name = im.instance_name;
                for (const std::uint32_t coord : coords) {
                  name += std::format("_{}", coord);
                }
                return shape.fields.Add(
                    mir::FieldDecl{
                        .name = std::format("{}_borrowed_handle", name),
                        .type = handle_type});
              }));
    }
    declared_instances_ = {
        hir_scope.instance_members.size(), std::move(declared)};
  }

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
      mir::RuntimeLibraryType{
          .kind = mir::RuntimeLibraryKind::kCancellationTarget});
  std::vector<DeclaredScope> scopes;
  scopes.reserve(hir_scope.procedural_scopes.size());
  for (const hir::ProceduralScopeId scope_id :
       hir_scope.procedural_scopes.Ids()) {
    const auto& scope = hir_scope.procedural_scopes.Get(scope_id);
    const std::string segment = hir::SegmentName(scope, scope_id);

    const mir::ClassId node_class = unit_lowerer.Unit().DeclareClass();
    ClassShape node_shape;
    node_shape.name = std::format("{}__{}", name_, segment);
    node_shape.base =
        mir::ClassRef{mir::RuntimeClassRef{.symbol = "lyra::runtime::Scope"}};
    node_shape.is_final = true;
    node_shape.self_pointer_type = unit_lowerer.Unit().types.PointerTo(
        unit_lowerer.Unit().types.Intern(
            mir::ObjectType{.class_id = node_class}),
        mir::PointerOwnership::kBorrowed);
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
                        .type = unit_lowerer.Unit().types.PointerTo(
                            unit_lowerer.Unit().types.Intern(
                                mir::ObjectType{.class_id = node_class}),
                            mir::PointerOwnership::kBorrowed)})},
        .cancellation_target = std::nullopt};

    // What a `disable` of this scope invalidates (LRM 9.6.2). Its targets are
    // the blocks and tasks a name reaches, which is the same set a hierarchical
    // path reaches, so a scope the source named owns one for that reason alone
    // and one it did not owns none -- no pass has to first find out which
    // scopes some `disable` names. It is one cell per instance shared by every
    // activation of the scope, which is what makes it a field here.
    if (scope.source_name.has_value()) {
      node.cancellation_target = shape.fields.Add(
          mir::FieldDecl{
              .name = std::format("{}__cancel_{}", segment, scope_id.value),
              .type = cancellation_target_type});
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
                unit_lowerer, hir_scope.procedural_scopes, shape.fields,
                ObservedStorage::kYes, s.body, SignatureBoundVars(s), s.name)});
  }
  shape.callable_signatures = {
      hir_scope.structural_subroutines.size(), std::move(signatures)};
  declared_subroutines_ = {
      hir_scope.structural_subroutines.size(), std::move(declared_subroutines)};

  std::vector<StaticVarBindings> process_statics;
  process_statics.reserve(hir_scope.processes.size());
  for (const hir::ProcessId id : hir_scope.processes.Ids()) {
    process_statics.push_back(BindBodyStatics(
        unit_lowerer, hir_scope.procedural_scopes, shape.fields,
        ObservedStorage::kYes, hir_scope.processes.Get(id).body, {},
        ProcessCallableName(id)));
  }
  process_static_bindings_ = {
      hir_scope.processes.size(), std::move(process_statics)};

  unit_lowerer.DefineClassShape(class_id_, std::move(shape));
  return class_id_;
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
    mir::CallableCode& ctor_code, std::optional<mir::CallableId> resolve_body,
    std::optional<mir::CallableId> init_body,
    std::optional<mir::CallableId> create_body) -> std::vector<mir::ExprId> {
  const mir::TypeId scope_ptr = unit.builtins.scope_ptr;
  const mir::TypeId self_ptr = cls.self_pointer_type;
  const mir::TypeId void_type = unit.builtins.void_type;
  const auto make_adapter =
      [&](std::string name,
          std::optional<mir::CallableId> body) -> mir::AbiAdapterId {
    mir::CallableCode code = mir::CallableCode::Defined();
    const mir::LocalId self =
        code.locals.Add(mir::LocalDecl{.name = "self", .type = scope_ptr});
    code.params = {self};
    code.result_type = void_type;
    if (body.has_value()) {
      const mir::ExprId self_ref = code.Body().exprs.Add(
          mir::Expr{.data = mir::LocalRef{.var = self}, .type = scope_ptr});
      const mir::ExprId typed = code.Body().exprs.Add(
          mir::Expr{
              .data = mir::PointerCastExpr{.operand = self_ref},
              .type = self_ptr});
      const mir::ExprId call = code.Body().exprs.Add(
          mir::Expr{
              .data =
                  mir::CallExpr{
                      .callee =
                          mir::Direct{
                              .target =
                                  mir::CallableTarget{
                                      .owner = cls_id, .slot = *body}},
                      .arguments = {typed}},
              .type = void_type});
      code.Body().AppendStmt(mir::ExprStmt{.expr = call});
    }
    return cls.abi_adapters.Add(
        mir::AbiAdapter{
            .name = std::move(name),
            .code = std::move(code),
            .foreign = std::nullopt});
  };
  const mir::AbiAdapterId resolve_abi =
      make_adapter("ResolveStateAbi", resolve_body);
  const mir::AbiAdapterId init_abi =
      make_adapter("InitializeStateAbi", init_body);
  const mir::AbiAdapterId create_abi =
      make_adapter("CreateProcessesAbi", create_body);

  // The exports this scope publishes, as their own constant: the table the
  // runtime holds is a pointer into contiguous storage, so the records must
  // outlive the definition that points at them rather than sit in its
  // initializer. A scope declaring none contributes an empty array, which the
  // same construction covers.
  mir::StaticConstantDecl exports_decl;
  exports_decl.name = "kExports";
  mir::RuntimeRecordBuilder exports(unit, exports_decl.body.exprs);
  std::vector<mir::ExprId> export_records;
  for (const mir::AbiAdapterId adapter_id : cls.abi_adapters.Ids()) {
    const mir::AbiAdapter& adapter = cls.abi_adapters.Get(adapter_id);
    if (!adapter.foreign.has_value()) {
      continue;
    }
    export_records.push_back(exports.Construct(
        mir::RuntimeLibraryKind::kScopeExport,
        {exports.StringRef(adapter.foreign->foreign_name),
         exports.ErasedFunctionRef(cls, adapter_id)}));
  }
  const auto export_count = static_cast<std::uint32_t>(export_records.size());
  exports_decl.value = exports.MachineArray(
      exports.Type(mir::RuntimeLibraryKind::kScopeExport),
      std::move(export_records));
  const mir::TypeId exports_type = exports.TypeOf(exports_decl.value);
  exports_decl.type = exports_type;
  const mir::StaticConstantId exports_id =
      cls.static_constants.Add(std::move(exports_decl));

  mir::StaticConstantDecl def;
  def.name = "kDefinition";
  mir::RuntimeRecordBuilder definition(unit, def.body.exprs);
  const mir::ExprId exports_ref = definition.Add(
      mir::Expr{
          .data = mir::StaticConstantRef{.constant = exports_id},
          .type = exports_type});
  const mir::ExprId exports_data = definition.Add(
      mir::Expr{
          .data = mir::MachineArrayDataExpr{.array = exports_ref},
          .type = unit.types.Intern(
              mir::PointerType{
                  .pointee =
                      definition.Type(mir::RuntimeLibraryKind::kScopeExport),
                  .ownership = mir::PointerOwnership::kBorrowed,
                  .mutability = mir::Mutability::kReadOnly})});
  const mir::ExprId export_table = definition.Construct(
      mir::RuntimeLibraryKind::kScopeExportTable,
      {exports_data, definition.MachineInt(export_count)});

  const mir::ExprId metadata = definition.Construct(
      mir::RuntimeLibraryKind::kScopeMetadata,
      {definition.MachineInt(cls.time_resolution.unit_power),
       definition.MachineInt(cls.time_resolution.precision_power)});
  const mir::ExprId program = definition.Construct(
      mir::RuntimeLibraryKind::kScopeProgram,
      {metadata, definition.FunctionRef(cls, resolve_abi),
       definition.FunctionRef(cls, init_abi),
       definition.FunctionRef(cls, create_abi), export_table});
  const mir::AbiAdapterId construct_abi =
      make_adapter("ConstructAbi", std::nullopt);
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
          .data = mir::StaticConstantRef{.constant = def_id},
          .type = const_type});
  const mir::ExprId addr = cex.Add(
      mir::Expr{
          .data = mir::AddressOfExpr{.operand = ref},
          .type = unit.types.PointerTo(
              const_type, mir::PointerOwnership::kBorrowed)});
  return {addr};
}

// Composes the base-init arg list (each prefix forwarded as a consuming use,
// followed by trailing args), moves the ctor callable into the class's
// method storage, and points the construction protocol at it. `ctor_code`
// must be finalized (params and result_type set) but not yet inserted into
// the arena.
void FinalizeConstructor(
    mir::CompilationUnit& unit, mir::Class& cls, mir::CallableCode ctor_code,
    const std::vector<mir::LocalId>& prefix_local_ids,
    const std::vector<mir::ExprId>& base_trailing_args) {
  std::optional<mir::BaseInit> base_init_opt;
  if (cls.base.has_value()) {
    std::vector<mir::ExprId> base_args;
    base_args.reserve(prefix_local_ids.size() + base_trailing_args.size());
    for (const mir::LocalId id : prefix_local_ids) {
      const mir::TypeId ty = ctor_code.locals.Get(id).type;
      const mir::ExprId local_ref = ctor_code.Body().exprs.Add(
          mir::Expr{.data = mir::LocalRef{.var = id}, .type = ty});
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
    base_init_opt = mir::BaseInit{.args = std::move(base_args)};
  }
  cls.constructor = mir::ConstructorDecl{
      .code = std::move(ctor_code),
      .base_init = std::move(base_init_opt),
      .member_inits = {}};
}

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
    const bool is_net = std::holds_alternative<hir::StructuralNetDecl>(d.kind);
    const auto* var = std::get_if<hir::StructuralVariableDecl>(&d.kind);
    const mir::TypeKind var_kind =
        unit_lowerer.Unit().types.Get(mir_value_type).Kind();
    // Owned children (pointer / vector / object), cross-instance reference
    // slots (borrowed pointers filled in the resolve phase), and named events
    // have no "value assignment" -- their declaration shape itself fixes the
    // field at construction. A net takes none either: its value is produced by
    // its drivers, seeded when each driver updates in the initialize phase.
    // Value-typed variables (integral, string, real, unpacked / dynamic array)
    // receive an LRM 10.5 initialization statement, run in the initialize
    // phase after the tree's references resolve, not in the constructor.
    const bool is_assignable_value =
        var != nullptr && var_kind != mir::TypeKind::kPointer &&
        var_kind != mir::TypeKind::kVector &&
        var_kind != mir::TypeKind::kObject &&
        var_kind != mir::TypeKind::kExternalUnitObject &&
        var_kind != mir::TypeKind::kEvent;
    if (is_assignable_value) {
      const mir::ExprId init_target = initialize_block.exprs.Add(
          mir::MakeFieldAccessExpr(
              init_self_read(),
              mir::FieldTarget{.owner = class_id_, .slot = mir_id},
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
            unit_lowerer.Unit(), initialize_block, init_target, value_id,
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
            BuildDefaultValueFromHir(unit_lowerer, init_frame, d.type));
        append_stmt(
            mir::MakeCapabilityInitializeCallExpr(
                init_target, prototype,
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
              BuildDefaultValueFromHir(unit_lowerer, init_frame, d.type));
        }
        emit_value_store(value_id);
      }
    }

    // A net cell fixes its declared type at construction (LRM 6.6.1), in the
    // constructor rather than the initialize phase: a net is a readable,
    // well-typed observable before any driver attaches, and before a cross-unit
    // reader seeds from it during the parent-first initialize phase, so a read
    // that early sees the net type's undriven value, never an uninitialized
    // cell. Drivers, attached at Resolve, update it from there.
    if (is_net) {
      const mir::ExprId net_target = ctor_block.exprs.Add(
          mir::MakeFieldAccessExpr(
              self_read(), mir::FieldTarget{.owner = class_id_, .slot = mir_id},
              mir_field_type));
      const mir::ExprId prototype = ctor_block.exprs.Add(
          BuildDefaultValueFromHir(unit_lowerer, ctor_frame, d.type));
      ctor_block.AppendStmt(
          mir::Stmt{
              .label = std::nullopt,
              .data = mir::ExprStmt{
                  .expr = ctor_block.exprs.Add(
                      mir::MakeCapabilityInitializeCallExpr(
                          net_target, prototype, void_type))}});
    }

    // A value signal, or a named event, records its address under its name so a
    // cross-unit referrer resolves it by name at construction. The excluded
    // members -- owned children and cross-unit reference slots -- are not
    // signals.
    const bool is_signal = var_kind != mir::TypeKind::kPointer &&
                           var_kind != mir::TypeKind::kVector &&
                           var_kind != mir::TypeKind::kObject &&
                           var_kind != mir::TypeKind::kExternalUnitObject;
    if (is_signal) {
      const mir::ExprId var_ref = ctor_block.exprs.Add(
          mir::MakeFieldAccessExpr(
              self_read(), mir::FieldTarget{.owner = class_id_, .slot = mir_id},
              mir_field_type));
      const mir::TypeId var_ptr_type = unit_lowerer.Unit().types.PointerTo(
          mir_field_type, mir::PointerOwnership::kBorrowed);
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
                              .target = support::BuiltinFn::kRegisterSignal},
                      .arguments = {self_read(), name_id, addr_id}},
              .type = void_type});
      ctor_block.AppendStmt(mir::ExprStmt{.expr = call});
    }
  }

  // The design root's Initialize phase brings up the packages' variables (LRM
  // 26.2 / 10.5). A package owns no runtime tree node, so the root calls its
  // receiver-less callables here, before the top modules initialize -- this
  // scope's Initialize runs parent-first, and the design root is every module's
  // ancestor. It runs design-wide in two passes: install every package's cells
  // (their declared type and default), then run every package's value
  // initializers, so a value initializer that reads another package's variable
  // always reaches installed storage. The plan is resolved by the whole-design
  // assembly and realized here; this scope carries it only for the design root,
  // so a source unit's scope and a nested scope both leave it empty.
  const auto call_package = [&](const std::string& pkg,
                                std::string_view callable,
                                std::vector<mir::ExprId> args) {
    unit_lowerer.Unit().AddExternalReferencedUnit(pkg);
    const mir::ExprId call = initialize_block.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee =
                        mir::Direct{
                            .target =
                                mir::ExternalUnitCallableTarget{
                                    .unit_name = pkg,
                                    .callable_name = std::string{callable}}},
                    .arguments = std::move(args)},
            .type = void_type});
    initialize_block.AppendStmt(mir::ExprStmt{.expr = call});
  };
  for (const std::string& pkg : package_init_plan_.install_order) {
    call_package(pkg, kPackageInstallCallableName, {});
  }
  for (const std::string& pkg : package_init_plan_.value_initialize_order) {
    call_package(pkg, kPackageInitializeCallableName, {});
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
    node_class.base = node_shape.base;
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
    const std::vector<mir::ExprId> node_base_trailing_args =
        InstallGeneratedDefinition(
            unit_lowerer.Unit(), node_class, name_node.class_id, node_ctor_code,
            std::nullopt, std::nullopt, std::nullopt);
    FinalizeConstructor(
        unit_lowerer.Unit(), node_class, std::move(node_ctor_code),
        node_ctor_prefix_local_ids, node_base_trailing_args);
    unit_lowerer.Unit().DefineClass(name_node.class_id, std::move(node_class));
  }

  // Build the whole name tree here, in this scope's own constructor: each node
  // hangs under the node of the scope around it, which is what the source
  // nesting means, while the borrowed handle to it lands on this class -- so
  // the objects nest and every one of them is still one step from a body.
  // Construction precedes every resolve, so the names registered below are in
  // place before anything asks for one.
  const auto build_name_tree =
      [&](const auto& self_ref, hir::ProceduralScopeId scope_id,
          std::optional<mir::FieldId> parent_handle) -> void {
    const auto& scope = hir_scope.procedural_scopes.Get(scope_id);
    const ScopeNameNode& name_node = *scopes_.Get(scope_id).name_node;
    AppendOwnedChildConstruction(
        unit_lowerer, ctor_frame, parent_handle, scope.source_name.value_or(""),
        name_node.class_id, std::nullopt, name_node.borrowed_handle);
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
      const mir::TypeId cell_type = mir_class.fields.Get(binding.field).type;
      const mir::ExprId cell = ctor_block.exprs.Add(
          mir::MakeFieldAccessExpr(
              self_read(),
              mir::FieldTarget{.owner = class_id_, .slot = binding.field},
              cell_type));
      const mir::ExprId addr = ctor_block.exprs.Add(
          mir::MakeAddressOfExpr(
              cell, unit_lowerer.Unit().types.PointerTo(
                        cell_type, mir::PointerOwnership::kBorrowed)));
      const mir::FieldId borrowed_handle =
          scopes_.Get(binding.scope).name_node->borrowed_handle;
      const mir::ExprId node = ctor_block.exprs.Add(
          mir::MakeFieldAccessExpr(
              self_read(),
              mir::FieldTarget{.owner = class_id_, .slot = borrowed_handle},
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
                                          support::BuiltinFn::kRegisterSignal},
                              .arguments = {node, name_lit, addr}},
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
    ProcessLowerer subroutine_lowerer(
        unit_lowerer, this, hir_scope.time_resolution, src.body, src.name,
        ctor_frame, scopes_, declared.statics);
    auto code_or = subroutine_lowerer.Run(src);
    if (!code_or) return std::unexpected(std::move(code_or.error()));
    mir_class.callables.Define(
        declared.callable, mir::CallableDecl{
                               .name = src.name,
                               .code = *std::move(code_or),
                               .foreign = std::nullopt,
                               .virtual_dispatch = std::nullopt});
    subroutine_callables.push_back(declared.callable);
    for (const StaticVarBinding& binding : declared.statics) {
      auto integ = IntegrateStaticInitializer(
          subroutine_lowerer, src.body, init_frame, binding);
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
            .name = std::format("{}__export", export_decl.foreign_name),
            .code = std::move(entry.code),
            .foreign = std::move(entry.linkage)});
  }

  for (const hir::ProcessId id : hir_scope.processes.Ids()) {
    const auto& p = hir_scope.processes.Get(id);
    const StaticVarBindings& statics = process_static_bindings_.Get(id);
    ProcessLowerer process_lowerer(
        unit_lowerer, this, hir_scope.time_resolution, p.body,
        ProcessCallableName(id), ctor_frame, scopes_, statics);
    auto code_or = process_lowerer.Run(p);
    if (!code_or) return std::unexpected(std::move(code_or.error()));
    const mir::CallableId body = mir_class.callables.Add(
        mir::CallableDecl{
            .name = ProcessCallableName(id),
            .code = *std::move(code_or),
            .foreign = std::nullopt,
            .virtual_dispatch = std::nullopt});
    AppendProcessRegistration(
        unit_lowerer, activate_frame, body, p.kind == hir::ProcessKind::kFinal);
    for (const StaticVarBinding& binding : statics) {
      auto integ = IntegrateStaticInitializer(
          process_lowerer, p.body, init_frame, binding);
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

  // The resolve, initialize, and activate phases are ordinary callables run by
  // the runtime after construction (LRM 23.3.3.2 / 6.8 / 9.2), each present
  // only when the scope has work for it. `self` is the phase's own receiver,
  // typed as this class. The body is added under its plain name; the runtime
  // reaches it through a per-phase ABI adapter installed in the scope's
  // definition.
  const auto add_body =
      [&](mir::Block& block, mir::CallableCode& code, mir::LocalId self,
          std::string name) -> std::optional<mir::CallableId> {
    if (block.root_stmts.empty()) {
      return std::nullopt;
    }
    code.params = {self};
    code.result_type = void_type;
    return mir_class.callables.Add(
        mir::CallableDecl{
            .name = std::move(name),
            .code = std::move(code),
            .foreign = std::nullopt,
            .virtual_dispatch = std::nullopt});
  };
  const std::optional<mir::CallableId> resolve_body =
      add_body(resolve_block, resolve_code, resolve_self_id, "ResolveState");
  const std::optional<mir::CallableId> init_body = add_body(
      initialize_block, initialize_code, init_self_id, "InitializeState");
  const std::optional<mir::CallableId> create_body = add_body(
      activate_block, activate_code, activate_self_id, "CreateProcesses");

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
