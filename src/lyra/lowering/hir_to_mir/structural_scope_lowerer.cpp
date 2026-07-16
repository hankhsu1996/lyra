#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <expected>
#include <format>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <unordered_set>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/procedural_scope.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/hir_to_mir/binding_origin.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/continuous_assign.hpp"
#include "lyra/lowering/hir_to_mir/declaration_initializer.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/expression/calls.hpp"
#include "lyra/lowering/hir_to_mir/lhs_observable.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/services_call.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// Adds the runtime scope base's construction prefix (parent, hierarchy
// segment, services) as ordinary ctor params, in the order the base
// constructor consumes them.
void AttachRuntimeScopeCtorPrefix(
    const mir::CompilationUnit& unit, mir::ClassShape& shape) {
  const auto& builtins = unit.builtins;
  shape.ctor_prefix_params.Add(
      mir::ParamDecl{.name = "parent", .type = builtins.scope_ptr});
  shape.ctor_prefix_params.Add(
      mir::ParamDecl{.name = "segment", .type = builtins.hierarchy_segment});
  shape.ctor_prefix_params.Add(
      mir::ParamDecl{.name = "services", .type = builtins.services});
}

// Wrap a value type in `ObservableType` iff it is a SystemVerilog value-storage
// data type (LRM 6.5 / 7.x). Handle / wrapper types (pointer / vector / object
// / external ref / external unit object) and named events (LRM 15 -- carry
// their own subscribe mechanism) pass through unwrapped.
auto MaybeWrapObservable(UnitLowerer& unit_lowerer, mir::TypeId t)
    -> mir::TypeId {
  const auto& data = unit_lowerer.Unit().types.Get(t).data;
  const bool wrap = std::holds_alternative<mir::PackedArrayType>(data) ||
                    std::holds_alternative<mir::EnumType>(data) ||
                    std::holds_alternative<mir::StringType>(data) ||
                    std::holds_alternative<mir::RealType>(data) ||
                    std::holds_alternative<mir::ShortRealType>(data) ||
                    std::holds_alternative<mir::RealTimeType>(data) ||
                    std::holds_alternative<mir::UnpackedArrayType>(data) ||
                    std::holds_alternative<mir::DynamicArrayType>(data) ||
                    std::holds_alternative<mir::QueueType>(data) ||
                    std::holds_alternative<mir::AssociativeArrayType>(data) ||
                    std::holds_alternative<mir::TupleType>(data) ||
                    std::holds_alternative<mir::UnionType>(data);
  if (!wrap) {
    return t;
  }
  return unit_lowerer.Unit().types.Intern(mir::ObservableType{.value = t});
}

struct GenerateChildSpec {
  hir::StructuralScopeId scope_id;
  const hir::StructuralScope* scope;
  std::string scope_name;
};

// Each instantiated block is its own concrete scalar child (its own class),
// distinguished on the hierarchy only by any index it carries; the genvar is
// folded into the body, so there is no runtime structural-param binding.
auto EnumerateGenerateChildSpecs(
    const hir::Generate& gen, UnitLowerer& unit_lowerer)
    -> std::vector<GenerateChildSpec> {
  std::vector<GenerateChildSpec> specs;
  specs.reserve(gen.data.items.size());
  for (const auto& item : gen.data.items) {
    const auto& item_scope = gen.child_scopes.Get(item.scope);
    specs.push_back(
        {.scope_id = item.scope,
         .scope = &item_scope,
         .scope_name = unit_lowerer.NextGenerateScopeName("gen")});
  }
  return specs;
}

auto MakeUniqueObjectPointer(UnitLowerer& unit_lowerer, mir::ClassId class_id)
    -> mir::TypeId {
  const mir::TypeId object_type =
      unit_lowerer.Unit().types.Intern(mir::ObjectType{.class_id = class_id});
  return unit_lowerer.Unit().types.PointerTo(
      object_type, mir::PointerOwnership::kUnique);
}

auto MakeUniqueExternalUnitPointer(
    UnitLowerer& unit_lowerer, std::string unit_name) -> mir::TypeId {
  const mir::TypeId object_type = unit_lowerer.Unit().types.Intern(
      mir::ExternalUnitObjectType{.unit_name = std::move(unit_name)});
  return unit_lowerer.Unit().types.PointerTo(
      object_type, mir::PointerOwnership::kUnique);
}

auto MakeBorrowedExternalUnitPointer(
    UnitLowerer& unit_lowerer, std::string unit_name) -> mir::TypeId {
  const mir::TypeId object_type = unit_lowerer.Unit().types.Intern(
      mir::ExternalUnitObjectType{.unit_name = std::move(unit_name)});
  return unit_lowerer.Unit().types.PointerTo(
      object_type, mir::PointerOwnership::kBorrowed);
}

// Recursively emits the per-leaf construction stmts for an external-unit
// instance member with `dims` remaining at this level. At the innermost level
// the leaf is built (`make_unique<ExtUnit>`) and handed to the parent to own
// (`AddOwnedChild`); its Segment (label plus the accumulated indices) is its
// by-name lookup key. At an outer level the recursion enumerates the current
// dim, appending the index and recursing, so an N-dimensional member unrolls
// into independent construct-and-own leaves.
void EmitExternalUnitDimLevel(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, mir::ExprId parent_self,
    const std::string& runtime_label, mir::TypeId leaf_pointer_type,
    std::optional<mir::FieldId> companion, std::span<const std::uint32_t> dims,
    std::vector<mir::ExprId>& indices) {
  mir::Block& block = *frame.current_block;
  const auto& builtins = unit_lowerer.Unit().builtins;
  const auto int_lit = [&](std::int64_t v) -> mir::ExprId {
    return block.exprs.Add(mir::MakeIntLiteral(builtins.int_type, v));
  };
  const auto string_literal = [&](const std::string& s) -> mir::ExprId {
    return block.exprs.Add(
        mir::Expr{
            .data = mir::StringLiteral{.value = s}, .type = builtins.string});
  };

  if (dims.empty()) {
    // The child's structural identity, fixed before its constructor runs.
    // Indices reflect the position this leaf occupies in its enclosing
    // dim chain (empty for a scalar instance).
    const mir::TypeId indices_type = unit_lowerer.Unit().types.Intern(
        mir::UnpackedArrayType{
            .element_type = builtins.int_type,
            .dim = mir::UnpackedRange::ZeroBased(indices.size())});
    const mir::ExprId indices_id = block.exprs.Add(
        mir::Expr{
            .data = mir::ArrayLiteralExpr{.elements = indices},
            .type = indices_type});
    const mir::ExprId segment_id = block.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee = mir::Construct{},
                    .arguments = {string_literal(runtime_label), indices_id}},
            .type = builtins.hierarchy_segment});

    std::vector<mir::ExprId> ctor_args = {
        parent_self, segment_id,
        block.exprs.Add(BuildServicesCallExpr(unit_lowerer, frame))};
    const mir::ExprId ctor_call_id = block.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee = mir::Construct{},
                    .arguments = std::move(ctor_args)},
            .type = leaf_pointer_type});

    // The runtime tree owns the child (AddOwnedChild consumes the freshly built
    // unique pointer). A scalar instance also stores the returned borrowed
    // handle in its companion member -- the layout-visible head a cross-unit
    // reference projects; an array element keeps none and is reached by indexed
    // GetChild.
    const mir::ExprId add_id = block.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee =
                        mir::Direct{
                            .target = support::BuiltinFn::kAddOwnedChild},
                    .arguments = {parent_self, ctor_call_id}},
            .type = builtins.scope_ptr});
    if (!companion.has_value()) {
      block.AppendStmt(mir::ExprStmt{.expr = add_id});
      return;
    }
    const mir::TypeId companion_type =
        frame.current_class->fields.Get(*companion).type;
    const mir::ExprId typed_handle = block.exprs.Add(
        mir::Expr{
            .data = mir::PointerCastExpr{.operand = add_id},
            .type = companion_type});
    const mir::ExprId member = block.exprs.Add(
        mir::MakeFieldAccessExpr(
            parent_self,
            mir::FieldTarget{
                .owner = frame.current_class_id, .slot = *companion},
            companion_type));
    const mir::ExprId assign = block.exprs.Add(
        mir::Expr{
            .data = mir::AssignExpr{.target = member, .value = typed_handle},
            .type = companion_type});
    block.AppendStmt(mir::ExprStmt{.expr = assign});
    return;
  }

  const std::span<const std::uint32_t> remaining_dims = dims.subspan(1);
  const std::uint32_t count = dims.front();
  for (std::uint32_t i = 0; i < count; ++i) {
    indices.push_back(int_lit(static_cast<std::int64_t>(i)));
    EmitExternalUnitDimLevel(
        unit_lowerer, frame, parent_self, runtime_label, leaf_pointer_type,
        companion, remaining_dims, indices);
    indices.pop_back();
  }
}

// Lowers an external-unit instance member to the MIR call sequence that builds
// each instance and hands it to the parent to own. The construction lives in
// the parent's constructor block at depth zero; a multi-dimensional member
// unrolls into `dims[0] * dims[1] * ...` independent construct-and-own leaves,
// each carrying its per-dimension index in its own Segment.
void AppendExternalUnitConstruction(
    UnitLowerer& unit_lowerer, const WalkFrame& frame,
    const std::string& target_unit, const std::string& runtime_label,
    std::optional<mir::FieldId> companion,
    std::span<const std::uint32_t> dims) {
  mir::Block& block = *frame.current_block;
  const mir::TypeId self_ptr_type = frame.current_class->self_pointer_type;
  const mir::ExprId parent_self =
      block.exprs.Add(MakeSelfRefExpr(frame, self_ptr_type));
  const mir::TypeId leaf_pointer_type =
      MakeUniqueExternalUnitPointer(unit_lowerer, target_unit);
  std::vector<mir::ExprId> indices;
  EmitExternalUnitDimLevel(
      unit_lowerer, frame, parent_self, runtime_label, leaf_pointer_type,
      companion, dims, indices);
}

// Emits the constructor-body construction call for each instance member. The
// runtime tree owns every built instance; a scalar instance also keeps a
// borrowed typed companion (the layout-visible head of a cross-unit route),
// while an array element is reached by indexed GetChild.
void EmitInstanceMemberConstruction(
    StructuralScopeLowerer& lowerer, WalkFrame frame) {
  const hir::StructuralScope& hir_scope = lowerer.HirScope();
  std::uint32_t next_instance = 0;
  for (const auto& im : hir_scope.instance_members) {
    const hir::InstanceMemberId id{next_instance++};
    AppendExternalUnitConstruction(
        lowerer.Owner(), frame, im.target_unit, im.instance_name,
        lowerer.InstanceCompanion(id), im.array_dims);
  }
}

// Allocates one MIR member per cross-unit reference. Every reference -- upward
// or downward, `$root`-anchored or named -- takes the same borrowed-pointer
// slot: the pointee matches the producer's actual storage cell so a read or
// drive reaches the right access protocol. A net producer owns a resolved
// cell (`ResolvedType`); any other value-storage signal is wrapped in
// `ObservableType` at its declaration so a write fires subscribers. The
// cross-unit pointer must point at that same cell -- otherwise the C++
// types mismatch. The route that fills each slot runs in the resolve phase,
// after the whole object tree exists.
void DeclareRoutedRefSlots(
    StructuralScopeLowerer& lowerer, mir::ClassShape& shape) {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  const hir::StructuralScope& hir_scope = lowerer.HirScope();
  std::uint32_t slot_index = 0;
  for (const auto& cu : hir_scope.routed_refs) {
    std::string member_name = "ep" + std::to_string(slot_index++);
    if (cu.target_net_type.has_value()) {
      const bool is_upward =
          std::holds_alternative<hir::UpwardRootHead>(cu.recipe.head) ||
          std::holds_alternative<hir::UpwardNamedHead>(cu.recipe.head);
      if (is_upward) {
        throw InternalError(
            "DeclareRoutedRefSlots: an upward routed reference to a net is not "
            "yet supported");
      }
    }
    const mir::TypeId value = unit_lowerer.TranslateType(cu.recipe.type);
    const mir::TypeId leaf = cu.target_net_type.has_value()
                                 ? unit_lowerer.Unit().types.Intern(
                                       mir::ResolvedType{.value = value})
                                 : MaybeWrapObservable(unit_lowerer, value);
    const mir::TypeId slot_type = unit_lowerer.Unit().types.PointerTo(
        leaf, mir::PointerOwnership::kBorrowed);
    const mir::FieldId slot = shape.fields.Add(
        mir::FieldDecl{.name = std::move(member_name), .type = slot_type});
    lowerer.AddRoutedRefTarget(slot, slot_type);
  }
}

// Linear search for a member by its source name (or by the synthesized name
// if no source name was recorded). Per-class member arenas are small enough
// that O(N) at construction time is irrelevant. The within-class name is
// unique by the structural-scope lowering's allocation rule, so this is a
// deterministic lookup despite the linear scan.
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
    ids.push_back(block.exprs.Add(
        mir::MakeIntLiteral(
            builtins.int_type, static_cast<std::int64_t>(idx))));
  }
  const mir::TypeId indices_type = unit_lowerer.Unit().types.Intern(
      mir::UnpackedArrayType{
          .element_type = builtins.int_type,
          .dim = mir::UnpackedRange::ZeroBased(indices.size())});
  return block.exprs.Add(
      mir::Expr{
          .data = mir::ArrayLiteralExpr{.elements = std::move(ids)},
          .type = indices_type});
}

auto BuildStringLiteral(
    UnitLowerer& unit_lowerer, mir::Block& block, const std::string& s)
    -> mir::ExprId {
  return block.exprs.Add(
      mir::Expr{
          .data = mir::StringLiteral{.value = s},
          .type = unit_lowerer.Unit().builtins.string});
}

// Finds a class member by its SV-visible name (source_name, or the mangled
// name when none). Per-class member arenas are small; the linear scan runs at
// lowering time, never on the simulation path.
auto FindFieldByName(const mir::ClassShape& shape, std::string_view name)
    -> std::optional<mir::FieldId> {
  for (std::size_t i = 0; i < shape.fields.size(); ++i) {
    const mir::FieldId id{static_cast<std::uint32_t>(i)};
    const auto& m = shape.fields.Get(id);
    const std::string_view key = m.source_name.empty() ? m.name : m.source_name;
    if (key == name) {
      return id;
    }
  }
  return std::nullopt;
}

// A route runs from its origin (the referrer's `self`) to the referenced leaf.
// Its receiver at each step is either **typed** -- layout-visible, known to
// point at a class this artifact owns, so descent takes a typed
// `FieldAccessExpr` on the receiver's class shape -- or **opaque**, a runtime
// `Scope*` reached through the SDK by name (a segment crossing into another
// unit's body). An indexed hop on a layout-visible segment uses the SDK
// `GetChild(name, indices)` fallback but downcasts back to typed, since MIR
// carries no typed vector-index primitive.
struct RouteReceiver {
  mir::ExprId expr{};
  // When set, the receiver points at this class's instance. When null the
  // receiver is a runtime `Scope*` and every following step is opaque.
  const mir::ClassShape* shape = nullptr;
  // The class identity paired with `shape`, carried alongside so an
  // owner-qualified field access on the receiver names the arena without a
  // reverse lookup from `shape`.
  std::optional<mir::ClassId> class_id = std::nullopt;
};

// The target class of a member whose type is a pointer / unique-pointer to an
// intra-unit object, directly or through vector wrappers. Nullopt for pointees
// that leave this artifact (`ExternalUnitObjectType`).
auto ClassBehindOwnedChildMember(
    const mir::CompilationUnit& unit, mir::TypeId member_type)
    -> std::optional<mir::ClassId> {
  mir::TypeId leaf = member_type;
  while (const auto* vec =
             std::get_if<mir::VectorType>(&unit.types.Get(leaf).data)) {
    leaf = vec->element;
  }
  const auto* ptr = std::get_if<mir::PointerType>(&unit.types.Get(leaf).data);
  if (ptr == nullptr) return std::nullopt;
  const auto& pointee = unit.types.Get(ptr->pointee).data;
  if (const auto* obj = std::get_if<mir::ObjectType>(&pointee)) {
    return obj->class_id;
  }
  return std::nullopt;
}

// Reaches a layout-visible child by name+indices via the SDK `GetChild`
// (emission fallback for an indexed vector access) and re-tags the result to
// the target's typed class pointer via `PointerCastExpr`, so the route stays
// typed for the following segment.
auto SdkChildAsTyped(
    UnitLowerer& unit_lowerer, mir::Block& block, mir::ExprId receiver,
    const std::string& name, std::span<const std::uint32_t> indices,
    mir::ClassId target_class_id) -> RouteReceiver {
  auto& unit = unit_lowerer.Unit();
  const mir::TypeId scope_ptr_type = unit.builtins.scope_ptr;
  const mir::ExprId raw = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{.target = support::BuiltinFn::kGetChild},
                  .arguments =
                      {receiver, BuildStringLiteral(unit_lowerer, block, name),
                       BuildIndicesLiteral(unit_lowerer, block, indices)}},
          .type = scope_ptr_type});
  const mir::ClassShape& target_shape =
      unit_lowerer.GetClassShape(target_class_id);
  const mir::TypeId typed_ptr = unit.types.PointerTo(
      unit.types.Intern(mir::ObjectType{.class_id = target_class_id}),
      mir::PointerOwnership::kBorrowed);
  const mir::ExprId typed = block.exprs.Add(
      mir::Expr{
          .data = mir::PointerCastExpr{.operand = raw}, .type = typed_ptr});
  return RouteReceiver{
      .expr = typed, .shape = &target_shape, .class_id = target_class_id};
}

// Reaches a child by name+indices as an opaque `Scope*` -- the realization of
// an opaque segment (one crossing into another unit's body).
auto SdkChildOpaque(
    UnitLowerer& unit_lowerer, mir::Block& block, mir::ExprId receiver,
    const std::string& name, std::span<const std::uint32_t> indices)
    -> RouteReceiver {
  const mir::TypeId scope_ptr_type = unit_lowerer.Unit().builtins.scope_ptr;
  const mir::ExprId step = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{.target = support::BuiltinFn::kGetChild},
                  .arguments =
                      {receiver, BuildStringLiteral(unit_lowerer, block, name),
                       BuildIndicesLiteral(unit_lowerer, block, indices)}},
          .type = scope_ptr_type});
  return RouteReceiver{
      .expr = step, .shape = nullptr, .class_id = std::nullopt};
}

// Establishes the route's starting receiver from the head. A downward head
// reaches an owned child of an enclosing scope: a scalar layout-visible child
// by a typed member access through its companion (no string lookup); an indexed
// one by the SDK `GetChild(name, indices)` fallback downcast back to typed; a
// child whose class is another unit by the same by-name lookup, opaque from
// there. `$root` and the visible-child climb are opaque runtime-SDK reaches.
auto BuildRouteAnchor(
    StructuralScopeLowerer& lowerer, const WalkFrame& frame,
    const hir::RoutedRefHead& head) -> RouteReceiver {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  auto& unit = unit_lowerer.Unit();
  mir::Block& block = *frame.current_block;
  const mir::TypeId scope_ptr_type = unit.builtins.scope_ptr;

  // An enclosing head climbs `hops` typed parent edges to an ancestor scope in
  // this unit; the leaf is a typed member of that scope's class, reached by the
  // same by-name typed descent a sibling or child route uses.
  if (const auto* eh = std::get_if<hir::EnclosingHead>(&head)) {
    const mir::EnclosingHops hops{.value = eh->hops.value};
    const mir::ExprId enclosing_self =
        BuildEnclosingScopeReceiver(frame, unit, hops);
    return RouteReceiver{
        .expr = enclosing_self,
        .shape = &lowerer.EnclosingClassShapeAtHops(
            hir::StructuralHops{.value = eh->hops.value}),
        .class_id = lowerer.EnclosingClassIdAtHops(
            hir::StructuralHops{.value = eh->hops.value})};
  }

  if (const auto* dh = std::get_if<hir::DownwardHead>(&head)) {
    const mir::EnclosingHops hops{.value = dh->hops.value};
    const mir::ExprId enclosing_self =
        BuildEnclosingScopeReceiver(frame, unit, hops);
    const mir::Class& enclosing_cls = frame.EnclosingClassAtHops(hops);
    const OwnedChildAnchor anchor =
        lowerer.TranslateOwnedChild(dh->hops, dh->child);

    if (!dh->head_indices.empty()) {
      if (anchor.target.has_value()) {
        return SdkChildAsTyped(
            unit_lowerer, block, enclosing_self, anchor.label, dh->head_indices,
            *anchor.target);
      }
      return SdkChildOpaque(
          unit_lowerer, block, enclosing_self, anchor.label, dh->head_indices);
    }

    if (!anchor.companion.has_value()) {
      throw InternalError(
          "BuildRouteAnchor: scalar owned-child head has no companion member");
    }
    const mir::TypeId head_field_type =
        enclosing_cls.fields.Get(*anchor.companion).type;
    const mir::ClassId enclosing_cls_id = frame.EnclosingClassIdAtHops(hops);
    const mir::ExprId head_access = block.exprs.Add(
        mir::Expr{
            .data =
                mir::FieldAccessExpr{
                    .receiver = enclosing_self,
                    .field =
                        mir::FieldTarget{
                            .owner = enclosing_cls_id,
                            .slot = *anchor.companion}},
            .type = head_field_type});
    if (anchor.target.has_value()) {
      return RouteReceiver{
          .expr = head_access,
          .shape = &unit_lowerer.GetClassShape(*anchor.target),
          .class_id = *anchor.target};
    }
    return RouteReceiver{
        .expr = head_access, .shape = nullptr, .class_id = std::nullopt};
  }

  if (std::holds_alternative<hir::UpwardRootHead>(head)) {
    const mir::ExprId self_ref = block.exprs.Add(
        MakeSelfRefExpr(frame, frame.current_class->self_pointer_type));
    const mir::ExprId root = block.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee =
                        mir::Direct{.target = support::BuiltinFn::kResolveRoot},
                    .arguments = {self_ref}},
            .type = scope_ptr_type});
    return RouteReceiver{
        .expr = root, .shape = nullptr, .class_id = std::nullopt};
  }

  const auto& un = std::get<hir::UpwardNamedHead>(head);
  // The visible-child climb walks the parent chain by name (LRM 23.8).
  const mir::ExprId self_ref = block.exprs.Add(
      MakeSelfRefExpr(frame, frame.current_class->self_pointer_type));
  const mir::ExprId matched = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = support::BuiltinFn::kResolveVisibleChild},
                  .arguments =
                      {self_ref,
                       BuildStringLiteral(unit_lowerer, block, un.head_name),
                       BuildIndicesLiteral(
                           unit_lowerer, block, un.head_indices)}},
          .type = scope_ptr_type});
  return RouteReceiver{
      .expr = matched, .shape = nullptr, .class_id = std::nullopt};
}

// Descends one intermediate segment. A layout-visible receiver (typed shape)
// projects the segment as a typed member -- a `FieldAccessExpr` when scalar,
// the `GetChild` fallback downcast to typed when indexed. An opaque receiver,
// or a segment crossing into another unit, descends by-name and stays opaque.
auto AppendRouteSegment(
    StructuralScopeLowerer& lowerer, mir::Block& block,
    const RouteReceiver& receiver, const hir::PathSegment& segment)
    -> RouteReceiver {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  auto& unit = unit_lowerer.Unit();

  if (receiver.shape == nullptr) {
    return SdkChildOpaque(
        unit_lowerer, block, receiver.expr, segment.name, segment.indices);
  }

  const auto step_field = FindFieldByName(*receiver.shape, segment.name);
  if (!step_field.has_value()) {
    throw InternalError(
        "AppendRouteSegment: descent step '" + segment.name +
        "' not found in typed class shape");
  }
  const mir::TypeId step_type = receiver.shape->fields.Get(*step_field).type;

  if (!segment.indices.empty()) {
    const std::optional<mir::ClassId> target =
        ClassBehindOwnedChildMember(unit, step_type);
    if (target.has_value()) {
      return SdkChildAsTyped(
          unit_lowerer, block, receiver.expr, segment.name, segment.indices,
          *target);
    }
    return SdkChildOpaque(
        unit_lowerer, block, receiver.expr, segment.name, segment.indices);
  }

  if (!receiver.class_id.has_value()) {
    throw InternalError(
        "AppendRouteSegment: layout-visible receiver missing class identity");
  }
  const mir::ExprId step_access = block.exprs.Add(
      mir::Expr{
          .data =
              mir::FieldAccessExpr{
                  .receiver = receiver.expr,
                  .field =
                      mir::FieldTarget{
                          .owner = *receiver.class_id, .slot = *step_field}},
          .type = step_type});
  const auto* step_ptr =
      std::get_if<mir::PointerType>(&unit.types.Get(step_type).data);
  if (step_ptr == nullptr) {
    throw InternalError(
        "AppendRouteSegment: intermediate typed step is not a pointer");
  }
  const auto& pointee_data = unit.types.Get(step_ptr->pointee).data;
  if (const auto* obj = std::get_if<mir::ObjectType>(&pointee_data)) {
    return RouteReceiver{
        .expr = step_access,
        .shape = &unit_lowerer.GetClassShape(obj->class_id),
        .class_id = obj->class_id};
  }
  return RouteReceiver{
      .expr = step_access, .shape = nullptr, .class_id = std::nullopt};
}

// Materializes the leaf signal reach as the borrowed-pointer value the slot
// takes: `AddressOf` of the typed member access when the receiver is
// layout-visible, or a `CastExpr` of `GetSignal`'s `void*` when opaque.
auto MaterializeLeaf(
    StructuralScopeLowerer& lowerer, mir::Block& block,
    const RouteReceiver& receiver, const hir::PathSegment& leaf,
    mir::TypeId slot_type) -> mir::ExprId {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  auto& unit = unit_lowerer.Unit();

  if (!leaf.indices.empty()) {
    throw InternalError(
        "MaterializeLeaf: leaf signal step carries indices; array selection "
        "on a cross-unit-ref leaf belongs to an expression-level access, not "
        "the descent");
  }

  if (receiver.shape != nullptr) {
    const auto member = FindFieldByName(*receiver.shape, leaf.name);
    if (!member.has_value()) {
      throw InternalError(
          "MaterializeLeaf: leaf signal '" + leaf.name +
          "' not found in typed class shape");
    }
    if (!receiver.class_id.has_value()) {
      throw InternalError(
          "MaterializeLeaf: layout-visible receiver missing class identity");
    }
    const mir::TypeId member_type = receiver.shape->fields.Get(*member).type;
    const mir::ExprId access = block.exprs.Add(
        mir::Expr{
            .data =
                mir::FieldAccessExpr{
                    .receiver = receiver.expr,
                    .field =
                        mir::FieldTarget{
                            .owner = *receiver.class_id, .slot = *member}},
            .type = member_type});
    return block.exprs.Add(
        mir::Expr{
            .data = mir::AddressOfExpr{.operand = access}, .type = slot_type});
  }

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
                       BuildStringLiteral(unit_lowerer, block, leaf.name)}},
          .type = void_ptr_type});
  return block.exprs.Add(
      mir::Expr{
          .data = mir::PointerCastExpr{.operand = raw}, .type = slot_type});
}

// Composes the resolve-phase pointer value that fills a cross-unit
// reference slot: anchor from the head, walk the descent segments, and
// materialize the leaf. The result flows into the ordinary assignment the
// caller emits into the resolve block.
auto BuildRouteValue(
    StructuralScopeLowerer& lowerer, const WalkFrame& frame,
    const hir::RoutedPathRecipe& recipe, mir::TypeId slot_type) -> mir::ExprId {
  const std::vector<hir::PathSegment>& path = recipe.path;
  if (path.empty()) {
    throw InternalError("BuildRouteValue: route has no leaf signal");
  }
  mir::Block& block = *frame.current_block;
  RouteReceiver receiver = BuildRouteAnchor(lowerer, frame, recipe.head);
  for (std::size_t i = 0; i + 1 < path.size(); ++i) {
    receiver = AppendRouteSegment(lowerer, block, receiver, path[i]);
  }
  return MaterializeLeaf(lowerer, block, receiver, path.back(), slot_type);
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
  for (std::size_t ci = 0; ci < hir_scope.routed_refs.size(); ++ci) {
    const hir::RoutedRefId hir_id{static_cast<std::uint32_t>(ci)};
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
void AppendProcessRegistration(
    UnitLowerer& unit_lowerer, const WalkFrame& activate_frame,
    mir::MethodId body, bool is_final) {
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
                              mir::MethodTarget{
                                  .owner = activate_frame.current_class_id,
                                  .slot = body}},
                  .arguments = {body_self}},
          .type = unit_lowerer.Unit().builtins.coroutine_void});
  const mir::ExprId reg_self =
      block.exprs.Add(MakeSelfRefExpr(activate_frame, self_ptr_type));
  const mir::ExprId reg_call = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = is_final
                                        ? support::BuiltinFn::kRegisterFinal
                                        : support::BuiltinFn::kRegisterInitial},
                  .arguments = {reg_self, body_call}},
          .type = unit_lowerer.Unit().builtins.void_type});
  block.AppendStmt(mir::ExprStmt{.expr = reg_call});
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
  for (const auto& pc : hir_scope.port_connections) {
    if (pc.direction == hir::PortDirection::kRef) {
      // A `ref` port reaches the child's reference member by the same route
      // navigation a routed reference uses, then binds it to the peer's cell
      // through the one canonical reference-store primitive. It holds no
      // persistent slot -- a `ref` needs no simulation-time reach, so the
      // member is reached once here in the resolve phase (LRM 23.3.3.2).
      const auto& recipe = std::get<hir::RoutedPathRecipe>(pc.endpoint);
      if (!std::holds_alternative<hir::DownwardHead>(recipe.head)) {
        throw InternalError(
            "InstallPortConnections: a ref port reaches its child downward");
      }
      const mir::TypeId value_type = unit_lowerer.TranslateType(recipe.type);
      const mir::TypeId ref_type = unit_lowerer.Unit().types.Intern(
          mir::RefType{
              .pointee = value_type, .mutability = mir::Mutability::kMutable});
      const mir::TypeId slot_type = unit_lowerer.Unit().types.PointerTo(
          ref_type, mir::PointerOwnership::kBorrowed);
      const mir::ExprId nav =
          BuildRouteValue(lowerer, resolve_frame, recipe, slot_type);
      const mir::ExprId target = resolve_block.exprs.Add(
          mir::Expr{.data = mir::DerefExpr{.pointer = nav}, .type = ref_type});

      auto peer_or =
          lowerer.LowerLhsExpr(hir_scope.exprs.Get(pc.peer), resolve_frame);
      if (!peer_or) return std::unexpected(std::move(peer_or.error()));
      const mir::ExprId peer_cell =
          resolve_block.exprs.Add(*std::move(peer_or));

      const mir::ExprId bind = BindReferenceSlot(
          unit_lowerer.Unit(), resolve_block, target, peer_cell);
      resolve_block.AppendStmt(mir::ExprStmt{.expr = bind});
      continue;
    }
    const auto& cell = std::get<hir::PortCellEndpoint>(pc.endpoint);
    const bool is_input = pc.direction == hir::PortDirection::kInput;
    // A port connection is a reactive edge: the source is read, the sink is
    // driven. An input port's source is the parent expression and its sink is
    // the child cell; an output port's source is the child cell and its sink
    // is the parent target. The edge is the same continuous assignment either
    // way; the sink's own MIR type -- resolved-net cell or observable cell --
    // picks the write protocol (LRM 23.3.3).
    const hir::ContinuousAssign assign{
        .span = pc.span,
        .lhs = is_input ? cell.cell : pc.peer,
        .rhs = is_input ? pc.peer : cell.cell,
        .sensitivity_list = pc.sensitivity};
    std::string name = std::format("process_{}", mir_class.methods.size());
    // A port connection is single-driver by construction (LRM 23.3.3),
    // so no port pair here can duplicate another's target; a fresh dedup
    // set per iteration exists only to satisfy the lowering's API.
    ContinuousAssignDrivenNets driven_nets;
    auto method_or = LowerContinuousAssign(
        lowerer, frame, resolve_frame, init_frame, std::move(name), assign,
        driven_nets);
    if (!method_or) return std::unexpected(std::move(method_or.error()));
    const mir::MethodId body = mir_class.methods.Add(std::move(*method_or));
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
// `AddOwnedChild(self, make_unique<Child>(self, HierarchySegment{label,
// indices}, services, ctor_args...))`: the child instance is built carrying
// its complete hierarchy identity, then handed to the parent to own. The
// runtime tree owns the child; the parent keeps no member, and a later
// reference reaches it by name through GetChild. `runtime_label` is the
// SV-visible identifier; an anonymous scope gets an empty label, which the
// runtime treats as non-addressable so a peer by-name lookup walks past it to
// the addressable descendants underneath. `arm_frame` must point at the block
// where the stmts land and carry the constructor's bindings so a `self` read
// resolves to the receiver binding.
void AppendOwnedChildConstruction(
    UnitLowerer& unit_lowerer, const WalkFrame& arm_frame,
    const std::string& runtime_label, mir::ClassId child_scope_id,
    std::optional<mir::ExprId> array_index,
    std::optional<mir::FieldId> companion_field) {
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
  const mir::TypeId indices_type = unit_lowerer.Unit().types.Intern(
      mir::UnpackedArrayType{
          .element_type = builtins.int_type,
          .dim = mir::UnpackedRange::ZeroBased(index_elems.size())});
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
  ctor_call_args.reserve(3);
  ctor_call_args.push_back(self_read());
  ctor_call_args.push_back(segment_id);
  ctor_call_args.push_back(
      arm_block.exprs.Add(BuildServicesCallExpr(unit_lowerer, arm_frame)));
  const mir::ExprId ctor_call_id = arm_block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Construct{},
                  .arguments = std::move(ctor_call_args)},
          .type = child_ptr_type});

  // The runtime tree owns the child (AddOwnedChild consumes the freshly built
  // unique pointer); ownership transfers after the child's constructor commits,
  // so a thrown subobject ctor leaves no half-attached scope. When the parent
  // keeps a typed companion handle (a materialized procedural scope), the
  // returned borrowed pointer is downcast and stored in that member -- the
  // typed segment intra-unit access navigates through it; otherwise the
  // returned handle is unused.
  const mir::ExprId add_call_id = arm_block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{.target = support::BuiltinFn::kAddOwnedChild},
                  .arguments = {self_read(), ctor_call_id}},
          .type = builtins.scope_ptr});
  if (!companion_field.has_value()) {
    arm_block.AppendStmt(mir::ExprStmt{.expr = add_call_id});
    return;
  }
  const mir::TypeId companion_type =
      owner_class.fields.Get(*companion_field).type;
  const mir::ExprId typed_handle = arm_block.exprs.Add(
      mir::Expr{
          .data = mir::PointerCastExpr{.operand = add_call_id},
          .type = companion_type});
  const mir::ExprId member = arm_block.exprs.Add(
      mir::MakeFieldAccessExpr(
          self_read(),
          mir::FieldTarget{
              .owner = arm_frame.current_class_id, .slot = *companion_field},
          companion_type));
  const mir::ExprId assign = arm_block.exprs.Add(
      mir::Expr{
          .data = mir::AssignExpr{.target = member, .value = typed_handle},
          .type = companion_type});
  arm_block.AppendStmt(mir::ExprStmt{.expr = assign});
}

// The correctness baseline for every generate construct: construct each
// instantiated block's own concrete scalar child directly (no runtime branch or
// loop), each carrying any constant hierarchy index it has. The genvar is
// folded into each body, so no induction-variable argument is threaded.
auto LowerResolvedGenerate(
    StructuralScopeLowerer& lowerer, WalkFrame frame,
    const GenerateBindings& gen_bindings, const hir::ResolvedGenerate& resolved)
    -> diag::Result<mir::Stmt> {
  mir::Block& block = *frame.current_block;
  const mir::TypeId int_type = lowerer.Owner().Unit().builtins.int_type;

  mir::Block body;
  const WalkFrame body_frame = frame.WithBlock(&body);
  for (const auto& item : resolved.items) {
    const auto& binding = gen_bindings.by_scope_id.at(item.scope.value);
    std::optional<mir::ExprId> index_id;
    if (item.index.has_value()) {
      index_id = body.exprs.Add(mir::MakeIntLiteral(int_type, *item.index));
    }
    AppendOwnedChildConstruction(
        lowerer.Owner(), body_frame, binding.label, binding.scope_id, index_id,
        binding.companion);
  }
  const mir::BlockId body_id = block.child_scopes.Add(std::move(body));
  return mir::Stmt{
      .label = std::nullopt, .data = mir::BlockStmt{.scope = body_id}};
}

auto LowerGenerateAsStmt(
    StructuralScopeLowerer& lowerer, WalkFrame frame, const hir::Generate& gen,
    const GenerateBindings& gen_bindings) -> diag::Result<mir::Stmt> {
  return LowerResolvedGenerate(lowerer, frame, gen_bindings, gen.data);
}

}  // namespace

auto StructuralScopeLowerer::DeclareShape() -> diag::Result<mir::ClassId> {
  UnitLowerer& unit_lowerer = *owner_;
  const hir::StructuralScope& hir_scope = *hir_scope_;
  StructuralScopeLowerer& lowerer = *this;

  // The identity is minted before the shape is populated so the class's own
  // `self_pointer_type` can name it.
  class_id_ = unit_lowerer.Unit().DeclareClass();
  const mir::TypeId self_object_type =
      unit_lowerer.Unit().types.Intern(mir::ObjectType{.class_id = class_id_});
  const mir::TypeId self_pointer_type = unit_lowerer.Unit().types.PointerTo(
      self_object_type, mir::PointerOwnership::kBorrowed);

  mir::ClassShape shape;
  shape.name = name_;
  shape.base = mir::ClassRef{mir::ExternalClassRef{
      .qualified_name = parent_ == nullptr ? "lyra::runtime::Instance"
                                           : "lyra::runtime::GenScope"}};
  shape.is_scope_tree_node = true;
  shape.is_final = true;
  shape.self_pointer_type = self_pointer_type;
  shape.time_resolution = hir_scope.time_resolution;

  AttachRuntimeScopeCtorPrefix(unit_lowerer.Unit(), shape);
  for (const auto& alias : hir_scope.type_aliases) {
    shape.type_aliases.push_back(
        mir::TypeAliasDecl{
            .name = alias.name,
            .target = unit_lowerer.TranslateType(alias.target)});
  }

  for (std::size_t i = 0; i < hir_scope.structural_data_objects.size(); ++i) {
    const hir::StructuralDataObjectId hir_id{static_cast<std::uint32_t>(i)};
    const auto& d = hir_scope.structural_data_objects.Get(hir_id);
    const mir::TypeId mir_value_type = unit_lowerer.TranslateType(d.type);
    // A net (LRM 6.5) and a variable are peer kinds: a `ref` / `const ref`
    // port (LRM 23.3.3.2) member aliases the connected variable (a reference);
    // a net owns a resolved cell whose value is the resolution of its drivers;
    // any other variable is an observable cell so writes route through
    // `Var<T>::Set` and subscribers fire.
    const auto* var = std::get_if<hir::StructuralVariableDecl>(&d.kind);
    const bool is_reference = var != nullptr && var->reference.has_value();
    const mir::TypeId mir_field_type = [&] {
      if (is_reference) {
        return unit_lowerer.Unit().types.Intern(
            mir::RefType{
                .pointee = mir_value_type,
                .mutability =
                    *var->reference == hir::ReferenceBinding::kConstRef
                        ? mir::Mutability::kReadOnly
                        : mir::Mutability::kMutable});
      }
      if (var == nullptr) {
        return unit_lowerer.Unit().types.Intern(
            mir::ResolvedType{.value = mir_value_type});
      }
      return MaybeWrapObservable(unit_lowerer, mir_value_type);
    }();
    const mir::FieldId mir_id = shape.fields.Add(
        mir::FieldDecl{.name = d.name, .type = mir_field_type});
    lowerer.MapStructuralDataObject(hir_id, mir_id);
  }

  DeclareRoutedRefSlots(lowerer, shape);

  // Reserve each subroutine's MIR id before any body lowers, so a call in
  // one body resolves a forward or mutual reference to a peer (LRM 13.7).
  // The id reserved here is the index the decl will occupy when it is later
  // added to the class's methods arena.
  for (std::size_t i = 0; i < hir_scope.structural_subroutines.size(); ++i) {
    lowerer.MapStructuralSubroutine(
        hir::StructuralSubroutineId{static_cast<std::uint32_t>(i)},
        mir::MethodId{static_cast<std::uint32_t>(i)});
  }

  // Recursively declare every owned generate child's class shape; each child
  // lowerer is retained for the body sweep.
  for (std::size_t gen_idx = 0; gen_idx < hir_scope.generates.size();
       ++gen_idx) {
    const auto& gen = hir_scope.generates.Get(
        hir::GenerateId{static_cast<std::uint32_t>(gen_idx)});
    GenerateBindings gen_bindings;
    gen_bindings.by_scope_id.resize(gen.child_scopes.size());

    auto specs = EnumerateGenerateChildSpecs(gen, unit_lowerer);
    for (auto& spec : specs) {
      const std::string child_label = spec.scope->source_name;
      auto child = std::make_unique<StructuralScopeLowerer>(
          unit_lowerer, &lowerer, std::move(spec.scope_name), *spec.scope);
      auto child_r = child->DeclareShape();
      if (!child_r) return std::unexpected(std::move(child_r.error()));

      const mir::ClassId child_id = *child_r;
      shape.contained.push_back(child_id);
      // A generate-for element carries a hierarchy index (reached by indexed
      // GetChild, no companion); an if/case arm or bare block is scalar and
      // keeps a borrowed typed companion for the layout-visible head of a
      // cross-unit route.
      bool is_scalar = true;
      for (const auto& item : gen.data.items) {
        if (item.scope.value == spec.scope_id.value && item.index.has_value()) {
          is_scalar = false;
          break;
        }
      }
      std::optional<mir::FieldId> companion;
      if (is_scalar) {
        const mir::TypeId companion_type = unit_lowerer.Unit().types.PointerTo(
            unit_lowerer.Unit().types.Intern(
                mir::ObjectType{.class_id = child_id}),
            mir::PointerOwnership::kBorrowed);
        companion = shape.fields.Add(
            mir::FieldDecl{
                .name = std::format("{}_companion", child_label),
                .source_name = child_label,
                .type = companion_type});
      }
      gen_bindings.by_scope_id.at(
          spec.scope_id.value) = ChildStructuralScopeBinding{
          .scope_id = child_id, .label = child_label, .companion = companion};
      children_.push_back(std::move(child));
    }

    lowerer.MapGenerate(
        hir::GenerateId{static_cast<std::uint32_t>(gen_idx)},
        std::move(gen_bindings));
  }

  // Instance-member companions: a scalar instance keeps a borrowed typed handle
  // on this class -- the layout-visible head a cross-unit reference projects
  // (`self -> s -> ...`). An instance array keeps none; the route reaches an
  // element by indexed GetChild.
  {
    std::uint32_t next_instance = 0;
    for (const auto& im : hir_scope.instance_members) {
      const hir::InstanceMemberId id{next_instance++};
      if (!im.array_dims.empty()) {
        lowerer.MapInstanceCompanion(id, std::nullopt);
        continue;
      }
      const mir::TypeId companion_type =
          MakeBorrowedExternalUnitPointer(unit_lowerer, im.target_unit);
      const mir::FieldId companion = shape.fields.Add(
          mir::FieldDecl{
              .name = std::format("{}_companion", im.instance_name),
              .source_name = im.instance_name,
              .type = companion_type});
      lowerer.MapInstanceCompanion(id, companion);
    }
  }

  // Procedural-storage scope plan (LRM 9.3.5 / 23.9). Pass 1, post-order: a
  // procedural scope materializes as a runtime hierarchy node iff it is
  // runtime-addressable AND it directly owns a static or a descendant
  // materializes. A scope is addressable exactly when it carries an SV block
  // identifier, which is what a hierarchical reference names; a process root,
  // an unnamed block, and a foreach scope carry none.
  std::vector<bool> materializes(hir_scope.procedural_scopes.size(), false);
  const auto compute_mat = [&](const auto& self_ref, hir::ProceduralScopeId sid,
                               const hir::ProceduralBody& body) -> bool {
    const auto& decl = hir_scope.procedural_scopes.Get(sid);
    bool descendant_materializes = false;
    for (const auto child : decl.direct_child_scopes) {
      if (self_ref(self_ref, child, body)) descendant_materializes = true;
    }
    const bool addressable = decl.label.has_value();
    bool owns_static = false;
    for (const auto var_id : decl.direct_declarations) {
      if (body.procedural_vars.Get(var_id).lifetime ==
          hir::VariableLifetime::kStatic) {
        owns_static = true;
        break;
      }
    }
    const bool mat = addressable && (owns_static || descendant_materializes);
    materializes[sid.value] = mat;
    return mat;
  };
  for (std::size_t i = 0; i < hir_scope.structural_subroutines.size(); ++i) {
    const auto& s = hir_scope.structural_subroutines.Get(
        hir::StructuralSubroutineId{static_cast<std::uint32_t>(i)});
    compute_mat(compute_mat, s.body.root_scope, s.body);
  }
  for (std::size_t i = 0; i < hir_scope.processes.size(); ++i) {
    const auto& p =
        hir_scope.processes.Get(hir::ProcessId{static_cast<std::uint32_t>(i)});
    compute_mat(compute_mat, p.body.root_scope, p.body);
  }

  // A runtime class exists only for a materialized scope; a non-materialized
  // scope contributes nothing to the MIR class graph. A materialized scope is
  // always a named begin/end, so its runtime segment is its SV label.
  std::vector<mir::ClassId> scope_classes(hir_scope.procedural_scopes.size());
  std::vector<mir::ClassShape> sub_shapes(hir_scope.procedural_scopes.size());
  for (std::size_t i = 0; i < hir_scope.procedural_scopes.size(); ++i) {
    if (!materializes[i]) continue;
    const auto& decl = hir_scope.procedural_scopes.Get(
        hir::ProceduralScopeId{static_cast<std::uint32_t>(i)});
    const mir::ClassId class_id = unit_lowerer.Unit().DeclareClass();
    scope_classes[i] = class_id;
    mir::ClassShape sub_shape;
    sub_shape.name = std::format("{}__{}", name_, *decl.label);
    sub_shape.base = mir::ClassRef{mir::ExternalClassRef{
        .qualified_name = "lyra::runtime::ProceduralStorageScope"}};
    sub_shape.is_scope_tree_node = true;
    sub_shape.is_final = true;
    const mir::TypeId sub_self_object =
        unit_lowerer.Unit().types.Intern(mir::ObjectType{.class_id = class_id});
    sub_shape.self_pointer_type = unit_lowerer.Unit().types.PointerTo(
        sub_self_object, mir::PointerOwnership::kBorrowed);
    sub_shape.time_resolution = hir_scope.time_resolution;
    AttachRuntimeScopeCtorPrefix(unit_lowerer.Unit(), sub_shape);
    // Carry the enclosing structural scope's type aliases (typedefs declared
    // at module / generate level) so the cpp renderer can name those types
    // when emitting a static's slot inside this nested class. C++ enclosing-
    // class scope lookup makes the alias usable unqualified at the use site.
    sub_shape.type_aliases = shape.type_aliases;
    sub_shapes[i] = std::move(sub_shape);
  }

  scope_materialization_.Resize(hir_scope.procedural_scopes.size());
  std::vector<std::vector<std::optional<StaticStoragePlacement>>>
      subroutine_placements(hir_scope.structural_subroutines.size());
  std::vector<std::vector<std::optional<StaticStoragePlacement>>>
      process_placements(hir_scope.processes.size());

  const auto owner_shape_of = [&](StorageOwner owner) -> mir::ClassShape& {
    return std::visit(
        Overloaded{
            [&](EnclosingClass) -> mir::ClassShape& { return shape; },
            [&](hir::ProceduralScopeId sid) -> mir::ClassShape& {
              return sub_shapes[sid.value];
            }},
        owner);
  };

  // Pass 2, top-down: a materialized scope records its runtime node under its
  // nearest materialized ancestor and becomes that ancestor for its subtree. A
  // static's physical storage owner is the nearest materialized addressable
  // ancestor, or the structural class when none -- so a static lexically inside
  // an unnamed block flows through to the enclosing named scope or the module,
  // never onto a non-materialized scope.
  const auto place =
      [&](const auto& self_ref, const hir::ProceduralBody& body,
          std::string_view callable_name,
          std::vector<std::optional<StaticStoragePlacement>>& per_callable,
          hir::ProceduralScopeId scope_id, StorageOwner nearest_owner) -> void {
    const auto& scope = hir_scope.procedural_scopes.Get(scope_id);
    StorageOwner static_owner = nearest_owner;
    StorageOwner child_nearest = nearest_owner;
    if (materializes[scope_id.value]) {
      const mir::ClassId my_class = scope_classes[scope_id.value];
      mir::ClassShape& parent_shape = owner_shape_of(nearest_owner);
      parent_shape.contained.push_back(my_class);
      // The parent keeps a borrowed typed handle to the runtime-owned child:
      // the companion member the intra-unit typed segment navigates through. It
      // is not a source-visible signal (object pointer, no source_name), so the
      // static-signal registration excludes it.
      const mir::TypeId companion_type = unit_lowerer.Unit().types.PointerTo(
          unit_lowerer.Unit().types.Intern(
              mir::ObjectType{.class_id = my_class}),
          mir::PointerOwnership::kBorrowed);
      const mir::FieldId companion_id = parent_shape.fields.Add(
          mir::FieldDecl{
              .name = std::format("{}_companion", *scope.label),
              .source_name = *scope.label,
              .type = companion_type});
      scope_materialization_.Record(
          scope_id, MaterializedProceduralScope{
                        .materialized = true,
                        .class_id = my_class,
                        .companion_field = companion_id,
                        .label = *scope.label,
                        .runtime_parent = nearest_owner});
      static_owner = StorageOwner{scope_id};
      child_nearest = StorageOwner{scope_id};
    }

    if (per_callable.size() < body.procedural_vars.size()) {
      per_callable.resize(body.procedural_vars.size());
    }
    for (const auto var_id : scope.direct_declarations) {
      const auto& v = body.procedural_vars.Get(var_id);
      if (v.lifetime != hir::VariableLifetime::kStatic) continue;

      const std::string mangled =
          std::format("{}__{}_{}", callable_name, v.name, var_id.value);
      const mir::TypeId type = unit_lowerer.TranslateType(v.type);
      const mir::TypeId field_type = MaybeWrapObservable(unit_lowerer, type);
      mir::FieldDecl field_decl{.name = mangled, .type = field_type};
      // The source-name registration follows the var's lexical scope, not its
      // physical owner. Only a static in a named block is hierarchically
      // addressable; one in an unnamed scope is hidden (mangled name only).
      if (scope.label.has_value()) {
        field_decl.source_name = v.name;
      }
      const mir::FieldId mid =
          owner_shape_of(static_owner).fields.Add(field_decl);

      per_callable[var_id.value] =
          StaticStoragePlacement{.owner = static_owner, .field = mid};
    }

    for (const auto child_id : scope.direct_child_scopes) {
      self_ref(
          self_ref, body, callable_name, per_callable, child_id, child_nearest);
    }
  };

  for (std::size_t i = 0; i < hir_scope.structural_subroutines.size(); ++i) {
    const auto& s = hir_scope.structural_subroutines.Get(
        hir::StructuralSubroutineId{static_cast<std::uint32_t>(i)});
    place(
        place, s.body, s.name, subroutine_placements[i], s.body.root_scope,
        StorageOwner{EnclosingClass{}});
  }
  for (std::size_t i = 0; i < hir_scope.processes.size(); ++i) {
    const auto& p =
        hir_scope.processes.Get(hir::ProcessId{static_cast<std::uint32_t>(i)});
    const std::string callable_name =
        std::format("process_{}", hir_scope.structural_subroutines.size() + i);
    place(
        place, p.body, callable_name, process_placements[i], p.body.root_scope,
        StorageOwner{EnclosingClass{}});
  }

  subroutine_storage_plans_.reserve(hir_scope.structural_subroutines.size());
  for (auto& placements : subroutine_placements) {
    subroutine_storage_plans_.emplace_back(
        scope_materialization_, std::move(placements));
  }
  process_storage_plans_.reserve(hir_scope.processes.size());
  for (auto& placements : process_placements) {
    process_storage_plans_.emplace_back(
        scope_materialization_, std::move(placements));
  }

  // Publish each procedural-storage scope's shape; the body sweep below
  // builds each class's constructor against the published shape.
  for (std::size_t i = 0; i < hir_scope.procedural_scopes.size(); ++i) {
    if (!materializes[i]) continue;
    unit_lowerer.DefineClassShape(scope_classes[i], std::move(sub_shapes[i]));
  }

  unit_lowerer.DefineClassShape(class_id_, std::move(shape));
  return class_id_;
}

// Builds a runtime scope's generated-behavior record as an ordinary constructed
// value and installs it on `cls`: a per-phase ABI adapter that downcasts the
// generic scope receiver to `cls` and forwards to the phase body (empty when
// the phase has none), wrapped in a ScopeProgram, plus -- for a unit instance
// -- a UnitDefinition adding the construct entry. A class that is not a runtime
// tree node gets none.
auto InstallGeneratedDefinition(
    mir::CompilationUnit& unit, mir::Class& cls, mir::ClassId cls_id,
    bool is_unit, mir::CallableCode& ctor_code,
    std::optional<mir::MethodId> resolve_body,
    std::optional<mir::MethodId> init_body,
    std::optional<mir::MethodId> create_body) -> std::vector<mir::ExprId> {
  if (!cls.is_scope_tree_node) {
    return {};
  }
  const mir::TypeId scope_ptr = unit.builtins.scope_ptr;
  const mir::TypeId self_ptr = cls.self_pointer_type;
  const mir::TypeId void_type = unit.builtins.void_type;
  const auto make_adapter =
      [&](std::string name,
          std::optional<mir::MethodId> body) -> mir::AbiAdapterId {
    mir::CallableCode code;
    const mir::LocalId self =
        code.locals.Add(mir::LocalDecl{.name = "self", .type = scope_ptr});
    code.params = {self};
    code.result_type = void_type;
    if (body.has_value()) {
      const mir::ExprId self_ref = code.body.exprs.Add(
          mir::Expr{.data = mir::LocalRef{.var = self}, .type = scope_ptr});
      const mir::ExprId typed = code.body.exprs.Add(
          mir::Expr{
              .data = mir::PointerCastExpr{.operand = self_ref},
              .type = self_ptr});
      const mir::ExprId call = code.body.exprs.Add(
          mir::Expr{
              .data =
                  mir::CallExpr{
                      .callee =
                          mir::Direct{
                              .target =
                                  mir::MethodTarget{
                                      .owner = cls_id, .slot = *body}},
                      .arguments = {typed}},
              .type = void_type});
      code.body.AppendStmt(mir::ExprStmt{.expr = call});
    }
    return cls.abi_adapters.Add(
        mir::AbiAdapter{.name = std::move(name), .code = std::move(code)});
  };
  const mir::AbiAdapterId resolve_abi =
      make_adapter("ResolveStateAbi", resolve_body);
  const mir::AbiAdapterId init_abi =
      make_adapter("InitializeStateAbi", init_body);
  const mir::AbiAdapterId create_abi =
      make_adapter("CreateProcessesAbi", create_body);

  mir::StaticConstantDecl def;
  def.name = "kDefinition";
  auto& ex = def.body.exprs;
  const auto intern = [&](mir::RuntimeLibraryKind kind) {
    return unit.types.Intern(mir::RuntimeLibraryType{.kind = kind});
  };
  const auto construct = [&](mir::RuntimeLibraryKind kind,
                             std::vector<mir::ExprId> args) {
    return ex.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee = mir::Construct{}, .arguments = std::move(args)},
            .type = intern(kind)});
  };
  const auto machine_int = [&](std::int64_t value) {
    return ex.Add(
        mir::Expr{
            .data = mir::MachineIntLiteral{.value = value},
            .type = unit.builtins.machine_int64});
  };
  const auto func_ref = [&](mir::AbiAdapterId a) {
    return ex.Add(
        mir::Expr{
            .data = mir::FunctionRef{.adapter = a},
            .type = intern(mir::RuntimeLibraryKind::kScopeEntry)});
  };

  const std::string def_name = is_unit ? cls.name : std::string{};
  const mir::ExprId name_lit = ex.Add(
      mir::Expr{
          .data = mir::StringLiteral{.value = def_name},
          .type = unit.builtins.string});
  const mir::ExprId def_name_ref = construct(
      mir::RuntimeLibraryKind::kAbiStringRef,
      {name_lit, machine_int(static_cast<std::int64_t>(def_name.size()))});
  const mir::ExprId metadata = construct(
      mir::RuntimeLibraryKind::kScopeMetadata,
      {def_name_ref, machine_int(cls.time_resolution.precision_power)});
  const mir::ExprId program = construct(
      mir::RuntimeLibraryKind::kScopeProgram,
      {metadata, func_ref(resolve_abi), func_ref(init_abi),
       func_ref(create_abi)});
  if (is_unit) {
    const mir::AbiAdapterId construct_abi =
        make_adapter("ConstructAbi", std::nullopt);
    def.value = construct(
        mir::RuntimeLibraryKind::kUnitDefinition,
        {program, func_ref(construct_abi)});
  } else {
    def.value = program;
  }
  def.type = ex.Get(def.value).type;
  const mir::TypeId const_type = def.type;
  const mir::StaticConstantId def_id = cls.static_constants.Add(std::move(def));

  // The constructor hands the base the address of the constant just installed.
  auto& cex = ctor_code.body.exprs;
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
      const mir::ExprId local_ref = ctor_code.body.exprs.Add(
          mir::Expr{.data = mir::LocalRef{.var = id}, .type = ty});
      if (unit.types.Get(ty).IsAliasHandle()) {
        base_args.push_back(local_ref);
      } else {
        base_args.push_back(ctor_code.body.exprs.Add(
            mir::Expr{
                .data = mir::MoveExpr{.operand = local_ref}, .type = ty}));
      }
    }
    for (const mir::ExprId e : base_trailing_args) {
      base_args.push_back(e);
    }
    base_init_opt = mir::BaseInit{.args = std::move(base_args)};
  }
  const mir::MethodId ctor_method_id = cls.methods.Add(
      mir::MethodDecl{
          .name = "<ctor>",
          .code = std::move(ctor_code),
          .virtual_dispatch = std::nullopt,
          .visibility = mir::MethodVisibility::kInternal});
  cls.constructor = mir::ConstructorDecl{
      .method = ctor_method_id,
      .base_init = std::move(base_init_opt),
      .member_inits = {}};
}

auto StructuralScopeLowerer::PopulateBodies(WalkFrame parent_frame)
    -> diag::Result<void> {
  UnitLowerer& unit_lowerer = *owner_;
  const hir::StructuralScope& hir_scope = *hir_scope_;
  StructuralScopeLowerer& lowerer = *this;

  const mir::ClassShape& shape = unit_lowerer.GetClassShape(class_id_);
  mir::Class mir_class;
  mir_class.name = shape.name;
  mir_class.base = shape.base;
  mir_class.is_scope_tree_node = shape.is_scope_tree_node;
  mir_class.is_final = shape.is_final;
  mir_class.self_pointer_type = shape.self_pointer_type;
  mir_class.time_resolution = shape.time_resolution;
  mir_class.fields = shape.fields;
  mir_class.contained = shape.contained;
  mir_class.type_aliases = shape.type_aliases;

  // A DPI-C import declares no body, so it never enters the subroutine body
  // machinery below. Its ABI projection and foreign name were resolved once at
  // AST-to-HIR; here they translate straight into the class's static-callable
  // namespace, the external-symbol callable the import call lowers against.
  for (std::size_t i = 0; i < hir_scope.foreign_imports.size(); ++i) {
    const hir::ForeignImportDecl& imp = hir_scope.foreign_imports.Get(
        hir::ForeignImportId{static_cast<std::uint32_t>(i)});
    std::vector<mir::ForeignParam> params;
    params.reserve(imp.params.size());
    for (const hir::DpiParamAbi& p : imp.params) {
      params.push_back(
          mir::ForeignParam{
              .sv_type = unit_lowerer.TranslateType(p.sv_type),
              .carrier = p.carrier,
              .direction = p.direction});
    }
    mir_class.static_callables.Add(
        mir::StaticCallableDecl{
            .name = imp.name,
            .params = std::move(params),
            .ret_sv_type = unit_lowerer.TranslateType(imp.ret_sv_type),
            .ret_abi = imp.ret_abi,
            .external = mir::ExternalSymbol{
                .foreign_name = imp.foreign_name, .is_pure = imp.is_pure}});
  }

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
  mir::CallableCode ctor_code;
  CallableBindings ctor_bindings(unit_lowerer.Unit(), ctor_code);
  const mir::LocalId self_id = seed_self(ctor_bindings);
  // Each prefix param the base contract demands lands as an ordinary local
  // after `self`, so a base call reads it as a plain LocalRef and the ctor
  // signature exposes it as a regular parameter.
  std::vector<mir::LocalId> ctor_prefix_local_ids;
  ctor_prefix_local_ids.reserve(shape.ctor_prefix_params.size());
  for (std::size_t i = 0; i < shape.ctor_prefix_params.size(); ++i) {
    const auto& p = shape.ctor_prefix_params.Get(
        mir::ParamId{static_cast<std::uint32_t>(i)});
    ctor_prefix_local_ids.push_back(ctor_bindings.DeclareAnonymous(
        mir::LocalDecl{.name = p.name, .type = p.type}));
  }
  mir::Block& ctor_block = ctor_code.body;
  const WalkFrame ctor_frame =
      parent_frame.WithClass(&mir_class, class_id_, outer_scope_link)
          .WithBlock(&ctor_block)
          .WithBindings(&ctor_bindings);

  mir::CallableCode initialize_code;
  CallableBindings init_bindings(unit_lowerer.Unit(), initialize_code);
  const mir::LocalId init_self_id = seed_self(init_bindings);
  mir::Block& initialize_block = initialize_code.body;
  const WalkFrame init_frame =
      parent_frame.WithClass(&mir_class, class_id_, outer_scope_link)
          .WithBlock(&initialize_block)
          .WithBindings(&init_bindings);

  mir::CallableCode resolve_code;
  CallableBindings resolve_bindings(unit_lowerer.Unit(), resolve_code);
  const mir::LocalId resolve_self_id = seed_self(resolve_bindings);
  mir::Block& resolve_block = resolve_code.body;
  const WalkFrame resolve_frame =
      parent_frame.WithClass(&mir_class, class_id_, outer_scope_link)
          .WithBlock(&resolve_block)
          .WithBindings(&resolve_bindings);

  mir::CallableCode activate_code;
  CallableBindings activate_bindings(unit_lowerer.Unit(), activate_code);
  const mir::LocalId activate_self_id = seed_self(activate_bindings);
  mir::Block& activate_block = activate_code.body;
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

  for (std::size_t i = 0; i < hir_scope.structural_data_objects.size(); ++i) {
    const hir::StructuralDataObjectId hir_id{static_cast<std::uint32_t>(i)};
    const auto& d = hir_scope.structural_data_objects.Get(hir_id);
    const mir::FieldId mir_id =
        lowerer.TranslateStructuralDataObject(hir::StructuralHops{0}, hir_id);
    const mir::TypeId mir_field_type = mir_class.fields.Get(mir_id).type;
    const mir::TypeId mir_value_type = unit_lowerer.TranslateType(d.type);
    const auto* var = std::get_if<hir::StructuralVariableDecl>(&d.kind);
    const bool is_net = var == nullptr;
    const bool is_reference = var != nullptr && var->reference.has_value();
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
        !is_reference && !is_net && var_kind != mir::TypeKind::kPointer &&
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
      // An observable cell init routes the value through the cell's wrapper so
      // its engine-side change-tracking sees it; a plain field gets a regular
      // assignment.
      const auto emit_value_store = [&](mir::ExprId value_id) {
        const mir::ExprId services_id = initialize_block.exprs.Add(
            mir::MakeServicesCallExpr(
                init_self_read(), unit_lowerer.Unit().builtins.services));
        append_stmt(BuildObservableAssignExpr(
            unit_lowerer.Unit(), initialize_block, services_id, init_target,
            value_id, std::nullopt, mir_value_type,
            unit_lowerer.Unit().builtins.void_type));
      };

      // Every observable value cell installs its declared representation and
      // default at construction (LRM 10.5), so its type is fixed by
      // construction and a later store -- including a user initializer -- is
      // verified against it rather than discovered from whichever store runs
      // first. A non-observable value member carries no cell wrapper, so it
      // installs its representation through an ordinary store of the default.
      if (mir::IsObservableCellType(
              unit_lowerer.Unit().types.Get(mir_field_type))) {
        const mir::ExprId prototype = initialize_block.exprs.Add(
            BuildDefaultValueFromHir(unit_lowerer, init_frame, d.type));
        append_stmt(
            mir::MakeObservableInitializeCallExpr(
                init_target, prototype,
                unit_lowerer.Unit().builtins.void_type));
        if (var->initializer.has_value()) {
          auto value_or = lowerer.LowerExpr(
              hir_scope.exprs.Get(*var->initializer), init_frame);
          if (!value_or) return std::unexpected(std::move(value_or.error()));
          emit_value_store(initialize_block.exprs.Add(*std::move(value_or)));
        }
      } else {
        mir::ExprId value_id{};
        if (var->initializer.has_value()) {
          auto value_or = lowerer.LowerExpr(
              hir_scope.exprs.Get(*var->initializer), init_frame);
          if (!value_or) return std::unexpected(std::move(value_or.error()));
          value_id = initialize_block.exprs.Add(*std::move(value_or));
        } else {
          value_id = initialize_block.exprs.Add(
              BuildDefaultValueFromHir(unit_lowerer, init_frame, d.type));
        }
        emit_value_store(value_id);
      }
    }

    // A net cell installs its empty-driver resolved value at construction (LRM
    // 6.6.1), in the constructor rather than the initialize phase: a net is a
    // readable, well-typed observable before any driver attaches, and before a
    // cross-unit reader seeds from it during the parent-first initialize phase,
    // so an undriven read sees the net type's undriven value, never an
    // uninitialized cell. Drivers, attached at Resolve, update it from there.
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
                      mir::MakeObservableInitializeCallExpr(
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

  // Build each materialized procedural-storage scope's class: copy its
  // already-published shape into a `mir::Class`, build a constructor whose
  // body constructs the immediate-child materialized scopes the same way an
  // owned-child instance is constructed, and commit. This runs before any
  // body lowers so static-init lowering against the structural constructor
  // (which writes `self->companion->...->static = init`) reaches storage the
  // runtime has already built. The structural constructor then appends
  // construction calls for top-level materialized scopes below, before the
  // subroutine and process bodies that initialize their statics.
  for (std::size_t i = 0; i < scope_materialization_.Size(); ++i) {
    const hir::ProceduralScopeId scope_id{static_cast<std::uint32_t>(i)};
    const auto& entry = scope_materialization_.Get(scope_id);
    if (!entry.materialized) continue;
    const mir::ClassShape& sub_shape =
        unit_lowerer.GetClassShape(entry.class_id);
    mir::Class sub_class;
    sub_class.name = sub_shape.name;
    sub_class.base = sub_shape.base;
    sub_class.is_scope_tree_node = sub_shape.is_scope_tree_node;
    sub_class.is_final = sub_shape.is_final;
    sub_class.self_pointer_type = sub_shape.self_pointer_type;
    sub_class.time_resolution = sub_shape.time_resolution;
    sub_class.fields = sub_shape.fields;
    sub_class.contained = sub_shape.contained;
    sub_class.type_aliases = sub_shape.type_aliases;

    mir::CallableCode sub_ctor_code;
    CallableBindings sub_ctor_bindings(unit_lowerer.Unit(), sub_ctor_code);
    const mir::LocalId sub_self_id = sub_ctor_bindings.Declare(
        BindingOriginId::Receiver(),
        mir::LocalDecl{.name = "self", .type = sub_shape.self_pointer_type});
    std::vector<mir::LocalId> sub_ctor_prefix_local_ids;
    sub_ctor_prefix_local_ids.reserve(sub_shape.ctor_prefix_params.size());
    for (std::size_t j = 0; j < sub_shape.ctor_prefix_params.size(); ++j) {
      const auto& p = sub_shape.ctor_prefix_params.Get(
          mir::ParamId{static_cast<std::uint32_t>(j)});
      sub_ctor_prefix_local_ids.push_back(sub_ctor_bindings.DeclareAnonymous(
          mir::LocalDecl{.name = p.name, .type = p.type}));
    }
    mir::Block& sub_ctor_block = sub_ctor_code.body;
    ScopeChainNode sub_scope_link{};
    const WalkFrame sub_ctor_frame =
        parent_frame.WithClass(&sub_class, entry.class_id, sub_scope_link)
            .WithBlock(&sub_ctor_block)
            .WithBindings(&sub_ctor_bindings);
    for (std::size_t j = 0; j < scope_materialization_.Size(); ++j) {
      const hir::ProceduralScopeId child_id{static_cast<std::uint32_t>(j)};
      const auto& child_entry = scope_materialization_.Get(child_id);
      const auto* parent_scope =
          std::get_if<hir::ProceduralScopeId>(&child_entry.runtime_parent);
      if (parent_scope == nullptr || parent_scope->value != i) continue;
      AppendOwnedChildConstruction(
          unit_lowerer, sub_ctor_frame, child_entry.label, child_entry.class_id,
          std::nullopt, child_entry.companion_field);
    }
    // Register each static on this procedural-storage scope as a by-name
    // signal so a descent (`Top.outer.x`, whether intra- or cross-unit) reaches
    // it through `GetSignal` once it has navigated to this scope by `GetChild`.
    // Only plain-value cells are signals; pointer / vector / object members
    // (a cross-unit reference slot, for one) are excluded.
    for (std::size_t mi = 0; mi < sub_class.fields.size(); ++mi) {
      const mir::FieldId mid{static_cast<std::uint32_t>(mi)};
      const auto& m = sub_class.fields.Get(mid);
      if (m.source_name.empty()) continue;
      const mir::TypeKind mk = unit_lowerer.Unit().types.Get(m.type).Kind();
      if (mk == mir::TypeKind::kPointer || mk == mir::TypeKind::kVector ||
          mk == mir::TypeKind::kObject) {
        continue;
      }
      const mir::ExprId sub_self = sub_ctor_block.exprs.Add(
          MakeSelfRefExpr(sub_ctor_frame, sub_shape.self_pointer_type));
      const mir::ExprId member_ref = sub_ctor_block.exprs.Add(
          mir::MakeFieldAccessExpr(
              sub_self, mir::FieldTarget{.owner = entry.class_id, .slot = mid},
              m.type));
      const mir::TypeId addr_type = unit_lowerer.Unit().types.PointerTo(
          m.type, mir::PointerOwnership::kBorrowed);
      const mir::ExprId addr = sub_ctor_block.exprs.Add(
          mir::MakeAddressOfExpr(member_ref, addr_type));
      const mir::ExprId name_lit = sub_ctor_block.exprs.Add(
          mir::Expr{
              .data = mir::StringLiteral{.value = m.source_name},
              .type = unit_lowerer.Unit().builtins.string});
      const mir::ExprId reg_self = sub_ctor_block.exprs.Add(
          MakeSelfRefExpr(sub_ctor_frame, sub_shape.self_pointer_type));
      const mir::ExprId call = sub_ctor_block.exprs.Add(
          mir::Expr{
              .data =
                  mir::CallExpr{
                      .callee =
                          mir::Direct{
                              .target = support::BuiltinFn::kRegisterSignal},
                      .arguments = {reg_self, name_lit, addr}},
              .type = void_type});
      sub_ctor_block.AppendStmt(mir::ExprStmt{.expr = call});
    }
    sub_ctor_code.params.clear();
    sub_ctor_code.params.reserve(1 + sub_ctor_prefix_local_ids.size());
    sub_ctor_code.params.push_back(sub_self_id);
    for (const mir::LocalId id : sub_ctor_prefix_local_ids) {
      sub_ctor_code.params.push_back(id);
    }
    sub_ctor_code.result_type = void_type;
    // Ctor code stays local so subsequent lowering can still append exprs
    // into its body; once complete, it is moved into the class's method
    // storage and referenced by the construction protocol.
    const bool sub_is_unit = false;
    const std::vector<mir::ExprId> sub_base_trailing_args =
        InstallGeneratedDefinition(
            unit_lowerer.Unit(), sub_class, entry.class_id, sub_is_unit,
            sub_ctor_code, std::nullopt, std::nullopt, std::nullopt);
    FinalizeConstructor(
        unit_lowerer.Unit(), sub_class, std::move(sub_ctor_code),
        sub_ctor_prefix_local_ids, sub_base_trailing_args);
    unit_lowerer.Unit().DefineClass(entry.class_id, std::move(sub_class));
  }

  // Construct every top-level materialized procedural-storage scope in the
  // structural constructor, before the subroutine and process bodies lower so
  // static inits that target storage inside one of these scopes reach it
  // through the already-built runtime child (by-name descent from `self`).
  for (std::size_t i = 0; i < scope_materialization_.Size(); ++i) {
    const hir::ProceduralScopeId scope_id{static_cast<std::uint32_t>(i)};
    const auto& entry = scope_materialization_.Get(scope_id);
    if (!entry.materialized) continue;
    if (!std::holds_alternative<EnclosingClass>(entry.runtime_parent)) continue;
    AppendOwnedChildConstruction(
        unit_lowerer, ctor_frame, entry.label, entry.class_id, std::nullopt,
        entry.companion_field);
  }

  // An exported subroutine (LRM 35.5) is reached from foreign C through its
  // wrapper, a free function outside the class, so it joins the class's
  // externally callable surface rather than its internal one.
  std::unordered_set<std::string_view> exported_names;
  for (const hir::ForeignExportDecl& e : hir_scope.foreign_exports) {
    exported_names.insert(e.sv_name);
  }

  for (std::size_t i = 0; i < hir_scope.structural_subroutines.size(); ++i) {
    const auto& src = hir_scope.structural_subroutines.Get(
        hir::StructuralSubroutineId{static_cast<std::uint32_t>(i)});
    ProcessLowerer subroutine_lowerer(
        unit_lowerer, &lowerer, hir_scope.time_resolution, src.body, src.name,
        exported_names.contains(src.name) ? mir::MethodVisibility::kPublic
                                          : mir::MethodVisibility::kInternal,
        ctor_frame, subroutine_storage_plans_[i]);
    auto decl_or = subroutine_lowerer.Run(src);
    if (!decl_or) return std::unexpected(std::move(decl_or.error()));
    const mir::MethodId added = mir_class.methods.Add(*std::move(decl_or));
    if (added.value != i) {
      throw InternalError(
          "StructuralScopeLowerer::PopulateBodies: subroutine added out of "
          "mapped id order");
    }
    for (const auto& pending :
         subroutine_lowerer.TakePendingStaticInitializers()) {
      auto integ = IntegratePendingStaticInitializer(
          subroutine_lowerer, src.body, init_frame, pending);
      if (!integ) return std::unexpected(std::move(integ.error()));
    }
  }

  // Each exported subroutine (LRM 35.5) already lowered as a method above, in
  // structural-subroutine id order, so its method id is its index. The wrapper
  // is synthesized to call that method on the instance recovered from the
  // running design, named here by the class's own instance identity.
  for (const hir::ForeignExportDecl& export_decl : hir_scope.foreign_exports) {
    std::optional<mir::MethodId> method_id;
    for (std::size_t i = 0; i < hir_scope.structural_subroutines.size(); ++i) {
      if (hir_scope.structural_subroutines
              .Get(hir::StructuralSubroutineId{static_cast<std::uint32_t>(i)})
              .name == export_decl.sv_name) {
        method_id = mir::MethodId{static_cast<std::uint32_t>(i)};
        break;
      }
    }
    if (!method_id.has_value()) {
      throw InternalError(
          "StructuralScopeLowerer::PopulateBodies: exported subroutine has no "
          "lowered method");
    }
    mir_class.foreign_export_wrappers.push_back(SynthesizeForeignExportWrapper(
        unit_lowerer, ctor_frame, class_id_, *method_id, export_decl,
        mir_class.name));
  }

  for (std::size_t i = 0; i < hir_scope.processes.size(); ++i) {
    const auto& p =
        hir_scope.processes.Get(hir::ProcessId{static_cast<std::uint32_t>(i)});
    std::string name = std::format("process_{}", mir_class.methods.size());
    ProcessLowerer process_lowerer(
        unit_lowerer, &lowerer, hir_scope.time_resolution, p.body,
        std::move(name), mir::MethodVisibility::kInternal, ctor_frame,
        process_storage_plans_[i]);
    auto decl_or = process_lowerer.Run(p);
    if (!decl_or) return std::unexpected(std::move(decl_or.error()));
    const mir::MethodId body = mir_class.methods.Add(*std::move(decl_or));
    AppendProcessRegistration(
        unit_lowerer, activate_frame, body, p.kind == hir::ProcessKind::kFinal);
    for (const auto& pending :
         process_lowerer.TakePendingStaticInitializers()) {
      auto integ = IntegratePendingStaticInitializer(
          process_lowerer, p.body, init_frame, pending);
      if (!integ) return std::unexpected(std::move(integ.error()));
    }
  }

  // Fill every routed-reference endpoint slot first in the resolve phase, so a
  // later resolve-phase consumer that reaches a target through a sealed
  // endpoint -- a continuous-assign driver attached to an enclosing or
  // cross-unit net, a port-cell connection -- dereferences a slot that is
  // already bound.
  InstallRoutedRefs(lowerer, resolve_frame);

  // Scope-local multi-driver dedup. Two continuous assigns in this scope
  // whose LHS reaches the same resolved-net target address the same net;
  // the lowering registers each net target into `driven_nets` and rejects
  // a duplicate. Design-global driver-count validation is a Seal-barrier
  // concern; this set catches only the trivially detectable same-scope
  // case.
  ContinuousAssignDrivenNets driven_nets;
  for (const auto& ca : hir_scope.continuous_assigns) {
    std::string name = std::format("process_{}", mir_class.methods.size());
    auto method_or = LowerContinuousAssign(
        lowerer, ctor_frame, resolve_frame, init_frame, std::move(name), ca,
        driven_nets);
    if (!method_or) return std::unexpected(std::move(method_or.error()));
    const mir::MethodId body = mir_class.methods.Add(std::move(*method_or));
    AppendProcessRegistration(unit_lowerer, activate_frame, body, false);
  }

  // Recurse into descendants. Every class's shape is already published, so a
  // body that names a peer's member resolves through the existing identity
  // model regardless of which sibling lowers next.
  for (auto& child : children_) {
    auto child_r = child->PopulateBodies(ctor_frame);
    if (!child_r) return std::unexpected(std::move(child_r.error()));
  }

  for (std::size_t i = 0; i < hir_scope.generates.size(); ++i) {
    auto stmt = LowerGenerateAsStmt(
        lowerer, ctor_frame,
        hir_scope.generates.Get(hir::GenerateId{static_cast<std::uint32_t>(i)}),
        lowerer.LookupGenerateBindings(
            hir::GenerateId{static_cast<std::uint32_t>(i)}));
    if (!stmt) return std::unexpected(std::move(stmt.error()));
    ctor_block.AppendStmt(*std::move(stmt));
  }

  EmitInstanceMemberConstruction(lowerer, ctor_frame);
  auto port_conn_r = InstallPortConnections(
      lowerer, ctor_frame, resolve_frame, init_frame, activate_frame);
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
  const auto add_body = [&](mir::Block& block, mir::CallableCode& code,
                            mir::LocalId self,
                            std::string name) -> std::optional<mir::MethodId> {
    if (block.root_stmts.empty()) {
      return std::nullopt;
    }
    code.params = {self};
    code.result_type = void_type;
    return mir_class.methods.Add(
        mir::MethodDecl{
            .name = std::move(name),
            .code = std::move(code),
            .virtual_dispatch = std::nullopt,
            .visibility = mir::MethodVisibility::kInternal});
  };
  const std::optional<mir::MethodId> resolve_body =
      add_body(resolve_block, resolve_code, resolve_self_id, "ResolveState");
  const std::optional<mir::MethodId> init_body = add_body(
      initialize_block, initialize_code, init_self_id, "InitializeState");
  const std::optional<mir::MethodId> create_body = add_body(
      activate_block, activate_code, activate_self_id, "CreateProcesses");

  const bool is_unit = parent_ == nullptr;
  const std::vector<mir::ExprId> base_trailing_args =
      InstallGeneratedDefinition(
          unit, mir_class, class_id_, is_unit, ctor_code, resolve_body,
          init_body, create_body);

  FinalizeConstructor(
      unit, mir_class, std::move(ctor_code), ctor_prefix_local_ids,
      base_trailing_args);

  unit.DefineClass(class_id_, std::move(mir_class));
  return {};
}

}  // namespace lyra::lowering::hir_to_mir
