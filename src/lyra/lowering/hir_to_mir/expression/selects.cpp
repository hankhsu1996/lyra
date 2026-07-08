#include "lyra/lowering/hir_to_mir/expression/selects.hpp"

#include <cstdint>
#include <expected>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/module_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type.hpp"

// HIR-to-MIR lowering for the three select families (`a[i]`, `a[hi:lo]`,
// `s.field`). Each family has a read-side and a write-side entry point; a
// select's meaning is independent of whether a process or a structural scope
// encloses it, so each entry point is one template over the pass class. The
// bodies fan into a single per-family `Build*` factory plus a per-family inner
// helper that handles the wrapping decisions.
//
// Naming convention used here, matching the rest of HIR-to-MIR:
//   - `Lower*` -- top-level HIR-to-MIR for a HIR construct, returns
//     `diag::Result<mir::Expr>`. The caller commits the returned node.
//   - `Build*` -- factory for a specific MIR node shape, returns `mir::Expr`
//     (or `diag::Result<mir::Expr>`). Does not commit unless documented.
//   - `Wrap*`  -- transforms an existing node into another node; may commit
//     intermediate steps as a side effect.
//   - `Unfold*` -- projects HIR structure to MIR-shaped data without
//     emitting a single node directly.

namespace lyra::lowering::hir_to_mir {

namespace {

// Read vs LHS side dispatch for `arr[i]`. The split is uniform across all
// container kinds (LRM 7.8.6 / 7.8.7 AA, LRM 7.10.1 queue, LRM 7.4 packed /
// unpacked) and mirrors the runtime API's bare-vs-`Ref`-suffix protocol;
// see `include/lyra/value/concepts.hpp`.
enum class AccessSide : std::uint8_t { kRead, kLhs };

auto ElementAccessCallee(AccessSide side) -> mir::Direct {
  return mir::Direct{
      .target = side == AccessSide::kLhs ? support::BuiltinFn::kElementRef
                                         : support::BuiltinFn::kElement};
}

// LRM 7.2.1 / 7.2.2: packed struct or union field table accessor. Used by
// the member-access lowerings.
auto GetAggregateFields(const hir::Type& t)
    -> const std::vector<hir::PackedAggregateField>& {
  if (t.IsPackedStruct()) return t.AsPackedStruct().fields;
  if (t.IsPackedUnion()) return t.AsPackedUnion().fields;
  throw InternalError(
      "GetAggregateFields: base type is not a packed struct or union");
}

// Read-side wrap that materialises a borrowed `PackedArrayRef` result into
// an owning `PackedArray` (Rust's `&[T]::to_owned() -> Vec<T>` pattern).
// LHS chains keep the view because they write through it; non-packed
// receivers fall through unchanged because their access already returns an
// owning value.
auto WrapPackedAsOwned(
    const mir::CompilationUnit& unit, mir::Block& block, mir::Expr access_call,
    mir::TypeId result_type) -> mir::Expr {
  if (!std::holds_alternative<mir::PackedArrayType>(
          unit.types.Get(result_type).data)) {
    return access_call;
  }
  const mir::ExprId access_id = block.exprs.Add(std::move(access_call));
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Direct{.target = support::BuiltinFn::kToOwned},
              .arguments = {access_id}},
      .type = result_type};
}

// The type a part-select of `source_type` materialises a field of `field_type`
// as. LRM 11.8.1: a part-select is unsigned regardless of the operands, and its
// state domain follows the value it selects from, so a field (LRM 7.2.1,
// selected as a part-select of the aggregate's storage) is produced with the
// field's dimensions but the aggregate's signedness-stripped state domain --
// not the field's own declared state. Naming this keeps the field-read's MIR
// node type equal to what the runtime produces; the field's declared signedness
// and, for a 2-state field inside a 4-state aggregate, its narrower state
// domain are reconciled downstream by an explicit conversion.
auto PartSelectNaturalType(
    mir::CompilationUnit& unit, mir::TypeId source_type, mir::TypeId field_type)
    -> mir::TypeId {
  const auto& source = unit.types.Get(source_type);
  const auto& field = unit.types.Get(field_type);
  if (!source.IsPackedArray() || !field.IsPackedArray()) return field_type;
  auto natural = field.AsPackedArray();
  natural.signedness = mir::Signedness::kUnsigned;
  natural.atom = source.AsPackedArray().atom;
  natural.form = mir::PackedArrayForm::kExplicit;
  return unit.types.Intern(std::move(natural));
}

// Reconciles a field read materialised at its part-select natural type to the
// field's declared type with an explicit conversion when they differ: the
// field's signedness (LRM 7.2.1) and, for a 2-state field inside a 4-state
// aggregate, the X-to-0 collapse into its narrower state domain. A no-op when
// the value already carries the declared type.
auto WrapSliceToDeclaredType(
    const mir::CompilationUnit& unit, mir::Block& block, mir::Expr owned,
    mir::TypeId final_type) -> mir::Expr {
  if (owned.type == final_type) return owned;
  const mir::ExprId owned_id = block.exprs.Add(std::move(owned));
  return BuildValueConversion(unit, block, owned_id, final_type);
}

// Append the receiver's declared range `[left:right]` as trailing select
// operands when the receiver is an unpacked array: the range is a fact of the
// receiver's static type, materialized here as a MIR operand rather than
// carried in the value. A packed receiver self-describes (its dims are storage
// representation) and a dynamic array is zero-based, so neither contributes an
// operand.
auto AppendReceiverRange(
    ModuleLowerer& module, mir::Block& block, mir::ExprId base_id,
    std::vector<mir::ExprId>& args) -> void {
  // The declared range is a fact of the receiver's value type. On the write
  // path the base is a place -- an observable cell or a reference -- so unwrap
  // those single-level indirections to reach the underlying value type.
  mir::TypeId base_type = block.exprs.Get(base_id).type;
  for (;;) {
    const auto& ty = module.Unit().types.Get(base_type);
    if (const auto* obs = std::get_if<mir::ObservableType>(&ty.data)) {
      base_type = obs->value;
    } else if (const auto* ref = std::get_if<mir::RefType>(&ty.data)) {
      base_type = ref->pointee;
    } else if (const auto* ptr = std::get_if<mir::PointerType>(&ty.data)) {
      base_type = ptr->pointee;
    } else {
      break;
    }
  }
  const auto& base_ty = module.Unit().types.Get(base_type);
  if (const auto* ua = std::get_if<mir::UnpackedArrayType>(&base_ty.data)) {
    const mir::TypeId int32_type = module.Unit().builtins.int32;
    args.push_back(
        block.exprs.Add(mir::MakeInt32Literal(int32_type, ua->dim.left)));
    args.push_back(
        block.exprs.Add(mir::MakeInt32Literal(int32_type, ua->dim.right)));
  }
}

// `arr[i]` element access (LRM 7.4.5 / 7.5 / 7.10). The callee carries just
// read-vs-write (`kElement` / `kElementRef`); the receiver's container kind
// picks the runtime overload at C++ render time. The raw source index is passed
// through, plus the receiver's declared range for the unpacked family; every
// selectable value resolves the coordinate against that range.
auto BuildElementAccessCallExpr(
    ModuleLowerer& module, mir::Block& block, mir::ExprId base_id,
    mir::ExprId idx_id, AccessSide side, mir::TypeId result_type) -> mir::Expr {
  std::vector<mir::ExprId> args = {base_id, idx_id};
  AppendReceiverRange(module, block, base_id, args);
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = ElementAccessCallee(side),
              .arguments = std::move(args)},
      .type = result_type};
}

// `arr[hi:lo]` / `arr[base+:w]` / `arr[base-:w]` range select, lowered to a raw
// selector `(a, b, form)`: a constant range passes its two source endpoints; an
// indexed part-select passes its base and (constant) width, with the direction
// in `form` (0 constant, 1 indexed-up, 2 indexed-down -- the runtime
// `value::SliceForm`). No count, offset, endpoint ordering, or rebase is
// computed here; the selected value resolves the ordinal window against its own
// declared range.
template <typename LowerOne>
auto BuildRangeSliceCallExpr(
    ModuleLowerer& module, mir::Block& block, const hir::RangeBounds& bounds,
    mir::ExprId base_id, mir::TypeId result_type, AccessSide side,
    LowerOne lower_one) -> diag::Result<mir::Expr> {
  struct RawSelector {
    mir::ExprId a;
    mir::ExprId b;
    std::int64_t form;
  };
  auto raw_or = std::visit(
      Overloaded{
          [&](const hir::RangeConstantBounds& c) -> diag::Result<RawSelector> {
            auto l = lower_one(c.left_bound);
            if (!l) return std::unexpected(std::move(l.error()));
            auto r = lower_one(c.right_bound);
            if (!r) return std::unexpected(std::move(r.error()));
            return RawSelector{.a = *l, .b = *r, .form = 0};
          },
          [&](const hir::RangeIndexedUpBounds& c) -> diag::Result<RawSelector> {
            auto base = lower_one(c.base_index);
            if (!base) return std::unexpected(std::move(base.error()));
            auto w = lower_one(c.width);
            if (!w) return std::unexpected(std::move(w.error()));
            return RawSelector{.a = *base, .b = *w, .form = 1};
          },
          [&](const hir::RangeIndexedDownBounds& c)
              -> diag::Result<RawSelector> {
            auto base = lower_one(c.base_index);
            if (!base) return std::unexpected(std::move(base.error()));
            auto w = lower_one(c.width);
            if (!w) return std::unexpected(std::move(w.error()));
            return RawSelector{.a = *base, .b = *w, .form = 2};
          },
      },
      bounds);
  if (!raw_or) return std::unexpected(std::move(raw_or.error()));
  const mir::TypeId int32_type = module.Unit().builtins.int32;
  const auto form_id =
      block.exprs.Add(mir::MakeInt32Literal(int32_type, raw_or->form));
  std::vector<mir::ExprId> args = {base_id, raw_or->a, raw_or->b, form_id};
  AppendReceiverRange(module, block, base_id, args);
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Direct{
                      .target = side == AccessSide::kLhs
                                    ? support::BuiltinFn::kSliceRef
                                    : support::BuiltinFn::kSlice},
              .arguments = std::move(args)},
      .type = result_type};
}

// LRM 7.2.1 packed struct / union field-as-slice. The field's
// `(bit_offset, bit_width)` projects to the same packed-path slice shape a
// range-select emits, so the runtime sees one slice form regardless of
// whether the source was `s.field` or `s[hi:lo]`.
auto BuildFieldSliceCallExpr(
    ModuleLowerer& module, mir::Block& block, mir::ExprId base_id,
    std::uint32_t bit_offset, std::uint32_t bit_width, mir::TypeId result_type,
    AccessSide side) -> mir::Expr {
  const mir::TypeId int32_type = module.Unit().builtins.int32;
  const auto offset_id = block.exprs.Add(
      mir::MakeInt32Literal(int32_type, static_cast<std::int64_t>(bit_offset)));
  const auto width_id = block.exprs.Add(
      mir::MakeInt32Literal(int32_type, static_cast<std::int64_t>(bit_width)));
  // A field occupies bits `[offset +: width]` -- a raw indexed-up part-select
  // (`value::SliceForm` `kIndexedUp` == 1); the value resolves the bit window.
  const auto form_id = block.exprs.Add(mir::MakeInt32Literal(int32_type, 1));
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Direct{
                      .target = side == AccessSide::kLhs
                                    ? support::BuiltinFn::kSliceRef
                                    : support::BuiltinFn::kSlice},
              .arguments = {base_id, offset_id, width_id, form_id}},
      .type = result_type};
}

// Per-kind inner helpers that combine the factory call with the
// read/write-side wrapping. RHS readers wrap with `WrapPackedAsOwned`
// (no-op for queue / AA); LHS writers leave the borrowed-view chain
// intact for `operator=` to consume.

auto LowerElementSelectInner(
    ModuleLowerer& module, mir::Block& block, mir::ExprId base_id,
    mir::ExprId idx_id, AccessSide side, mir::TypeId result_type,
    bool wrap_packed_as_owned) -> mir::Expr {
  mir::Expr access_call = BuildElementAccessCallExpr(
      module, block, base_id, idx_id, side, result_type);
  if (!wrap_packed_as_owned) return access_call;
  return WrapPackedAsOwned(
      module.Unit(), block, std::move(access_call), result_type);
}

template <typename LowerOne>
auto LowerRangeSelectInner(
    ModuleLowerer& module, mir::Block& block, const hir::RangeBounds& bounds,
    mir::ExprId base_id, mir::TypeId result_type, LowerOne lower_one,
    AccessSide side) -> diag::Result<mir::Expr> {
  auto slice_or = BuildRangeSliceCallExpr(
      module, block, bounds, base_id, result_type, side, lower_one);
  if (!slice_or) return std::unexpected(std::move(slice_or.error()));
  if (side == AccessSide::kLhs) return *std::move(slice_or);
  return WrapPackedAsOwned(
      module.Unit(), block, *std::move(slice_or), result_type);
}

// Packed-struct / union field access (LRM 7.2.1: a field "can be selected as if
// it were a packed array"). A read materialises the part-select at its natural
// type, then converts to the field's declared type, so the field's signedness
// and 2-state-vs-4-state domain are honoured. A write emits the slice against
// the field's declared type; the aggregate's storage reconciles the field's
// representation when the assignment lands.
auto LowerMemberAccessInner(
    ModuleLowerer& module, mir::Block& block,
    const hir::PackedAggregateField& field, mir::ExprId base_id,
    mir::TypeId result_type, AccessSide side) -> mir::Expr {
  if (side == AccessSide::kLhs) {
    return BuildFieldSliceCallExpr(
        module, block, base_id, field.bit_offset, field.bit_width, result_type,
        side);
  }
  const mir::TypeId source_type = block.exprs.Get(base_id).type;
  const mir::TypeId slice_type =
      PartSelectNaturalType(module.Unit(), source_type, result_type);
  mir::Expr slice_call = BuildFieldSliceCallExpr(
      module, block, base_id, field.bit_offset, field.bit_width, slice_type,
      side);
  mir::Expr owned = WrapPackedAsOwned(
      module.Unit(), block, std::move(slice_call), slice_type);
  return WrapSliceToDeclaredType(
      module.Unit(), block, std::move(owned), result_type);
}

}  // namespace

template <ExprLowerer Lowerer>
auto LowerHirElementSelectExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::ElementSelectExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto& module = lowerer.Module();
  const auto& exprs = lowerer.HirExprs();
  auto& block = *frame.current_block;
  const bool is_write = frame.is_lvalue_target;
  const WalkFrame sub_frame = frame.WithLvalueTarget(false);

  const auto& hir_base = exprs.Get(sel.base_value);
  auto base_or = lowerer.LowerExpr(hir_base, sub_frame);
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const mir::ExprId base_id = block.exprs.Add(*std::move(base_or));

  const auto& hir_idx = exprs.Get(sel.index);
  auto idx_or = lowerer.LowerExpr(hir_idx, sub_frame);
  if (!idx_or) return std::unexpected(std::move(idx_or.error()));
  const mir::ExprId idx_id = block.exprs.Add(*std::move(idx_or));

  const auto& hir_base_ty = module.Hir().types.Get(hir_base.type);
  // LRM 6.16: indexed character read `s[i]` is the element-value access, the
  // read-side dual of the element-reference write. It joins the generic
  // element-access path (the explicit `getc` / `putc` methods are a separate
  // lowering); the value-vs-reference pair mirrors a packed array element.
  if (hir_base_ty.Kind() == hir::TypeKind::kString) {
    return mir::Expr{
        .data =
            mir::CallExpr{
                .callee = ElementAccessCallee(AccessSide::kRead),
                .arguments = {base_id, idx_id}},
        .type = result_type};
  }
  // LRM 7.10.1: queue's `q[$+1] = v` appends, so a queue element write
  // must dispatch as LHS even from this RHS entry point. The lvalue-target
  // marker carried on the walk frame surfaces that case. The string and queue
  // arms are reachable only in a procedural scope: slang forbids referencing
  // an element of a dynamic-typed variable outside a procedural context, so a
  // structural instantiation never takes them.
  const AccessSide side =
      (is_write && std::holds_alternative<hir::QueueType>(hir_base_ty.data))
          ? AccessSide::kLhs
          : AccessSide::kRead;
  return LowerElementSelectInner(
      module, block, base_id, idx_id, side, result_type, true);
}

template <ExprLowerer Lowerer>
auto LowerHirRangeSelectExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::RangeSelectExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto& module = lowerer.Module();
  const auto& exprs = lowerer.HirExprs();
  auto& block = *frame.current_block;

  const auto& hir_base = exprs.Get(sel.base_value);
  auto base_or = lowerer.LowerExpr(hir_base, frame);
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const mir::ExprId base_id = block.exprs.Add(*std::move(base_or));

  auto lower_one = [&](hir::ExprId id) -> diag::Result<mir::ExprId> {
    auto lowered = lowerer.LowerExpr(exprs.Get(id), frame);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    return block.exprs.Add(*std::move(lowered));
  };
  return LowerRangeSelectInner(
      module, block, sel.bounds, base_id, result_type, lower_one,
      AccessSide::kRead);
}

// LRM 7.2.1: packed struct / union field access "can be selected as if it
// were a packed array". HIR -> MIR resolves the field-table index to a
// concrete `(offset, count)` slice -- the same MIR shape `s[hi:lo]`
// produces.
template <ExprLowerer Lowerer>
auto LowerHirMemberAccessExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::MemberAccessExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto& module = lowerer.Module();
  const auto& exprs = lowerer.HirExprs();
  auto& block = *frame.current_block;
  const auto& base_hir_expr = exprs.Get(sel.base_value);
  // LRM 8.4: a class property access reaches the object through the handle. The
  // handle is read (the receiver), and the property is named by its class-local
  // member id, which is its declaration-order field index.
  if (module.Hir().types.Get(base_hir_expr.type).Kind() ==
      hir::TypeKind::kClassHandle) {
    auto base_or = lowerer.LowerExpr(base_hir_expr, frame);
    if (!base_or) return std::unexpected(std::move(base_or.error()));
    const mir::ExprId base_id = block.exprs.Add(*std::move(base_or));
    return mir::MakeFieldAccessExpr(
        base_id, mir::FieldId{sel.field_index}, result_type);
  }
  // LRM 7.2: an unpacked struct lowers to a generic product (`TupleType`);
  // member access is a positional projection by declaration-order index.
  if (module.Hir().types.Get(base_hir_expr.type).Kind() ==
      hir::TypeKind::kUnpackedStruct) {
    auto base_or = lowerer.LowerExpr(base_hir_expr, frame);
    if (!base_or) return std::unexpected(std::move(base_or.error()));
    const mir::ExprId base_id = block.exprs.Add(*std::move(base_or));
    return mir::Expr{
        .data = mir::TupleGetExpr{.tuple = base_id, .index = sel.field_index},
        .type = result_type};
  }
  // LRM 7.3: an unpacked union member read projects the active member by index
  // out of the overlapping-storage value (`UnionGetExpr`); reading an
  // inactive member is undefined and the backend returns that member's default.
  if (module.Hir().types.Get(base_hir_expr.type).Kind() ==
      hir::TypeKind::kUnpackedUnion) {
    auto base_or = lowerer.LowerExpr(base_hir_expr, frame);
    if (!base_or) return std::unexpected(std::move(base_or.error()));
    const mir::ExprId base_id = block.exprs.Add(*std::move(base_or));
    return mir::Expr{
        .data =
            mir::UnionGetExpr{.union_value = base_id, .index = sel.field_index},
        .type = result_type};
  }
  const auto& fields =
      GetAggregateFields(module.Hir().types.Get(base_hir_expr.type));
  if (sel.field_index >= fields.size()) {
    throw InternalError("LowerHirMemberAccessExpr: field_index out of range");
  }
  auto base_or = lowerer.LowerExpr(base_hir_expr, frame);
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const mir::ExprId base_id = block.exprs.Add(*std::move(base_or));
  return LowerMemberAccessInner(
      module, block, fields[sel.field_index], base_id, result_type,
      AccessSide::kRead);
}

template <ExprLowerer Lowerer>
auto LowerHirElementSelectExprLhs(
    Lowerer& lowerer, WalkFrame frame, const hir::ElementSelectExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto& module = lowerer.Module();
  const auto& exprs = lowerer.HirExprs();
  auto& block = *frame.current_block;

  const auto& hir_base = exprs.Get(sel.base_value);
  auto base_or = lowerer.LowerLhsExpr(hir_base, frame);
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const mir::ExprId base_id = block.exprs.Add(*std::move(base_or));

  const auto& hir_idx = exprs.Get(sel.index);
  auto idx_or = lowerer.LowerExpr(hir_idx, frame);
  if (!idx_or) return std::unexpected(std::move(idx_or.error()));
  const mir::ExprId idx_id = block.exprs.Add(*std::move(idx_or));

  const auto& hir_base_ty = module.Hir().types.Get(hir_base.type);
  // LRM 6.16: indexed character write `s[i] = ...` is the element-reference
  // access, the write-side dual of the element-value read. It joins the generic
  // element-access path so the place flows through the observable mutate route
  // and the assignment shape uniformly.
  if (hir_base_ty.Kind() == hir::TypeKind::kString) {
    return mir::Expr{
        .data =
            mir::CallExpr{
                .callee = ElementAccessCallee(AccessSide::kLhs),
                .arguments = {base_id, idx_id}},
        .type = result_type};
  }
  return LowerElementSelectInner(
      module, block, base_id, idx_id, AccessSide::kLhs, result_type, false);
}

template <ExprLowerer Lowerer>
auto LowerHirRangeSelectExprLhs(
    Lowerer& lowerer, WalkFrame frame, const hir::RangeSelectExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto& module = lowerer.Module();
  const auto& exprs = lowerer.HirExprs();
  auto& block = *frame.current_block;

  const auto& hir_base = exprs.Get(sel.base_value);
  auto base_or = lowerer.LowerLhsExpr(hir_base, frame);
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const mir::ExprId base_id = block.exprs.Add(*std::move(base_or));

  auto lower_one = [&](hir::ExprId id) -> diag::Result<mir::ExprId> {
    auto lowered = lowerer.LowerExpr(exprs.Get(id), frame);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    return block.exprs.Add(*std::move(lowered));
  };
  return LowerRangeSelectInner(
      module, block, sel.bounds, base_id, result_type, lower_one,
      AccessSide::kLhs);
}

template <ExprLowerer Lowerer>
auto LowerHirMemberAccessExprLhs(
    Lowerer& lowerer, WalkFrame frame, const hir::MemberAccessExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto& module = lowerer.Module();
  const auto& exprs = lowerer.HirExprs();
  auto& block = *frame.current_block;
  const auto& base_hir_expr = exprs.Get(sel.base_value);
  // LRM 8.4: a class property write reaches the object through the handle. The
  // handle is read to reach the shared object, and the property place is the
  // member access through it; the write itself targets the object's storage.
  if (module.Hir().types.Get(base_hir_expr.type).Kind() ==
      hir::TypeKind::kClassHandle) {
    auto base_or = lowerer.LowerExpr(base_hir_expr, frame);
    if (!base_or) return std::unexpected(std::move(base_or.error()));
    const mir::ExprId base_id = block.exprs.Add(*std::move(base_or));
    return mir::MakeFieldAccessExpr(
        base_id, mir::FieldId{sel.field_index}, result_type);
  }
  // LRM 7.2: an unpacked-struct member write is a positional projection by
  // index over the base place. The observable root's write routes through the
  // cell's mutate path later, so the place is just the projection here.
  if (module.Hir().types.Get(base_hir_expr.type).Kind() ==
      hir::TypeKind::kUnpackedStruct) {
    auto base_or = lowerer.LowerLhsExpr(base_hir_expr, frame);
    if (!base_or) return std::unexpected(std::move(base_or.error()));
    const mir::ExprId base_id = block.exprs.Add(*std::move(base_or));
    return mir::Expr{
        .data = mir::TupleGetExpr{.tuple = base_id, .index = sel.field_index},
        .type = result_type};
  }
  // LRM 7.3: an unpacked union member write is the write-side access form
  // `UnionGetRefExpr` over the union place (the by-reference counterpart of
  // the `UnionGetExpr` read). The observable root routes through the cell's
  // mutate path later; the place is just the projection here.
  if (module.Hir().types.Get(base_hir_expr.type).Kind() ==
      hir::TypeKind::kUnpackedUnion) {
    auto base_or = lowerer.LowerLhsExpr(base_hir_expr, frame);
    if (!base_or) return std::unexpected(std::move(base_or.error()));
    const mir::ExprId base_id = block.exprs.Add(*std::move(base_or));
    return mir::Expr{
        .data =
            mir::UnionGetRefExpr{
                .union_value = base_id, .index = sel.field_index},
        .type = result_type};
  }
  const auto& fields =
      GetAggregateFields(module.Hir().types.Get(base_hir_expr.type));
  if (sel.field_index >= fields.size()) {
    throw InternalError(
        "LowerHirMemberAccessExprLhs: field_index out of range");
  }
  auto base_or = lowerer.LowerLhsExpr(base_hir_expr, frame);
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const mir::ExprId base_id = block.exprs.Add(*std::move(base_or));
  return LowerMemberAccessInner(
      module, block, fields[sel.field_index], base_id, result_type,
      AccessSide::kLhs);
}

// One concrete instantiation per pass class. The handler templates are defined
// in this file rather than the header so the file-local helpers stay private,
// so the dispatchers in process_lowerer.cpp / structural_scope_lowerer.cpp link
// against the symbols emitted here.
template auto LowerHirElementSelectExpr(
    ProcessLowerer&, WalkFrame, const hir::ElementSelectExpr&, mir::TypeId)
    -> diag::Result<mir::Expr>;
template auto LowerHirElementSelectExpr(
    const StructuralScopeLowerer&, WalkFrame, const hir::ElementSelectExpr&,
    mir::TypeId) -> diag::Result<mir::Expr>;
template auto LowerHirRangeSelectExpr(
    ProcessLowerer&, WalkFrame, const hir::RangeSelectExpr&, mir::TypeId)
    -> diag::Result<mir::Expr>;
template auto LowerHirRangeSelectExpr(
    const StructuralScopeLowerer&, WalkFrame, const hir::RangeSelectExpr&,
    mir::TypeId) -> diag::Result<mir::Expr>;
template auto LowerHirMemberAccessExpr(
    ProcessLowerer&, WalkFrame, const hir::MemberAccessExpr&, mir::TypeId)
    -> diag::Result<mir::Expr>;
template auto LowerHirMemberAccessExpr(
    const StructuralScopeLowerer&, WalkFrame, const hir::MemberAccessExpr&,
    mir::TypeId) -> diag::Result<mir::Expr>;
template auto LowerHirElementSelectExprLhs(
    ProcessLowerer&, WalkFrame, const hir::ElementSelectExpr&, mir::TypeId)
    -> diag::Result<mir::Expr>;
template auto LowerHirElementSelectExprLhs(
    const StructuralScopeLowerer&, WalkFrame, const hir::ElementSelectExpr&,
    mir::TypeId) -> diag::Result<mir::Expr>;
template auto LowerHirRangeSelectExprLhs(
    ProcessLowerer&, WalkFrame, const hir::RangeSelectExpr&, mir::TypeId)
    -> diag::Result<mir::Expr>;
template auto LowerHirRangeSelectExprLhs(
    const StructuralScopeLowerer&, WalkFrame, const hir::RangeSelectExpr&,
    mir::TypeId) -> diag::Result<mir::Expr>;
template auto LowerHirMemberAccessExprLhs(
    ProcessLowerer&, WalkFrame, const hir::MemberAccessExpr&, mir::TypeId)
    -> diag::Result<mir::Expr>;
template auto LowerHirMemberAccessExprLhs(
    const StructuralScopeLowerer&, WalkFrame, const hir::MemberAccessExpr&,
    mir::TypeId) -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
