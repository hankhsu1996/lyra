#include "lyra/lowering/hir_to_mir/expression/selects.hpp"

#include <cstdint>
#include <expected>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/component_index.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/expression/operators.hpp"
#include "lyra/lowering/hir_to_mir/flat_packed_type.hpp"
#include "lyra/lowering/hir_to_mir/packed_projection.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/binary_op.hpp"
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

// An element read. A write is not a call: it is a descent step on the target's
// designator, so no write-side access entry exists.
auto ElementAccessCallee() -> mir::Direct {
  return mir::Direct{.target = support::BuiltinFn::kElement};
}

auto ProjectedMemberAt(
    const PackedProjection& projection, base::ComponentIndex index)
    -> const ProjectedMember& {
  if (index.value >= projection.members.size()) {
    throw InternalError("ProjectedMemberAt: member index out of range");
  }
  return projection.members[index.value];
}

// Read-side wrap that materialises a borrowed packed view into an owning value
// (Rust's `&[T]::to_owned() -> Vec<T>` pattern). A non-packed receiver falls
// through unchanged because its access already returns an owning value.
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
// carried in the value. A packed receiver's coordinates are its own dims, which
// it carries, and a dynamic array is zero-based, so neither states a range.
auto AppendReceiverRange(
    UnitLowerer& unit_lowerer, mir::Block& block, mir::TypeId base_type,
    std::vector<mir::ExprId>& args) -> void {
  // The declared range is a fact of the receiver's value type. On the write
  // path the base is a place -- reached through a capability wrapper, or
  // through a pointer -- so unwrap those indirections to reach the underlying
  // value type. Which wrappers exist is not restated here; asking the type
  // system is what keeps a newly admitted wrapper from silently losing the
  // range.
  for (;;) {
    const auto& ty = unit_lowerer.Unit().types.Get(base_type);
    if (ty.IsCapabilityWrapper()) {
      base_type = ty.WrappedValueType();
    } else if (const auto* ptr = std::get_if<mir::PointerType>(&ty.data)) {
      base_type = ptr->pointee;
    } else {
      break;
    }
  }
  const auto& base_ty = unit_lowerer.Unit().types.Get(base_type);
  if (const auto* ua = std::get_if<mir::UnpackedArrayType>(&base_ty.data)) {
    const mir::TypeId int_type = unit_lowerer.Unit().builtins.int_type;
    args.push_back(
        block.exprs.Add(mir::MakeIntLiteral(int_type, ua->dim.left)));
    args.push_back(
        block.exprs.Add(mir::MakeIntLiteral(int_type, ua->dim.right)));
  }
}

// Append the shape a packed part-select's result takes as a trailing operand,
// sourced from the select's static result type. The bounds decide which bits
// are selected; the result type decides how they are structured. A receiver can
// only supply that structure when it is itself the array being selected from --
// a packed aggregate's base is a flat bit run, and its members' shapes live in
// the member types alone (LRM 7.2.1 / 7.3.1). Every other container's slice
// yields its own element structure, so none contributes an operand.
auto AppendResultShape(
    UnitLowerer& unit_lowerer, mir::Block& block, mir::TypeId result_type,
    std::vector<mir::ExprId>& args) -> void {
  const auto& result_ty = unit_lowerer.Unit().types.Get(result_type);
  if (!result_ty.IsIntegralPacked()) return;
  args.push_back(BuildPackedShapePrototype(
      block, result_ty.AsIntegralPacked(), result_type));
}

// Extends a write target by one descent step. A target that already designates
// a part of some owner's value gains another selector; a target that is still a
// place becomes the owner of a new designation. The place boundary therefore
// falls out of the lowering's own recursion -- the innermost expression that is
// not itself a descent is the owner -- and no consumer recovers it afterwards.
auto ProjectOnto(
    mir::Block& block, mir::Expr base, mir::Selector selector,
    mir::TypeId result_type) -> mir::Expr {
  if (auto* projection = std::get_if<mir::ValueProjectionExpr>(&base.data)) {
    projection->path.push_back(std::move(selector));
    base.type = result_type;
    return base;
  }
  return mir::Expr{
      .data =
          mir::ValueProjectionExpr{
              .owner = block.exprs.Add(std::move(base)),
              .path = {std::move(selector)}},
      .type = result_type};
}

// `arr[i]` element access (LRM 7.4.5 / 7.5 / 7.10). A read is a call whose
// receiver's container kind picks the runtime overload; a write is a descent
// step on the target's designator. Either way the raw source index is passed
// through, plus the receiver's declared range for the unpacked family; every
// selectable value resolves the coordinate against that range.
auto BuildElementAccessCallExpr(
    UnitLowerer& unit_lowerer, mir::Block& block, mir::ExprId base_id,
    mir::ExprId idx_id, mir::TypeId result_type) -> mir::Expr {
  std::vector<mir::ExprId> args = {base_id, idx_id};
  AppendReceiverRange(unit_lowerer, block, block.exprs.Get(base_id).type, args);
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = ElementAccessCallee(), .arguments = std::move(args)},
      .type = result_type};
}

// The write-side counterpart: one element descent step on the target.
auto ProjectElement(
    UnitLowerer& unit_lowerer, mir::Block& block, mir::Expr base,
    mir::ExprId idx_id, mir::TypeId result_type) -> mir::Expr {
  std::vector<mir::ExprId> operands = {idx_id};
  AppendReceiverRange(unit_lowerer, block, base.type, operands);
  return ProjectOnto(
      block, std::move(base),
      mir::ElementSelector{
          .operands = std::move(operands), .projected_type = result_type},
      result_type);
}

// `arr[hi:lo]` / `arr[base+:w]` / `arr[base-:w]` range select, lowered to a raw
// selector `(a, b, form)`: a constant range passes its two source endpoints; an
// indexed part-select passes its base and (constant) width, with the direction
// in `form` (0 constant, 1 indexed-up, 2 indexed-down -- the runtime
// `value::SliceForm`). No count, offset, endpoint ordering, or rebase is
// computed here; the selected value resolves the ordinal window against its own
// declared range.
template <typename LowerOne>
auto UnfoldRangeSelectOperands(
    UnitLowerer& unit_lowerer, mir::Block& block,
    const hir::RangeBounds& bounds, mir::TypeId base_type,
    mir::TypeId result_type, LowerOne lower_one)
    -> diag::Result<std::vector<mir::ExprId>> {
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
  const mir::TypeId int_type = unit_lowerer.Unit().builtins.int_type;
  const auto form_id =
      block.exprs.Add(mir::MakeIntLiteral(int_type, raw_or->form));
  std::vector<mir::ExprId> operands = {raw_or->a, raw_or->b, form_id};
  AppendResultShape(unit_lowerer, block, result_type, operands);
  AppendReceiverRange(unit_lowerer, block, base_type, operands);
  return operands;
}

// A range select read: the window operands against a receiver.
template <typename LowerOne>
auto BuildRangeSliceCallExpr(
    UnitLowerer& unit_lowerer, mir::Block& block,
    const hir::RangeBounds& bounds, mir::ExprId base_id,
    mir::TypeId result_type, LowerOne lower_one) -> diag::Result<mir::Expr> {
  auto operands_or = UnfoldRangeSelectOperands(
      unit_lowerer, block, bounds, block.exprs.Get(base_id).type, result_type,
      lower_one);
  if (!operands_or) return std::unexpected(std::move(operands_or.error()));
  std::vector<mir::ExprId> args = {base_id};
  args.insert(args.end(), operands_or->begin(), operands_or->end());
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Direct{.target = support::BuiltinFn::kSlice},
              .arguments = std::move(args)},
      .type = result_type};
}

// The write-side counterpart: one window descent step on the target.
template <typename LowerOne>
auto ProjectSlice(
    UnitLowerer& unit_lowerer, mir::Block& block,
    const hir::RangeBounds& bounds, mir::Expr base, mir::TypeId result_type,
    LowerOne lower_one) -> diag::Result<mir::Expr> {
  auto operands_or = UnfoldRangeSelectOperands(
      unit_lowerer, block, bounds, base.type, result_type, lower_one);
  if (!operands_or) return std::unexpected(std::move(operands_or.error()));
  return ProjectOnto(
      block, std::move(base),
      mir::SliceSelector{
          .operands = *std::move(operands_or), .projected_type = result_type},
      result_type);
}

// LRM 7.2.1 packed struct / union field-as-slice. The field's
// `(bit_offset, bit_width)` projects to the same packed-path slice shape a
// range-select emits, so the runtime sees one slice form regardless of
// whether the source was `s.field` or `s[hi:lo]`.
auto UnfoldFieldSliceOperands(
    UnitLowerer& unit_lowerer, mir::Block& block, std::uint32_t bit_offset,
    std::uint32_t bit_width, mir::TypeId result_type)
    -> std::vector<mir::ExprId> {
  const mir::TypeId int_type = unit_lowerer.Unit().builtins.int_type;
  const auto offset_id = block.exprs.Add(
      mir::MakeIntLiteral(int_type, static_cast<std::int64_t>(bit_offset)));
  const auto width_id = block.exprs.Add(
      mir::MakeIntLiteral(int_type, static_cast<std::int64_t>(bit_width)));
  // A field occupies bits `[offset +: width]` -- a raw indexed-up part-select
  // (`value::SliceForm` `kIndexedUp` == 1); the value resolves the bit window.
  const auto form_id = block.exprs.Add(mir::MakeIntLiteral(int_type, 1));
  std::vector<mir::ExprId> operands = {offset_id, width_id, form_id};
  AppendResultShape(unit_lowerer, block, result_type, operands);
  return operands;
}

auto BuildFieldSliceCallExpr(
    UnitLowerer& unit_lowerer, mir::Block& block, mir::ExprId base_id,
    std::uint32_t bit_offset, std::uint32_t bit_width, mir::TypeId result_type)
    -> mir::Expr {
  std::vector<mir::ExprId> operands = UnfoldFieldSliceOperands(
      unit_lowerer, block, bit_offset, bit_width, result_type);
  std::vector<mir::ExprId> args = {base_id};
  args.insert(args.end(), operands.begin(), operands.end());
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Direct{.target = support::BuiltinFn::kSlice},
              .arguments = std::move(args)},
      .type = result_type};
}

// The write-side counterpart: a packed aggregate's member reached as one
// constant-bounds window descent step on the target.
auto ProjectFieldSlice(
    UnitLowerer& unit_lowerer, mir::Block& block, mir::Expr base,
    std::uint32_t bit_offset, std::uint32_t bit_width, mir::TypeId result_type)
    -> mir::Expr {
  return ProjectOnto(
      block, std::move(base),
      mir::SliceSelector{
          .operands = UnfoldFieldSliceOperands(
              unit_lowerer, block, bit_offset, bit_width, result_type),
          .projected_type = result_type},
      result_type);
}

// The value `base_id` names, guarded by the tag naming member `index` (LRM
// 11.9). The guard yields that value, so a read composes the ordinary member
// slice onto it and a write designates a part of it -- either way the access
// itself stays the one every packed member uses. The check has to be part of
// evaluating the access and not a test hoisted ahead of it: LRM 11.3.5
// requires a short-circuited operand to raise none of the run-time errors its
// evaluation would have.
auto BuildTagGuard(
    UnitLowerer& unit_lowerer, mir::Block& block, mir::ExprId base_id,
    const PackedProjection& projection, base::ComponentIndex index,
    std::string_view message) -> mir::Expr {
  auto& unit = unit_lowerer.Unit();
  // A write-side base still designates the storage, while the tag is a fact of
  // the value that storage holds, so the test reads the value while the guard
  // passes the base through at the value's type.
  const mir::TypeId base_type = block.exprs.Get(base_id).type;
  const bool base_is_cell = unit.types.Get(base_type).IsCapabilityWrapper();
  const mir::TypeId value_type =
      base_is_cell ? unit.types.Get(base_type).WrappedValueType() : base_type;
  const mir::ExprId tag_subject =
      base_is_cell ? block.exprs.Add(mir::MakeDerefExpr(base_id, value_type))
                   : base_id;
  const mir::ExprId test =
      BuildPackedTagTest(unit_lowerer, block, tag_subject, projection, index);
  const mir::ExprId message_id = block.exprs.Add(
      mir::MakeStringLiteral(unit.builtins.string, std::string{message}));
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Direct{.target = support::BuiltinFn::kRequire},
              .arguments = {base_id, test, message_id}},
      .type = value_type};
}

// The subject a member access reaches through: the base itself when nothing
// distinguishes the members, and the tag guard's result when a tag does.
auto GuardedSubject(
    UnitLowerer& unit_lowerer, mir::Block& block, mir::ExprId base_id,
    const PackedProjection& projection, base::ComponentIndex index,
    std::string_view message) -> mir::ExprId {
  if (projection.tag_bits == 0) return base_id;
  return block.exprs.Add(
      BuildTagGuard(unit_lowerer, block, base_id, projection, index, message));
}

// Per-kind inner helpers that combine the factory call with the
// read/write-side wrapping. RHS readers wrap with `WrapPackedAsOwned`
// (no-op for queue / AA); LHS writers leave the borrowed-view chain
// intact for `operator=` to consume.

auto LowerElementSelectInner(
    UnitLowerer& unit_lowerer, mir::Block& block, mir::ExprId base_id,
    mir::ExprId idx_id, mir::TypeId result_type, bool wrap_packed_as_owned)
    -> mir::Expr {
  mir::Expr access_call = BuildElementAccessCallExpr(
      unit_lowerer, block, base_id, idx_id, result_type);
  if (!wrap_packed_as_owned) return access_call;
  return WrapPackedAsOwned(
      unit_lowerer.Unit(), block, std::move(access_call), result_type);
}

template <typename LowerOne>
auto LowerRangeSelectInner(
    UnitLowerer& unit_lowerer, mir::Block& block,
    const hir::RangeBounds& bounds, mir::ExprId base_id,
    mir::TypeId result_type, LowerOne lower_one) -> diag::Result<mir::Expr> {
  auto slice_or = BuildRangeSliceCallExpr(
      unit_lowerer, block, bounds, base_id, result_type, lower_one);
  if (!slice_or) return std::unexpected(std::move(slice_or.error()));
  return WrapPackedAsOwned(
      unit_lowerer.Unit(), block, *std::move(slice_or), result_type);
}

// Packed-struct / union field access (LRM 7.2.1: a field "can be selected as if
// it were a packed array"). A read materialises the part-select at its natural
// type, then converts to the field's declared type, so the field's signedness
// and 2-state-vs-4-state domain are honoured. A write emits the slice against
// the field's declared type; the aggregate's storage reconciles the field's
// representation when the assignment lands.
auto LowerMemberAccessInner(
    UnitLowerer& unit_lowerer, mir::Block& block,
    const PackedProjection& projection, base::ComponentIndex index,
    mir::ExprId base_id, mir::TypeId result_type) -> mir::Expr {
  const mir::TypeId source_type = block.exprs.Get(base_id).type;
  const mir::TypeId slice_type =
      PartSelectNaturalType(unit_lowerer.Unit(), source_type, result_type);
  const ProjectedMember& member = ProjectedMemberAt(projection, index);
  const mir::ExprId subject = GuardedSubject(
      unit_lowerer, block, base_id, projection, index,
      "read of a tagged union member inconsistent with the current tag "
      "(LRM 11.9)");
  mir::Expr slice_call = BuildFieldSliceCallExpr(
      unit_lowerer, block, subject,
      static_cast<std::uint32_t>(member.bit_offset),
      static_cast<std::uint32_t>(member.bit_width), slice_type);
  mir::Expr owned = WrapPackedAsOwned(
      unit_lowerer.Unit(), block, std::move(slice_call), slice_type);
  return WrapSliceToDeclaredType(
      unit_lowerer.Unit(), block, std::move(owned), result_type);
}

}  // namespace

auto BuildPackedRunRead(
    UnitLowerer& unit_lowerer, mir::Block& block, mir::ExprId base,
    std::uint64_t bit_offset, std::uint64_t bit_width, mir::TypeId result_type)
    -> mir::Expr {
  mir::Expr run = BuildFieldSliceCallExpr(
      unit_lowerer, block, base, static_cast<std::uint32_t>(bit_offset),
      static_cast<std::uint32_t>(bit_width), result_type);
  return WrapPackedAsOwned(
      unit_lowerer.Unit(), block, std::move(run), result_type);
}

auto BuildPackedMemberRead(
    UnitLowerer& unit_lowerer, mir::Block& block, mir::ExprId base,
    const PackedProjection& projection, base::ComponentIndex index,
    mir::TypeId result_type) -> mir::Expr {
  return LowerMemberAccessInner(
      unit_lowerer, block, projection, index, base, result_type);
}

auto BuildPackedTagTest(
    UnitLowerer& unit_lowerer, mir::Block& block, mir::ExprId base,
    const PackedProjection& projection, base::ComponentIndex index)
    -> mir::ExprId {
  if (projection.tag_bits == 0) {
    throw InternalError(
        "BuildPackedTagTest: value carries no tag to test against");
  }
  auto& unit = unit_lowerer.Unit();
  // The tag is a run of the aggregate's own vector, so it is read in the
  // aggregate's state domain -- which the projection states, and the base
  // expression may not (a write-side base is still the storage cell).
  const mir::TypeId tag_type = InternFlatPacked(
      unit, projection.tag_bits,
      projection.four_state ? mir::BitAtom::kLogic : mir::BitAtom::kBit);
  const mir::ExprId tag = block.exprs.Add(BuildPackedRunRead(
      unit_lowerer, block, base, projection.bit_width - projection.tag_bits,
      projection.tag_bits, tag_type));
  const mir::ExprId named = block.exprs.Add(
      mir::MakeIntLiteral(
          unit.builtins.int_type, static_cast<std::int64_t>(index.value)));
  return block.exprs.Add(BuildMirBinaryExpr(
      unit, block, mir::BinaryOp::kCaseEquality, tag,
      ConvertToType(unit, block, named, tag_type), unit.builtins.bit1));
}

template <ExprLowerer Lowerer>
auto LowerHirElementSelectExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::ElementSelectExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto& unit_lowerer = lowerer.Owner();
  const auto& exprs = lowerer.HirExprs();
  auto& block = *frame.current_block;

  const auto& hir_base = exprs.Get(sel.base_value);
  auto base_or = lowerer.LowerExpr(hir_base, frame);
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const mir::ExprId base_id = block.exprs.Add(*std::move(base_or));

  const auto& hir_idx = exprs.Get(sel.index);
  auto idx_or = lowerer.LowerExpr(hir_idx, frame);
  if (!idx_or) return std::unexpected(std::move(idx_or.error()));
  const mir::ExprId idx_id = block.exprs.Add(*std::move(idx_or));

  const auto& hir_base_ty = unit_lowerer.Hir().types.Get(hir_base.type);
  // LRM 6.16: indexed character read `s[i]` is the element-value access, the
  // read-side dual of the element-reference write. It joins the generic
  // element-access path (the explicit `getc` / `putc` methods are a separate
  // lowering); the value-vs-reference pair mirrors a packed array element.
  if (hir_base_ty.Kind() == hir::TypeKind::kString) {
    return mir::Expr{
        .data =
            mir::CallExpr{
                .callee = ElementAccessCallee(),
                .arguments = {base_id, idx_id}},
        .type = result_type};
  }
  return LowerElementSelectInner(
      unit_lowerer, block, base_id, idx_id, result_type, true);
}

template <ExprLowerer Lowerer>
auto LowerHirRangeSelectExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::RangeSelectExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto& unit_lowerer = lowerer.Owner();
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
      unit_lowerer, block, sel.bounds, base_id, result_type, lower_one);
}

// LRM 7.2.1: packed struct / union field access "can be selected as if it
// were a packed array". HIR -> MIR resolves the field-table index to a
// concrete `(offset, count)` slice -- the same MIR shape `s[hi:lo]`
// produces. LRM 7.2 / 7.3: an unpacked struct / union lowers to the generic
// product / sum-arm selection primitive; a packed one to a bit slice.
template <ExprLowerer Lowerer>
auto LowerHirMemberAccessExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::MemberAccessExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto& unit_lowerer = lowerer.Owner();
  const auto& exprs = lowerer.HirExprs();
  auto& block = *frame.current_block;
  const auto& base_hir_expr = exprs.Get(sel.base_value);
  if (unit_lowerer.Hir().types.Get(base_hir_expr.type).Kind() ==
      hir::TypeKind::kUnpackedStruct) {
    auto base_or = lowerer.LowerExpr(base_hir_expr, frame);
    if (!base_or) return std::unexpected(std::move(base_or.error()));
    const mir::ExprId base_id = block.exprs.Add(*std::move(base_or));
    return mir::Expr{
        .data = mir::TupleGetExpr{.tuple = base_id, .index = sel.field_index},
        .type = result_type};
  }
  if (unit_lowerer.Hir().types.Get(base_hir_expr.type).Kind() ==
      hir::TypeKind::kUnpackedUnion) {
    auto base_or = lowerer.LowerExpr(base_hir_expr, frame);
    if (!base_or) return std::unexpected(std::move(base_or.error()));
    const mir::ExprId base_id = block.exprs.Add(*std::move(base_or));
    // LRM 11.9: a tagged-union dot-access is a run-time-checked read; the
    // untagged form is the type-loophole read. Route by the source-level tag.
    const auto& union_ty = std::get<hir::UnpackedUnionType>(
        unit_lowerer.Hir().types.Get(base_hir_expr.type).data);
    if (union_ty.tagged) {
      return mir::Expr{
          .data =
              mir::TaggedGetExpr{
                  .union_value = base_id, .tag_index = sel.field_index},
          .type = result_type};
    }
    return mir::Expr{
        .data =
            mir::UnionGetExpr{.union_value = base_id, .index = sel.field_index},
        .type = result_type};
  }
  const PackedProjection projection = ProjectPackedAggregate(
      unit_lowerer, unit_lowerer.Hir().types.Get(base_hir_expr.type).data);
  auto base_or = lowerer.LowerExpr(base_hir_expr, frame);
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const mir::ExprId base_id = block.exprs.Add(*std::move(base_or));
  return LowerMemberAccessInner(
      unit_lowerer, block, projection, sel.field_index, base_id, result_type);
}

// LRM 8.4: a class property read reaches the object through the handle.
// The handle is read (the receiver), and the property is named
// owner-qualified -- the class arena that declares the property is stated on
// the HIR node, so an inherited property (LRM 8.13) lands on the base
// class's slot, not on the receiver's runtime-class slot.
template <ExprLowerer Lowerer>
auto LowerHirClassPropertyAccessExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::ClassPropertyAccessExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto& unit_lowerer = lowerer.Owner();
  auto& block = *frame.current_block;
  const auto& base_hir_expr = lowerer.HirExprs().Get(sel.base_value);
  auto base_or = lowerer.LowerExpr(base_hir_expr, frame);
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const mir::ExprId base_id = block.exprs.Add(*std::move(base_or));
  return mir::MakeFieldAccessExpr(
      base_id, unit_lowerer.TranslateClassPropertyTarget(sel.target),
      result_type);
}

template <ExprLowerer Lowerer>
auto LowerHirElementSelectExprLhs(
    Lowerer& lowerer, WalkFrame frame, const hir::ElementSelectExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto& unit_lowerer = lowerer.Owner();
  const auto& exprs = lowerer.HirExprs();
  auto& block = *frame.current_block;

  const auto& hir_base = exprs.Get(sel.base_value);
  auto base_or = lowerer.LowerLhsExpr(hir_base, frame);
  if (!base_or) return std::unexpected(std::move(base_or.error()));

  const auto& hir_idx = exprs.Get(sel.index);
  auto idx_or = lowerer.LowerExpr(hir_idx, frame);
  if (!idx_or) return std::unexpected(std::move(idx_or.error()));
  const mir::ExprId idx_id = block.exprs.Add(*std::move(idx_or));

  return ProjectElement(
      unit_lowerer, block, *std::move(base_or), idx_id, result_type);
}

template <ExprLowerer Lowerer>
auto LowerHirRangeSelectExprLhs(
    Lowerer& lowerer, WalkFrame frame, const hir::RangeSelectExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto& unit_lowerer = lowerer.Owner();
  const auto& exprs = lowerer.HirExprs();
  auto& block = *frame.current_block;

  const auto& hir_base = exprs.Get(sel.base_value);
  auto base_or = lowerer.LowerLhsExpr(hir_base, frame);
  if (!base_or) return std::unexpected(std::move(base_or.error()));

  auto lower_one = [&](hir::ExprId id) -> diag::Result<mir::ExprId> {
    auto lowered = lowerer.LowerExpr(exprs.Get(id), frame);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    return block.exprs.Add(*std::move(lowered));
  };
  return ProjectSlice(
      unit_lowerer, block, sel.bounds, *std::move(base_or), result_type,
      lower_one);
}

template <ExprLowerer Lowerer>
auto LowerHirMemberAccessExprLhs(
    Lowerer& lowerer, WalkFrame frame, const hir::MemberAccessExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto& unit_lowerer = lowerer.Owner();
  const auto& exprs = lowerer.HirExprs();
  auto& block = *frame.current_block;
  const auto& base_hir_expr = exprs.Get(sel.base_value);
  // LRM 7.2: an unpacked-struct member write is a positional projection by
  // index over the base place. The observable root's write routes through the
  // cell's mutate path later, so the place is just the projection here.
  if (unit_lowerer.Hir().types.Get(base_hir_expr.type).Kind() ==
      hir::TypeKind::kUnpackedStruct) {
    auto base_or = lowerer.LowerLhsExpr(base_hir_expr, frame);
    if (!base_or) return std::unexpected(std::move(base_or.error()));
    return ProjectOnto(
        block, *std::move(base_or),
        mir::ComponentSelector{
            .index = sel.field_index, .projected_type = result_type},
        result_type);
  }
  // LRM 7.3: an unpacked union member write descends into the union value. An
  // untagged member write makes that member active, so it is a descent step on
  // the designator; a tagged one instead requires the member to already be the
  // current tag (LRM 11.9), which is its own node. The observable root routes
  // through the cell's mutate path later.
  if (unit_lowerer.Hir().types.Get(base_hir_expr.type).Kind() ==
      hir::TypeKind::kUnpackedUnion) {
    auto base_or = lowerer.LowerLhsExpr(base_hir_expr, frame);
    if (!base_or) return std::unexpected(std::move(base_or.error()));
    const auto& union_ty = std::get<hir::UnpackedUnionType>(
        unit_lowerer.Hir().types.Get(base_hir_expr.type).data);
    if (union_ty.tagged) {
      const mir::ExprId base_id = block.exprs.Add(*std::move(base_or));
      return mir::Expr{
          .data =
              mir::TaggedGetRefExpr{
                  .union_value = base_id, .tag_index = sel.field_index},
          .type = result_type};
    }
    return ProjectOnto(
        block, *std::move(base_or),
        mir::UnionMemberSelector{
            .index = sel.field_index, .projected_type = result_type},
        result_type);
  }
  const PackedProjection projection = ProjectPackedAggregate(
      unit_lowerer, unit_lowerer.Hir().types.Get(base_hir_expr.type).data);
  auto base_or = lowerer.LowerLhsExpr(base_hir_expr, frame);
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const ProjectedMember& member =
      ProjectedMemberAt(projection, sel.field_index);
  mir::Expr base = *std::move(base_or);
  // A tagged member's write designates a part of the guard's result, so the
  // guard becomes the designation's owner and the member stays one ordinary
  // window descent step on it.
  if (projection.tag_bits != 0) {
    const mir::ExprId base_id = block.exprs.Add(std::move(base));
    base = BuildTagGuard(
        unit_lowerer, block, base_id, projection, sel.field_index,
        "write to a tagged union member inconsistent with the current tag "
        "(LRM 11.9)");
  }
  return ProjectFieldSlice(
      unit_lowerer, block, std::move(base),
      static_cast<std::uint32_t>(member.bit_offset),
      static_cast<std::uint32_t>(member.bit_width), result_type);
}

// LRM 8.4: a class property write reaches the object through the handle.
// The place is the same owner-qualified `FieldAccessExpr` the read produces,
// so the write and read share one path (a class field is a reference-storage
// receiver, and the mutate flow is the usual observable-cell path when the
// property is itself an observable cell).
template <ExprLowerer Lowerer>
auto LowerHirClassPropertyAccessExprLhs(
    Lowerer& lowerer, WalkFrame frame, const hir::ClassPropertyAccessExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto& unit_lowerer = lowerer.Owner();
  auto& block = *frame.current_block;
  const auto& base_hir_expr = lowerer.HirExprs().Get(sel.base_value);
  auto base_or = lowerer.LowerExpr(base_hir_expr, frame);
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const mir::ExprId base_id = block.exprs.Add(*std::move(base_or));
  return mir::MakeFieldAccessExpr(
      base_id, unit_lowerer.TranslateClassPropertyTarget(sel.target),
      result_type);
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
template auto LowerHirClassPropertyAccessExpr(
    ProcessLowerer&, WalkFrame, const hir::ClassPropertyAccessExpr&,
    mir::TypeId) -> diag::Result<mir::Expr>;
template auto LowerHirClassPropertyAccessExpr(
    const StructuralScopeLowerer&, WalkFrame,
    const hir::ClassPropertyAccessExpr&, mir::TypeId)
    -> diag::Result<mir::Expr>;
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
template auto LowerHirClassPropertyAccessExprLhs(
    ProcessLowerer&, WalkFrame, const hir::ClassPropertyAccessExpr&,
    mir::TypeId) -> diag::Result<mir::Expr>;
template auto LowerHirClassPropertyAccessExprLhs(
    const StructuralScopeLowerer&, WalkFrame,
    const hir::ClassPropertyAccessExpr&, mir::TypeId)
    -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
