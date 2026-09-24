#include "lyra/lowering/hir_to_mir/expression/selects.hpp"

#include <cstdint>
#include <expected>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/component_index.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/binary_op.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/expression/operators.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/lowering/hir_to_mir/lhs_store.hpp"
#include "lyra/lowering/hir_to_mir/packed_projection.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/select_position.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/mir/type_descriptor.hpp"

// HIR-to-MIR lowering for the three select families (`a[i]`, `a[hi:lo]`,
// `s.field`). Each family has a read-side and a write-side entry point; a
// select's meaning is independent of whether a process or a structural scope
// encloses it, so each entry point is one template over the pass class. Both
// sides state the step a select takes the same way, so reading a part and
// designating one cannot disagree about where it is.
//
// A select is written in the coordinates the declaration chose, and what it
// hands the value below is the value's own numbering from zero: this is where
// one becomes the other, because this is the layer that still knows which
// declaration the coordinates belong to.
//
// Naming convention used here, matching the rest of HIR-to-MIR:
//   - `Lower*` -- top-level HIR-to-MIR for a HIR construct, returns
//     `diag::Result<mir::Expr>`. The caller commits the returned node.
//   - `Build*` -- factory for a specific MIR node shape, returns `mir::Expr`
//     (or `diag::Result<mir::Expr>`). Does not commit unless documented.
//   - `Wrap*`  -- transforms an existing node into another node; may commit
//     intermediate steps as a side effect.

namespace lyra::lowering::hir_to_mir {

namespace {

auto ProjectedMemberAt(
    const PackedProjection& projection, base::ComponentIndex index)
    -> const ProjectedMember& {
  if (index.value >= projection.members.size()) {
    throw InternalError("ProjectedMemberAt: member index out of range");
  }
  return projection.members[index.value];
}

// The value a select's receiver holds, which is what numbers the select's
// coordinates: the receiver itself, or the storage a cell it names stands for.
auto ReceiverValueType(const mir::CompilationUnit& unit, mir::TypeId receiver)
    -> mir::TypeId {
  return mir::ValueTypeOf(unit, receiver);
}

// The read `step` takes from `receiver`: its value entry, answering at the
// part's type.
auto MakeStepRead(
    const mir::CompilationUnit& unit, mir::Block& block,
    const DescentStep& step, mir::ExprId receiver) -> mir::Expr {
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Direct{.target = step.value_entry, .receiver = receiver},
              .arguments = StepArguments(unit, block, step)},
      .type = step.part_type};
}

// A run of a fixed count of parts: where it starts in the receiver's own
// numbering, and how many parts it takes. A packed value's bit-select,
// part-select and aggregate member, and an unpacked array's slice, are all this
// one step.
auto RunStep(mir::ExprId start, std::uint64_t count, mir::TypeId part_type)
    -> DescentStep {
  return DescentStep{
      .value_entry = support::BuiltinFn::kSlice,
      .part_entry = support::BuiltinFn::kSliceRef,
      .position = std::nullopt,
      .operands = {start},
      .count = count,
      .part_type = part_type};
}

// Read-side wrap that materialises a borrowed packed view into an owning value
// (Rust's `&[T]::to_owned() -> Vec<T>` pattern). A non-packed receiver falls
// through unchanged because its access already returns an owning value.
auto WrapPackedAsOwned(
    const mir::CompilationUnit& unit, mir::Block& block, mir::Expr access_call,
    mir::TypeId result_type) -> mir::Expr {
  if (!unit.types.Get(result_type).IsIntegralPacked()) {
    return access_call;
  }
  const mir::ExprId access_id = block.exprs.Add(std::move(access_call));
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Direct{
                      .target = support::BuiltinFn::kToOwned,
                      .receiver = access_id},
              .arguments = {}},
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
  if (!source.IsIntegralPacked() || !field.IsIntegralPacked()) {
    return field_type;
  }
  mir::PackedArrayType natural = field.PackedShape();
  natural.signedness = mir::Signedness::kUnsigned;
  natural.state_kind = source.PackedShape().state_kind;
  return unit.types.Intern(mir::Type{std::move(natural)});
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

// LRM 7.4.5 / 7.4.6 / 7.10.1 / 11.5.1 `arr[hi:lo]`, `arr[base+:w]` and
// `arr[base-:w]`, as the step they take into the receiver.
//
// A packed value and a fixed-size or dynamic array take a run of a fixed
// count, which the select's own result type states, starting at the part of
// the run lowest in the receiver's numbering. For a constant range that is the
// bound the declaration's direction puts there -- the right bound of a packed
// value, whose numbering starts at its least significant bit, and the left
// bound of an unpacked one, whose numbering starts at its left (the front end
// has already held the range to the declaration's direction). An indexed
// select counts `w` from its base; where positions grow opposite to the way the
// select counts, the run starts `w - 1` steps below the base.
//
// A queue's slice is bounded by two positions instead, which the running
// program can move (`$`, LRM 7.10.1), so its count is the queue's to work out.
template <typename LowerOne>
auto RangeStep(
    UnitLowerer& unit_lowerer, mir::Block& block,
    const hir::RangeBounds& bounds, mir::TypeId receiver_type,
    mir::TypeId result_type, LowerOne lower_one) -> diag::Result<DescentStep> {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  const mir::TypeId value_type = ReceiverValueType(unit, receiver_type);
  const mir::Type& receiver = unit.types.Get(value_type);

  if (receiver.Is<mir::QueueType>()) {
    struct Bounds {
      mir::ExprId lo;
      mir::ExprId hi;
    };
    // `base +: w` spans `base` through `base + w - 1`, and `base -: w` spans
    // `base - w + 1` through `base`, both in positions, since a queue is
    // declared zero-based.
    const auto span = [&](hir::ExprId base, hir::ExprId width,
                          bool up) -> diag::Result<Bounds> {
      auto base_id = lower_one(base);
      if (!base_id) return std::unexpected(std::move(base_id.error()));
      auto width_id = lower_one(width);
      if (!width_id) return std::unexpected(std::move(width_id.error()));
      const mir::ExprId other =
          BuildSpanEnd(unit, block, *base_id, *width_id, up);
      return up ? Bounds{.lo = *base_id, .hi = other}
                : Bounds{.lo = other, .hi = *base_id};
    };
    auto queue_bounds = std::visit(
        Overloaded{
            [&](const hir::RangeConstantBounds& c) -> diag::Result<Bounds> {
              auto lo = lower_one(c.left_bound);
              if (!lo) return std::unexpected(std::move(lo.error()));
              auto hi = lower_one(c.right_bound);
              if (!hi) return std::unexpected(std::move(hi.error()));
              return Bounds{.lo = *lo, .hi = *hi};
            },
            [&](const hir::RangeIndexedUpBounds& c) -> diag::Result<Bounds> {
              return span(c.base_index, c.width, true);
            },
            [&](const hir::RangeIndexedDownBounds& c) -> diag::Result<Bounds> {
              return span(c.base_index, c.width, false);
            },
        },
        bounds);
    if (!queue_bounds) return std::unexpected(std::move(queue_bounds.error()));
    return DescentStep{
        .value_entry = support::BuiltinFn::kSlice,
        .part_entry = support::BuiltinFn::kSliceRef,
        .position = std::nullopt,
        .operands = {queue_bounds->lo, queue_bounds->hi},
        .count = std::nullopt,
        .part_type = result_type};
  }

  const PositionMap map = PositionMapOf(unit, value_type);
  // How many positions the run covers, which the select's own result type
  // states: its bits, or its elements.
  const mir::Type& result = unit.types.Get(result_type);
  const std::uint64_t run = result.IsIntegralPacked()
                                ? result.PackedShape().BitWidth()
                                : result.Get<mir::UnpackedArrayType>().Size();
  // The shift from the base's own position to the run's lowest when the run
  // counts toward lower positions: back over every position the run covers
  // beyond the base's own step.
  const std::int64_t below = map.step - static_cast<std::int64_t>(run);
  // The index the run's lowest position is named by, and how far below that
  // index's own position the run starts.
  struct Start {
    hir::ExprId index;
    std::int64_t shift = 0;
  };
  const Start start = std::visit(
      Overloaded{
          [&](const hir::RangeConstantBounds& c) {
            return Start{
                .index = map.from_right ? c.right_bound : c.left_bound,
                .shift = 0};
          },
          [&](const hir::RangeIndexedUpBounds& c) {
            return Start{
                .index = c.base_index, .shift = map.reversed ? below : 0};
          },
          [&](const hir::RangeIndexedDownBounds& c) {
            return Start{
                .index = c.base_index, .shift = map.reversed ? 0 : below};
          },
      },
      bounds);
  auto index = lower_one(start.index);
  if (!index) return std::unexpected(std::move(index.error()));
  return RunStep(
      WrapIndexAsPosition(unit, block, map, *index, start.shift), run,
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
      base_is_cell
          ? block.exprs.Add(mir::MakeCellLoadCallExpr(base_id, value_type))
          : base_id;
  const mir::ExprId test =
      BuildPackedTagTest(unit_lowerer, block, tag_subject, projection, index);
  const mir::ExprId message_id = block.exprs.Add(
      mir::MakeStringLiteral(unit.builtins.string, std::string{message}));
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Direct{
                      .target = support::BuiltinFn::kRequire,
                      .receiver = base_id},
              .arguments = {test, message_id}},
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
  mir::Expr owned = BuildPackedRunRead(
      unit_lowerer, block, subject, member.bit_offset, member.bit_width,
      slice_type);
  return WrapSliceToDeclaredType(
      unit_lowerer.Unit(), block, std::move(owned), result_type);
}

// The member an unpacked aggregate's dot access reaches, and nothing where the
// base is packed and its member is a window into one bit vector instead.
// Reaching a product's component and reaching a union's member are different
// operations: every component of a product is live at once, while a union holds
// one member at a time and a tagged union carries which (LRM 7.2 / 7.3 /
// 7.3.2). The aggregate the access is written against is what settles which.
// Writing a member reaches it the same way reading it does; where the
// occurrence stands is what makes one of them a write.
auto UnpackedMemberReach(
    const hir::Type& base_ty, mir::ExprId base_id, base::ComponentIndex index,
    mir::TypeId member_type) -> std::optional<mir::Expr> {
  if (base_ty.Is<hir::UnpackedStructType>() ||
      base_ty.Is<hir::UnpackedUnionType>()) {
    return mir::MakePartAccessExpr(base_id, index, member_type);
  }
  return std::nullopt;
}

}  // namespace

auto ElementStep(
    UnitLowerer& unit_lowerer, mir::Block& block, mir::TypeId receiver_type,
    mir::ExprId idx_id, mir::TypeId part_type) -> DescentStep {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  const mir::TypeId value_type = ReceiverValueType(unit, receiver_type);
  const mir::Type& receiver = unit.types.Get(value_type);
  // An associative array is reached by the key the source wrote, which is a
  // value of the index type rather than a place in any order (LRM 7.8).
  if (receiver.Is<mir::AssociativeArrayType>()) {
    return DescentStep{
        .value_entry = support::BuiltinFn::kElement,
        .part_entry = support::BuiltinFn::kElementRef,
        .position = std::nullopt,
        .operands = {idx_id},
        .count = std::nullopt,
        .part_type = part_type};
  }
  const PositionMap map = PositionMapOf(unit, value_type);
  const mir::ExprId position = WrapIndexAsPosition(unit, block, map, idx_id, 0);
  if (receiver.IsIntegralPacked()) {
    return RunStep(position, static_cast<std::uint64_t>(map.step), part_type);
  }
  return DescentStep{
      .value_entry = support::BuiltinFn::kElement,
      .part_entry = support::BuiltinFn::kElementRef,
      .position = std::nullopt,
      .operands = {position},
      .count = std::nullopt,
      .part_type = part_type};
}

auto BuildElementAccessCallExpr(
    UnitLowerer& unit_lowerer, mir::Block& block, mir::ExprId base_id,
    mir::ExprId idx_id, mir::TypeId result_type) -> mir::Expr {
  const DescentStep step = ElementStep(
      unit_lowerer, block, block.exprs.Get(base_id).type, idx_id, result_type);
  return MakeStepRead(unit_lowerer.Unit(), block, step, base_id);
}

auto BuildPackedRunRead(
    UnitLowerer& unit_lowerer, mir::Block& block, mir::ExprId base,
    std::uint64_t bit_offset, std::uint64_t bit_width, mir::TypeId result_type)
    -> mir::Expr {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  const DescentStep step = RunStep(
      BuildConstantPosition(unit, block, static_cast<std::int64_t>(bit_offset)),
      bit_width, result_type);
  return WrapPackedAsOwned(
      unit, block, MakeStepRead(unit, block, step, base), result_type);
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
  const mir::TypeId tag_type = mir::PackedVectorOf(
      unit.types, projection.tag_bits, projection.state_kind);
  const mir::ExprId tag = block.exprs.Add(BuildPackedRunRead(
      unit_lowerer, block, base, projection.bit_width - projection.tag_bits,
      projection.tag_bits, tag_type));
  const mir::ExprId named =
      BuildIntLiteral(unit, block, static_cast<std::int64_t>(index.value));
  return block.exprs.Add(BuildMirBinaryExpr(
      unit, block, hir::BinaryOp::kCaseEquality, tag,
      ConvertToType(unit, block, named, tag_type), unit.builtins.bit1));
}

template <ExprLowerer Lowerer>
auto LowerHirElementSelectExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::ElementSelectExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
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

  const hir::Type& hir_base_ty = unit_lowerer.Hir().types.Get(hir_base.type);
  // LRM 6.16: indexed character read `s[i]` is the element-value access, the
  // read-side dual of the element-reference write. It answers with the
  // character itself, so there is no view to materialise.
  mir::Expr access_call = BuildElementAccessCallExpr(
      unit_lowerer, block, base_id, idx_id, result_type);
  if (hir_base_ty.Is<hir::StringType>()) {
    return access_call;
  }
  return WrapPackedAsOwned(
      unit_lowerer.Unit(), block, std::move(access_call), result_type);
}

template <ExprLowerer Lowerer>
auto LowerHirRangeSelectExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::RangeSelectExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
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
  auto step = RangeStep(
      unit_lowerer, block, sel.bounds, block.exprs.Get(base_id).type,
      result_type, lower_one);
  if (!step) return std::unexpected(std::move(step.error()));
  return WrapPackedAsOwned(
      unit_lowerer.Unit(), block,
      MakeStepRead(unit_lowerer.Unit(), block, *step, base_id), result_type);
}

// LRM 7.2.1: packed struct / union field access "can be selected as if it
// were a packed array". HIR -> MIR resolves the field-table index to the run of
// bits the field occupies -- the same MIR shape `s[hi:lo]` produces.
template <ExprLowerer Lowerer>
auto LowerHirMemberAccessExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::MemberAccessExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  const auto& exprs = lowerer.HirExprs();
  auto& block = *frame.current_block;
  const auto& base_hir_expr = exprs.Get(sel.base_value);
  const hir::Type& base_ty = unit_lowerer.Hir().types.Get(base_hir_expr.type);
  auto base_or = lowerer.LowerExpr(base_hir_expr, frame);
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const mir::ExprId base_id = block.exprs.Add(*std::move(base_or));
  if (std::optional<mir::Expr> member =
          UnpackedMemberReach(base_ty, base_id, sel.field_index, result_type)) {
    return *std::move(member);
  }
  const PackedProjection projection =
      ProjectPackedAggregate(unit_lowerer, base_ty);
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
  auto& block = *frame.current_block;
  const auto& base_hir_expr = lowerer.HirExprs().Get(sel.base_value);
  auto base_or = lowerer.LowerExpr(base_hir_expr, frame);
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const mir::ExprId base_id = block.exprs.Add(*std::move(base_or));
  return BuildClassPropertyAccess(
      lowerer, frame, base_id, sel.target, result_type);
}

template <ExprLowerer Lowerer>
auto LowerHirElementSelectExprLhs(
    Lowerer& lowerer, WalkFrame frame, const hir::ElementSelectExpr& sel,
    mir::TypeId result_type) -> diag::Result<WriteTarget> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  const auto& exprs = lowerer.HirExprs();
  auto& block = *frame.current_block;

  const auto& hir_base = exprs.Get(sel.base_value);
  auto base_or = lowerer.LowerLhsExpr(hir_base, frame);
  if (!base_or) return std::unexpected(std::move(base_or.error()));

  const auto& hir_idx = exprs.Get(sel.index);
  auto idx_or = lowerer.LowerExpr(hir_idx, frame);
  if (!idx_or) return std::unexpected(std::move(idx_or.error()));
  const mir::ExprId idx_id = block.exprs.Add(*std::move(idx_or));

  const mir::TypeId container =
      TargetValueType(unit_lowerer.Unit(), block, *base_or);
  return DescendInto(
      *std::move(base_or),
      ElementStep(unit_lowerer, block, container, idx_id, result_type));
}

template <ExprLowerer Lowerer>
auto LowerHirRangeSelectExprLhs(
    Lowerer& lowerer, WalkFrame frame, const hir::RangeSelectExpr& sel,
    mir::TypeId result_type) -> diag::Result<WriteTarget> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
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
  const mir::TypeId container =
      TargetValueType(unit_lowerer.Unit(), block, *base_or);
  auto step = RangeStep(
      unit_lowerer, block, sel.bounds, container, result_type, lower_one);
  if (!step) return std::unexpected(std::move(step.error()));
  return DescendInto(*std::move(base_or), *std::move(step));
}

template <ExprLowerer Lowerer>
auto LowerHirMemberAccessExprLhs(
    Lowerer& lowerer, WalkFrame frame, const hir::MemberAccessExpr& sel,
    mir::TypeId result_type) -> diag::Result<WriteTarget> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  const auto& exprs = lowerer.HirExprs();
  auto& block = *frame.current_block;
  const auto& base_hir_expr = exprs.Get(sel.base_value);
  const hir::Type& base_ty = unit_lowerer.Hir().types.Get(base_hir_expr.type);
  auto base_or = lowerer.LowerLhsExpr(base_hir_expr, frame);
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  // An unpacked aggregate's member is named by its declaration-order position.
  // Whether every part is live at once or one at a time, and what replacing one
  // settles, is the value's own semantics and reaches the step through the
  // domain its type names.
  if (base_ty.Is<hir::UnpackedStructType>() ||
      base_ty.Is<hir::UnpackedUnionType>()) {
    return DescendInto(
        *std::move(base_or), DescentStep{
                                 .value_entry = support::BuiltinFn::kPart,
                                 .part_entry = support::BuiltinFn::kPartRef,
                                 .position = sel.field_index,
                                 .operands = {},
                                 .count = std::nullopt,
                                 .part_type = result_type});
  }
  const PackedProjection projection =
      ProjectPackedAggregate(unit_lowerer, base_ty);
  const ProjectedMember& member =
      ProjectedMemberAt(projection, sel.field_index);
  // A tag mismatch fails the write (LRM 11.9), and the check is a step of the
  // statement rather than a link in the descent: nothing short-circuits a write
  // target, so the check has no occurrence to be evaluated inside of, and the
  // descent stays the one window step a packed member always is.
  if (projection.tag_bits != 0) {
    const mir::ExprId subject =
        ReadTargetValue(unit_lowerer.Unit(), block, *base_or);
    const mir::ExprId guard = block.exprs.Add(BuildTagGuard(
        unit_lowerer, block, subject, projection, sel.field_index,
        "write to a tagged union member inconsistent with the current tag "
        "(LRM 11.9)"));
    block.AppendStmt(mir::ExprStmt{.expr = guard});
  }
  return DescendInto(
      *std::move(base_or),
      RunStep(
          BuildConstantPosition(
              unit_lowerer.Unit(), block,
              static_cast<std::int64_t>(member.bit_offset)),
          member.bit_width, result_type));
}

// LRM 8.4: a class property write reaches the object through the handle. The
// place is the same one the read produces, so the write and read share one path
// (a class field is a reference-storage receiver, and the mutate flow is the
// usual observable-cell path when the property is itself an observable cell).
template <ExprLowerer Lowerer>
auto LowerHirClassPropertyAccessExprLhs(
    Lowerer& lowerer, WalkFrame frame, const hir::ClassPropertyAccessExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto& block = *frame.current_block;
  const auto& base_hir_expr = lowerer.HirExprs().Get(sel.base_value);
  auto base_or = lowerer.LowerExpr(base_hir_expr, frame);
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const mir::ExprId base_id = block.exprs.Add(*std::move(base_or));
  return BuildClassPropertyAccess(
      lowerer, frame, base_id, sel.target, result_type);
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
    -> diag::Result<WriteTarget>;
template auto LowerHirElementSelectExprLhs(
    const StructuralScopeLowerer&, WalkFrame, const hir::ElementSelectExpr&,
    mir::TypeId) -> diag::Result<WriteTarget>;
template auto LowerHirRangeSelectExprLhs(
    ProcessLowerer&, WalkFrame, const hir::RangeSelectExpr&, mir::TypeId)
    -> diag::Result<WriteTarget>;
template auto LowerHirRangeSelectExprLhs(
    const StructuralScopeLowerer&, WalkFrame, const hir::RangeSelectExpr&,
    mir::TypeId) -> diag::Result<WriteTarget>;
template auto LowerHirMemberAccessExprLhs(
    ProcessLowerer&, WalkFrame, const hir::MemberAccessExpr&, mir::TypeId)
    -> diag::Result<WriteTarget>;
template auto LowerHirMemberAccessExprLhs(
    const StructuralScopeLowerer&, WalkFrame, const hir::MemberAccessExpr&,
    mir::TypeId) -> diag::Result<WriteTarget>;
template auto LowerHirClassPropertyAccessExprLhs(
    ProcessLowerer&, WalkFrame, const hir::ClassPropertyAccessExpr&,
    mir::TypeId) -> diag::Result<mir::Expr>;
template auto LowerHirClassPropertyAccessExprLhs(
    const StructuralScopeLowerer&, WalkFrame,
    const hir::ClassPropertyAccessExpr&, mir::TypeId)
    -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
