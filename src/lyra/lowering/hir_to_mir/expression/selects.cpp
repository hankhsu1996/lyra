#include "lyra/lowering/hir_to_mir/expression/selects.hpp"

#include <cstddef>
#include <cstdint>
#include <expected>
#include <optional>
#include <span>
#include <string>
#include <string_view>
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
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/block_builder.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/expression/operators.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/lowering/hir_to_mir/packed_projection.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/select_position.hpp"
#include "lyra/lowering/hir_to_mir/sensitivity_wait.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/unit_object_access.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/mir/type_descriptor.hpp"

// HIR-to-MIR lowering for the three ways the source selects part of a value:
// `a[i]`, `a[hi:lo]` with its `+:` and `-:` forms, and `s.member`.
//
// A select is lowered in two moves. First it becomes a step: which entry
// reaches the part, at what position in the selected value's own numbering,
// and for a slice of a fixed size how many parts it takes. What kind of part
// that is follows from what is selected from and not from how it was written:
//
//   logic [7:0] v;            v[3]    a slice of one bit, at position 3
//   int a[1:8];               a[k]    an element, at position k - 1
//   struct packed {..} s;     s.b     a slice, at the bits `b` occupies
//   struct {..} u;            u.b     a component, by its declaration order
//
// A select is written in the coordinates the declaration chose, and a step
// states a position counted from zero: this is where one becomes the other,
// because this is the layer that still knows which declaration the coordinates
// belong to (LRM 7.4.5, 11.5.1). There is one function per source form, and
// nothing else places a part.
//
// Then the step is used. A select read as a value calls the step's value entry
// on its base, lowered as a value. A select named as a part -- by a write, a
// reference, a wait, a join of nets -- adds the step to the path its base
// names, so `s.b[1]` is the owner `s` and two steps, and whatever is done with
// the part is done to that one statement of it.
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

// The value a select's receiver holds, which is what numbers the select's
// coordinates: the receiver itself, or the storage a cell it names stands for.
auto ReceiverValueType(const mir::CompilationUnit& unit, mir::TypeId receiver)
    -> mir::TypeId {
  return mir::ValueTypeOf(unit, receiver);
}

// A fixed count of parts in a row: where they start in the receiver's own
// numbering, and how many there are. A packed value's bit-select, part-select
// and aggregate member, and an unpacked array's slice, are all this one step.
auto SliceStep(mir::ExprId start, std::uint64_t count, mir::TypeId part_type)
    -> DescentStep {
  return DescentStep{
      .value_entry = support::BuiltinFn::kSlice,
      .part_entry = support::BuiltinFn::kSliceRef,
      .position = std::nullopt,
      .operands = {start},
      .count = count,
      .part_type = part_type};
}

// The two positions bounding a queue's slice, lowest first.
struct QueueBounds {
  mir::ExprId lo;
  mir::ExprId hi;
};

// `base +: w` spans `base` through `base + w - 1`, and `base -: w` spans
// `base - w + 1` through `base`, both in positions, since a queue is declared
// zero-based.
template <ExprLowerer Lowerer>
auto QueueSpan(
    Lowerer& lowerer, WalkFrame frame, hir::ExprId base, hir::ExprId width,
    bool up) -> diag::Result<QueueBounds> {
  mir::Block& block = *frame.current_block;
  auto base_or = lowerer.LowerExpr(lowerer.HirExprs().Get(base), frame);
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  // The base is one bound and the other is counted from it, so it is
  // evaluated once and both read that.
  const mir::ExprId base_id =
      EvaluatedOnce(frame, block.exprs.Add(*std::move(base_or)));
  auto width_or = lowerer.LowerExpr(lowerer.HirExprs().Get(width), frame);
  if (!width_or) return std::unexpected(std::move(width_or.error()));
  const mir::ExprId width_id = block.exprs.Add(*std::move(width_or));
  const mir::ExprId other =
      BuildSpanEnd(lowerer.Owner().Unit(), block, base_id, width_id, up);
  return up ? QueueBounds{.lo = base_id, .hi = other}
            : QueueBounds{.lo = other, .hi = base_id};
}

// A queue's slice is bounded by two positions, which the running program can
// move (`$`, LRM 7.10.1), so how many elements it takes is the queue's to work
// out.
template <ExprLowerer Lowerer>
auto QueueSliceStep(
    Lowerer& lowerer, WalkFrame frame, const hir::RangeBounds& bounds,
    mir::TypeId result_type) -> diag::Result<DescentStep> {
  mir::Block& block = *frame.current_block;
  auto queue_bounds = std::visit(
      Overloaded{
          [&](const hir::RangeConstantBounds& c) -> diag::Result<QueueBounds> {
            auto lo =
                lowerer.LowerExpr(lowerer.HirExprs().Get(c.left_bound), frame);
            if (!lo) return std::unexpected(std::move(lo.error()));
            const mir::ExprId lo_id = block.exprs.Add(*std::move(lo));
            auto hi =
                lowerer.LowerExpr(lowerer.HirExprs().Get(c.right_bound), frame);
            if (!hi) return std::unexpected(std::move(hi.error()));
            return QueueBounds{
                .lo = lo_id, .hi = block.exprs.Add(*std::move(hi))};
          },
          [&](const hir::RangeIndexedUpBounds& c) {
            return QueueSpan(lowerer, frame, c.base_index, c.width, true);
          },
          [&](const hir::RangeIndexedDownBounds& c) {
            return QueueSpan(lowerer, frame, c.base_index, c.width, false);
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

// The step `receiver[hi:lo]`, `receiver[base+:w]` and `receiver[base-:w]` take
// (LRM 7.4.5 / 7.4.6 / 7.10.1 / 11.5.1).
//
// A packed value and a fixed-size or dynamic array take a slice of a fixed
// count, which the select's own result type states, starting at the part of
// the slice lowest in the receiver's numbering. For a constant range that is
// the bound the declaration's direction puts there -- the right bound of a
// packed value, whose numbering starts at its least significant bit, and the
// left bound of an unpacked one, whose numbering starts at its left (the front
// end has already held the range to the declaration's direction). An indexed
// select counts `w` from its base; where positions grow opposite to the way the
// select counts, the slice starts `w - 1` steps below the base.
template <ExprLowerer Lowerer>
auto RangeStep(
    Lowerer& lowerer, WalkFrame frame, const hir::RangeBounds& bounds,
    mir::TypeId receiver_type, mir::TypeId result_type)
    -> diag::Result<DescentStep> {
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  mir::Block& block = *frame.current_block;
  const mir::TypeId value_type = ReceiverValueType(unit, receiver_type);
  if (unit.types.Get(value_type).Is<mir::QueueType>()) {
    return QueueSliceStep(lowerer, frame, bounds, result_type);
  }

  const PositionMap map = PositionMapOf(unit, value_type);
  // How many positions the slice covers, which the select's own result type
  // states: its bits, or its elements.
  const mir::Type& result = unit.types.Get(result_type);
  const std::uint64_t count = result.IsIntegralPacked()
                                  ? result.PackedShape().BitWidth()
                                  : result.Get<mir::UnpackedArrayType>().Size();
  // The shift from the base's own position to the slice's lowest when the
  // slice counts toward lower positions: back over every position the slice
  // covers beyond the base's own step.
  const std::int64_t below = map.step - static_cast<std::int64_t>(count);
  // The index the slice's lowest position is named by, and how far below that
  // index's own position the slice starts.
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
  auto index = lowerer.LowerExpr(lowerer.HirExprs().Get(start.index), frame);
  if (!index) return std::unexpected(std::move(index.error()));
  const mir::ExprId index_id = block.exprs.Add(*std::move(index));
  return SliceStep(
      WrapIndexAsPosition(unit, block, map, index_id, start.shift), count,
      result_type);
}

// The tag a step into member `index` of the tagged union `projection`
// describes needs the union to carry.
auto RequiredTagOf(
    const PackedProjection& projection, base::ComponentIndex index)
    -> RequiredTag {
  if (projection.tag_bits == 0) {
    throw InternalError("RequiredTagOf: value carries no tag to test against");
  }
  return RequiredTag{.tag_bits = projection.tag_bits, .member = index};
}

// The step member `index` of a packed aggregate takes: the bits of the
// aggregate's vector the member occupies (LRM 7.2.1, 7.3.1), and the tag the
// aggregate has to carry where it is a tagged union naming more than one
// member.
auto PackedMemberStep(
    mir::CompilationUnit& unit, mir::Block& block,
    const PackedProjection& projection, base::ComponentIndex index,
    mir::TypeId part_type) -> DescentStep {
  if (index.value >= projection.members.size()) {
    throw InternalError("PackedMemberStep: member index out of range");
  }
  const ProjectedMember& member = projection.members[index.value];
  DescentStep step = SliceStep(
      BuildConstantPosition(
          unit, block, static_cast<std::int64_t>(member.bit_offset)),
      member.bit_width, part_type);
  if (projection.tag_bits != 0) {
    step.required_tag = RequiredTagOf(projection, index);
  }
  return step;
}

// The step `aggregate.member` takes (LRM 7.2, 7.3). An unpacked aggregate's
// member is storage of its own, named by its declaration-order position;
// whether every part is live at once or one at a time is the value's own
// semantics and reaches the step through the domain its type names. A packed
// aggregate's member is bits of the one vector the aggregate is.
auto MemberStep(
    UnitLowerer& unit_lowerer, mir::Block& block, const hir::Type& aggregate,
    base::ComponentIndex index, mir::TypeId part_type) -> DescentStep {
  if (aggregate.Is<hir::UnpackedStructType>() ||
      aggregate.Is<hir::UnpackedUnionType>()) {
    return DescentStep{
        .value_entry = support::BuiltinFn::kComponent,
        .part_entry = support::BuiltinFn::kComponentRef,
        .position = index,
        .operands = {},
        .count = std::nullopt,
        .part_type = part_type};
  }
  return PackedMemberStep(
      unit_lowerer.Unit(), block,
      ProjectPackedAggregate(unit_lowerer, aggregate), index, part_type);
}

// Whether reaching a member of `aggregate` is checked against a tag carried in
// the aggregate's own bits (LRM 11.9), which is so only of a packed tagged
// union naming more than one member.
auto CarriesATag(const UnitLowerer& unit_lowerer, const hir::Type& aggregate)
    -> bool {
  return aggregate.Is<hir::PackedUnionType>() &&
         ProjectPackedAggregate(unit_lowerer, aggregate).tag_bits != 0;
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

// The one-bit test that the tag the value `base` carries is `tag`. Bit-pattern
// equality, not a logical compare, so a tag carrying x or z names no member.
auto BuildTagTest(
    UnitLowerer& unit_lowerer, mir::Block& block, mir::ExprId base,
    const RequiredTag& tag) -> mir::ExprId {
  auto& unit = unit_lowerer.Unit();
  // The tag is the most significant bits of the union's own vector (LRM 7.3.2),
  // so where it starts and the state domain it is read in are the union's.
  const mir::TypeId union_type = block.exprs.Get(base).type;
  const std::uint64_t union_bits =
      unit.types.Get(union_type).PackedShape().BitWidth();
  const mir::IntegralStateKind state_kind =
      unit.types.Get(union_type).PackedShape().state_kind;
  const mir::TypeId tag_type =
      mir::PackedVectorOf(unit.types, tag.tag_bits, state_kind);
  const mir::ExprId carried = block.exprs.Add(BuildPackedBitsRead(
      unit_lowerer, block, base, union_bits - tag.tag_bits, tag.tag_bits,
      tag_type));
  const mir::ExprId named =
      BuildIntLiteral(unit, block, static_cast<std::int64_t>(tag.member.value));
  return block.exprs.Add(BuildMirBinaryExpr(
      unit, block, hir::BinaryOp::kCaseEquality, carried,
      ConvertToType(unit, block, named, tag_type), unit.builtins.bit1));
}

// The value `base_id` names, guarded by its tag being `tag` (LRM 11.9). The
// guard yields that value, so the member's own slice composes onto it. In a
// read the check has to be part of evaluating the access and not a test
// hoisted ahead of it: LRM 11.3.5 requires a short-circuited operand to raise
// none of the run-time errors its evaluation would have. The guard names
// `base_id` twice, to test it and to yield it, so `base_id` is a read that
// evaluates nothing.
auto BuildTagGuard(
    UnitLowerer& unit_lowerer, mir::Block& block, mir::ExprId base_id,
    const RequiredTag& tag, std::string_view message) -> mir::Expr {
  auto& unit = unit_lowerer.Unit();
  const mir::TypeId value_type = block.exprs.Get(base_id).type;
  const mir::ExprId test = BuildTagTest(unit_lowerer, block, base_id, tag);
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

// The value of the member `step` reaches in the value `base_id` names, at the
// member's declared type. A component is a value of its own. A packed member
// is a view of the aggregate's vector (LRM 7.2.1: it "can be selected as if it
// were a packed array"), so it is read at its part-select natural type, kept,
// and brought to the declared type, behind the tag check where the aggregate is
// a tagged union.
auto ReadMember(
    UnitLowerer& unit_lowerer, mir::Block& block, const DescentStep& step,
    mir::ExprId base_id, mir::TypeId result_type) -> mir::Expr {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  if (step.position.has_value()) {
    return StepRead(unit, block, step, base_id);
  }
  const mir::ExprId subject =
      step.required_tag.has_value()
          ? block.exprs.Add(BuildTagGuard(
                unit_lowerer, block, base_id, *step.required_tag,
                "read of a tagged union member inconsistent with the current "
                "tag (LRM 11.9)"))
          : base_id;
  mir::Expr owned =
      OwnedValue(unit, block, StepRead(unit, block, step, subject));
  return WrapSliceToDeclaredType(unit, block, std::move(owned), result_type);
}

// Whether a read of `expr` finds its value lying in storage, as opposed to
// being handed a value that lies nowhere: a name, or a part of what a name
// reaches. This is a question about reading and not about what may be written:
// a concatenation names destinations to a write and is still a value built
// from its operands to a read.
template <ExprLowerer Lowerer>
auto LiesInStorage(Lowerer& lowerer, const hir::Expr& expr) -> bool {
  const auto base_lies = [&](hir::ExprId base) {
    return LiesInStorage(lowerer, lowerer.HirExprs().Get(base));
  };
  return std::visit(
      Overloaded{
          [](const hir::PrimaryExpr&) { return true; },
          [&](const hir::ElementSelectExpr& e) {
            return base_lies(e.base_value);
          },
          [&](const hir::RangeSelectExpr& e) {
            return base_lies(e.base_value);
          },
          [&](const hir::MemberAccessExpr& e) {
            return base_lies(e.base_value);
          },
          // A property lies in the object its handle reaches (LRM 8.4), however
          // the handle was come by.
          [](const hir::ClassPropertyAccessExpr&) { return true; },
          [](const hir::InterfaceMemberAccessExpr&) { return true; },
          [](const hir::UnaryExpr&) { return false; },
          [](const hir::BinaryExpr&) { return false; },
          [](const hir::ConditionalExpr&) { return false; },
          [](const hir::AssignExpr&) { return false; },
          [](const hir::IncDecExpr&) { return false; },
          [](const hir::CallExpr&) { return false; },
          [](const hir::ConversionExpr&) { return false; },
          [](const hir::ValueRangeExpr&) { return false; },
          [](const hir::InsideExpr&) { return false; },
          [](const hir::InterfaceInstanceAccessExpr&) { return false; },
          [](const hir::ConcatExpr&) { return false; },
          [](const hir::StreamingConcatExpr&) { return false; },
          [](const hir::ReplicationExpr&) { return false; },
          [](const hir::AssignmentPatternExpr&) { return false; },
          [](const hir::AssignmentPatternReplicationExpr&) { return false; },
          [](const hir::DynamicArrayNewExpr&) { return false; },
          [](const hir::ClassNewExpr&) { return false; },
          [](const hir::AssociativeAssignmentPatternExpr&) { return false; },
          [](const hir::AssignmentPatternKeyedExpr&) { return false; },
          [](const hir::TaggedUnionExpr&) { return false; },
          [](const hir::DynamicCastExpr&) { return false; }},
      expr.data);
}

// The queue a select read as a value is taken from, evaluated here, once (LRM
// 11.4.1: the source wrote it once, and both the select and each `$` under it
// take it). A queue lying in storage is read where it lies, so only what is
// computed on the way to it is evaluated and the queue itself is not copied; a
// queue that lies nowhere -- a function's result -- is kept in a local.
template <ExprLowerer Lowerer>
auto EvaluateSelectedQueue(
    Lowerer& lowerer, const WalkFrame& at, const hir::Expr& base)
    -> diag::Result<SettledPath> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  mir::Block& block = *at.current_block;
  // A sampled read answers with the value each cell kept (LRM 16.5.1), which
  // is a question a read of the value asks and a path does not.
  if (at.reads_as_of == ReadsAsOf::kNow && LiesInStorage(lowerer, base)) {
    auto path = lowerer.LowerAccessPath(base, at);
    if (!path) return std::unexpected(std::move(path.error()));
    return SettledForRead(unit_lowerer, at, *std::move(path));
  }
  auto value = lowerer.LowerExpr(base, at);
  if (!value) return std::unexpected(std::move(value.error()));
  return SettledPath{
      .named_in = &block,
      .owner = EvaluatedOnce(at, block.exprs.Add(*std::move(value))),
      .descent = {}};
}

// The queue a select named as a part is taken from, where it is taken from
// one: `base` with what it computes evaluated here, once, since the part and
// each `$` under the select both take it (LRM 7.10.1, 11.4.1).
auto SettleSelectedQueue(
    const UnitLowerer& unit_lowerer, const WalkFrame& frame,
    const hir::Expr& base_expr, AccessPath& base)
    -> std::optional<SettledPath> {
  if (!unit_lowerer.Hir().types.Get(base_expr.type).Is<hir::QueueType>()) {
    return std::nullopt;
  }
  base = Settled(unit_lowerer, frame, std::move(base));
  return SettledPath{
      .named_in = frame.current_block,
      .owner = base.owner,
      .descent = base.descent};
}

}  // namespace

auto ElementStep(
    UnitLowerer& unit_lowerer, mir::Block& block, mir::TypeId receiver_type,
    mir::ExprId idx_id, mir::TypeId part_type) -> DescentStep {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  const mir::TypeId value_type = ReceiverValueType(unit, receiver_type);
  // An associative array is reached by the key the source wrote, which is a
  // value of the index type rather than a place in any order (LRM 7.8).
  if (unit.types.Get(value_type).Is<mir::AssociativeArrayType>()) {
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
  if (unit.types.Get(value_type).IsIntegralPacked()) {
    return SliceStep(position, static_cast<std::uint64_t>(map.step), part_type);
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
  return StepRead(unit_lowerer.Unit(), block, step, base_id);
}

auto BuildPackedBitsRead(
    UnitLowerer& unit_lowerer, mir::Block& block, mir::ExprId base,
    std::uint64_t bit_offset, std::uint64_t bit_width, mir::TypeId result_type)
    -> mir::Expr {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  const DescentStep step = SliceStep(
      BuildConstantPosition(unit, block, static_cast<std::int64_t>(bit_offset)),
      bit_width, result_type);
  return OwnedValue(unit, block, StepRead(unit, block, step, base));
}

auto BuildPackedMemberRead(
    UnitLowerer& unit_lowerer, mir::Block& block, mir::ExprId base,
    const PackedProjection& projection, base::ComponentIndex index,
    mir::TypeId result_type) -> mir::Expr {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  const mir::TypeId natural =
      PartSelectNaturalType(unit, block.exprs.Get(base).type, result_type);
  return ReadMember(
      unit_lowerer, block,
      PackedMemberStep(unit, block, projection, index, natural), base,
      result_type);
}

auto BuildPackedTagTest(
    UnitLowerer& unit_lowerer, mir::Block& block, mir::ExprId base,
    const PackedProjection& projection, base::ComponentIndex index)
    -> mir::ExprId {
  return BuildTagTest(
      unit_lowerer, block, base, RequiredTagOf(projection, index));
}

void AppendTagChecks(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, AccessPath& target) {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Block& block = *frame.current_block;
  // How many of the leading steps have had what they evaluate settled into
  // locals.
  std::size_t settled = 0;
  for (std::size_t taken = 0; taken < target.descent.size(); ++taken) {
    if (!target.descent[taken].required_tag.has_value()) {
      continue;
    }
    const RequiredTag tag = *target.descent[taken].required_tag;
    // The check and the write both take the owner and the steps before this
    // one, so what those evaluate is evaluated here and both read the result.
    target.owner = SettledOwner(unit_lowerer, frame, target.owner);
    for (; settled < taken; ++settled) {
      for (mir::ExprId& operand : target.descent[settled].operands) {
        operand = EvaluatedOnce(frame, operand);
      }
    }
    // The tag is a fact of the value the step is taken into, which is what the
    // steps before it reach.
    const std::span<const DescentStep> before =
        std::span<const DescentStep>(target.descent).first(taken);
    const mir::ExprId stepped_into = EvaluatedOnce(
        frame, PathOwnedValue(
                   unit, block,
                   AccessPath{
                       .owner = target.owner,
                       .descent = {before.begin(), before.end()}}));
    block.AppendStmt(
        mir::ExprStmt{
            .expr = block.exprs.Add(BuildTagGuard(
                unit_lowerer, block, stepped_into, tag,
                "write to a tagged union member inconsistent with the current "
                "tag (LRM 11.9)"))});
  }
}

template <ExprLowerer Lowerer>
auto LowerHirElementSelectExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::ElementSelectExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  const auto& exprs = lowerer.HirExprs();
  const hir::Expr& base_expr = exprs.Get(sel.base_value);
  // An element of a queue may be indexed from the queue's own last index (LRM
  // 7.10.1), so the queue is evaluated once in a block of steps, where the
  // select is written, and the read is what those steps yield.
  if (unit_lowerer.Hir().types.Get(base_expr.type).Is<hir::QueueType>()) {
    BlockBuilder steps(frame);
    mir::Block& body = steps.Body();
    auto queue = EvaluateSelectedQueue(lowerer, steps.Frame(), base_expr);
    if (!queue) return std::unexpected(std::move(queue.error()));
    const mir::ExprId queue_id =
        PathValue(unit_lowerer.Unit(), body, NamedIn(*queue, body));
    auto index = lowerer.LowerExpr(
        exprs.Get(sel.index), steps.Frame().WithSelectedQueue(&*queue));
    if (!index) return std::unexpected(std::move(index.error()));
    const mir::ExprId index_id = body.exprs.Add(*std::move(index));
    return steps.Build(body.exprs.Add(OwnedValue(
        unit_lowerer.Unit(), body,
        BuildElementAccessCallExpr(
            unit_lowerer, body, queue_id, index_id, result_type))));
  }
  auto& block = *frame.current_block;
  auto base_or = lowerer.LowerExpr(base_expr, frame);
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const mir::ExprId base_id = block.exprs.Add(*std::move(base_or));
  auto idx_or = lowerer.LowerExpr(exprs.Get(sel.index), frame);
  if (!idx_or) return std::unexpected(std::move(idx_or.error()));
  const mir::ExprId idx_id = block.exprs.Add(*std::move(idx_or));

  mir::Expr access_call = BuildElementAccessCallExpr(
      unit_lowerer, block, base_id, idx_id, result_type);
  // LRM 6.16: indexed character read `s[i]` is the element-value access, the
  // read-side dual of the element-reference write. It answers with the
  // character itself, so there is no view to materialise.
  if (unit_lowerer.Hir().types.Get(base_expr.type).Is<hir::StringType>()) {
    return access_call;
  }
  return OwnedValue(unit_lowerer.Unit(), block, std::move(access_call));
}

template <ExprLowerer Lowerer>
auto LowerHirRangeSelectExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::RangeSelectExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  const UnitLowerer& unit_lowerer = lowerer.Owner();
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  const hir::Expr& base_expr = lowerer.HirExprs().Get(sel.base_value);
  // A queue's slice works both of its ends out from one base, and either may
  // be counted from the queue's own last index (LRM 7.10.1), so the queue and
  // that base are each evaluated once in a block of steps, where the select is
  // written, and the read is what those steps yield.
  if (unit_lowerer.Hir().types.Get(base_expr.type).Is<hir::QueueType>()) {
    BlockBuilder steps(frame);
    mir::Block& body = steps.Body();
    auto queue = EvaluateSelectedQueue(lowerer, steps.Frame(), base_expr);
    if (!queue) return std::unexpected(std::move(queue.error()));
    const mir::ExprId queue_id = PathValue(unit, body, NamedIn(*queue, body));
    auto step = RangeStep(
        lowerer, steps.Frame().WithSelectedQueue(&*queue), sel.bounds,
        body.exprs.Get(queue_id).type, result_type);
    if (!step) return std::unexpected(std::move(step.error()));
    return steps.Build(body.exprs.Add(
        OwnedValue(unit, body, StepRead(unit, body, *step, queue_id))));
  }
  auto& block = *frame.current_block;
  auto base_or = lowerer.LowerExpr(base_expr, frame);
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const mir::ExprId base_id = block.exprs.Add(*std::move(base_or));
  auto step = RangeStep(
      lowerer, frame, sel.bounds, block.exprs.Get(base_id).type, result_type);
  if (!step) return std::unexpected(std::move(step.error()));
  return OwnedValue(unit, block, StepRead(unit, block, *step, base_id));
}

template <ExprLowerer Lowerer>
auto LowerHirMemberAccessExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::MemberAccessExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  const hir::Expr& base_expr = lowerer.HirExprs().Get(sel.base_value);
  const hir::Type& aggregate = unit_lowerer.Hir().types.Get(base_expr.type);
  // A member behind a tag is read by naming its aggregate twice, to test the
  // tag and to take the member, so the aggregate is evaluated once into a
  // local of a block of steps, and the read is what those steps yield.
  std::optional<BlockBuilder> steps;
  if (CarriesATag(unit_lowerer, aggregate)) {
    steps.emplace(frame);
  }
  const WalkFrame& at = steps.has_value() ? steps->Frame() : frame;
  auto& block = *at.current_block;
  auto base_or = lowerer.LowerExpr(base_expr, at);
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  mir::ExprId base_id = block.exprs.Add(*std::move(base_or));
  if (steps.has_value()) {
    base_id = EvaluatedOnce(at, base_id);
  }
  const mir::TypeId natural = PartSelectNaturalType(
      unit_lowerer.Unit(), block.exprs.Get(base_id).type, result_type);
  const DescentStep step =
      MemberStep(unit_lowerer, block, aggregate, sel.field_index, natural);
  mir::Expr read = ReadMember(unit_lowerer, block, step, base_id, result_type);
  if (!steps.has_value()) {
    return read;
  }
  return steps->Build(block.exprs.Add(std::move(read)));
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
  auto base_or =
      lowerer.LowerExpr(lowerer.HirExprs().Get(sel.base_value), frame);
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const mir::ExprId base_id = block.exprs.Add(*std::move(base_or));
  return BuildClassPropertyAccess(
      lowerer, frame, base_id, sel.target, result_type);
}

template <ExprLowerer Lowerer>
auto LowerHirElementSelectExprPath(
    Lowerer& lowerer, WalkFrame frame, const hir::ElementSelectExpr& sel,
    mir::TypeId result_type) -> diag::Result<AccessPath> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  auto& block = *frame.current_block;
  const hir::Expr& base_expr = lowerer.HirExprs().Get(sel.base_value);
  auto base = lowerer.LowerAccessPath(base_expr, frame);
  if (!base) return std::unexpected(std::move(base.error()));
  const std::optional<SettledPath> queue =
      SettleSelectedQueue(unit_lowerer, frame, base_expr, *base);
  auto idx_or = lowerer.LowerExpr(
      lowerer.HirExprs().Get(sel.index),
      queue.has_value() ? frame.WithSelectedQueue(&*queue) : frame);
  if (!idx_or) return std::unexpected(std::move(idx_or.error()));
  const mir::ExprId idx_id = block.exprs.Add(*std::move(idx_or));
  const mir::TypeId container =
      PathValueType(unit_lowerer.Unit(), block, *base);
  return DescendInto(
      *std::move(base),
      ElementStep(unit_lowerer, block, container, idx_id, result_type));
}

template <ExprLowerer Lowerer>
auto LowerHirRangeSelectExprPath(
    Lowerer& lowerer, WalkFrame frame, const hir::RangeSelectExpr& sel,
    mir::TypeId result_type) -> diag::Result<AccessPath> {
  auto& block = *frame.current_block;
  const hir::Expr& base_expr = lowerer.HirExprs().Get(sel.base_value);
  auto base = lowerer.LowerAccessPath(base_expr, frame);
  if (!base) return std::unexpected(std::move(base.error()));
  const std::optional<SettledPath> queue =
      SettleSelectedQueue(lowerer.Owner(), frame, base_expr, *base);
  const mir::TypeId container =
      PathValueType(lowerer.Owner().Unit(), block, *base);
  auto step = RangeStep(
      lowerer, queue.has_value() ? frame.WithSelectedQueue(&*queue) : frame,
      sel.bounds, container, result_type);
  if (!step) return std::unexpected(std::move(step.error()));
  return DescendInto(*std::move(base), *std::move(step));
}

template <ExprLowerer Lowerer>
auto LowerHirMemberAccessExprPath(
    Lowerer& lowerer, WalkFrame frame, const hir::MemberAccessExpr& sel,
    mir::TypeId result_type) -> diag::Result<AccessPath> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  const hir::Expr& base_expr = lowerer.HirExprs().Get(sel.base_value);
  auto base = lowerer.LowerAccessPath(base_expr, frame);
  if (!base) return std::unexpected(std::move(base.error()));
  return DescendInto(
      *std::move(base), MemberStep(
                            unit_lowerer, *frame.current_block,
                            unit_lowerer.Hir().types.Get(base_expr.type),
                            sel.field_index, result_type));
}

// LRM 8.4: a class property is reached through the handle naming its object.
// The path holds the object and which of its properties, and what the path is
// taken for decides how the object is reached, so the handle the source wrote
// once is named once.
template <ExprLowerer Lowerer>
auto PropertyPath(
    Lowerer& lowerer, const WalkFrame& frame, mir::ExprId receiver,
    const hir::ClassPropertyTarget& target, mir::TypeId result_type)
    -> AccessPath {
  return AccessPath{
      .owner =
          ObjectProperty{
              .object = ReportedObject(lowerer.Owner().Unit(), frame, receiver),
              .property = PropertyNameOf(lowerer, target),
              .type = result_type},
      .descent = {}};
}

template <ExprLowerer Lowerer>
auto LowerHirClassPropertyAccessExprPath(
    Lowerer& lowerer, WalkFrame frame, const hir::ClassPropertyAccessExpr& sel,
    mir::TypeId result_type) -> diag::Result<AccessPath> {
  auto base_or =
      lowerer.LowerExpr(lowerer.HirExprs().Get(sel.base_value), frame);
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  return PropertyPath(
      lowerer, frame, frame.current_block->exprs.Add(*std::move(base_or)),
      sel.target, result_type);
}

template <ExprLowerer Lowerer>
auto LowerHirInterfaceMemberAccessExpr(
    Lowerer& lowerer, WalkFrame frame,
    const hir::InterfaceMemberAccessExpr& sel) -> diag::Result<mir::Expr> {
  auto held = HeldInterfaceMember(lowerer, frame, sel);
  if (!held) return std::unexpected(std::move(held.error()));
  const mir::CompilationUnit& unit = lowerer.Owner().Unit();
  const mir::ExprId storage = ReportedPlace(unit, frame, *held);
  return mir::Expr{
      .data = mir::DerefExpr{.pointer = storage},
      .type = unit.types.Get(frame.current_block->exprs.Get(storage).type)
                  .template Get<mir::PointerType>()
                  .pointee};
}

template <ExprLowerer Lowerer>
auto LowerHirInterfaceInstanceAccessExpr(
    Lowerer& lowerer, WalkFrame frame,
    const hir::InterfaceInstanceAccessExpr& sel, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  auto object = HeldInterfaceObject(lowerer, frame, sel);
  if (!object) return std::unexpected(std::move(object.error()));
  return InterfaceValueOf(
      lowerer.Owner().Unit(), *frame.current_block, *object, result_type);
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
template auto LowerHirElementSelectExprPath(
    ProcessLowerer&, WalkFrame, const hir::ElementSelectExpr&, mir::TypeId)
    -> diag::Result<AccessPath>;
template auto LowerHirElementSelectExprPath(
    const StructuralScopeLowerer&, WalkFrame, const hir::ElementSelectExpr&,
    mir::TypeId) -> diag::Result<AccessPath>;
template auto LowerHirRangeSelectExprPath(
    ProcessLowerer&, WalkFrame, const hir::RangeSelectExpr&, mir::TypeId)
    -> diag::Result<AccessPath>;
template auto LowerHirRangeSelectExprPath(
    const StructuralScopeLowerer&, WalkFrame, const hir::RangeSelectExpr&,
    mir::TypeId) -> diag::Result<AccessPath>;
template auto LowerHirMemberAccessExprPath(
    ProcessLowerer&, WalkFrame, const hir::MemberAccessExpr&, mir::TypeId)
    -> diag::Result<AccessPath>;
template auto LowerHirMemberAccessExprPath(
    const StructuralScopeLowerer&, WalkFrame, const hir::MemberAccessExpr&,
    mir::TypeId) -> diag::Result<AccessPath>;
template auto PropertyPath(
    ProcessLowerer&, const WalkFrame&, mir::ExprId,
    const hir::ClassPropertyTarget&, mir::TypeId) -> AccessPath;
template auto PropertyPath(
    const StructuralScopeLowerer&, const WalkFrame&, mir::ExprId,
    const hir::ClassPropertyTarget&, mir::TypeId) -> AccessPath;
template auto LowerHirClassPropertyAccessExprPath(
    ProcessLowerer&, WalkFrame, const hir::ClassPropertyAccessExpr&,
    mir::TypeId) -> diag::Result<AccessPath>;
template auto LowerHirClassPropertyAccessExprPath(
    const StructuralScopeLowerer&, WalkFrame,
    const hir::ClassPropertyAccessExpr&, mir::TypeId)
    -> diag::Result<AccessPath>;
template auto LowerHirInterfaceMemberAccessExpr(
    ProcessLowerer&, WalkFrame, const hir::InterfaceMemberAccessExpr&)
    -> diag::Result<mir::Expr>;
template auto LowerHirInterfaceMemberAccessExpr(
    const StructuralScopeLowerer&, WalkFrame,
    const hir::InterfaceMemberAccessExpr&) -> diag::Result<mir::Expr>;
template auto LowerHirInterfaceInstanceAccessExpr(
    ProcessLowerer&, WalkFrame, const hir::InterfaceInstanceAccessExpr&,
    mir::TypeId) -> diag::Result<mir::Expr>;
template auto LowerHirInterfaceInstanceAccessExpr(
    const StructuralScopeLowerer&, WalkFrame,
    const hir::InterfaceInstanceAccessExpr&, mir::TypeId)
    -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
