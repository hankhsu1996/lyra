#include "lyra/lowering/hir_to_mir/expression/selects.hpp"

#include <cstdint>
#include <expected>
#include <optional>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/component_index.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/binary_op.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/bitstream.hpp"
#include "lyra/lowering/hir_to_mir/block_builder.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/expression/operators.hpp"
#include "lyra/lowering/hir_to_mir/packed_projection.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/select_position.hpp"
#include "lyra/lowering/hir_to_mir/sensitivity_wait.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/unit_object_access.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/builtin_fn.hpp"

// HIR-to-MIR lowering for the three ways the source selects part of a value:
// `a[i]`, `a[hi:lo]` with its `+:` and `-:` forms, and `s.member`.
//
// A select is lowered in two moves. First it becomes a step: which kind of
// part it reaches, at what position in the selected value's own numbering,
// and for several elements in a row how many it takes. What kind of part that
// is follows from what is selected from and not from how it was written:
//
//   logic [7:0] v;            v[3]    one bit, at position 3
//   int a[1:8];               a[k]    an element, at position k - 1
//   struct packed {..} s;     s.b     the bits `b` occupies
//   struct {..} u;            u.b     a component, by its declaration order
//
// A select is written in the coordinates the declaration chose, and a step
// states a position counted from zero: this is where one becomes the other,
// because this is the layer that still knows which declaration the coordinates
// belong to (LRM 7.4.5, 11.5.1). There is one function per source form, and
// nothing else places a part.
//
// Then the step is used. A select read as a value takes the step on the place
// its base names, so nothing on the way to the part is read whole, or on the
// base's value where it names no storage. A select named as a part -- by a
// write, a reference, a wait, a join of nets -- adds the step to the path its
// base names, so `s.b[1]` is the owner `s` and two steps, and whatever is done
// with the part is done to that one statement of it.
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

// Bits of an integral value: where they start in its own numbering, and as
// many as `part_type` is wide (LRM 11.5.1). A bit-select, a part-select and a
// packed aggregate's member are all this one step.
auto BitsStep(mir::ExprId start, mir::TypeId part_type) -> DescentStep {
  return DescentStep{.to = StepToBits{.start = start}, .part_type = part_type};
}

// One element, reached by a position the program computes.
auto OneElementStep(mir::ExprId position, mir::TypeId part_type)
    -> DescentStep {
  return DescentStep{
      .to = StepToElement{.position = position}, .part_type = part_type};
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
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  // The base is one bound and the other is counted from it, so the position it
  // names is evaluated once and both read that.
  const mir::ExprId base_id = EvaluatedOnce(
      frame,
      BuildOrdinalPosition(unit, block, block.exprs.Add(*std::move(base_or))));
  auto width_or = lowerer.LowerExpr(lowerer.HirExprs().Get(width), frame);
  if (!width_or) return std::unexpected(std::move(width_or.error()));
  const mir::ExprId width_id = block.exprs.Add(*std::move(width_or));
  const mir::ExprId other = BuildSpanEnd(unit, block, base_id, width_id, up);
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
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  auto queue_bounds = std::visit(
      Overloaded{
          [&](const hir::RangeConstantBounds& c) -> diag::Result<QueueBounds> {
            auto lo =
                lowerer.LowerExpr(lowerer.HirExprs().Get(c.left_bound), frame);
            if (!lo) return std::unexpected(std::move(lo.error()));
            const mir::ExprId lo_id = BuildOrdinalPosition(
                unit, block, block.exprs.Add(*std::move(lo)));
            auto hi =
                lowerer.LowerExpr(lowerer.HirExprs().Get(c.right_bound), frame);
            if (!hi) return std::unexpected(std::move(hi.error()));
            return QueueBounds{
                .lo = lo_id,
                .hi = BuildOrdinalPosition(
                    unit, block, block.exprs.Add(*std::move(hi)))};
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
      .to =
          StepToQueueRun{
              .lowest = queue_bounds->lo, .highest = queue_bounds->hi},
      .part_type = result_type};
}

// How many positions a slice of a value that numbers its parts covers, which
// the select's own result type states: its bits, or its elements.
auto SliceCount(const mir::CompilationUnit& unit, mir::TypeId result_type)
    -> std::uint64_t {
  const mir::Type& result = unit.types.Get(result_type);
  return result.IsIntegral() ? result.Integral().bit_width
                             : result.Get<mir::UnpackedArrayType>().Size();
}

// Where a slice of `count` positions starts: the index its lowest position is
// named by, and how far below that index's own position the slice starts.
//
// For a constant range the index is the bound the declaration's direction puts
// at the slice's lowest position -- the right bound of a packed value, whose
// numbering starts at its least significant bit, and the left bound of an
// unpacked one, whose numbering starts at its left (the front end has already
// held the range to the declaration's direction). An indexed select counts `w`
// from its base; where positions grow opposite to the way the select counts,
// the slice starts `w - 1` steps below the base.
struct SliceStart {
  hir::ExprId index;
  std::int64_t shift = 0;
};

auto SliceStartOf(
    const hir::RangeBounds& bounds, const PositionMap& map, std::uint64_t count)
    -> SliceStart {
  // Back over every position the slice covers beyond the base's own step.
  const std::int64_t below = map.step - static_cast<std::int64_t>(count);
  return std::visit(
      Overloaded{
          [&](const hir::RangeConstantBounds& c) {
            return SliceStart{
                .index = map.from_right ? c.right_bound : c.left_bound,
                .shift = 0};
          },
          [&](const hir::RangeIndexedUpBounds& c) {
            return SliceStart{
                .index = c.base_index, .shift = map.reversed ? below : 0};
          },
          [&](const hir::RangeIndexedDownBounds& c) {
            return SliceStart{
                .index = c.base_index, .shift = map.reversed ? 0 : below};
          },
      },
      bounds);
}

// The step a slice of `count` positions takes from the index `index_id` its
// start names.
auto SliceStepAt(
    mir::CompilationUnit& unit, mir::Block& block, const PositionMap& map,
    const SliceStart& start, mir::ExprId index_id, std::uint64_t count,
    mir::TypeId result_type) -> DescentStep {
  const mir::ExprId position =
      WrapIndexAsPosition(unit, block, map, index_id, start.shift);
  if (unit.types.Get(result_type).IsIntegral()) {
    return BitsStep(position, result_type);
  }
  return DescentStep{
      .to = StepToElementRun{.start = position, .count = count},
      .part_type = result_type};
}

// The step `receiver[hi:lo]`, `receiver[base+:w]` and `receiver[base-:w]` take
// (LRM 7.4.5 / 7.4.6 / 7.10.1 / 11.5.1). A packed value and a fixed-size or
// dynamic array take a slice of a fixed count, starting at the part of the
// slice lowest in the receiver's numbering.
template <ExprLowerer Lowerer>
auto RangeStep(
    Lowerer& lowerer, WalkFrame frame, const hir::RangeBounds& bounds,
    hir::TypeId receiver_type, mir::TypeId result_type)
    -> diag::Result<DescentStep> {
  const UnitLowerer& unit_lowerer = lowerer.Owner();
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  mir::Block& block = *frame.current_block;
  if (unit_lowerer.Hir().types.Get(receiver_type).Is<hir::QueueType>()) {
    return QueueSliceStep(lowerer, frame, bounds, result_type);
  }

  const PositionMap map = PositionMapOf(unit_lowerer, receiver_type);
  const std::uint64_t count = SliceCount(unit, result_type);
  const SliceStart start = SliceStartOf(bounds, map, count);
  auto index = lowerer.LowerExpr(lowerer.HirExprs().Get(start.index), frame);
  if (!index) return std::unexpected(std::move(index.error()));
  return SliceStepAt(
      unit, block, map, start, block.exprs.Add(*std::move(index)), count,
      result_type);
}

// The tag a member of the packed aggregate `projection` describes needs carried
// in the aggregate's own bits: none where they carry no tag.
auto TagInBitsOf(const PackedProjection& projection, base::ComponentIndex index)
    -> std::optional<TagInBits> {
  if (projection.tag_bits == 0) {
    return std::nullopt;
  }
  return TagInBits{.tag_bits = projection.tag_bits, .member = index};
}

// The tag a step into member `index` of `aggregate` needs the union to hold
// (LRM 11.9): none outside a tagged union, nor in a packed one of one member,
// whose tag has no bits (LRM 7.3.2).
auto TagRequiredByMember(
    const UnitLowerer& unit_lowerer, const hir::Type& aggregate,
    base::ComponentIndex index) -> std::optional<TagRequirement> {
  if (const auto* unpacked = aggregate.As<hir::UnpackedUnionType>()) {
    if (!unpacked->tagged) {
      return std::nullopt;
    }
    return HeldTag{.member = index};
  }
  if (!aggregate.Is<hir::PackedUnionType>()) {
    return std::nullopt;
  }
  return TagInBitsOf(ProjectPackedAggregate(unit_lowerer, aggregate), index);
}

// The step member `index` of a packed aggregate takes: the bits of the
// aggregate's vector the member occupies (LRM 7.2.1, 7.3.1), and the tag the
// step needs the aggregate to carry, where it needs one.
auto PackedMemberStep(
    mir::CompilationUnit& unit, mir::Block& block,
    const PackedProjection& projection, base::ComponentIndex index,
    mir::TypeId part_type, std::optional<TagInBits> required_tag)
    -> DescentStep {
  if (index.value >= projection.members.size()) {
    throw InternalError("PackedMemberStep: member index out of range");
  }
  const ProjectedMember& member = projection.members[index.value];
  return DescentStep{
      .to =
          StepToBits{
              .start = BuildConstantPosition(
                  unit, block, static_cast<std::int64_t>(member.bit_offset)),
              .required_tag = required_tag},
      .part_type = part_type};
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
  const mir::ExprId owned_id =
      block.AsValue(unit.types, block.exprs.Add(std::move(owned)));
  return BuildValueConversion(unit, block, owned_id, final_type);
}

// The member `step` reaches in what `base_id` names, at the member's declared
// type. A structure's component and a union's member are the place they lie
// in, which a read then reads. A packed member is bits of
// the aggregate's vector (LRM 7.2.1: it "can be selected as if it were a packed
// array"), so it is read at its part-select natural type and brought to the
// declared type.
auto ReadMember(
    UnitLowerer& unit_lowerer, mir::Block& block, const DescentStep& step,
    mir::ExprId base_id, mir::TypeId result_type) -> mir::Expr {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  return WrapSliceToDeclaredType(
      unit, block, StepForRead(unit, block, step, base_id), result_type);
}

// Where a read of `expr` finds its value: in no storage, handed a value that
// lies nowhere; in storage that lasts only while the steps reading it run -- an
// object whose one handle those steps hold; or in storage that outlives them --
// a name, or a part of what a name reaches. This is a question about reading
// and not about what may be written: a concatenation names destinations to a
// write and is still a value built from its operands to a read.
enum class StorageFound : std::uint8_t {
  kNone,
  kWhileTheStepsRun,
  kBeyondTheSteps
};

template <ExprLowerer Lowerer>
auto StorageFoundBy(Lowerer& lowerer, const hir::Expr& expr) -> StorageFound {
  const auto base_lies = [&](hir::ExprId base) {
    return StorageFoundBy(lowerer, lowerer.HirExprs().Get(base));
  };
  return std::visit(
      Overloaded{
          [](const hir::PrimaryExpr&) { return StorageFound::kBeyondTheSteps; },
          [&](const hir::ElementSelectExpr& e) {
            return base_lies(e.base_value);
          },
          [&](const hir::RangeSelectExpr& e) {
            return base_lies(e.base_value);
          },
          [&](const hir::MemberAccessExpr& e) {
            return base_lies(e.base_value);
          },
          // A property lies in the object its handle reaches (LRM 8.4): one
          // the steps hold the only handle to lives while they run, and one a
          // stored handle reaches outlives them.
          [&](const hir::ClassPropertyAccessExpr& e) {
            switch (base_lies(e.base_value)) {
              case StorageFound::kNone:
              case StorageFound::kWhileTheStepsRun:
                return StorageFound::kWhileTheStepsRun;
              case StorageFound::kBeyondTheSteps:
                return StorageFound::kBeyondTheSteps;
            }
            throw InternalError("StorageFoundBy: unknown storage found");
          },
          [](const hir::InterfaceMemberAccessExpr&) {
            return StorageFound::kBeyondTheSteps;
          },
          [](const hir::UnaryExpr&) { return StorageFound::kNone; },
          [](const hir::BinaryExpr&) { return StorageFound::kNone; },
          [](const hir::ConditionalExpr&) { return StorageFound::kNone; },
          [](const hir::AssignExpr&) { return StorageFound::kNone; },
          [](const hir::IncDecExpr&) { return StorageFound::kNone; },
          [](const hir::CallExpr&) { return StorageFound::kNone; },
          [](const hir::CalleeDefaultExpr&) { return StorageFound::kNone; },
          [](const hir::ConversionExpr&) { return StorageFound::kNone; },
          [](const hir::ConstantExpr&) { return StorageFound::kNone; },
          [](const hir::ValueRangeExpr&) { return StorageFound::kNone; },
          [](const hir::InsideExpr&) { return StorageFound::kNone; },
          [](const hir::InterfaceInstanceAccessExpr&) {
            return StorageFound::kNone;
          },
          [](const hir::ConcatExpr&) { return StorageFound::kNone; },
          [](const hir::StreamingConcatExpr&) { return StorageFound::kNone; },
          [](const hir::ReplicationExpr&) { return StorageFound::kNone; },
          [](const hir::AssignmentPatternExpr&) { return StorageFound::kNone; },
          [](const hir::AssignmentPatternReplicationExpr&) {
            return StorageFound::kNone;
          },
          [](const hir::DynamicArrayNewExpr&) { return StorageFound::kNone; },
          [](const hir::ClassNewExpr&) { return StorageFound::kNone; },
          [](const hir::AssociativeAssignmentPatternExpr&) {
            return StorageFound::kNone;
          },
          [](const hir::AssignmentPatternKeyedExpr&) {
            return StorageFound::kNone;
          },
          [](const hir::TaggedUnionExpr&) { return StorageFound::kNone; },
          [](const hir::DynamicCastExpr&) { return StorageFound::kNone; },
          [](const hir::CovergroupNewExpr&) { return StorageFound::kNone; },
          [](const hir::CovergroupMethodCallExpr&) {
            return StorageFound::kNone;
          },
          // An option is a field of what the coverage library keeps for the
          // instance or the type, which outlives every read (LRM 19.10).
          [](const hir::CovergroupOptionExpr&) {
            return StorageFound::kBeyondTheSteps;
          },
          [](const hir::RandomizeExpr&) { return StorageFound::kNone; },
          [](const hir::RandModeSetExpr&) { return StorageFound::kNone; },
          [](const hir::RandModeGetExpr&) { return StorageFound::kNone; },
          [](const hir::ConstraintModeSetExpr&) { return StorageFound::kNone; },
          [](const hir::ConstraintModeGetExpr&) {
            return StorageFound::kNone;
          }},
      expr.data);
}

// The value a select is taken from, as evaluated once for it.
struct SelectedBase {
  SettledPath path;
  // Whether the value lies in storage that outlives the block of steps it is
  // evaluated in, rather than in a local of that block or in an object only
  // that block holds a handle to.
  bool outlives_the_steps;
};

// The value a select read as a value is taken from, evaluated here, once,
// because the select names it more than once (LRM 11.4.1): a queue for the
// part and for each `$` under the select (LRM 7.10.1). A value lying in storage
// is named where it lies -- in an object a function's handle reaches too, which
// the handle the steps hold keeps alive while they run -- so only what is
// computed on the way to it is evaluated and the value itself is not copied; a
// value that lies nowhere -- a function's result, a sampled value, a part of
// either -- is kept in a local, which is then the place it lies in.
template <ExprLowerer Lowerer>
auto EvaluateSelectedBase(
    Lowerer& lowerer, const WalkFrame& at, const hir::Expr& base)
    -> diag::Result<SelectedBase> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  mir::Block& block = *at.current_block;
  // A sampled read answers with the value each cell kept (LRM 16.5.1), which
  // is a question a read of the value asks and a path does not.
  const StorageFound found = at.reads_as_of == ReadsAsOf::kNow
                                 ? StorageFoundBy(lowerer, base)
                                 : StorageFound::kNone;
  const auto named_where_it_lies =
      [&](bool outlives_the_steps) -> diag::Result<SelectedBase> {
    auto path = lowerer.LowerAccessPath(base, at);
    if (!path) return std::unexpected(std::move(path.error()));
    return SelectedBase{
        .path = SettledForRead(unit_lowerer, at, *std::move(path)),
        .outlives_the_steps = outlives_the_steps};
  };
  switch (found) {
    case StorageFound::kNone:
      break;
    case StorageFound::kWhileTheStepsRun:
      return named_where_it_lies(false);
    case StorageFound::kBeyondTheSteps:
      return named_where_it_lies(true);
  }
  auto value = lowerer.LowerExpr(base, at);
  if (!value) return std::unexpected(std::move(value.error()));
  const mir::TypeId type = value->type;
  return SelectedBase{
      .path =
          SettledPath{
              .named_in = &block,
              .owner = block.LocalPlace(
                  DeclareLocal(at, block.exprs.Add(*std::move(value))), type),
              .descent = {}},
      .outlives_the_steps = false};
}

// Whether reading `expr` reaches through a member a tag guards (LRM 11.9), at
// any of the selects it is written as.
template <ExprLowerer Lowerer>
auto ReachesThroughATag(Lowerer& lowerer, const hir::Expr& expr) -> bool {
  const auto base_reaches = [&](hir::ExprId base) {
    return ReachesThroughATag(lowerer, lowerer.HirExprs().Get(base));
  };
  if (const auto* member = std::get_if<hir::MemberAccessExpr>(&expr.data)) {
    const hir::Type& aggregate = lowerer.Owner().Hir().types.Get(
        lowerer.HirExprs().Get(member->base_value).type);
    return TagRequiredByMember(lowerer.Owner(), aggregate, member->field_index)
               .has_value() ||
           base_reaches(member->base_value);
  }
  if (const auto* element = std::get_if<hir::ElementSelectExpr>(&expr.data)) {
    return base_reaches(element->base_value);
  }
  if (const auto* range = std::get_if<hir::RangeSelectExpr>(&expr.data)) {
    return base_reaches(range->base_value);
  }
  return false;
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

template <ExprLowerer Lowerer>
auto LowerStreamWindow(
    Lowerer& lowerer, const WalkFrame& frame, hir::TypeId array_type,
    const hir::StreamRange& range) -> diag::Result<StreamWindow> {
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  mir::Block& block = *frame.current_block;
  const PositionMap map = PositionMapOf(lowerer.Owner(), array_type);
  // A bound written in the array's declared numbering as a position in its own,
  // formed as a select forms one, so a bound holding x or z names none.
  const auto position = [&](hir::ExprId index) -> diag::Result<mir::ExprId> {
    auto lowered = lowerer.LowerExpr(lowerer.HirExprs().Get(index), frame);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    return EvaluatedOnce(
        frame, WrapIndexAsPosition(
                   unit, block, map, block.exprs.Add(*std::move(lowered)), 0));
  };
  // The far end of `width` elements counted from `base`, up or down the
  // declared numbering, which runs against the array's own where it is
  // reversed.
  const auto far_end = [&](mir::ExprId base, hir::ExprId width,
                           bool up) -> diag::Result<mir::ExprId> {
    auto lowered = lowerer.LowerExpr(lowerer.HirExprs().Get(width), frame);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    return EvaluatedOnce(
        frame, BuildSpanEnd(
                   unit, block, base, block.exprs.Add(*std::move(lowered)),
                   up != map.reversed));
  };
  // The window's lowest and highest positions in the array's own numbering.
  struct Ends {
    mir::ExprId lowest;
    mir::ExprId highest;
  };
  auto ends = std::visit(
      Overloaded{
          [&](hir::ExprId element) -> diag::Result<Ends> {
            auto at = position(element);
            if (!at) return std::unexpected(std::move(at.error()));
            return Ends{.lowest = *at, .highest = *at};
          },
          [&](const hir::RangeBounds& bounds) -> diag::Result<Ends> {
            return std::visit(
                Overloaded{
                    [&](const hir::RangeConstantBounds& b)
                        -> diag::Result<Ends> {
                      auto left = position(b.left_bound);
                      if (!left)
                        return std::unexpected(std::move(left.error()));
                      auto right = position(b.right_bound);
                      if (!right) {
                        return std::unexpected(std::move(right.error()));
                      }
                      return Ends{.lowest = *left, .highest = *right};
                    },
                    [&](const hir::RangeIndexedUpBounds& b)
                        -> diag::Result<Ends> {
                      auto base = position(b.base_index);
                      if (!base)
                        return std::unexpected(std::move(base.error()));
                      auto other = far_end(*base, b.width, true);
                      if (!other) {
                        return std::unexpected(std::move(other.error()));
                      }
                      return map.reversed
                                 ? Ends{.lowest = *other, .highest = *base}
                                 : Ends{.lowest = *base, .highest = *other};
                    },
                    [&](const hir::RangeIndexedDownBounds& b)
                        -> diag::Result<Ends> {
                      auto base = position(b.base_index);
                      if (!base)
                        return std::unexpected(std::move(base.error()));
                      auto other = far_end(*base, b.width, false);
                      if (!other) {
                        return std::unexpected(std::move(other.error()));
                      }
                      return map.reversed
                                 ? Ends{.lowest = *base, .highest = *other}
                                 : Ends{.lowest = *other, .highest = *base};
                    }},
                bounds);
          }},
      range);
  if (!ends) return std::unexpected(std::move(ends.error()));
  // A range behaves as a slice of its array (LRM 11.4.14.4), which names no
  // element where it ends before it starts or a bound holds x or z (LRM
  // 7.10.1): the positions between the two ends, read as a count, none where
  // that is not positive, and x read as no position.
  const mir::TypeId position_type = mir::PositionType(unit.types);
  const mir::ExprId spanned = EvaluatedOnce(
      frame,
      BuildToInt64Call(
          unit, block,
          block.exprs.Add(
              mir::MakeBinary(
                  unit, block, mir::BinaryOp::kAdd,
                  block.exprs.Add(
                      mir::MakeBinary(
                          unit, block, mir::BinaryOp::kSub, ends->highest,
                          ends->lowest, position_type)),
                  BuildConstantPosition(unit, block, 1), position_type))));
  return StreamWindow{
      .start = ends->lowest,
      .count = EvaluatedOnce(
          frame, mir::BuildMachineIntConditional(
                     unit, block,
                     mir::BuildMachineIntComparison(
                         unit, block, mir::BinaryOp::kLessEqual, spanned,
                         mir::BuildMachineIntLiteral(unit, block, 0)),
                     mir::BuildMachineIntLiteral(unit, block, 0), spanned))};
}

template <ExprLowerer Lowerer>
auto LowerStreamWindowRun(
    Lowerer& lowerer, const WalkFrame& frame, const hir::StreamOperand& operand)
    -> diag::Result<mir::Expr> {
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  mir::Block& block = *frame.current_block;
  const hir::Expr& array = lowerer.HirExprs().Get(operand.value);
  const UnitLowerer& unit_lowerer = lowerer.Owner();
  if (HoldsClassHandle(unit, unit_lowerer.TranslateType(array.type))) {
    return diag::Fail(
        array.span, diag::DiagCode::kUnsupportedExpressionForm,
        "streaming the object a class handle refers to is not yet supported");
  }
  auto base = EvaluateSelectedBase(lowerer, frame, array);
  if (!base) return std::unexpected(std::move(base.error()));
  const bool queue =
      unit_lowerer.Hir().types.Get(array.type).Is<hir::QueueType>();
  auto window = LowerStreamWindow(
      lowerer, queue ? frame.WithSelectedQueue(&base->path) : frame, array.type,
      *operand.with);
  if (!window) return std::unexpected(std::move(window.error()));
  return mir::MakeBuiltinCall(
      unit, block, support::BuiltinFn::kWindowToBitVec,
      PathAsPlace(unit, block, NamedIn(base->path, block)),
      {window->start, window->count}, unit.builtins.bit_vec);
}

auto MemberStep(
    UnitLowerer& unit_lowerer, mir::Block& block, const hir::Type& aggregate,
    base::ComponentIndex index, mir::TypeId part_type) -> DescentStep {
  if (aggregate.Is<hir::UnpackedStructType>()) {
    return DescentStep{
        .to = StepToComponent{.position = index}, .part_type = part_type};
  }
  if (const auto* unpacked = aggregate.As<hir::UnpackedUnionType>()) {
    if (unpacked->tagged) {
      return DescentStep{
          .to = StepToTaggedMember{.position = index}, .part_type = part_type};
    }
    return DescentStep{
        .to = StepToUnionMember{.position = index}, .part_type = part_type};
  }
  const PackedProjection projection =
      ProjectPackedAggregate(unit_lowerer, aggregate);
  return PackedMemberStep(
      unit_lowerer.Unit(), block, projection, index, part_type,
      TagInBitsOf(projection, index));
}

auto BuildHoldsMember(
    UnitLowerer& unit_lowerer, mir::Block& block, mir::ExprId subject,
    hir::TypeId subject_type, base::ComponentIndex index,
    hir::CaseKeyword keyword) -> mir::ExprId {
  const std::optional<TagRequirement> tag = TagRequiredByMember(
      unit_lowerer, unit_lowerer.Hir().types.Get(subject_type), index);
  if (!tag.has_value()) {
    return BuildMachineBool(unit_lowerer.Unit(), block, true);
  }
  return BuildTagMatches(unit_lowerer.Unit(), block, *tag, subject, keyword);
}

auto ElementStep(
    UnitLowerer& unit_lowerer, mir::Block& block, hir::TypeId receiver_type,
    mir::ExprId idx_id, mir::TypeId part_type) -> DescentStep {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  const hir::Type& receiver = unit_lowerer.Hir().types.Get(receiver_type);
  // An associative array is reached by the key the source wrote, which is a
  // value of the index type rather than a place in any order (LRM 7.8).
  if (receiver.Is<hir::AssociativeArrayType>()) {
    return DescentStep{
        .to = StepToAssocElement{.key = idx_id}, .part_type = part_type};
  }
  // A character of a string is named the way `getc` and `putc` take one (LRM
  // Table 6-9, 6.16.2, 6.16.3): as an `int`, which an index is converted to
  // with each x or z bit read as 0 (LRM 6.11.2).
  const mir::ExprId index =
      receiver.Is<hir::StringType>()
          ? ConvertToType(unit, block, idx_id, unit.builtins.int_type)
          : idx_id;
  const PositionMap map = PositionMapOf(unit_lowerer, receiver_type);
  const mir::ExprId position = WrapIndexAsPosition(unit, block, map, index, 0);
  if (receiver.IsIntegral()) {
    return BitsStep(position, part_type);
  }
  if (receiver.Is<hir::StringType>()) {
    return DescentStep{
        .to = StepToCharacter{.position = position}, .part_type = part_type};
  }
  return OneElementStep(position, part_type);
}

auto BuildElementAccessCallExpr(
    UnitLowerer& unit_lowerer, mir::Block& block, mir::ExprId base_id,
    hir::TypeId base_type, mir::ExprId idx_id, mir::TypeId result_type)
    -> mir::Expr {
  const DescentStep step =
      ElementStep(unit_lowerer, block, base_type, idx_id, result_type);
  return StepRead(unit_lowerer.Unit(), block, step, base_id);
}

auto BuildPackedBitsRead(
    UnitLowerer& unit_lowerer, mir::Block& block, mir::ExprId base,
    std::uint64_t bit_offset, mir::TypeId result_type) -> mir::Expr {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  const DescentStep step = BitsStep(
      BuildConstantPosition(unit, block, static_cast<std::int64_t>(bit_offset)),
      result_type);
  return StepRead(unit, block, step, base);
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
      PackedMemberStep(unit, block, projection, index, natural, std::nullopt),
      base, result_type);
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
  // select is written. Where the queue outlives those steps they yield the
  // reference to the element, which is then the place the element lies in;
  // where it lies in a local of theirs, they yield the element's value.
  if (unit_lowerer.Hir().types.Get(base_expr.type).Is<hir::QueueType>()) {
    BlockBuilder steps(frame);
    mir::Block& body = steps.Body();
    auto queue = EvaluateSelectedBase(lowerer, steps.Frame(), base_expr);
    if (!queue) return std::unexpected(std::move(queue.error()));
    const mir::ExprId queue_id =
        PathAsPlace(unit_lowerer.Unit(), body, NamedIn(queue->path, body));
    auto index = lowerer.LowerExpr(
        exprs.Get(sel.index), steps.Frame().WithSelectedQueue(&queue->path));
    if (!index) return std::unexpected(std::move(index.error()));
    const mir::ExprId index_id = body.exprs.Add(*std::move(index));
    const DescentStep element =
        ElementStep(unit_lowerer, body, base_expr.type, index_id, result_type);
    if (!queue->outlives_the_steps) {
      return steps.Build(body.exprs.Add(
          StepRead(unit_lowerer.Unit(), body, element, queue_id)));
    }
    const mir::ExprId reference =
        frame.current_block->exprs.Add(steps.Build(body.exprs.Add(
            ElementReference(unit_lowerer.Unit(), body, element, queue_id))));
    return mir::MakeDerefExpr(reference, result_type);
  }
  auto& block = *frame.current_block;
  auto base_or = lowerer.LowerAsPlace(base_expr, frame);
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const mir::ExprId base_id = block.exprs.Add(*std::move(base_or));
  auto idx_or = lowerer.LowerExpr(exprs.Get(sel.index), frame);
  if (!idx_or) return std::unexpected(std::move(idx_or.error()));
  const mir::ExprId idx_id = block.exprs.Add(*std::move(idx_or));

  // The element is named where it lies, and read only where a value is wanted.
  return StepForRead(
      unit_lowerer.Unit(), block,
      ElementStep(unit_lowerer, block, base_expr.type, idx_id, result_type),
      base_id);
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
    auto queue = EvaluateSelectedBase(lowerer, steps.Frame(), base_expr);
    if (!queue) return std::unexpected(std::move(queue.error()));
    const mir::ExprId queue_id =
        PathAsPlace(unit, body, NamedIn(queue->path, body));
    auto step = RangeStep(
        lowerer, steps.Frame().WithSelectedQueue(&queue->path), sel.bounds,
        base_expr.type, result_type);
    if (!step) return std::unexpected(std::move(step.error()));
    return steps.Build(body.exprs.Add(StepRead(unit, body, *step, queue_id)));
  }
  auto& block = *frame.current_block;
  auto base_or = lowerer.LowerAsPlace(base_expr, frame);
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const mir::ExprId base_id = block.exprs.Add(*std::move(base_or));
  auto step =
      RangeStep(lowerer, frame, sel.bounds, base_expr.type, result_type);
  if (!step) return std::unexpected(std::move(step.error()));
  return StepRead(unit, block, *step, base_id);
}

template <ExprLowerer Lowerer>
auto LowerHirMemberAccessExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::MemberAccessExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  const hir::Expr& base_expr = lowerer.HirExprs().Get(sel.base_value);
  const hir::Type& aggregate = unit_lowerer.Hir().types.Get(base_expr.type);
  mir::Block& block = *frame.current_block;
  const auto read_member = [&](mir::Block& into, mir::ExprId base_id) {
    const mir::TypeId natural =
        PartSelectNaturalType(unit, into.exprs.Get(base_id).type, result_type);
    return ReadMember(
        unit_lowerer, into,
        MemberStep(unit_lowerer, into, aggregate, sel.field_index, natural),
        base_id, result_type);
  };
  // A member behind a tag is read by naming its aggregate twice, to test the
  // tag and to take the member (LRM 11.3.5). A union lying in storage is read
  // with its whole select as one checked access; one met here is a value --
  // computed, or sampled (LRM 16.5.1) -- held in a local of a block of steps,
  // checked there, and the member read out among them.
  if (TagRequiredByMember(unit_lowerer, aggregate, sel.field_index)
          .has_value()) {
    BlockBuilder steps(frame);
    mir::Block& body = steps.Body();
    auto base = lowerer.LowerExpr(base_expr, steps.Frame());
    if (!base) return std::unexpected(std::move(base.error()));
    const mir::TypeId union_type = base->type;
    const mir::ExprId held_union = body.LocalPlace(
        DeclareLocal(steps.Frame(), body.exprs.Add(*std::move(base))),
        union_type);
    const DescentStep member = MemberStep(
        unit_lowerer, body, aggregate, sel.field_index,
        PartSelectNaturalType(unit, union_type, result_type));
    AppendReadChecks(
        unit, body, AccessPath{.owner = held_union, .descent = {member}});
    return steps.Build(body.AsValue(
        unit.types, body.exprs.Add(ReadMember(
                        unit_lowerer, body, member, held_union, result_type))));
  }
  auto base_or = lowerer.LowerAsPlace(base_expr, frame);
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  return read_member(block, block.exprs.Add(*std::move(base_or)));
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
  return DescendInto(
      *std::move(base),
      ElementStep(unit_lowerer, block, base_expr.type, idx_id, result_type));
}

template <ExprLowerer Lowerer>
auto LowerHirRangeSelectExprPath(
    Lowerer& lowerer, WalkFrame frame, const hir::RangeSelectExpr& sel,
    mir::TypeId result_type) -> diag::Result<AccessPath> {
  const hir::Expr& base_expr = lowerer.HirExprs().Get(sel.base_value);
  auto base = lowerer.LowerAccessPath(base_expr, frame);
  if (!base) return std::unexpected(std::move(base.error()));
  const std::optional<SettledPath> queue =
      SettleSelectedQueue(lowerer.Owner(), frame, base_expr, *base);
  auto step = RangeStep(
      lowerer, queue.has_value() ? frame.WithSelectedQueue(&*queue) : frame,
      sel.bounds, base_expr.type, result_type);
  if (!step) return std::unexpected(std::move(step.error()));
  return DescendInto(*std::move(base), *std::move(step));
}

template <ExprLowerer Lowerer>
auto LowerHirArraySliceWithIndices(
    Lowerer& lowerer, WalkFrame frame, const hir::RangeSelectExpr& sel,
    mir::TypeId result_type) -> diag::Result<NamedMemory> {
  const UnitLowerer& unit_lowerer = lowerer.Owner();
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  mir::Block& block = *frame.current_block;
  const hir::Expr& base_expr = lowerer.HirExprs().Get(sel.base_value);
  auto base = lowerer.LowerAccessPath(base_expr, frame);
  if (!base) return std::unexpected(std::move(base.error()));
  const PositionMap map = PositionMapOf(unit_lowerer, base_expr.type);
  const std::uint64_t count = SliceCount(unit, result_type);
  const SliceStart start = SliceStartOf(sel.bounds, map, count);
  auto index = lowerer.LowerExpr(lowerer.HirExprs().Get(start.index), frame);
  if (!index) return std::unexpected(std::move(index.error()));
  const mir::ExprId index_id =
      EvaluatedOnce(frame, block.exprs.Add(*std::move(index)));
  // The array's index at the slice's leftmost position, and at its rightmost,
  // which lies `count - 1` positions on: an index falls as positions grow
  // where the array is declared descending (LRM 7.4.5).
  const std::int64_t sign = map.reversed ? -1 : 1;
  const std::int64_t to_left = sign * start.shift;
  const std::int64_t to_right =
      to_left + (sign * (static_cast<std::int64_t>(count) - 1));
  const mir::ExprId index_number =
      EvaluatedOnce(frame, BuildToInt64Call(unit, block, index_id));
  const auto offset = [&](std::int64_t by) {
    return mir::BuildMachineIntBinary(
        unit, block, mir::BinaryOp::kAdd, index_number,
        mir::BuildMachineIntLiteral(unit, block, by));
  };
  return NamedMemory{
      .path = DescendInto(
          *std::move(base),
          SliceStepAt(unit, block, map, start, index_id, count, result_type)),
      .slice =
          SliceIndices{.left = offset(to_left), .right = offset(to_right)}};
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

template <ExprLowerer Lowerer>
auto IsTagCheckedRead(
    Lowerer& lowerer, const WalkFrame& frame, const hir::Expr& select) -> bool {
  return frame.reads_as_of == ReadsAsOf::kNow &&
         ReachesThroughATag(lowerer, select) &&
         StorageFoundBy(lowerer, select) != StorageFound::kNone;
}

template <ExprLowerer Lowerer>
auto LowerTagCheckedUse(
    Lowerer& lowerer, WalkFrame frame, const hir::Expr& select,
    const TagCheckedUse& use) -> diag::Result<mir::Expr> {
  BlockBuilder steps(frame);
  mir::Block& body = steps.Body();
  auto path = lowerer.LowerAccessPath(select, steps.Frame());
  if (!path) return std::unexpected(std::move(path.error()));
  const SettledPath settled =
      SettledForRead(lowerer.Owner(), steps.Frame(), *std::move(path));
  auto value = use(steps, NamedIn(settled, body));
  if (!value) return std::unexpected(std::move(value.error()));
  return steps.Build(*value);
}

template <ExprLowerer Lowerer>
auto LowerTagCheckedRead(
    Lowerer& lowerer, WalkFrame frame, const hir::Expr& select)
    -> diag::Result<mir::Expr> {
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  return LowerTagCheckedUse(
      lowerer, frame, select,
      [&](BlockBuilder& steps,
          const AccessPath& path) -> diag::Result<mir::ExprId> {
        // A read answers with the storage it reached until its value is kept,
        // so the value is kept here, before the steps end.
        mir::Block& body = steps.Body();
        const mir::ExprId value = PathValueAsDeclared(unit, body, path);
        const mir::TypeId type = body.exprs.Get(value).type;
        return body.ReadLocal(steps.DeclareLocal(type, value), type);
      });
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
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  const mir::ExprId storage = ReportedPlace(unit, frame, *held);
  return mir::MakeDerefExpr(
      frame.current_block->DerefOperand(unit.types, storage),
      unit.types.Get(frame.current_block->exprs.Get(storage).type)
          .template Get<mir::PointerType>()
          .pointee);
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
// so each pass class's dispatcher links against the symbols emitted here.
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
template auto LowerHirArraySliceWithIndices(
    ProcessLowerer&, WalkFrame, const hir::RangeSelectExpr&, mir::TypeId)
    -> diag::Result<NamedMemory>;
template auto LowerHirMemberAccessExprPath(
    ProcessLowerer&, WalkFrame, const hir::MemberAccessExpr&, mir::TypeId)
    -> diag::Result<AccessPath>;
template auto LowerHirMemberAccessExprPath(
    const StructuralScopeLowerer&, WalkFrame, const hir::MemberAccessExpr&,
    mir::TypeId) -> diag::Result<AccessPath>;
template auto PropertyPath(
    ProcessLowerer&, const WalkFrame&, mir::ExprId,
    const hir::ClassPropertyTarget&, mir::TypeId) -> AccessPath;
template auto IsTagCheckedRead(
    ProcessLowerer&, const WalkFrame&, const hir::Expr&) -> bool;
template auto IsTagCheckedRead(
    const StructuralScopeLowerer&, const WalkFrame&, const hir::Expr&) -> bool;
template auto LowerTagCheckedUse(
    ProcessLowerer&, WalkFrame, const hir::Expr&, const TagCheckedUse&)
    -> diag::Result<mir::Expr>;
template auto LowerTagCheckedUse(
    const StructuralScopeLowerer&, WalkFrame, const hir::Expr&,
    const TagCheckedUse&) -> diag::Result<mir::Expr>;
template auto LowerTagCheckedRead(ProcessLowerer&, WalkFrame, const hir::Expr&)
    -> diag::Result<mir::Expr>;
template auto LowerTagCheckedRead(
    const StructuralScopeLowerer&, WalkFrame, const hir::Expr&)
    -> diag::Result<mir::Expr>;
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

template auto LowerStreamWindow(
    ProcessLowerer&, const WalkFrame&, hir::TypeId, const hir::StreamRange&)
    -> diag::Result<StreamWindow>;
template auto LowerStreamWindow(
    const StructuralScopeLowerer&, const WalkFrame&, hir::TypeId,
    const hir::StreamRange&) -> diag::Result<StreamWindow>;
template auto LowerStreamWindowRun(
    ProcessLowerer&, const WalkFrame&, const hir::StreamOperand&)
    -> diag::Result<mir::Expr>;
template auto LowerStreamWindowRun(
    const StructuralScopeLowerer&, const WalkFrame&, const hir::StreamOperand&)
    -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
