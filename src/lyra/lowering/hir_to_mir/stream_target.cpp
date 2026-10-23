#include "lyra/lowering/hir_to_mir/stream_target.hpp"

#include <cstddef>
#include <cstdint>
#include <expected>
#include <numeric>
#include <optional>
#include <span>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/diag_message.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/bitstream.hpp"
#include "lyra/lowering/hir_to_mir/expression/selects.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// What a member's bits are handed to once they are cut: a variable, given
// them read back at its type, or a stream nested in the target that re-orders
// its bits, which cuts them among its own members in turn.
struct Recipient {
  std::variant<const StreamVariable*, const StreamTarget*> form;
  diag::SourceSpan span;
};

// The segment of the stream one member takes, as a segment of an Erlang bit
// pattern is (LRM 11.4.14.3, 11.4.14.4): as many bits as its type fixes; the
// elements a `with` names on `array`, which the range says when its turn comes
// and which are written where they are cut; or, where its width is the running
// program's, the rest -- its empty width, and for the first such member also
// as many whole elements as the members after it leave room for.
struct FixedSegment {
  StreamShape shape;
  Recipient recipient;
};
struct WindowSegment {
  const WindowedPlace* array;
  std::uint64_t element_width;
};
struct RestSegment {
  StreamFill fill;
  Recipient recipient;
};
using Segment = std::variant<FixedSegment, WindowSegment, RestSegment>;

// The bits a member is given where every member whose width the running
// program decides is empty, which is all that each of those after the first
// is given (LRM 11.4.14.4).
auto EmptyWidth(const Segment& segment) -> std::uint64_t {
  return std::visit(
      Overloaded{
          [](const FixedSegment& f) { return f.shape.width; },
          [](const WindowSegment&) -> std::uint64_t { return 0; },
          [](const RestSegment& r) { return r.fill.empty_width; }},
      segment);
}

auto SegmentOf(
    const mir::CompilationUnit& unit, const StreamTargetMember& member)
    -> Segment;

// The segments of the stream, one for each member of `stream`, in the order
// the source wrote them. A stream nested in it that re-orders nothing is its
// members laid in its place, as a nested concatenation is (LRM 11.4.14.2,
// 11.4.14.3).
void AppendSegments(
    const mir::CompilationUnit& unit, const StreamTarget& stream,
    std::vector<Segment>& segments) {
  for (const StreamTargetMember& member : stream.members) {
    const auto* nested = std::get_if<StreamTarget>(&member.form);
    if (nested != nullptr && nested->block_bits == 0) {
      AppendSegments(unit, *nested, segments);
      continue;
    }
    segments.push_back(SegmentOf(unit, member));
  }
}

auto SegmentsOf(const mir::CompilationUnit& unit, const StreamTarget& stream)
    -> std::vector<Segment> {
  std::vector<Segment> segments;
  AppendSegments(unit, stream, segments);
  return segments;
}

// A stream nested in a stream target is given what its own members are given
// together (LRM 11.4.14.3): as many bits as they fix, where every one does,
// and otherwise their empty widths and the steps their first member whose
// width the program decides is given elements in.
auto NestedSegmentOf(
    const mir::CompilationUnit& unit, const StreamTarget& stream,
    const Recipient& recipient) -> Segment {
  StreamShape shape{
      .width = 0, .state_kind = mir::IntegralStateKind::kTwoState};
  StreamFill fill{.empty_width = 0, .first_element = 0, .multiplier = 0};
  bool fixed = true;
  for (const Segment& segment : SegmentsOf(unit, stream)) {
    fill.empty_width += EmptyWidth(segment);
    std::visit(
        Overloaded{
            [&](const FixedSegment& f) {
              shape.width += f.shape.width;
              if (f.shape.state_kind == mir::IntegralStateKind::kFourState) {
                shape.state_kind = mir::IntegralStateKind::kFourState;
              }
            },
            [&](const WindowSegment& w) {
              fixed = false;
              if (fill.multiplier == 0) {
                fill.multiplier = w.element_width;
              }
            },
            [&](const RestSegment& r) {
              fixed = false;
              if (fill.multiplier == 0) {
                fill.first_element = r.fill.first_element;
                fill.multiplier = r.fill.multiplier;
              }
            }},
        segment);
  }
  if (fixed) {
    return FixedSegment{.shape = shape, .recipient = recipient};
  }
  return RestSegment{.fill = fill, .recipient = recipient};
}

auto SegmentOf(
    const mir::CompilationUnit& unit, const StreamTargetMember& member)
    -> Segment {
  return std::visit(
      Overloaded{
          [&](const StreamVariable& variable) -> Segment {
            const Recipient recipient{.form = &variable, .span = member.span};
            if (const std::optional<StreamShape> fixed =
                    FixedStreamShapeOf(unit, variable.type)) {
              return FixedSegment{.shape = *fixed, .recipient = recipient};
            }
            return RestSegment{
                .fill = StreamFillOf(unit, variable.type),
                .recipient = recipient};
          },
          [&](const WindowedPlace& windowed) -> Segment {
            return WindowSegment{
                .array = &windowed,
                .element_width =
                    FixedStreamShapeOf(unit, windowed.element)->width};
          },
          [&](const StreamTarget& nested) -> Segment {
            return NestedSegmentOf(
                unit, nested, Recipient{.form = &nested, .span = member.span});
          }},
      member.form);
}

auto Deliver(
    UnitLowerer& unit_lowerer, const WalkFrame& frame,
    const Recipient& recipient, mir::ExprId bits, const ShareSink& sink)
    -> diag::Result<void> {
  const mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Block& block = *frame.current_block;
  return std::visit(
      Overloaded{
          [&](const StreamVariable* variable) -> diag::Result<void> {
            const mir::ExprId value =
                block.exprs.Get(bits).type == unit.builtins.bit_vec
                    ? BuildFromBitVec(unit, block, bits, variable->type)
                    : BuildFromBitstream(unit, block, bits, variable->type);
            sink(
                Share{
                    .place = variable->place,
                    .value = block.AsValue(unit.types, value)});
            return {};
          },
          [&](const StreamTarget* nested) -> diag::Result<void> {
            return DistributeStreamTarget(
                unit_lowerer, frame, *nested, bits, recipient.span, sink);
          }},
      recipient.form);
}

// Where every member's type fixes its width, each is given its bits of the
// integral stream `source`, re-ordered once and then cut among the members
// from the most significant end. Where the stream carries more bits than the
// members need, the surplus is at its least significant end and is dropped,
// which is why the usable bits are taken before the re-ordering rather than
// after.
auto DistributeFixedStream(
    UnitLowerer& unit_lowerer, const WalkFrame& frame,
    std::span<const FixedSegment> segments, std::uint64_t block_bits,
    mir::ExprId source, const ShareSink& sink) -> diag::Result<void> {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Block& block = *frame.current_block;
  std::uint64_t members_width = 0;
  for (const FixedSegment& segment : segments) {
    members_width += segment.shape.width;
  }
  const mir::IntegralType stream =
      unit.types.Get(block.exprs.Get(source).type).Integral();
  if (stream.bit_width < members_width) {
    throw InternalError(
        "lvalue: the front end refuses a source with fewer bits than a "
        "streaming target needs (LRM 11.4.14.3) -- please report this as a "
        "bug");
  }
  // How wide each member's share is, and never its state domain: the bits come
  // from the source, so the stream's domain is the source's, and a member's
  // own is applied where its share is read back at its type.
  const mir::TypeId stream_type =
      mir::PackedVectorOf(unit.types, members_width, stream.state_kind);
  const mir::ExprId held = EvaluatedOnce(
      frame, BuildReorderedStream(
                 unit, block,
                 block.exprs.Add(BuildPackedBitsRead(
                     unit_lowerer, block, source,
                     stream.bit_width - members_width, stream_type)),
                 block_bits));
  std::uint64_t offset = members_width;
  for (const FixedSegment& segment : segments) {
    offset -= segment.shape.width;
    const mir::ExprId bits = block.exprs.Add(BuildPackedBitsRead(
        unit_lowerer, block, held, offset,
        mir::PackedVectorOf(
            unit.types, segment.shape.width, stream.state_kind)));
    auto handed = Deliver(unit_lowerer, frame, segment.recipient, bits, sink);
    if (!handed) return handed;
  }
  return {};
}

// Where some member's bits are the running program's (LRM 11.4.14.4), the
// stream is a run of bits, and each member is given its bits in turn from a
// position that moves as it goes, its share handed on before the next member
// is cut, so a `with` range reads what the members before it were given.
auto DistributeRun(
    UnitLowerer& unit_lowerer, const WalkFrame& frame,
    std::span<const Segment> segments, std::uint64_t block_bits,
    mir::ExprId source, const ShareSink& sink) -> diag::Result<void> {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Block& block = *frame.current_block;
  const mir::TypeId count_type = unit.builtins.machine_int64;
  const auto count = [&](std::uint64_t n) {
    return mir::BuildMachineCount(unit, block, n);
  };
  const auto binary = [&](mir::BinaryOp op) {
    return [&, op](mir::ExprId lhs, mir::ExprId rhs) {
      return mir::BuildMachineIntBinary(unit, block, op, lhs, rhs);
    };
  };
  const auto add = binary(mir::BinaryOp::kAdd);
  const auto sub = binary(mir::BinaryOp::kSub);
  const auto mul = binary(mir::BinaryOp::kMul);
  const auto div = binary(mir::BinaryOp::kDiv);
  const auto compare = [&](mir::BinaryOp op, mir::ExprId lhs, mir::ExprId rhs) {
    return mir::BuildMachineIntComparison(unit, block, op, lhs, rhs);
  };
  const auto require_enough = [&](mir::ExprId holds) {
    mir::AppendRunTimeCheck(
        unit, block, holds,
        base::DiagMessage(
            "a stream of bits holds fewer bits than the variables it is "
            "unpacked into need")
            .TakeText());
  };
  // The most of `n` bits that whole elements `each` bits wide cover.
  const auto whole_steps = [&](mir::ExprId n, std::uint64_t each) {
    return mul(div(n, count(each)), count(each));
  };

  // LRM 11.4.14.4: the bits the members are given where every one whose width
  // the program decides is empty, and past those the bits the stream holds in
  // whole elements of every `with` range and of the first other such member,
  // each a multiple of their greatest common divisor -- the ranges drawing
  // their elements from them and that member given what is left, every later
  // one left empty. What is past that is the surplus, at the least significant
  // end.
  std::uint64_t empty = 0;
  std::uint64_t step = 0;
  std::optional<std::size_t> greedy;
  for (std::size_t i = 0; i < segments.size(); ++i) {
    empty += EmptyWidth(segments[i]);
    std::visit(
        Overloaded{
            [](const FixedSegment&) {},
            [&](const WindowSegment& w) {
              step = std::gcd(step, w.element_width);
            },
            [&](const RestSegment& r) {
              if (!greedy.has_value()) {
                step = std::gcd(step, r.fill.multiplier);
                greedy = i;
              }
            }},
        segments[i]);
  }
  // What the members after each are given where they are empty.
  std::vector<std::uint64_t> after(segments.size(), 0);
  for (std::size_t i = segments.size(); i-- > 1;) {
    after[i - 1] = after[i] + EmptyWidth(segments[i]);
  }

  const mir::ExprId whole =
      EvaluatedOnce(frame, BuildRunOf(unit, block, source));
  const mir::ExprId length =
      EvaluatedOnce(frame, BuildRunLength(unit, block, whole));
  require_enough(compare(mir::BinaryOp::kGreaterEqual, length, count(empty)));
  const mir::ExprId usable = EvaluatedOnce(
      frame, step == 0 ? count(empty)
                       : add(count(empty),
                             whole_steps(sub(length, count(empty)), step)));
  const mir::ExprId run =
      block_bits == 0
          ? whole
          : EvaluatedOnce(
                frame,
                BuildReorderedStream(
                    unit, block,
                    BuildRunExtract(unit, block, whole, count(0), usable),
                    block_bits));

  const mir::LocalId at = DeclareLocal(frame, count(0));
  const auto at_now = [&] { return block.ReadLocal(at, count_type); };
  const auto move_at = [&](mir::ExprId given) {
    block.AppendStmt(
        mir::ExprStmt{
            .expr = block.exprs.Add(BuildStoreExpr(
                unit, block,
                AccessPath{
                    .owner = block.LocalPlace(at, count_type), .descent = {}},
                add(at_now(), given)))});
  };

  for (std::size_t i = 0; i < segments.size(); ++i) {
    const auto handed = std::visit(
        Overloaded{
            [&](const FixedSegment& f) -> diag::Result<void> {
              const mir::ExprId bits = EvaluatedOnce(
                  frame, BuildRunLoad(unit, block, run, at_now(), f.shape));
              move_at(count(f.shape.width));
              return Deliver(unit_lowerer, frame, f.recipient, bits, sink);
            },
            [&](const WindowSegment& w) -> diag::Result<void> {
              const WindowedPlace& windowed = *w.array;
              auto window = windowed.window(frame, windowed.array);
              if (!window) return std::unexpected(std::move(window.error()));
              const mir::ExprId bits = EvaluatedOnce(
                  frame, mul(window->count, count(w.element_width)));
              require_enough(compare(
                  mir::BinaryOp::kLessEqual, add(at_now(), bits), usable));
              const mir::TypeId elements = unit.types.Intern(
                  mir::Type{
                      mir::DynamicArrayType{.element_type = windowed.element}});
              const mir::ExprId read = EvaluatedOnce(
                  frame, BuildFromBitVec(
                             unit, block,
                             BuildRunExtract(unit, block, run, at_now(), bits),
                             elements));
              const mir::ExprId filled = EvaluatedOnce(
                  frame, block.exprs.Add(
                             mir::MakeBuiltinCall(
                                 unit, block, support::BuiltinFn::kWithWindow,
                                 PathValue(unit, block, windowed.array),
                                 {window->start, window->count, read},
                                 windowed.type)));
              move_at(bits);
              sink(Share{.place = windowed.array, .value = filled});
              return {};
            },
            [&](const RestSegment& r) -> diag::Result<void> {
              // Every such member is given its empty width, which the room
              // left after the members behind it has to hold. The first is
              // also given, where that room holds its first element, that
              // element and as many whole steps as fit after it.
              const mir::ExprId past = EvaluatedOnce(
                  frame, sub(sub(usable, at_now()),
                             count(after[i] + r.fill.empty_width)));
              require_enough(
                  compare(mir::BinaryOp::kGreaterEqual, past, count(0)));
              const auto whole_elements_past = [&] {
                const mir::ExprId beyond = EvaluatedOnce(
                    frame, sub(past, count(r.fill.first_element)));
                return mir::BuildMachineIntConditional(
                    unit, block,
                    compare(mir::BinaryOp::kLessThan, beyond, count(0)),
                    count(0),
                    add(count(r.fill.first_element),
                        whole_steps(beyond, r.fill.multiplier)));
              };
              const mir::ExprId given = EvaluatedOnce(
                  frame, greedy == i ? add(count(r.fill.empty_width),
                                           whole_elements_past())
                                     : count(r.fill.empty_width));
              const mir::ExprId bits = EvaluatedOnce(
                  frame, BuildRunExtract(unit, block, run, at_now(), given));
              move_at(given);
              return Deliver(unit_lowerer, frame, r.recipient, bits, sink);
            }},
        segments[i]);
    if (!handed) return handed;
  }
  return {};
}

// One member of a stream target: a variable, an array a `with` ranges, whose
// range is asked when its turn comes, or a stream nested in it. The front end
// holds the rest -- a concatenation, a pattern, a `with` on anything but one
// unpacked array -- out of a stream target (LRM 11.4.14.3, 11.4.14.4).
template <ExprLowerer L>
auto LowerStreamMember(
    L& lowerer, const hir::StreamOperand& operand, WalkFrame frame)
    -> diag::Result<StreamTargetMember> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  const hir::Expr& expr = lowerer.HirExprs().Get(operand.value);
  const auto* nested = std::get_if<hir::StreamingConcatExpr>(&expr.data);
  if (IsJoin(expr) && (nested == nullptr || operand.with.has_value())) {
    throw InternalError(
        "lvalue: a stream target holds a concatenation, a pattern, or a "
        "`with` on a stream, which the front end refuses (LRM 11.4.14.3, "
        "11.4.14.4)");
  }
  if (nested != nullptr) {
    auto lowered = LowerStreamTarget(lowerer, *nested, frame);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    return StreamTargetMember{.form = *std::move(lowered), .span = expr.span};
  }
  auto place = lowerer.LowerLhsExpr(expr, frame);
  if (!place) return std::unexpected(std::move(place.error()));
  const mir::TypeId type = unit_lowerer.TranslateType(expr.type);
  if (!operand.with.has_value()) {
    return StreamTargetMember{
        .form = StreamVariable{.place = *std::move(place), .type = type},
        .span = expr.span};
  }
  const hir::TypeId array_type = expr.type;
  const hir::StreamRange range = *operand.with;
  const bool queue =
      unit_lowerer.Hir().types.Get(array_type).Is<hir::QueueType>();
  return StreamTargetMember{
      .form =
          WindowedPlace{
              .array = *std::move(place),
              .type = type,
              .element =
                  *unit_lowerer.Unit().types.Get(type).ContainerElementType(),
              .window =
                  [&lowerer, array_type, range, queue](
                      const WalkFrame& at, const AccessPath& array) {
                    const SettledPath settled{
                        .named_in = at.current_block,
                        .owner = array.owner,
                        .descent = array.descent};
                    return LowerStreamWindow(
                        lowerer, queue ? at.WithSelectedQueue(&settled) : at,
                        array_type, range);
                  }},
      .span = expr.span};
}

}  // namespace

template <ExprLowerer L>
auto LowerStreamTarget(
    L& lowerer, const hir::StreamingConcatExpr& stream, WalkFrame frame)
    -> diag::Result<StreamTarget> {
  StreamTarget target{.block_bits = stream.block_bits, .members = {}};
  target.members.reserve(stream.operands.size());
  for (const hir::StreamOperand& operand : stream.operands) {
    auto member = LowerStreamMember(lowerer, operand, frame);
    if (!member) return std::unexpected(std::move(member.error()));
    target.members.push_back(*std::move(member));
  }
  return target;
}

template auto LowerStreamTarget(
    ProcessLowerer&, const hir::StreamingConcatExpr&, WalkFrame)
    -> diag::Result<StreamTarget>;
template auto LowerStreamTarget(
    const StructuralScopeLowerer&, const hir::StreamingConcatExpr&, WalkFrame)
    -> diag::Result<StreamTarget>;

auto DistributeStreamTarget(
    UnitLowerer& unit_lowerer, const WalkFrame& frame,
    const StreamTarget& stream, mir::ExprId value, diag::SourceSpan span,
    const ShareSink& sink) -> diag::Result<void> {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Block& block = *frame.current_block;
  const std::vector<Segment> segments = SegmentsOf(unit, stream);
  auto source = BuildToBitstream(unit, block, value, span);
  if (!source) return std::unexpected(std::move(source.error()));
  std::vector<FixedSegment> fixed;
  for (const Segment& segment : segments) {
    if (const auto* size = std::get_if<FixedSegment>(&segment)) {
      fixed.push_back(*size);
    }
  }
  if (fixed.size() == segments.size() &&
      unit.types.Get(block.exprs.Get(*source).type).IsIntegral()) {
    return DistributeFixedStream(
        unit_lowerer, frame, fixed, stream.block_bits, *source, sink);
  }
  return DistributeRun(
      unit_lowerer, frame, segments, stream.block_bits, *source, sink);
}

}  // namespace lyra::lowering::hir_to_mir
