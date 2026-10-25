#include "lyra/lowering/hir_to_mir/bitstream.hpp"

#include <algorithm>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <utility>
#include <vector>

#include "lyra/base/diag_message.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/lowering/hir_to_mir/value_operations.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto WidenedWith(StreamShape shape, StreamShape part) -> StreamShape {
  return StreamShape{
      .width = shape.width + part.width,
      .state_kind = part.state_kind == mir::IntegralStateKind::kFourState
                        ? mir::IntegralStateKind::kFourState
                        : shape.state_kind};
}

// What a stream's parts take together, the first part whose width the program
// decides filling for all of them.
auto FillOfParts(
    const mir::CompilationUnit& unit, std::span<const mir::TypeId> parts)
    -> StreamFill {
  StreamFill fill{.empty_width = 0, .first_element = 0, .multiplier = 0};
  for (const mir::TypeId part : parts) {
    const StreamFill own = StreamFillOf(unit, part);
    fill.empty_width += own.empty_width;
    if (fill.multiplier == 0) {
      fill.first_element = own.first_element;
      fill.multiplier = own.multiplier;
    }
  }
  return fill;
}

// The run of bits the integral stream `bits` is.
auto RunOfBits(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId bits)
    -> mir::ExprId {
  return block.exprs.Add(
      mir::MakeBuiltinCall(
          unit, block, support::BuiltinFn::kBitVecOf, std::nullopt,
          {block.AsValue(unit.types, bits)}, unit.builtins.bit_vec));
}

// The run `run` widened to `width` bits, a machine count, by zero bits on its
// right.
auto PaddedTo(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId run,
    mir::ExprId width) -> mir::ExprId {
  return block.exprs.Add(
      mir::MakeBuiltinCall(
          unit, block, support::BuiltinFn::kBitVecPaddedTo, run, {width},
          unit.builtins.bit_vec));
}

// The run `run`, of exactly `shape.width` bits, read back as a value of
// `dst_type`, whose type fixes that width.
auto FromFixedRun(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId run,
    StreamShape shape, mir::TypeId dst_type) -> mir::ExprId {
  return BuildFromBitstream(
      unit, block,
      BuildRunLoad(
          unit, block, run, mir::BuildMachineCount(unit, block, 0), shape),
      dst_type);
}

// Whether `type`, or any type a value of it holds -- a member, an element --
// is one `holds` answers for.
template <typename Holds>
auto HoldsAnywhere(
    const mir::CompilationUnit& unit, mir::TypeId type, const Holds& holds)
    -> bool {
  const mir::Type& resolved = unit.types.Get(type);
  if (holds(resolved)) {
    return true;
  }
  if (const mir::RecordId* record = mir::RecordOf(resolved)) {
    return std::ranges::any_of(
        mir::RecordElements(unit, *record), [&](mir::TypeId element) {
          return HoldsAnywhere(unit, element, holds);
        });
  }
  if (const std::optional<mir::TypeId> element =
          resolved.ContainerElementType()) {
    return HoldsAnywhere(unit, *element, holds);
  }
  return false;
}

}  // namespace

auto HoldsClassHandle(const mir::CompilationUnit& unit, mir::TypeId type)
    -> bool {
  return HoldsAnywhere(unit, type, [](const mir::Type& part) {
    return part.Is<mir::ManagedRefType>();
  });
}

auto IsStreamDestination(const mir::CompilationUnit& unit, mir::TypeId type)
    -> bool {
  return !HoldsAnywhere(unit, type, [](const mir::Type& part) {
    return part.Is<mir::ManagedRefType>() ||
           part.Is<mir::AssociativeArrayType>();
  });
}

auto FixedStreamShapeOfParts(
    const mir::CompilationUnit& unit, std::span<const mir::TypeId> parts)
    -> std::optional<StreamShape> {
  StreamShape shape{
      .width = 0, .state_kind = mir::IntegralStateKind::kTwoState};
  for (const mir::TypeId component : parts) {
    const std::optional<StreamShape> part = FixedStreamShapeOf(unit, component);
    if (!part.has_value()) {
      return std::nullopt;
    }
    shape = WidenedWith(shape, *part);
  }
  return shape;
}

auto FixedStreamShapeOf(const mir::CompilationUnit& unit, mir::TypeId type)
    -> std::optional<StreamShape> {
  const mir::Type& resolved = unit.types.Get(type);
  if (resolved.IsIntegral()) {
    const mir::IntegralType& integral = resolved.Integral();
    return StreamShape{
        .width = integral.bit_width, .state_kind = integral.state_kind};
  }
  if (const auto* tuple = resolved.As<mir::TupleType>()) {
    return FixedStreamShapeOfParts(unit, tuple->elements);
  }
  // A structure streams its members end to end, an untagged union as its first
  // member, and a tagged union not at all (LRM 11.4.14.1).
  if (const mir::RecordId* record = mir::RecordOf(resolved)) {
    const std::span<const mir::TypeId> members =
        mir::RecordElements(unit, *record);
    switch (mir::RecordKindOf(unit, *record)) {
      case mir::RecordKind::kStruct:
        return FixedStreamShapeOfParts(unit, members);
      case mir::RecordKind::kUnion:
        return FixedStreamShapeOf(unit, members.front());
      case mir::RecordKind::kTaggedUnion:
        return std::nullopt;
    }
    throw InternalError("FixedStreamShapeOf: unknown record kind");
  }
  if (resolved.Is<mir::UnpackedArrayType>()) {
    const auto& array = resolved.Get<mir::UnpackedArrayType>();
    const std::optional<StreamShape> element =
        FixedStreamShapeOf(unit, array.element_type);
    if (!element.has_value()) {
      return std::nullopt;
    }
    return StreamShape{
        .width = array.Size() * element->width,
        .state_kind = element->state_kind};
  }
  return std::nullopt;
}

// LRM 11.4.14.4: a part whose type fixes its width takes that many bits; a
// string takes whole characters (LRM 6.24.3); a dynamic array or a queue takes
// whole elements, or, where an element's own width is the program's, a single
// one taking all; and an aggregate takes its parts', the first of them whose
// width is the program's filling for all.
auto StreamFillOf(const mir::CompilationUnit& unit, mir::TypeId type)
    -> StreamFill {
  if (const std::optional<StreamShape> fixed = FixedStreamShapeOf(unit, type)) {
    return StreamFill{
        .empty_width = fixed->width, .first_element = 0, .multiplier = 0};
  }
  const mir::Type& resolved = unit.types.Get(type);
  if (resolved.Is<mir::StringType>()) {
    return StreamFill{.empty_width = 0, .first_element = 0, .multiplier = 8};
  }
  if (resolved.Is<mir::DynamicArrayType>() || resolved.Is<mir::QueueType>()) {
    const mir::TypeId element = *resolved.ContainerElementType();
    if (const std::optional<StreamShape> fixed =
            FixedStreamShapeOf(unit, element)) {
      return StreamFill{
          .empty_width = 0, .first_element = 0, .multiplier = fixed->width};
    }
    // A single element takes all, which is what its own first element and
    // steps take past its empty width.
    const StreamFill one = StreamFillOf(unit, element);
    return StreamFill{
        .empty_width = 0,
        .first_element = one.empty_width + one.first_element,
        .multiplier = one.multiplier};
  }
  if (const auto* array = resolved.As<mir::UnpackedArrayType>()) {
    const StreamFill element = StreamFillOf(unit, array->element_type);
    return StreamFill{
        .empty_width = array->Size() * element.empty_width,
        .first_element = element.first_element,
        .multiplier = element.multiplier};
  }
  if (const mir::RecordId* record = mir::RecordOf(resolved)) {
    return FillOfParts(unit, mir::RecordElements(unit, *record));
  }
  throw InternalError(
      "StreamFillOf: a stream is read back into a type that holds none, which "
      "the front end refuses (LRM 6.24.3)");
}

auto BuildToBitstream(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId value_id,
    diag::SourceSpan span) -> diag::Result<mir::ExprId> {
  const mir::TypeId type = block.exprs.Get(value_id).type;
  if (type == unit.builtins.bit_vec) {
    return value_id;
  }
  if (HoldsClassHandle(unit, type)) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "streaming the object a class handle refers to is not yet supported");
  }
  const std::optional<StreamShape> shape = FixedStreamShapeOf(unit, type);
  if (!shape.has_value()) {
    return BuildValueOperation(
        unit, block, support::BuiltinFn::kToBitVec, value_id, {},
        unit.builtins.bit_vec);
  }
  const mir::TypeId stream =
      mir::PackedVectorOf(unit.types, shape->width, shape->state_kind);
  // A packed value's stream is its own bits, read unsigned (LRM 6.24.3).
  if (unit.types.Get(type).IsIntegral()) {
    return AtType(unit, block, value_id, stream);
  }
  return BuildValueOperation(
      unit, block, support::BuiltinFn::kToBitstream, value_id, {}, stream);
}

auto BuildRunOf(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId stream)
    -> mir::ExprId {
  return block.exprs.Get(stream).type == unit.builtins.bit_vec
             ? stream
             : RunOfBits(unit, block, stream);
}

namespace {

// A run of no bits, which a run is built up from one part after another.
auto BuildEmptyRun(const mir::CompilationUnit& unit, mir::Block& block)
    -> mir::ExprId {
  return block.exprs.Add(
      mir::Expr{
          .data = mir::CallExpr{.callee = mir::Construct{}, .arguments = {}},
          .type = unit.builtins.bit_vec});
}

// `part`, an integral stream or a run of bits, laid after the bits the local
// `run` holds (LRM 11.4.14.1).
void AppendToRun(
    const mir::CompilationUnit& unit, mir::Block& block, mir::LocalId run,
    mir::ExprId part) {
  const support::BuiltinFn entry =
      block.exprs.Get(part).type == unit.builtins.bit_vec
          ? support::BuiltinFn::kBitVecAppend
          : support::BuiltinFn::kBitVecExtend;
  block.AppendStmt(
      mir::ExprStmt{
          .expr = block.exprs.Add(
              mir::MakeBuiltinCall(
                  unit, block, entry,
                  block.LocalPlace(run, unit.builtins.bit_vec),
                  {block.AsValue(unit.types, part)},
                  unit.builtins.void_type))});
}

}  // namespace

auto BuildRunOfZeros(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId width)
    -> mir::ExprId {
  return PaddedTo(unit, block, BuildEmptyRun(unit, block), width);
}

void InsertIntoRun(
    const mir::CompilationUnit& unit, mir::Block& block, mir::LocalId run,
    mir::ExprId at, mir::ExprId part) {
  block.AppendStmt(
      mir::ExprStmt{
          .expr = block.exprs.Add(
              mir::MakeBuiltinCall(
                  unit, block, support::BuiltinFn::kBitVecInsertBits,
                  block.LocalPlace(run, unit.builtins.bit_vec),
                  {at, block.AsValue(unit.types, part)},
                  unit.builtins.void_type))});
}

auto BuildRunJoin(
    const mir::CompilationUnit& unit, mir::Block& block, mir::LocalId run,
    std::span<const mir::ExprId> parts) -> mir::ExprId {
  block.AppendStmt(
      mir::LocalDeclStmt{.target = run, .init = BuildEmptyRun(unit, block)});
  for (const mir::ExprId part : parts) {
    AppendToRun(unit, block, run, part);
  }
  return block.ReadLocal(run, unit.builtins.bit_vec);
}

auto BuildReorderedStream(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId stream_id,
    std::uint64_t block_bits) -> mir::ExprId {
  if (block_bits == 0) {
    return stream_id;
  }
  const mir::TypeId stream_type = block.exprs.Get(stream_id).type;
  const support::BuiltinFn entry =
      stream_type == unit.builtins.bit_vec
          ? support::BuiltinFn::kBitVecReverseBlocks
          : support::BuiltinFn::kReverseBlocks;
  return block.exprs.Add(
      mir::MakeBuiltinCall(
          unit, block, entry, stream_id,
          {mir::BuildMachineCount(unit, block, block_bits)}, stream_type));
}

auto BuildRunLength(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId run)
    -> mir::ExprId {
  return block.exprs.Add(
      mir::MakeBuiltinCall(
          unit, block, support::BuiltinFn::kBitVecLen, run, {},
          unit.builtins.machine_int64));
}

auto BuildRunExtract(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId run,
    mir::ExprId at, mir::ExprId count) -> mir::ExprId {
  return block.exprs.Add(
      mir::MakeBuiltinCall(
          unit, block, support::BuiltinFn::kBitVecExtractBits, run, {at, count},
          unit.builtins.bit_vec));
}

auto BuildRunLoad(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId run,
    mir::ExprId at, StreamShape shape) -> mir::ExprId {
  const mir::TypeId bits =
      mir::PackedVectorOf(unit.types, shape.width, shape.state_kind);
  return block.exprs.Add(
      mir::MakeBuiltinCall(
          unit, block, support::BuiltinFn::kBitVecLoad, run, std::nullopt,
          {bits}, {at}, bits));
}

auto BuildFromBitstream(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId bits_id,
    mir::TypeId dst_type) -> mir::ExprId {
  // A stream is a sequence of bits, so its own shape is on its type and asking
  // for it cannot fail; only the destination can be a type with no stream.
  const mir::IntegralType stream =
      unit.types.Get(block.exprs.Get(bits_id).type).Integral();
  const std::optional<StreamShape> target = FixedStreamShapeOf(unit, dst_type);
  if (!target.has_value()) {
    throw InternalError(
        "BuildFromBitstream: an integral stream is read back into a type whose "
        "width the program decides, which reads a run of bits -- please report "
        "this as a bug");
  }
  const std::uint64_t stream_width = stream.bit_width;
  if (stream_width > target->width) {
    throw InternalError(
        "BuildFromBitstream: a stream wider than what it fills reaches here "
        "only after whoever divided it took its leading bits -- please report "
        "this as a bug");
  }
  mir::ExprId filled = bits_id;
  if (stream_width < target->width) {
    // The stream lands in the most significant bits and zeros fill the rest:
    // widened, which puts it in the least significant ones, and moved up. The
    // zeros carry no x or z, so the stream's own domain is the result's.
    const mir::TypeId widened =
        mir::PackedVectorOf(unit.types, target->width, stream.state_kind);
    filled = block.exprs.Add(
        mir::MakeBuiltinCall(
            unit, block, support::BuiltinFn::kShiftLeft,
            ConvertToType(unit, block, bits_id, widened),
            {mir::BuildIntLiteral(
                unit, block,
                static_cast<std::int64_t>(target->width - stream_width))},
            widened));
  }
  // A packed destination is those bits, at the signedness and state domain it
  // declares.
  if (unit.types.Get(dst_type).IsIntegral()) {
    return ConvertToType(unit, block, filled, dst_type);
  }
  // The destination takes its stream in the state domain its own parts settle,
  // which a stream cast from a value of the other domain is brought to.
  const mir::ExprId stream_id = AtType(
      unit, block, filled,
      mir::PackedVectorOf(unit.types, target->width, target->state_kind));
  return BuildValueOperation(
      unit, block, support::BuiltinFn::kFromBitstream, std::nullopt,
      {stream_id}, dst_type);
}

auto BuildFromStreamAssigned(
    const mir::CompilationUnit& unit, const WalkFrame& frame,
    mir::ExprId bits_id, mir::TypeId dst_type) -> mir::ExprId {
  mir::Block& block = *frame.current_block;
  const std::optional<StreamShape> fixed = FixedStreamShapeOf(unit, dst_type);
  if (block.exprs.Get(bits_id).type != unit.builtins.bit_vec) {
    if (fixed.has_value()) {
      return BuildFromBitstream(unit, block, bits_id, dst_type);
    }
    return BuildFromStreamAssigned(
        unit, frame, RunOfBits(unit, block, bits_id), dst_type);
  }
  const mir::ExprId run =
      EvaluatedOnce(frame, block.AsValue(unit.types, bits_id));
  const mir::ExprId length =
      EvaluatedOnce(frame, BuildRunLength(unit, block, run));
  if (fixed.has_value()) {
    const mir::ExprId width = mir::BuildMachineCount(unit, block, fixed->width);
    mir::AppendRunTimeCheck(
        unit, block,
        mir::BuildMachineIntComparison(
            unit, block, mir::BinaryOp::kLessEqual, length, width),
        base::DiagMessage(
            "a streaming concatenation holds more bits than the variable it "
            "is assigned to")
            .TakeText());
    return FromFixedRun(
        unit, block,
        PaddedTo(
            unit, block, run,
            mir::BuildMachineCount(unit, block, fixed->width)),
        *fixed, dst_type);
  }
  // The fewest elements as wide as the stream: a stream no wider than the
  // destination's empty width is widened to it, and a wider one to that width,
  // the first element of the part whose width the program decides, and as many
  // whole steps past it as cover the rest.
  const StreamFill fill = StreamFillOf(unit, dst_type);
  const mir::ExprId past = EvaluatedOnce(
      frame, mir::BuildMachineIntBinary(
                 unit, block, mir::BinaryOp::kSub, length,
                 mir::BuildMachineCount(unit, block, fill.empty_width)));
  const mir::ExprId beyond = EvaluatedOnce(
      frame, mir::BuildMachineIntBinary(
                 unit, block, mir::BinaryOp::kSub, past,
                 mir::BuildMachineCount(unit, block, fill.first_element)));
  const mir::ExprId steps = mir::BuildMachineIntBinary(
      unit, block, mir::BinaryOp::kMul,
      mir::BuildMachineIntBinary(
          unit, block, mir::BinaryOp::kDiv,
          mir::BuildMachineIntBinary(
              unit, block, mir::BinaryOp::kAdd, beyond,
              mir::BuildMachineCount(unit, block, fill.multiplier - 1)),
          mir::BuildMachineCount(unit, block, fill.multiplier)),
      mir::BuildMachineCount(unit, block, fill.multiplier));
  const mir::ExprId width = mir::BuildMachineIntConditional(
      unit, block,
      mir::BuildMachineIntComparison(
          unit, block, mir::BinaryOp::kLessEqual, past,
          mir::BuildMachineCount(unit, block, 0)),
      mir::BuildMachineCount(unit, block, fill.empty_width),
      mir::BuildMachineIntBinary(
          unit, block, mir::BinaryOp::kAdd,
          mir::BuildMachineCount(
              unit, block, fill.empty_width + fill.first_element),
          mir::BuildMachineIntConditional(
              unit, block,
              mir::BuildMachineIntComparison(
                  unit, block, mir::BinaryOp::kLessEqual, beyond,
                  mir::BuildMachineCount(unit, block, 0)),
              mir::BuildMachineCount(unit, block, 0), steps)));
  return BuildFromBitVec(
      unit, block, PaddedTo(unit, block, run, width), dst_type);
}

auto BuildFromBitstreamCast(
    const mir::CompilationUnit& unit, const WalkFrame& frame,
    mir::ExprId bits_id, mir::TypeId dst_type) -> mir::ExprId {
  mir::Block& block = *frame.current_block;
  const std::optional<StreamShape> fixed = FixedStreamShapeOf(unit, dst_type);
  if (block.exprs.Get(bits_id).type != unit.builtins.bit_vec) {
    if (fixed.has_value()) {
      return BuildFromBitstream(unit, block, bits_id, dst_type);
    }
    return BuildFromBitVec(
        unit, block, RunOfBits(unit, block, bits_id), dst_type);
  }
  if (fixed.has_value()) {
    const mir::ExprId run =
        EvaluatedOnce(frame, block.AsValue(unit.types, bits_id));
    mir::AppendRunTimeCheck(
        unit, block,
        mir::BuildMachineIntComparison(
            unit, block, mir::BinaryOp::kEquality,
            BuildRunLength(unit, block, run),
            mir::BuildMachineCount(unit, block, fixed->width)),
        base::DiagMessage(
            "a bit-stream cast's operand does not hold as many bits as its "
            "type")
            .TakeText());
    return FromFixedRun(unit, block, run, *fixed, dst_type);
  }
  return BuildFromBitVec(unit, block, bits_id, dst_type);
}

auto BuildFromBitVec(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId run,
    mir::TypeId dst_type) -> mir::ExprId {
  const mir::Type& dst = unit.types.Get(dst_type);
  const mir::ExprId held = block.AsValue(unit.types, run);
  if (dst.Is<mir::StringType>()) {
    return block.exprs.Add(
        mir::MakeBuiltinCall(
            unit, block, support::BuiltinFn::kStringFromBitVec, std::nullopt,
            {held}, dst_type));
  }
  return BuildValueOperation(
      unit, block, support::BuiltinFn::kFromBitVec, std::nullopt, {held},
      dst_type);
}

}  // namespace lyra::lowering::hir_to_mir
