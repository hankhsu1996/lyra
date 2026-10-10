#include "lyra/lowering/hir_to_mir/bitstream.hpp"

#include <cstdint>
#include <optional>
#include <span>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/lowering/hir_to_mir/struct_methods.hpp"
#include "lyra/mir/expr.hpp"
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

// Several kinds of type reach here -- one whose bit count only the running
// program has, and one whose stream the value layer does not carry yet -- and
// this cannot tell them apart, so it names neither. A union is among them and
// its width its type does fix, so naming a dynamic size would be false for it.
auto RefuseUnfixedStream(diag::SourceSpan span)
    -> std::unexpected<diag::Diagnostic> {
  return diag::Fail(
      span, diag::DiagCode::kUnsupportedExpressionForm,
      "reading a value of this type as a stream of bits is not yet supported "
      "(LRM 6.24.3)");
}

}  // namespace

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
  if (const std::optional<std::span<const mir::TypeId>> parts =
          mir::ProductElements(unit, type)) {
    return FixedStreamShapeOfParts(unit, *parts);
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

auto BuildToBitstream(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId value_id,
    diag::SourceSpan span) -> diag::Result<mir::ExprId> {
  const std::optional<StreamShape> shape =
      FixedStreamShapeOf(unit, block.exprs.Get(value_id).type);
  if (!shape.has_value()) {
    return RefuseUnfixedStream(span);
  }
  const mir::TypeId stream =
      mir::PackedVectorOf(unit.types, shape->width, shape->state_kind);
  // A packed value's stream is its own bits, read unsigned (LRM 6.24.3).
  if (unit.types.Get(block.exprs.Get(value_id).type).IsIntegral()) {
    return ConvertToType(unit, block, value_id, stream);
  }
  return BuildValueOperation(
      unit, block, support::BuiltinFn::kToBitstream, value_id, {}, stream);
}

auto BuildReorderedStream(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId stream_id,
    std::uint64_t block_bits) -> mir::ExprId {
  if (block_bits == 0) {
    return stream_id;
  }
  const mir::TypeId stream_type = block.exprs.Get(stream_id).type;
  const mir::ExprId size_id = BuildMachineIntLiteral(
      unit, block, static_cast<std::int64_t>(block_bits));
  return block.exprs.Add(MakeBuiltinCall(
      unit, block, support::BuiltinFn::kReverseBlocks, stream_id, {size_id},
      stream_type));
}

auto BuildFromBitstream(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId bits_id,
    mir::TypeId dst_type, diag::SourceSpan span) -> diag::Result<mir::ExprId> {
  // A stream is a sequence of bits, so its own shape is on its type and asking
  // for it cannot fail; only the destination can be a type with no stream. By
  // value: the pool's view does not survive the interning below.
  const mir::IntegralType stream =
      unit.types.Get(block.exprs.Get(bits_id).type).Integral();
  const std::optional<StreamShape> target = FixedStreamShapeOf(unit, dst_type);
  if (!target.has_value()) {
    return RefuseUnfixedStream(span);
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
    // The pad carries no x or z of its own, so a 2-state run states what it is
    // and leaves the stream's own domain to decide the join's.
    const mir::TypeId pad_type = mir::PackedVectorOf(
        unit.types, target->width - stream_width,
        mir::IntegralStateKind::kTwoState);
    const mir::ExprId pad_id =
        BuildIntegralLiteral(unit, block, pad_type, mir::IntegralConstant{});
    filled = block.exprs.Add(MakeBuiltinCall(
        unit, block, support::BuiltinFn::kConcatBits, bits_id, {pad_id},
        mir::PackedVectorOf(unit.types, target->width, stream.state_kind)));
  }
  // A packed destination is those bits, at the signedness and state domain it
  // declares.
  if (unit.types.Get(dst_type).IsIntegral()) {
    return ConvertToType(unit, block, filled, dst_type);
  }
  // The destination takes its stream in the state domain its own parts settle,
  // which a stream cast from a value of the other domain is brought to.
  const mir::ExprId stream_id = ConvertToType(
      unit, block, filled,
      mir::PackedVectorOf(unit.types, target->width, target->state_kind));
  // The prototype is read for its shape alone -- how wide each part of the
  // destination is and what representation it holds -- which a sequence of bits
  // carries none of.
  const mir::ExprId prototype_id =
      block.exprs.Add(BuildDefaultValueExpr(unit, block, dst_type));
  return BuildValueOperation(
      unit, block, support::BuiltinFn::kFromBitstream, std::nullopt,
      {stream_id, prototype_id}, dst_type);
}

}  // namespace lyra::lowering::hir_to_mir
