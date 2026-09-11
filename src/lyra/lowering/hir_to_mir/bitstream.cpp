#include "lyra/lowering/hir_to_mir/bitstream.hpp"

#include <cstdint>
#include <optional>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
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

auto FixedStreamShapeOf(const mir::TypePool& types, mir::TypeId type)
    -> std::optional<StreamShape> {
  const mir::Type& resolved = types.Get(type);
  if (resolved.IsIntegralPacked()) {
    const mir::PackedArrayType& packed = resolved.PackedShape();
    return StreamShape{
        .width = packed.BitWidth(), .state_kind = packed.state_kind};
  }
  if (resolved.IsProduct()) {
    StreamShape shape{
        .width = 0, .state_kind = mir::IntegralStateKind::kTwoState};
    for (const mir::TypeId component : resolved.ProductComponentTypes()) {
      const std::optional<StreamShape> part =
          FixedStreamShapeOf(types, component);
      if (!part.has_value()) {
        return std::nullopt;
      }
      shape = WidenedWith(shape, *part);
    }
    return shape;
  }
  if (resolved.Is<mir::UnpackedArrayType>()) {
    const auto& array = resolved.Get<mir::UnpackedArrayType>();
    const std::optional<StreamShape> element =
        FixedStreamShapeOf(types, array.element_type);
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
      FixedStreamShapeOf(unit.types, block.exprs.Get(value_id).type);
  if (!shape.has_value()) {
    return RefuseUnfixedStream(span);
  }
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = support::BuiltinFn::kToBitstream,
                          .receiver = value_id},
                  .arguments = {}},
          .type = mir::PackedVectorOf(
              unit.types, shape->width, shape->state_kind)});
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
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = support::BuiltinFn::kReverseBlocks,
                          .receiver = stream_id},
                  .arguments = {size_id}},
          .type = stream_type});
}

auto BuildFromBitstream(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId bits_id,
    mir::TypeId dst_type, diag::SourceSpan span) -> diag::Result<mir::Expr> {
  // A stream is a run of bits, so its own shape is on its type and asking for
  // it cannot fail; only the destination can be a type with no stream. By
  // value: the pool's view does not survive the interning below.
  const mir::PackedArrayType stream =
      unit.types.Get(block.exprs.Get(bits_id).type).PackedShape();
  const std::optional<StreamShape> target =
      FixedStreamShapeOf(unit.types, dst_type);
  if (!target.has_value()) {
    return RefuseUnfixedStream(span);
  }
  const std::uint64_t stream_width = stream.BitWidth();
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
    filled = block.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee =
                        mir::Direct{
                            .target = support::BuiltinFn::kConcat,
                            .receiver = bits_id},
                    .arguments = {pad_id}},
            .type = mir::PackedVectorOf(
                unit.types, target->width, stream.state_kind)});
  }
  // The prototype is read for its shape alone -- how wide each part of the
  // destination is and what representation it holds -- which a sequence of bits
  // carries none of.
  const mir::ExprId prototype_id =
      block.exprs.Add(BuildDefaultValueExpr(unit, block, dst_type));
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Direct{.target = support::BuiltinFn::kFromBitstream},
              .arguments = {filled, prototype_id}},
      .type = dst_type};
}

}  // namespace lyra::lowering::hir_to_mir
