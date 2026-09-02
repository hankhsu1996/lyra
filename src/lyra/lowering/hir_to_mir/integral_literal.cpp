#include "lyra/lowering/hir_to_mir/integral_literal.hpp"

#include <cstddef>
#include <cstdint>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/packed_type_descriptor.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// The constant's numeric value in one machine integer, sign-extended from the
// width its type declares. Only reached where that width fits the carrier and
// no bit is X or Z, so nothing is lost.
auto CarrierValue(
    const mir::IntegralConstant& c, const mir::PackedArrayType& shape)
    -> std::int64_t {
  const std::uint64_t width = shape.BitWidth();
  const std::uint64_t raw = c.value_words.empty() ? 0U : c.value_words[0];
  const std::uint64_t mask =
      width >= 64U ? ~std::uint64_t{0} : (std::uint64_t{1} << width) - 1U;
  const std::uint64_t masked = raw & mask;
  if (shape.signedness == mir::Signedness::kSigned && width < 64U) {
    const std::uint64_t sign_bit = std::uint64_t{1} << (width - 1U);
    if ((masked & sign_bit) != 0U) {
      return static_cast<std::int64_t>(masked | ~mask);
    }
  }
  return static_cast<std::int64_t>(masked);
}

// One plane of a constant's bits, as the machine words a factory reads it out
// of. `count` is the plane's declared length, so a plane the constant carries
// nothing for is stated as zeros rather than as a shorter run.
auto BuildWordPlane(
    const mir::CompilationUnit& unit, mir::Block& block,
    const std::vector<std::uint64_t>& words, std::size_t count) -> mir::ExprId {
  std::vector<mir::ExprId> elements;
  elements.reserve(count);
  for (std::size_t i = 0; i < count; ++i) {
    const std::uint64_t word = i < words.size() ? words[i] : 0U;
    elements.push_back(block.exprs.Add(
        mir::Expr{
            .data =
                mir::MachineIntLiteral{
                    .value = static_cast<std::int64_t>(word)},
            .type = unit.builtins.machine_word}));
  }
  return block.exprs.Add(
      mir::Expr{
          .data = mir::ArrayLiteralExpr{.elements = std::move(elements)},
          .type = mir::MachineArrayOf(
              unit.types, unit.builtins.machine_word, count)});
}

auto BuildFactoryCall(
    mir::Block& block, mir::TypeId type, support::BuiltinFn factory,
    std::vector<mir::ExprId> arguments) -> mir::ExprId {
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = factory,
                          .qualification = mir::TypeQualifier{.type = type}},
                  .arguments = std::move(arguments)},
          .type = type});
}

}  // namespace

auto BuildMachineIntLiteral(
    const mir::CompilationUnit& unit, mir::Block& block, std::int64_t value)
    -> mir::ExprId {
  return block.exprs.Add(
      mir::Expr{
          .data = mir::MachineIntLiteral{.value = value},
          .type = unit.builtins.machine_int64});
}

auto BuildIntegralLiteral(
    const mir::CompilationUnit& unit, mir::Block& block, mir::TypeId type,
    const mir::IntegralConstant& value) -> mir::ExprId {
  const mir::PackedArrayType& shape = unit.types.Get(type).PackedShape();
  const mir::ExprId packed_type = mir::BuildPackedTypeRef(unit, block, type);
  const bool has_unknown =
      !value.state_words.empty() && value.state_words[0] != 0U;
  const std::uint64_t width = shape.BitWidth();
  if (width <= 64U && !has_unknown) {
    const mir::ExprId carrier =
        BuildMachineIntLiteral(unit, block, CarrierValue(value, shape));
    return BuildFactoryCall(
        block, type, support::BuiltinFn::kFromInt, {carrier, packed_type});
  }

  const bool is_four_state =
      shape.state_kind == mir::IntegralStateKind::kFourState;
  if (has_unknown && !is_four_state) {
    throw InternalError(
        "BuildIntegralLiteral: a 2-state type cannot carry an X or Z bit");
  }
  // A 2-state destination has no unknown plane at all, which is an empty run of
  // words rather than a run of zeros -- the runtime tells the two apart.
  const std::size_t words = (width + 63U) / 64U;
  const mir::ExprId value_plane =
      BuildWordPlane(unit, block, value.value_words, words);
  const mir::ExprId unknown_plane =
      BuildWordPlane(unit, block, value.state_words, is_four_state ? words : 0);
  return BuildFactoryCall(
      block, type, support::BuiltinFn::kFromWords,
      {value_plane, unknown_plane, packed_type});
}

auto BuildIntLiteral(
    const mir::CompilationUnit& unit, mir::Block& block, std::int64_t value)
    -> mir::ExprId {
  return BuildIntegralLiteral(
      unit, block, unit.builtins.int_type,
      mir::IntegralConstant{
          .value_words = {static_cast<std::uint64_t>(value)},
          .state_words = {}});
}

auto BuildIntegerLiteral(
    const mir::CompilationUnit& unit, mir::Block& block, std::int64_t value)
    -> mir::ExprId {
  return BuildIntegralLiteral(
      unit, block, unit.builtins.integer,
      mir::IntegralConstant{
          .value_words = {static_cast<std::uint64_t>(value)},
          .state_words = {}});
}

auto BuildBit1Literal(
    const mir::CompilationUnit& unit, mir::Block& block, bool value)
    -> mir::ExprId {
  return BuildIntegralLiteral(
      unit, block, unit.builtins.bit1,
      mir::IntegralConstant{
          .value_words = {value ? 1ULL : 0ULL}, .state_words = {}});
}

}  // namespace lyra::lowering::hir_to_mir
