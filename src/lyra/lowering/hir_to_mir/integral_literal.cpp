#include "lyra/lowering/hir_to_mir/integral_literal.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/integral_constant_id.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// One plane sized and masked the way `width` declares it. A caller may have
// written a short run of words, or left the sign bits above the width set.
auto CanonicalPlane(
    const std::vector<std::uint64_t>& words, std::uint64_t width)
    -> std::vector<std::uint64_t> {
  std::vector<std::uint64_t> out((width + 63U) / 64U, 0U);
  for (std::size_t i = 0; i < out.size(); ++i) {
    out[i] = i < words.size() ? words[i] : 0U;
  }
  if (const std::uint64_t used = width % 64U; used != 0U) {
    out.back() &= (std::uint64_t{1} << used) - 1U;
  }
  return out;
}

// The plane an unknown state is carried in. A two-state type is handed no plane
// at all rather than a run of zeros: the runtime refuses one, because a plane
// there would be storage its shape says it does not have.
auto CanonicalStatePlane(
    const std::vector<std::uint64_t>& words, std::uint64_t width,
    bool is_four_state) -> std::vector<std::uint64_t> {
  return is_four_state ? CanonicalPlane(words, width)
                       : std::vector<std::uint64_t>{};
}

}  // namespace

auto CanonicalIntegralConstant(
    const mir::PackedArrayType& shape, const mir::IntegralConstant& value)
    -> mir::IntegralConstant {
  const bool is_four_state =
      shape.state_kind == mir::IntegralStateKind::kFourState;
  const bool has_unknown = std::ranges::any_of(
      value.state_words, [](std::uint64_t word) { return word != 0U; });
  if (has_unknown && !is_four_state) {
    throw InternalError(
        "CanonicalIntegralConstant: a 2-state type cannot carry an X or Z bit");
  }
  const std::uint64_t width = shape.BitWidth();
  return mir::IntegralConstant{
      .value_words = CanonicalPlane(value.value_words, width),
      .state_words =
          CanonicalStatePlane(value.state_words, width, is_four_state)};
}

auto BuildMachineIntLiteral(
    const mir::CompilationUnit& unit, mir::Block& block, std::int64_t value)
    -> mir::ExprId {
  return block.exprs.Add(
      mir::Expr{
          .data = mir::MachineIntLiteral{.value = value},
          .type = unit.builtins.machine_int64});
}

auto MakeIntegralLiteral(
    const mir::CompilationUnit& unit, mir::TypeId type,
    const mir::IntegralConstant& value) -> mir::Expr {
  const mir::IntegralConstantId constant = unit.integral_constants.Intern(
      mir::IntegralConstantDecl{
          .type = type,
          .value = CanonicalIntegralConstant(
              unit.types.Get(type).PackedShape(), value)});
  return mir::Expr{
      .data =
          mir::ReferenceExpr{
              .target = mir::IntegralConstantRef{.constant = constant}},
      .type = type};
}

auto BuildIntegralLiteral(
    const mir::CompilationUnit& unit, mir::Block& block, mir::TypeId type,
    const mir::IntegralConstant& value) -> mir::ExprId {
  return block.exprs.Add(MakeIntegralLiteral(unit, type, value));
}

auto FoldedOr(
    const mir::CompilationUnit& unit,
    const std::optional<mir::IntegralConstant>& folded, mir::Expr unfolded)
    -> mir::Expr {
  if (!folded) {
    return unfolded;
  }
  return MakeIntegralLiteral(unit, unfolded.type, *folded);
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
