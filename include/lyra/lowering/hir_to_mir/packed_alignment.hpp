#pragma once

#include <algorithm>
#include <cstdint>
#include <variant>

#include "lyra/common/constant.hpp"
#include "lyra/common/constant_arena.hpp"
#include "lyra/common/integral_constant.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/expression.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/hir/operator.hpp"

namespace lyra::lowering::hir_to_mir {

// Isolate the lowest set bit of n as a power of two.
// Returns 0 for n == 0. Examples: 24 -> 8, 32 -> 32, 12 -> 4, 1 -> 1.
inline auto LowestSetBitPowerOfTwo(uint32_t n) -> uint32_t {
  return n & (~n + 1);
}

// Convert a general "multiple-of-N bits" value into the power-of-two
// alignment contract required by BitRangeProjection::guaranteed_alignment_bits.
// Returns the largest power of two that divides n, capped at 2^30.
// Returns 1 for n == 0 (no alignment guarantee).
//
// All producers of guaranteed_alignment_bits must go through this function
// to enforce the power-of-two invariant.
inline auto PowerOfTwoAlignment(uint32_t n) -> uint32_t {
  if (n == 0) return 1;
  return std::min(LowestSetBitPowerOfTwo(n), 1U << 30);
}

// Determine the guaranteed power-of-two bit-alignment of a HIR expression's
// runtime value. Returns the largest power of two that the expression is
// provably always a multiple of.
//
// Conservative: returns 1 for any pattern not recognized.
// Never returns 0 or values larger than 2^30.
inline auto GetHirExpressionAlignmentBits(
    const hir::Arena& hir_arena, const ConstantArena& constants,
    hir::ExpressionId expr_id) -> uint32_t {
  const auto& expr = hir_arena[expr_id];

  // Constant: alignment = largest power of 2 dividing the value.
  // Zero is trivially aligned to any power of two; cap at a large value.
  if (expr.kind == hir::ExpressionKind::kConstant) {
    const auto& data = std::get<hir::ConstantExpressionData>(expr.data);
    const auto& constant = constants[data.constant];
    const auto* ic = std::get_if<IntegralConstant>(&constant.value);
    if (ic != nullptr && !ic->value.empty()) {
      auto val = ic->value[0];
      if (val == 0) return 1U << 30;
      auto raw = static_cast<uint32_t>(val & (~val + 1));
      return std::min(raw, 1U << 30);
    }
    return 1;
  }

  // Binary multiply: alignment >= max(alignment(lhs), alignment(rhs)).
  // For i*32, the constant 32 guarantees the result is always a multiple of 32.
  if (expr.kind == hir::ExpressionKind::kBinaryOp) {
    const auto& data = std::get<hir::BinaryExpressionData>(expr.data);
    if (data.op == hir::BinaryOp::kMultiply) {
      uint32_t lhs_align =
          GetHirExpressionAlignmentBits(hir_arena, constants, data.lhs);
      uint32_t rhs_align =
          GetHirExpressionAlignmentBits(hir_arena, constants, data.rhs);
      return std::max(lhs_align, rhs_align);
    }
    if (data.op == hir::BinaryOp::kLogicalShiftLeft ||
        data.op == hir::BinaryOp::kArithmeticShiftLeft) {
      const auto& rhs_expr = hir_arena[data.rhs];
      if (rhs_expr.kind == hir::ExpressionKind::kConstant) {
        const auto& rhs_data =
            std::get<hir::ConstantExpressionData>(rhs_expr.data);
        const auto& rhs_const = constants[rhs_data.constant];
        const auto* ic = std::get_if<IntegralConstant>(&rhs_const.value);
        if (ic != nullptr && !ic->value.empty() && ic->value[0] < 30) {
          return 1U << static_cast<uint32_t>(ic->value[0]);
        }
      }
    }
  }

  // Cast: alignment preserved (integer widening/truncation).
  if (expr.kind == hir::ExpressionKind::kCast) {
    const auto& data = std::get<hir::CastExpressionData>(expr.data);
    return GetHirExpressionAlignmentBits(hir_arena, constants, data.operand);
  }

  return 1;
}

// Compute guaranteed power-of-two alignment for an indexed part-select offset.
//
// offset = index - subtracted_constant (form depends on base direction)
// alignment(a - c) = min(alignment(a), alignment(c)) when c != 0.
// alignment(a - 0) = alignment(a).
//
// This is the single authority for part-select offset alignment. It is called
// at the same site that constructs the offset operand, so there is one code
// path that knows both the offset formula and its alignment guarantee.
inline auto ComputePartSelectOffsetAlignment(
    uint32_t index_alignment, int64_t subtracted_constant) -> uint32_t {
  if (subtracted_constant == 0) {
    return index_alignment;
  }
  auto abs_const = static_cast<uint64_t>(
      subtracted_constant >= 0 ? subtracted_constant : -subtracted_constant);
  auto const_alignment =
      std::min(static_cast<uint32_t>(abs_const & (~abs_const + 1)), 1U << 30);
  return std::min(index_alignment, const_alignment);
}

}  // namespace lyra::lowering::hir_to_mir
