#include "lyra/mir/binary_op.hpp"

#include <optional>

#include "lyra/base/internal_error.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::mir {

auto BinaryOpAsBuiltinFn(BinaryOp op) -> std::optional<support::BuiltinFn> {
  switch (op) {
    case BinaryOp::kShiftLeft:
      return support::BuiltinFn::kShiftLeft;
    case BinaryOp::kLogicalShiftRight:
      return support::BuiltinFn::kLogicalShiftRight;
    case BinaryOp::kArithmeticShiftRight:
      return support::BuiltinFn::kArithmeticShiftRight;
    case BinaryOp::kAdd:
    case BinaryOp::kSub:
    case BinaryOp::kMul:
    case BinaryOp::kDiv:
    case BinaryOp::kMod:
    case BinaryOp::kBitwiseAnd:
    case BinaryOp::kBitwiseOr:
    case BinaryOp::kBitwiseXor:
    case BinaryOp::kEquality:
    case BinaryOp::kInequality:
    case BinaryOp::kGreaterEqual:
    case BinaryOp::kGreaterThan:
    case BinaryOp::kLessEqual:
    case BinaryOp::kLessThan:
    case BinaryOp::kLogicalAnd:
    case BinaryOp::kLogicalOr:
      return std::nullopt;
  }
  throw InternalError("BinaryOpAsBuiltinFn: unknown MIR BinaryOp");
}

}  // namespace lyra::mir
