#include "lyra/mir/binary_op.hpp"

#include <optional>

#include "lyra/support/builtin_fn.hpp"

namespace lyra::mir {

auto BinaryOpAsBuiltinFn(BinaryOp op) -> std::optional<support::BuiltinFn> {
  switch (op) {
    case BinaryOp::kPower:
      return support::BuiltinFn::kPow;
    case BinaryOp::kShiftLeft:
      return support::BuiltinFn::kShiftLeft;
    case BinaryOp::kLogicalShiftRight:
      return support::BuiltinFn::kLogicalShiftRight;
    case BinaryOp::kArithmeticShiftRight:
      return support::BuiltinFn::kArithmeticShiftRight;
    case BinaryOp::kBitwiseXnor:
      return support::BuiltinFn::kBitwiseXnor;
    case BinaryOp::kLogicalImplication:
      return support::BuiltinFn::kLogicalImplication;
    case BinaryOp::kLogicalEquivalence:
      return support::BuiltinFn::kLogicalEquivalence;
    case BinaryOp::kWildcardEquality:
      return support::BuiltinFn::kWildcardEquals;
    case BinaryOp::kCaseEquality:
      return support::BuiltinFn::kCaseEqual;
    case BinaryOp::kCasezEquality:
      return support::BuiltinFn::kCasezEquals;
    case BinaryOp::kCasexEquality:
      return support::BuiltinFn::kCasexEquals;
    default:
      return std::nullopt;
  }
}

}  // namespace lyra::mir
