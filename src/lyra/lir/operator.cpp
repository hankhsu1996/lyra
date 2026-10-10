#include "lyra/lir/operator.hpp"

#include <string_view>

#include "lyra/base/internal_error.hpp"

namespace lyra::lir {

auto BinaryOpName(BinaryOp op) -> std::string_view {
  switch (op) {
    case BinaryOp::kAdd:
      return "add";
    case BinaryOp::kSub:
      return "sub";
    case BinaryOp::kMul:
      return "mul";
    case BinaryOp::kDiv:
      return "div";
    case BinaryOp::kMod:
      return "mod";
    case BinaryOp::kBitwiseAnd:
      return "and";
    case BinaryOp::kBitwiseOr:
      return "or";
    case BinaryOp::kBitwiseXor:
      return "xor";
    case BinaryOp::kEquality:
      return "eq";
    case BinaryOp::kInequality:
      return "ne";
    case BinaryOp::kLessThan:
      return "lt";
    case BinaryOp::kLessEqual:
      return "le";
    case BinaryOp::kGreaterThan:
      return "gt";
    case BinaryOp::kGreaterEqual:
      return "ge";
    case BinaryOp::kLogicalAnd:
      return "logical_and";
    case BinaryOp::kLogicalOr:
      return "logical_or";
  }
  throw InternalError("lir: unknown binary operator");
}

auto BinaryAnswerOf(BinaryOp op) -> BinaryAnswer {
  switch (op) {
    case BinaryOp::kAdd:
    case BinaryOp::kSub:
    case BinaryOp::kMul:
    case BinaryOp::kDiv:
    case BinaryOp::kMod:
    case BinaryOp::kBitwiseAnd:
    case BinaryOp::kBitwiseOr:
    case BinaryOp::kBitwiseXor:
      return BinaryAnswer::kOperandType;
    case BinaryOp::kEquality:
    case BinaryOp::kInequality:
    case BinaryOp::kLessThan:
    case BinaryOp::kLessEqual:
    case BinaryOp::kGreaterThan:
    case BinaryOp::kGreaterEqual:
    case BinaryOp::kLogicalAnd:
    case BinaryOp::kLogicalOr:
      return BinaryAnswer::kOneBit;
  }
  throw InternalError("lir: unknown binary operator");
}

auto UnaryOpName(UnaryOp op) -> std::string_view {
  switch (op) {
    case UnaryOp::kMinus:
      return "neg";
    case UnaryOp::kBitwiseNot:
      return "not";
    case UnaryOp::kLogicalNot:
      return "logical_not";
  }
  throw InternalError("lir: unknown unary operator");
}

}  // namespace lyra::lir
