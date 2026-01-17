#pragma once

namespace lyra::mir {

enum class UnaryOp {
  // Arithmetic
  kPlus,
  kMinus,
  kPreincrement,
  kPostincrement,
  kPredecrement,
  kPostdecrement,

  // Logical
  kLogicalNot,

  // Bitwise
  kBitwiseNot,

  // Reduction
  kReductionAnd,
  kReductionNand,
  kReductionOr,
  kReductionNor,
  kReductionXor,
  kReductionXnor,
};

enum class BinaryOp {
  // Arithmetic (unsigned)
  kAdd,
  kSubtract,
  kMultiply,
  kDivide,
  kMod,
  kPower,

  // Arithmetic (signed)
  kDivideSigned,
  kModSigned,

  // Bitwise
  kBitwiseAnd,
  kBitwiseOr,
  kBitwiseXor,
  kBitwiseXnor,

  // Logical
  kLogicalAnd,
  kLogicalOr,
  kLogicalImplication,
  kLogicalEquivalence,

  // Comparison (equality)
  kEqual,
  kNotEqual,
  kCaseEqual,
  kCaseNotEqual,
  kWildcardEqual,
  kWildcardNotEqual,

  // Comparison (relational, unsigned)
  kLessThan,
  kLessThanEqual,
  kGreaterThan,
  kGreaterThanEqual,

  // Comparison (relational, signed)
  kLessThanSigned,
  kLessThanEqualSigned,
  kGreaterThanSigned,
  kGreaterThanEqualSigned,

  // Shift
  kLogicalShiftLeft,
  kLogicalShiftRight,
  kArithmeticShiftLeft,
  kArithmeticShiftRight,
};

inline auto ToString(UnaryOp op) -> const char* {
  switch (op) {
    case UnaryOp::kPlus:
      return "+";
    case UnaryOp::kMinus:
      return "-";
    case UnaryOp::kPreincrement:
      return "++x";
    case UnaryOp::kPostincrement:
      return "x++";
    case UnaryOp::kPredecrement:
      return "--x";
    case UnaryOp::kPostdecrement:
      return "x--";
    case UnaryOp::kLogicalNot:
      return "!";
    case UnaryOp::kBitwiseNot:
      return "~";
    case UnaryOp::kReductionAnd:
      return "&";
    case UnaryOp::kReductionNand:
      return "~&";
    case UnaryOp::kReductionOr:
      return "|";
    case UnaryOp::kReductionNor:
      return "~|";
    case UnaryOp::kReductionXor:
      return "^";
    case UnaryOp::kReductionXnor:
      return "~^";
  }
  return "?";
}

inline auto ToString(BinaryOp op) -> const char* {
  switch (op) {
    case BinaryOp::kAdd:
      return "+";
    case BinaryOp::kSubtract:
      return "-";
    case BinaryOp::kMultiply:
      return "*";
    case BinaryOp::kDivide:
      return "/";
    case BinaryOp::kMod:
      return "%";
    case BinaryOp::kPower:
      return "**";
    case BinaryOp::kDivideSigned:
      return "/s";
    case BinaryOp::kModSigned:
      return "%s";
    case BinaryOp::kBitwiseAnd:
      return "&";
    case BinaryOp::kBitwiseOr:
      return "|";
    case BinaryOp::kBitwiseXor:
      return "^";
    case BinaryOp::kBitwiseXnor:
      return "~^";
    case BinaryOp::kLogicalAnd:
      return "&&";
    case BinaryOp::kLogicalOr:
      return "||";
    case BinaryOp::kLogicalImplication:
      return "->";
    case BinaryOp::kLogicalEquivalence:
      return "<->";
    case BinaryOp::kEqual:
      return "==";
    case BinaryOp::kNotEqual:
      return "!=";
    case BinaryOp::kCaseEqual:
      return "===";
    case BinaryOp::kCaseNotEqual:
      return "!==";
    case BinaryOp::kWildcardEqual:
      return "==?";
    case BinaryOp::kWildcardNotEqual:
      return "!=?";
    case BinaryOp::kLessThan:
      return "<";
    case BinaryOp::kLessThanEqual:
      return "<=";
    case BinaryOp::kGreaterThan:
      return ">";
    case BinaryOp::kGreaterThanEqual:
      return ">=";
    case BinaryOp::kLessThanSigned:
      return "<s";
    case BinaryOp::kLessThanEqualSigned:
      return "<=s";
    case BinaryOp::kGreaterThanSigned:
      return ">s";
    case BinaryOp::kGreaterThanEqualSigned:
      return ">=s";
    case BinaryOp::kLogicalShiftLeft:
      return "<<";
    case BinaryOp::kLogicalShiftRight:
      return ">>";
    case BinaryOp::kArithmeticShiftLeft:
      return "<<<";
    case BinaryOp::kArithmeticShiftRight:
      return ">>>";
  }
  return "?";
}

}  // namespace lyra::mir
