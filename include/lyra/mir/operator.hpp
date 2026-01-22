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

  // Value predicates
  kIsKnown,  // Returns 1 if no X/Z bits, 0 otherwise (always 2-state 1-bit)

  // Math functions (real)
  kLn,
  kLog10,
  kExp,
  kSqrt,
  kFloor,
  kCeil,
  kSin,
  kCos,
  kTan,
  kAsin,
  kAcos,
  kAtan,
  kSinh,
  kCosh,
  kTanh,
  kAsinh,
  kAcosh,
  kAtanh,

  // Math functions (integral)
  kClog2,
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

  // Case statement matching (wildcards on both sides)
  kCaseZMatch,  // casez: Z bits from both sides are wildcards
  kCaseXMatch,  // casex: X and Z bits from both sides are wildcards

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

  // Math functions (real binary)
  kAtan2,
  kHypot,
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
    case UnaryOp::kIsKnown:
      return "$isknown";
    case UnaryOp::kLn:
      return "$ln";
    case UnaryOp::kLog10:
      return "$log10";
    case UnaryOp::kExp:
      return "$exp";
    case UnaryOp::kSqrt:
      return "$sqrt";
    case UnaryOp::kFloor:
      return "$floor";
    case UnaryOp::kCeil:
      return "$ceil";
    case UnaryOp::kSin:
      return "$sin";
    case UnaryOp::kCos:
      return "$cos";
    case UnaryOp::kTan:
      return "$tan";
    case UnaryOp::kAsin:
      return "$asin";
    case UnaryOp::kAcos:
      return "$acos";
    case UnaryOp::kAtan:
      return "$atan";
    case UnaryOp::kSinh:
      return "$sinh";
    case UnaryOp::kCosh:
      return "$cosh";
    case UnaryOp::kTanh:
      return "$tanh";
    case UnaryOp::kAsinh:
      return "$asinh";
    case UnaryOp::kAcosh:
      return "$acosh";
    case UnaryOp::kAtanh:
      return "$atanh";
    case UnaryOp::kClog2:
      return "$clog2";
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
    case BinaryOp::kCaseZMatch:
      return "casez_match";
    case BinaryOp::kCaseXMatch:
      return "casex_match";
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
    case BinaryOp::kAtan2:
      return "$atan2";
    case BinaryOp::kHypot:
      return "$hypot";
  }
  return "?";
}

}  // namespace lyra::mir
