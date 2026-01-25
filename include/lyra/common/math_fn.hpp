#pragma once

// Math function identity for IEEE 1800-2023 §20.8 functions.
//
// CONTRACT: This file contains ONLY:
//   - Function identity (enum values)
//   - Arity (argument count)
//   - String representation
//
// NO lowering policy, NO type rules, NO backend details.
// Those belong in lowering layers.

namespace lyra {

enum class MathFn {
  // Real unary (IEEE 1800 §20.8.1)
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

  // Integral unary
  kClog2,

  // Real binary (IEEE 1800 §20.8.2)
  kPow,
  kAtan2,
  kHypot,
};

inline auto GetMathFnArity(MathFn fn) -> int {
  switch (fn) {
    case MathFn::kPow:
    case MathFn::kAtan2:
    case MathFn::kHypot:
      return 2;
    default:
      return 1;
  }
}

inline auto ToString(MathFn fn) -> const char* {
  switch (fn) {
    case MathFn::kLn:
      return "$ln";
    case MathFn::kLog10:
      return "$log10";
    case MathFn::kExp:
      return "$exp";
    case MathFn::kSqrt:
      return "$sqrt";
    case MathFn::kFloor:
      return "$floor";
    case MathFn::kCeil:
      return "$ceil";
    case MathFn::kSin:
      return "$sin";
    case MathFn::kCos:
      return "$cos";
    case MathFn::kTan:
      return "$tan";
    case MathFn::kAsin:
      return "$asin";
    case MathFn::kAcos:
      return "$acos";
    case MathFn::kAtan:
      return "$atan";
    case MathFn::kSinh:
      return "$sinh";
    case MathFn::kCosh:
      return "$cosh";
    case MathFn::kTanh:
      return "$tanh";
    case MathFn::kAsinh:
      return "$asinh";
    case MathFn::kAcosh:
      return "$acosh";
    case MathFn::kAtanh:
      return "$atanh";
    case MathFn::kClog2:
      return "$clog2";
    case MathFn::kPow:
      return "$pow";
    case MathFn::kAtan2:
      return "$atan2";
    case MathFn::kHypot:
      return "$hypot";
  }
  return "?";
}

}  // namespace lyra
