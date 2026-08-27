#pragma once

#include <cstdint>

#include "lyra/hir/expr_id.hpp"

namespace lyra::hir {

enum class ConversionKind : std::uint8_t {
  kImplicit,
  kPropagated,
  kStreamingConcat,
  kExplicit,
  kBitstreamCast,
};

// How the source spelled the literal. Diagnostic metadata that never drives
// simulation behavior, so it is not a dispatch set.
enum class IntegerLiteralBase : std::uint8_t {
  kBinary,
  kOctal,
  kDecimal,
  kHexadecimal,
  kUnbased,
};

struct ConversionExpr {
  ExprId operand;
  ConversionKind kind;
};

}  // namespace lyra::hir
