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

// Diagnostic metadata; never drives simulation behavior.
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
