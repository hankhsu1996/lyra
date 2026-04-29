#pragma once

#include <cstdint>

#include "lyra/mir/expr_id.hpp"

namespace lyra::mir {

enum class ConversionKind : std::uint8_t {
  kImplicit,
  kPropagated,
  kStreamingConcat,
  kExplicit,
  kBitstreamCast,
};

struct ConversionExpr {
  ExprId operand;
  ConversionKind kind;
};

}  // namespace lyra::mir
