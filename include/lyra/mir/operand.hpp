#pragma once

#include <variant>

#include "lyra/common/constant.hpp"
#include "lyra/common/type.hpp"
#include "lyra/mir/handle.hpp"

namespace lyra::mir {

using OperandPayload = std::variant<Constant, PlaceId>;

struct Operand {
  enum class Kind {
    kConst,   // constant value
    kUse,     // read from a Place (implicit read)
    kPoison,  // invalid / unreachable value
  };

  Kind kind;
  OperandPayload payload;

  static auto Const(const Constant& c) -> Operand {
    return {.kind = Kind::kConst, .payload = c};
  }

  static auto Use(PlaceId id) -> Operand {
    return {.kind = Kind::kUse, .payload = id};
  }

  static auto Poison() -> Operand {
    return {.kind = Kind::kPoison, .payload = {}};
  }
};

// TypedOperand: Operand with explicit type information.
// Used for string-like arguments where type determines coercion behavior.
struct TypedOperand {
  Operand operand;
  TypeId type;
};

}  // namespace lyra::mir
