#pragma once

#include <variant>

#include "lyra/common/constant.hpp"

namespace lyra::mir {

struct Place;

using OperandPayload = std::variant<Constant, Place*>;

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

  static auto Use(Place* p) -> Operand {
    return {.kind = Kind::kUse, .payload = p};
  }

  static auto Poison() -> Operand {
    return {.kind = Kind::kPoison, .payload = {}};
  }
};

}  // namespace lyra::mir
