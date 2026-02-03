#pragma once

#include <variant>

#include "lyra/common/constant.hpp"
#include "lyra/common/type.hpp"
#include "lyra/mir/handle.hpp"

namespace lyra::mir {

// TempId: wrapper for temp_id values in UseTemp operands.
// Distinguishes temp references from PlaceId in the variant.
struct TempId {
  int value;
};

using OperandPayload = std::variant<Constant, PlaceId, TempId>;

struct Operand {
  enum class Kind {
    kConst,    // constant value
    kUse,      // read from a Place (implicit read)
    kUseTemp,  // read from an SSA temp (block param or statement result)
    kPoison,   // invalid / unreachable value
  };

  Kind kind;
  OperandPayload payload;

  static auto Const(const Constant& c) -> Operand {
    return {.kind = Kind::kConst, .payload = c};
  }

  static auto Use(PlaceId id) -> Operand {
    return {.kind = Kind::kUse, .payload = id};
  }

  // UseTemp: reference an SSA temp by its id.
  // Used for block params and temps that don't need Place storage.
  static auto UseTemp(int temp_id) -> Operand {
    return {.kind = Kind::kUseTemp, .payload = TempId{temp_id}};
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
