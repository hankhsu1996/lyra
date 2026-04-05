#pragma once

#include <variant>

#include "lyra/common/constant.hpp"
#include "lyra/common/type.hpp"
#include "lyra/mir/external_ref.hpp"
#include "lyra/mir/handle.hpp"

namespace lyra::mir {

// TempKind: distinguishes SSA value temps from addressable place temps.
// - kValue: SSA temp (no address, no storage). Defined by DefineTemp or
// BlockParam.
// - kPlace: Addressable temp (PlaceRoot::kTemp). Used for call returns,
// writebacks.
enum class TempKind {
  kValue,  // SSA value temp - no storage, referenced via UseTemp
  kPlace,  // Addressable temp - has storage, referenced via PlaceRoot::kTemp
};

// TempMetadata: authoritative kind/type info for a temp_id.
// Indexed by temp_id in Function/Process::temp_metadata.
struct TempMetadata {
  TempKind kind = TempKind::kPlace;
  TypeId type{};
};

// TempId: wrapper for temp_id values in UseTemp operands.
// Distinguishes temp references from PlaceId in the variant.
struct TempId {
  int value;

  auto operator==(const TempId& other) const -> bool = default;

  template <typename H>
  friend auto AbslHashValue(H h, TempId id) -> H {
    return H::combine(std::move(h), id.value);
  }
};

using OperandPayload = std::variant<Constant, PlaceId, TempId, ExternalRefId>;

struct Operand {
  enum class Kind {
    kConst,        // constant value
    kUse,          // read from a Place (implicit read)
    kUseTemp,      // read from an SSA temp (block param or statement result)
    kPoison,       // invalid / unreachable value
    kExternalRef,  // read from a non-local external reference (recipe handle)
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

  static auto ExternalRef(ExternalRefId id) -> Operand {
    return {.kind = Kind::kExternalRef, .payload = id};
  }
};

// TypedOperand: Operand with explicit type information.
// Used for string-like arguments where type determines coercion behavior.
struct TypedOperand {
  Operand operand;
  TypeId type;
};

}  // namespace lyra::mir
