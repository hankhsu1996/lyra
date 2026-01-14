#pragma once

#include <cstdint>
#include <variant>

#include "absl/container/inlined_vector.h"
#include "lyra/common/symbol.hpp"

namespace lyra {

// Allocation ID for anonymous storage (from kAllocate instruction)
using AllocationId = uint64_t;

// Path element - field access or array index
struct PathElement {
  enum class Kind : uint8_t { kField, kIndex };
  Kind kind;
  uint32_t value;  // field_id or index

  static auto Field(uint32_t field_id) -> PathElement {
    return {.kind = Kind::kField, .value = field_id};
  }

  static auto Index(uint32_t index) -> PathElement {
    return {.kind = Kind::kIndex, .value = index};
  }

  auto operator==(const PathElement& other) const -> bool = default;
};

// Root of an address - either a named variable or anonymous allocation
struct VarRoot {
  common::SymbolId symbol;
  auto operator==(const VarRoot&) const -> bool = default;
};

struct AllocRoot {
  AllocationId id;
  auto operator==(const AllocRoot&) const -> bool = default;
};

using AddressRoot = std::variant<VarRoot, AllocRoot>;

// Flat address: root + path of field/index elements
struct Address {
  AddressRoot root;
  absl::InlinedVector<PathElement, 4> path;  // Stack-allocated for depth <= 4

  // Factory methods
  static auto Var(common::SymbolId symbol) -> Address {
    return {.root = VarRoot{.symbol = symbol}, .path = {}};
  }

  static auto Alloc(AllocationId id) -> Address {
    return {.root = AllocRoot{.id = id}, .path = {}};
  }

  // Path building (returns new Address, original unchanged)
  [[nodiscard]] auto WithField(uint32_t field_id) const -> Address {
    Address result = *this;
    result.path.push_back(PathElement::Field(field_id));
    return result;
  }

  [[nodiscard]] auto WithIndex(uint32_t index) const -> Address {
    Address result = *this;
    result.path.push_back(PathElement::Index(index));
    return result;
  }

  // Root accessors
  [[nodiscard]] auto IsVar() const -> bool {
    return std::holds_alternative<VarRoot>(root);
  }

  [[nodiscard]] auto IsAlloc() const -> bool {
    return std::holds_alternative<AllocRoot>(root);
  }

  [[nodiscard]] auto GetSymbol() const -> common::SymbolId {
    return std::get<VarRoot>(root).symbol;
  }

  [[nodiscard]] auto GetAllocationId() const -> AllocationId {
    return std::get<AllocRoot>(root).id;
  }

  // Comparison
  auto operator==(const Address& other) const -> bool = default;
};

}  // namespace lyra
