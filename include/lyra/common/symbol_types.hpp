#pragma once

#include <cstdint>
#include <string>
#include <utility>

#include "lyra/common/scope_types.hpp"
#include "lyra/common/type.hpp"

namespace lyra {

struct SymbolId {
  uint32_t value = 0;

  auto operator==(const SymbolId&) const -> bool = default;
  auto operator<=>(const SymbolId&) const = default;
  explicit operator bool() const {
    return value != UINT32_MAX;
  }

  template <typename H>
  friend auto AbslHashValue(H h, SymbolId id) -> H {
    return H::combine(std::move(h), id.value);
  }
};

constexpr SymbolId kInvalidSymbolId{UINT32_MAX};

enum class SymbolKind {
  kInstance,
  kPackage,
  kVariable,
  kParameter,
  kFunction,
  kTask,
};

struct Symbol {
  SymbolKind kind = SymbolKind::kVariable;
  std::string name;
  TypeId type;
  ScopeId scope;
};

}  // namespace lyra
