#pragma once

#include <cstdint>
#include <string>

#include "absl/container/flat_hash_map.h"
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

struct ScopeId {
  uint32_t value = 0;

  auto operator==(const ScopeId&) const -> bool = default;
  auto operator<=>(const ScopeId&) const = default;
  explicit operator bool() const {
    return value != UINT32_MAX;
  }

  template <typename H>
  friend auto AbslHashValue(H h, ScopeId id) -> H {
    return H::combine(std::move(h), id.value);
  }
};

constexpr ScopeId kInvalidScopeId{UINT32_MAX};

enum class ScopeKind {
  kModule,
  kBlock,
  kPackage,
  kFunction,
  kTask,
  kGenerate,
};

enum class SymbolKind {
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

struct Scope {
  ScopeKind kind = ScopeKind::kBlock;
  ScopeId parent = kInvalidScopeId;
  absl::flat_hash_map<std::string, SymbolId> symbols;
};

}  // namespace lyra
