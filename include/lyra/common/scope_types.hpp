#pragma once

#include <cstdint>
#include <utility>

namespace lyra {

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
  kRoot,
  kModule,
  kBlock,
  kPackage,
  kFunction,
  kTask,
  kGenerate,
};

struct Scope {
  ScopeKind kind = ScopeKind::kBlock;
  ScopeId parent = kInvalidScopeId;
};

}  // namespace lyra
