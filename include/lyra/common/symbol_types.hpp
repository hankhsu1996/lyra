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

// Storage classification for symbols.
// Set during Phase 0 registration to determine runtime storage requirements.
enum class StorageClass {
  // Design-level storage: module/package variables, ports.
  // These get design slots allocated during HIR→MIR lowering.
  kDesignStorage,
  // Compile-time only: parameters, localparams, enum members, genvars.
  // No runtime storage - values are elaboration-time constants.
  kConstOnly,
  // Local storage: function arguments, function locals, block locals.
  // These get stack allocation during execution.
  kLocalStorage,
};

struct Symbol {
  SymbolKind kind = SymbolKind::kVariable;
  std::string name;
  TypeId type;
  ScopeId scope;
  StorageClass storage_class = StorageClass::kDesignStorage;
};

}  // namespace lyra
