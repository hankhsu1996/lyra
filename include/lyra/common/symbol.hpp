#pragma once

#include <cassert>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

#include "lyra/common/constant.hpp"

namespace lyra::common {

using SymbolId = uint32_t;
inline constexpr SymbolId kInvalidSymbolId = UINT32_MAX;

// Symbol's parent scope kind (for codegen qualified name generation)
enum class SymbolScopeKind {
  kNone,           // No special scope (local variable)
  kPackage,        // Symbol is from a package
  kGenerateBlock,  // Symbol is from a named generate block
};

// Symbol kind for type-safety in constant handling.
// Only kParameter symbols may carry constant_value.
enum class SymbolKind {
  kVariable,   // Regular variable (net, reg, logic, etc.)
  kParameter,  // Parameter or localparam with compile-time constant value
  kFunction,   // Function or task symbol
  kInstance,   // Module/generate instance
};

struct SymbolInfo {
  std::string name;
  SymbolKind kind{SymbolKind::kVariable};
  SymbolScopeKind scope_kind{SymbolScopeKind::kNone};
  std::string scope_name;  // Package or generate block name

  // Resolved constant value for parameters/localparams after elaboration.
  // INVARIANT: Only valid when kind == kParameter.
  // Used by MIR→LIR to emit kConstant instead of variable load.
  std::optional<Constant> constant_value;
};

// Abstract interface for resolving SymbolId to name and scope info.
// Thread this to codegen and diagnostics that need symbol names.
class SymbolNameResolver {
 public:
  SymbolNameResolver() = default;
  SymbolNameResolver(const SymbolNameResolver&) = default;
  SymbolNameResolver(SymbolNameResolver&&) = default;
  auto operator=(const SymbolNameResolver&) -> SymbolNameResolver& = default;
  auto operator=(SymbolNameResolver&&) -> SymbolNameResolver& = default;
  virtual ~SymbolNameResolver() = default;

  [[nodiscard]] virtual auto Name(SymbolId id) const -> std::string_view = 0;
  [[nodiscard]] virtual auto ScopeKind(SymbolId id) const
      -> SymbolScopeKind = 0;
  [[nodiscard]] virtual auto ScopeName(SymbolId id) const
      -> std::string_view = 0;
};

// Slang-free symbol table for storing and looking up symbol info.
// Registration from slang symbols is done by SymbolRegistrar in lowering/.
class SymbolTable : public SymbolNameResolver {
 public:
  // Add a symbol with explicit info (used by SymbolRegistrar)
  // INVARIANT: constant_value may only be set for kParameter symbols.
  auto Add(SymbolInfo info) -> SymbolId {
    assert(
        (!info.constant_value.has_value() ||
         info.kind == SymbolKind::kParameter) &&
        "constant_value is only valid for kParameter symbols");
    auto id = static_cast<SymbolId>(symbols_.size());
    symbols_.push_back(std::move(info));
    return id;
  }

  [[nodiscard]] auto GetInfo(SymbolId id) const -> const SymbolInfo& {
    assert(id < symbols_.size() && "SymbolId out of bounds - wrong table?");
    return symbols_[id];
  }

  // SymbolNameResolver interface
  [[nodiscard]] auto Name(SymbolId id) const -> std::string_view override {
    assert(id < symbols_.size() && "SymbolId out of bounds");
    return symbols_[id].name;
  }

  [[nodiscard]] auto ScopeKind(SymbolId id) const -> SymbolScopeKind override {
    assert(id < symbols_.size() && "SymbolId out of bounds");
    return symbols_[id].scope_kind;
  }

  [[nodiscard]] auto ScopeName(SymbolId id) const -> std::string_view override {
    assert(id < symbols_.size() && "SymbolId out of bounds");
    return symbols_[id].scope_name;
  }

  // Look up constant value for a symbol (if set during registration)
  [[nodiscard]] auto GetConstant(SymbolId id) const
      -> const std::optional<Constant>& {
    assert(id < symbols_.size() && "SymbolId out of bounds");
    return symbols_[id].constant_value;
  }

  [[nodiscard]] auto Size() const -> size_t {
    return symbols_.size();
  }

 private:
  std::vector<SymbolInfo> symbols_;
};

}  // namespace lyra::common
