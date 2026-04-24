#pragma once

#include <format>
#include <unordered_map>
#include <utility>
#include <vector>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/parameter_direction.hpp"
#include "lyra/common/symbol_types.hpp"
#include "lyra/common/type.hpp"

namespace lyra::hir {

// HIR-time callable parameter descriptor.
// Materialized from slang SubroutineSymbol during AST-to-HIR lowering
// and stored in a Lyra-owned table keyed by SymbolId.
struct HirCallableParamDesc {
  ParameterDirection direction = ParameterDirection::kInput;
  TypeId type;
};

// HIR-time callable signature view.
// The canonical Lyra-owned representation of callable metadata.
struct HirCallableSignature {
  TypeId return_type;
  std::vector<HirCallableParamDesc> params;
};

// Compile-global table of callable signatures keyed by SymbolId.
// Populated during AST-to-HIR Phase 0 (symbol pre-registration) and
// Phase 1 (function/task lowering) when slang SubroutineSymbol is
// available. Consumed by deferred assertion lowering and any other
// HIR consumer that needs callable metadata.
class HirCallableSignatureTable {
 public:
  void Insert(SymbolId sym, HirCallableSignature sig) {
    auto [it, inserted] = entries_.emplace(sym.value, std::move(sig));
    if (!inserted) {
      throw common::InternalError(
          "HirCallableSignatureTable::Insert",
          std::format(
              "duplicate callable signature for SymbolId {}", sym.value));
    }
  }

  [[nodiscard]] auto Lookup(SymbolId sym) const -> const HirCallableSignature* {
    auto it = entries_.find(sym.value);
    if (it != entries_.end()) {
      return &it->second;
    }
    return nullptr;
  }

 private:
  std::unordered_map<uint32_t, HirCallableSignature> entries_;
};

}  // namespace lyra::hir
