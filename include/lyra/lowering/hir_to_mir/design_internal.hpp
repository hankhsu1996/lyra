#pragma once

#include <cstdint>
#include <format>
#include <functional>
#include <unordered_map>
#include <vector>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/local_slot_id.hpp"
#include "lyra/common/symbol_types.hpp"
#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "lyra/mir/connection_endpoint.hpp"
#include "lyra/mir/module_body.hpp"

namespace lyra::hir {
struct Module;
}

namespace lyra::lowering::hir_to_mir {

// Canonical body-local slot identity. Captures representative module's
// SymbolId and the LocalSlotId assigned by CollectBodyLocalDecls.
// Used for resolver construction and per-instance symbol mapping.
struct BodyLocalSlotEntry {
  SymbolId sym;
  common::LocalSlotId local_slot;
  TypeId type;
};

// Maps (instance_sym, variable_sym) pairs to topology-owned endpoints.
// Built from canonical body-local declaration data after construction
// objects are finalized. The compound key preserves instance identity
// even if variable symbols overlap across specialization groups.
class InstanceSlotResolver {
 public:
  void Insert(
      SymbolId instance_sym, SymbolId variable_sym, mir::BoundEndpoint ref) {
    ResolverKey key{instance_sym, variable_sym};
    auto [it, inserted] = map_.emplace(key, ref);
    if (!inserted) {
      throw common::InternalError(
          "InstanceSlotResolver::Insert",
          std::format(
              "duplicate (instance={}, variable={}) in resolver",
              instance_sym.value, variable_sym.value));
    }
    // Reverse map: variable_sym -> (instance_sym, ref).
    // Valid because per-instance symbols are unique in the elaborated AST.
    reverse_[variable_sym] = ReverseLookup{instance_sym, ref};
  }

  auto Resolve(SymbolId instance_sym, SymbolId variable_sym) const
      -> mir::BoundEndpoint {
    ResolverKey key{instance_sym, variable_sym};
    auto it = map_.find(key);
    if (it == map_.end()) {
      throw common::InternalError(
          "InstanceSlotResolver::Resolve",
          std::format(
              "(instance={}, variable={}) not found in resolver",
              instance_sym.value, variable_sym.value));
    }
    return it->second;
  }

  // Check whether a variable symbol is resolver-addressable (body-local
  // and registered). Non-throwing classification probe.
  [[nodiscard]] auto Contains(SymbolId variable_sym) const -> bool {
    return reverse_.find(variable_sym) != reverse_.end();
  }

  // Resolve by variable symbol alone (for finding parent instance from
  // expression variable references). Validated unique at insertion time.
  auto ResolveByVariable(SymbolId variable_sym) const -> mir::BoundEndpoint {
    auto it = reverse_.find(variable_sym);
    if (it == reverse_.end()) {
      throw common::InternalError(
          "InstanceSlotResolver::ResolveByVariable",
          std::format("variable {} not found in resolver", variable_sym.value));
    }
    return it->second.ref;
  }

  // Find the owning instance for a variable symbol.
  auto FindOwnerInstance(SymbolId variable_sym) const -> SymbolId {
    auto it = reverse_.find(variable_sym);
    if (it == reverse_.end()) {
      throw common::InternalError(
          "InstanceSlotResolver::FindOwnerInstance",
          std::format("variable {} not found in resolver", variable_sym.value));
    }
    return it->second.instance_sym;
  }

 private:
  struct ResolverKey {
    SymbolId instance_sym;
    SymbolId variable_sym;
    auto operator==(const ResolverKey&) const -> bool = default;
  };

  struct ResolverKeyHash {
    auto operator()(const ResolverKey& k) const noexcept -> size_t {
      return std::hash<uint32_t>{}(k.instance_sym.value) ^
             (std::hash<uint32_t>{}(k.variable_sym.value) << 16);
    }
  };

  struct ReverseLookup {
    SymbolId instance_sym;
    mir::BoundEndpoint ref;
  };

  std::unordered_map<ResolverKey, mir::BoundEndpoint, ResolverKeyHash> map_;
  std::unordered_map<SymbolId, ReverseLookup, SymbolIdHash> reverse_;
};

// Recursively find any NameRef symbol in an expression tree.
auto FindAnyNameRef(hir::ExpressionId expr_id, const hir::Arena& arena)
    -> std::optional<SymbolId>;

// Build per-instance places for expression lowering in a body-local context.
auto BuildPerInstancePlaces(
    const hir::Module& inst_mod, const hir::Module& rep_mod,
    const std::vector<BodyLocalSlotEntry>& body_slots,
    const SymbolTable& symbol_table, mir::Arena& body_arena) -> PlaceMap;

// Compile a parent expression as a body-local function.
auto LowerExprAsBodyFunction(
    hir::ExpressionId expr_id, TypeId result_type, const LoweringInput& input,
    const DesignDeclarations& decls, mir::Arena& body_arena,
    const PlaceMap& per_instance_places) -> Result<mir::FunctionId>;

}  // namespace lyra::lowering::hir_to_mir
