#pragma once

#include <algorithm>
#include <optional>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/procedural_scope.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// Tag for "storage lives directly on the class enclosing the body" -- the
// `mir::Class` a `ProcessLowerer` is bound to: for a module / generate /
// package body, the class the surrounding `StructuralScope` lowers to; for
// an SV class method, the SV class itself. Distinct tag (not an optional
// procedural-scope id) keeps the `StorageOwner` sum explicit and forbids an
// overloaded nullopt at every consumer.
struct EnclosingClass {
  auto operator==(const EnclosingClass&) const -> bool = default;
};

// Where one piece of static persistent storage physically lives. Either the
// body's enclosing class, or a materialized procedural-storage scope
// (LRM 9.3.5 / 23.9) identified by its HIR id.
using StorageOwner = std::variant<EnclosingClass, hir::ProceduralScopeId>;

// One materialized procedural-storage scope. It is a runtime hierarchy child
// of its runtime parent, reachable through `companion_field` on the
// parent's class. `runtime_parent` is the immediate lexical parent
// (another procedural scope, or the enclosing structural class).
struct MaterializedProceduralScope {
  mir::ClassId class_id{};
  mir::FieldId companion_field{};
  StorageOwner runtime_parent;
};

// Where a static-lifetime body local's persistent storage physically lives:
// on its own lexical scope's class (a static declared in an unnamed
// begin/end lives on that unnamed scope's class, not on any enclosing
// named scope's).
struct StaticStoragePlacement {
  StorageOwner owner;
  mir::FieldId field;
};

// Registry of materialized procedural-storage scopes for one structural
// scope. Every `ProceduralScopeDecl` materializes, indexed by HIR
// `ProceduralScopeId`. Walks from any `StorageOwner` to the body's
// enclosing class assemble the companion-field chain consumers use to
// project storage from `self`.
class ProceduralScopeMaterializationTable {
 public:
  void Resize(std::size_t n) {
    by_scope_id_.assign(n, MaterializedProceduralScope{});
  }

  void Record(hir::ProceduralScopeId scope, MaterializedProceduralScope entry) {
    if (scope.value >= by_scope_id_.size()) {
      throw InternalError(
          "ProceduralScopeMaterializationTable::Record: scope id out of range");
    }
    by_scope_id_[scope.value] = entry;
  }

  [[nodiscard]] auto Get(hir::ProceduralScopeId scope) const
      -> const MaterializedProceduralScope& {
    if (scope.value >= by_scope_id_.size()) {
      throw InternalError(
          "ProceduralScopeMaterializationTable::Get: scope id out of range");
    }
    return by_scope_id_[scope.value];
  }

  [[nodiscard]] auto Size() const -> std::size_t {
    return by_scope_id_.size();
  }

  // The companion-field path from the body's enclosing class `self` down
  // to `owner`. Empty when owner is the enclosing class; one entry per
  // intervening materialized procedural scope otherwise.
  [[nodiscard]] auto CompanionChainTo(StorageOwner owner) const
      -> std::vector<mir::FieldId> {
    std::vector<mir::FieldId> chain;
    while (const auto* scope_id = std::get_if<hir::ProceduralScopeId>(&owner)) {
      const auto& entry = Get(*scope_id);
      chain.push_back(entry.companion_field);
      owner = entry.runtime_parent;
    }
    // Reverse so the chain reads outermost-first (enclosing self toward
    // owner) -- the order consumers walk when projecting through `self`.
    std::ranges::reverse(chain);
    return chain;
  }

 private:
  std::vector<MaterializedProceduralScope> by_scope_id_;
};

// One request to materialize a static declaration's initializer in the
// Initialize phase. Body lowering emits these instead of lowering the
// initializer expression itself; the declaration-initializer lowering path
// consumes the list in the Initialize phase context, lowers the HIR init
// expression there, and writes an assignment to `placement`. Carrying the
// HIR expression handle (not a pre-lowered MIR expression) keeps the body's
// MIR arena separate from the Initialize phase's arena and lets each phase
// lower in its own context.
struct PendingStaticInitializer {
  hir::ProceduralVarId var{};
  hir::TypeId hir_type{};
  std::optional<hir::ExprId> init_expr;
  StaticStoragePlacement placement{};
  mir::TypeId storage_type{};
};

// Per-callable storage plan. Holds the static-var placements for one
// callable's body and a borrowed reference to the structural scope's
// shared procedural-scope materialization table; chain queries forward to
// the table because chains are properties of the runtime topology, not of
// the var.
class CallableStoragePlan {
 public:
  CallableStoragePlan() = default;

  CallableStoragePlan(
      const ProceduralScopeMaterializationTable& scopes,
      std::vector<std::optional<StaticStoragePlacement>> placements)
      : scopes_(&scopes), placements_(std::move(placements)) {
  }

  [[nodiscard]] auto StaticPlacement(hir::ProceduralVarId var) const
      -> std::optional<StaticStoragePlacement> {
    if (var.value >= placements_.size()) return std::nullopt;
    return placements_[var.value];
  }

  [[nodiscard]] auto ScopeMaterialization(hir::ProceduralScopeId scope) const
      -> const MaterializedProceduralScope& {
    return scopes_->Get(scope);
  }

  [[nodiscard]] auto CompanionChainTo(StorageOwner owner) const
      -> std::vector<mir::FieldId> {
    return scopes_->CompanionChainTo(owner);
  }

  [[nodiscard]] auto Scopes() const
      -> const ProceduralScopeMaterializationTable& {
    return *scopes_;
  }

 private:
  const ProceduralScopeMaterializationTable* scopes_ = nullptr;
  std::vector<std::optional<StaticStoragePlacement>> placements_;
};

}  // namespace lyra::lowering::hir_to_mir
