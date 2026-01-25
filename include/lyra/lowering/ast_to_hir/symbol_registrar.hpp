#pragma once

#include <string>
#include <unordered_map>

#include <slang/ast/Symbol.h>

#include "lyra/common/symbol_types.hpp"
#include "lyra/lowering/ast_to_hir/context.hpp"

namespace lyra::lowering::ast_to_hir {

class SymbolRegistrar {
 public:
  explicit SymbolRegistrar(Context* ctx);

  auto Register(
      const slang::ast::Symbol& slang_sym, SymbolKind kind, TypeId type,
      StorageClass storage_class = StorageClass::kDesignStorage) -> SymbolId;

  // Register a synthetic (compiler-generated) symbol with no slang backing
  auto RegisterSynthetic(
      std::string name, SymbolKind kind, TypeId type,
      StorageClass storage_class = StorageClass::kDesignStorage) -> SymbolId;

  [[nodiscard]] auto Lookup(const slang::ast::Symbol& slang_sym) const
      -> SymbolId;

  void PushScope(ScopeKind kind, std::string name = {});
  void PopScope();

  [[nodiscard]] auto CurrentScope() const -> ScopeId {
    return current_scope_;
  }

 private:
  friend class LookupOnlyGuard;
  Context* ctx_;
  std::unordered_map<const slang::ast::Symbol*, SymbolId> slang_to_id_;
  ScopeId current_scope_ = kInvalidScopeId;
  bool lookup_only_ = false;
};

// RAII scope guard for SymbolRegistrar - ensures PopScope on all exit paths.
class ScopeGuard {
 public:
  ScopeGuard(SymbolRegistrar& registrar, ScopeKind kind, std::string name = {})
      : registrar_(registrar) {
    registrar_.PushScope(kind, std::move(name));
  }
  ~ScopeGuard() {
    registrar_.PopScope();
  }

  ScopeGuard(const ScopeGuard&) = delete;
  ScopeGuard(ScopeGuard&&) = delete;
  auto operator=(const ScopeGuard&) -> ScopeGuard& = delete;
  auto operator=(ScopeGuard&&) -> ScopeGuard& = delete;

 private:
  SymbolRegistrar& registrar_;
};

// RAII guard: forbids symbol creation for its lifetime.
// Register() for already-registered symbols still succeeds (idempotent).
// Register() for unregistered symbols throws InternalError.
class LookupOnlyGuard {
 public:
  explicit LookupOnlyGuard(SymbolRegistrar& registrar)
      : registrar_(registrar), saved_(registrar.lookup_only_) {
    registrar_.lookup_only_ = true;
  }
  ~LookupOnlyGuard() {
    registrar_.lookup_only_ = saved_;
  }

  LookupOnlyGuard(const LookupOnlyGuard&) = delete;
  LookupOnlyGuard(LookupOnlyGuard&&) = delete;
  auto operator=(const LookupOnlyGuard&) -> LookupOnlyGuard& = delete;
  auto operator=(LookupOnlyGuard&&) -> LookupOnlyGuard& = delete;

 private:
  SymbolRegistrar& registrar_;
  bool saved_;
};

}  // namespace lyra::lowering::ast_to_hir
