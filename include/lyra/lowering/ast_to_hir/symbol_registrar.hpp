#pragma once

#include <unordered_map>

#include <slang/ast/Symbol.h>

#include "lyra/common/symbol_types.hpp"
#include "lyra/lowering/ast_to_hir/context.hpp"

namespace lyra::lowering::ast_to_hir {

class SymbolRegistrar {
 public:
  explicit SymbolRegistrar(Context* ctx);

  auto Register(
      const slang::ast::Symbol& slang_sym, SymbolKind kind, TypeId type)
      -> SymbolId;

  [[nodiscard]] auto Lookup(const slang::ast::Symbol& slang_sym) const
      -> SymbolId;

  void PushScope(ScopeKind kind);
  void PopScope();

  [[nodiscard]] auto CurrentScope() const -> ScopeId {
    return current_scope_;
  }

 private:
  Context* ctx_;
  std::unordered_map<const slang::ast::Symbol*, SymbolId> slang_to_id_;
  ScopeId current_scope_ = kInvalidScopeId;
};

// RAII scope guard for SymbolRegistrar - ensures PopScope on all exit paths.
class ScopeGuard {
 public:
  ScopeGuard(SymbolRegistrar& registrar, ScopeKind kind)
      : registrar_(registrar) {
    registrar_.PushScope(kind);
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

}  // namespace lyra::lowering::ast_to_hir
