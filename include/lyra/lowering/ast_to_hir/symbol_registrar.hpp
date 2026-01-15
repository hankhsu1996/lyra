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

}  // namespace lyra::lowering::ast_to_hir
