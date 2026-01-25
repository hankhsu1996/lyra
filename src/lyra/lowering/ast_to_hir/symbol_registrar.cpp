#include "lyra/lowering/ast_to_hir/symbol_registrar.hpp"

#include <string>

#include "lyra/common/internal_error.hpp"

namespace lyra::lowering::ast_to_hir {

SymbolRegistrar::SymbolRegistrar(Context* ctx) : ctx_(ctx) {
}

auto SymbolRegistrar::Register(
    const slang::ast::Symbol& slang_sym, SymbolKind kind, TypeId type,
    StorageClass storage_class) -> SymbolId {
  auto it = slang_to_id_.find(&slang_sym);
  if (it != slang_to_id_.end()) {
    return it->second;
  }

  if (lookup_only_) {
    throw common::InternalError(
        "SymbolRegistrar::Register",
        std::format(
            "symbol '{}' not pre-registered (creation forbidden "
            "during lowering phase)",
            slang_sym.name));
  }

  SymbolId id = ctx_->symbol_table->Add(
      Symbol{
          .kind = kind,
          .name = std::string(slang_sym.name),
          .type = type,
          .scope = current_scope_,
          .storage_class = storage_class,
      });
  slang_to_id_.emplace(&slang_sym, id);
  return id;
}

auto SymbolRegistrar::RegisterSynthetic(
    std::string name, SymbolKind kind, TypeId type, StorageClass storage_class)
    -> SymbolId {
  return ctx_->symbol_table->Add(
      Symbol{
          .kind = kind,
          .name = std::move(name),
          .type = type,
          .scope = current_scope_,
          .storage_class = storage_class,
      });
}

auto SymbolRegistrar::Lookup(const slang::ast::Symbol& slang_sym) const
    -> SymbolId {
  auto it = slang_to_id_.find(&slang_sym);
  if (it != slang_to_id_.end()) {
    return it->second;
  }
  return kInvalidSymbolId;
}

void SymbolRegistrar::PushScope(ScopeKind kind, std::string name) {
  ScopeId new_scope = ctx_->scope_table->Add(
      Scope{
          .kind = kind,
          .parent = current_scope_,
          .name = std::move(name),
      });
  current_scope_ = new_scope;
}

void SymbolRegistrar::PopScope() {
  if (current_scope_) {
    current_scope_ = (*ctx_->scope_table)[current_scope_].parent;
  }
}

}  // namespace lyra::lowering::ast_to_hir
