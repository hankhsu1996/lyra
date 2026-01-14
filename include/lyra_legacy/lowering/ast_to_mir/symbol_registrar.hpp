#pragma once

#include <optional>
#include <string>
#include <unordered_map>
#include <utility>

#include <slang/ast/Scope.h>
#include <slang/ast/Symbol.h>

#include "lyra/common/constant.hpp"
#include "lyra/common/symbol.hpp"

namespace lyra::lowering::ast_to_mir {

// Adapter that registers slang symbols into a SymbolTable.
// This isolates slang dependencies to the lowering layer.
class SymbolRegistrar {
 public:
  explicit SymbolRegistrar(common::SymbolTable& table) : table_(table) {
  }

  // Register a regular symbol (variable, function, instance, etc.)
  auto Register(const slang::ast::Symbol* slang_sym) -> common::SymbolId {
    return RegisterImpl(slang_sym, common::SymbolKind::kVariable, std::nullopt);
  }

  // Register a parameter with its resolved constant value.
  // Sets kind=kParameter, which is required for constant_value.
  auto RegisterParameter(
      const slang::ast::Symbol* slang_sym, common::Constant constant)
      -> common::SymbolId {
    return RegisterImpl(
        slang_sym, common::SymbolKind::kParameter, std::move(constant));
  }

 private:
  auto RegisterImpl(
      const slang::ast::Symbol* slang_sym, common::SymbolKind kind,
      std::optional<common::Constant> constant) -> common::SymbolId {
    auto it = slang_to_id_.find(slang_sym);
    if (it != slang_to_id_.end()) {
      return it->second;
    }

    // Determine parent scope info for codegen qualified names
    common::SymbolScopeKind scope_kind = common::SymbolScopeKind::kNone;
    std::string scope_name;
    if (const auto* parent_scope = slang_sym->getParentScope()) {
      const auto& parent_symbol = parent_scope->asSymbol();
      if (parent_symbol.kind == slang::ast::SymbolKind::Package) {
        scope_kind = common::SymbolScopeKind::kPackage;
        scope_name = std::string(parent_symbol.name);
      } else if (parent_symbol.kind == slang::ast::SymbolKind::GenerateBlock) {
        if (!parent_symbol.name.empty()) {
          scope_kind = common::SymbolScopeKind::kGenerateBlock;
          scope_name = std::string(parent_symbol.name);
        }
      }
    }

    auto id = table_.Add(
        common::SymbolInfo{
            .name = std::string(slang_sym->name),
            .kind = kind,
            .scope_kind = scope_kind,
            .scope_name = std::move(scope_name),
            .constant_value = std::move(constant)});
    slang_to_id_.emplace(slang_sym, id);
    return id;
  }

  common::SymbolTable& table_;
  std::unordered_map<const slang::ast::Symbol*, common::SymbolId> slang_to_id_;
};

}  // namespace lyra::lowering::ast_to_mir
