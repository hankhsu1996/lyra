#pragma once

#include <type_traits>
#include <vector>

#include <slang/ast/Scope.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/ClassSymbols.h>
#include <slang/ast/types/AllTypes.h>

namespace lyra::lowering::ast_to_hir {

// Hands `visit` every member of `scope`, and of each scope within it that a
// declaration outside a body may stand in: a class (LRM 8.3 admits a class
// declaration as a class item), each live specialization of a parameterized
// one (LRM 8.25), and each generate block this elaboration built (LRM 27). A
// member is visited before anything inside it. A visitor that can fail answers
// with a `diag::Result<void>` and the walk stops at its first failure; one that
// cannot answers with nothing, and so does the walk.
template <typename Visit>
auto WalkDeclarationScopes(const slang::ast::Scope& scope, Visit& visit)
    -> std::invoke_result_t<Visit&, const slang::ast::Symbol&> {
  constexpr bool kCanFail =
      !std::is_void_v<std::invoke_result_t<Visit&, const slang::ast::Symbol&>>;
  for (const auto& member : scope.members()) {
    if constexpr (kCanFail) {
      if (auto r = visit(member); !r) return r;
    } else {
      visit(member);
    }
    std::vector<const slang::ast::Scope*> inner;
    if (member.kind == slang::ast::SymbolKind::ClassType) {
      inner.push_back(&member.as<slang::ast::ClassType>());
    } else if (member.kind == slang::ast::SymbolKind::GenericClassDef) {
      for (const auto& spec :
           member.as<slang::ast::GenericClassDefSymbol>().specializations()) {
        inner.push_back(&spec.getCanonicalType().as<slang::ast::ClassType>());
      }
    } else if (member.kind == slang::ast::SymbolKind::GenerateBlock) {
      const auto& block = member.as<slang::ast::GenerateBlockSymbol>();
      if (!block.isUninstantiated) {
        inner.push_back(&block);
      }
    } else if (member.kind == slang::ast::SymbolKind::GenerateBlockArray) {
      for (const auto* entry :
           member.as<slang::ast::GenerateBlockArraySymbol>().entries) {
        inner.push_back(entry);
      }
    }
    for (const slang::ast::Scope* scope_within : inner) {
      if constexpr (kCanFail) {
        if (auto r = WalkDeclarationScopes(*scope_within, visit); !r) return r;
      } else {
        WalkDeclarationScopes(*scope_within, visit);
      }
    }
  }
  if constexpr (kCanFail) return {};
}

}  // namespace lyra::lowering::ast_to_hir
