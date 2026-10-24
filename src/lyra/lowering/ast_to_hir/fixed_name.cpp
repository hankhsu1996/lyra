#include "lyra/lowering/ast_to_hir/fixed_name.hpp"

#include <cstdint>
#include <string>

#include <slang/ast/Scope.h>
#include <slang/ast/Symbol.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/hir/fixed_name.hpp"
#include "lyra/lowering/ast_to_hir/unit_identity.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// The innermost scope of the design hierarchy `scope` is or lies in, where
// one is: the one whose instance's name a run puts in front of everything
// declared inside it.
auto InnermostInstanceScope(const slang::ast::Scope& scope)
    -> const slang::ast::Scope* {
  for (const slang::ast::Scope* level = &scope; level != nullptr;
       level = level->asSymbol().getParentScope()) {
    if (IsInstanceScope(*level)) return level;
  }
  return nullptr;
}

// How many scopes of the design hierarchy `from` is or lies in short of
// `to`, which encloses it.
auto ScopesOut(const slang::ast::Scope& from, const slang::ast::Scope& to)
    -> std::uint32_t {
  std::uint32_t count = 0;
  for (const slang::ast::Scope* level = &from; level != &to;
       level = level->asSymbol().getParentScope()) {
    if (level == nullptr) {
      throw InternalError(
          "ScopesOut: a class is replicated with a scope outside the instance "
          "declaring it");
    }
    if (IsInstanceScope(*level)) ++count;
  }
  return count;
}

}  // namespace

auto FixedNameOf(const slang::ast::Scope& scope) -> hir::FixedName {
  const slang::ast::Scope* instance = InnermostInstanceScope(scope);
  if (instance == &scope) return hir::NameBelowInstance{};
  const slang::ast::Symbol& symbol = scope.asSymbol();
  if (symbol.kind == slang::ast::SymbolKind::CompilationUnit) {
    return hir::WholeName{};
  }
  // The front end writes the instance's name, then `.`, then what the
  // instance declares, so what follows that is the scope's name below it.
  const std::string name = symbol.getHierarchicalPath();
  if (instance == nullptr) return hir::WholeName{.text = name};
  const std::string instance_name = instance->asSymbol().getHierarchicalPath();
  return hir::NameBelowInstance{
      .text = name.substr(instance_name.size() + 1),
      .scopes_out = ScopesOut(ReplicatingScope(symbol), *instance)};
}

}  // namespace lyra::lowering::ast_to_hir
