#include "lyra/lowering/ast_to_hir/lifetime_extension.hpp"

#include <cstddef>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

#include <slang/ast/ASTVisitor.h>
#include <slang/ast/Statement.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/statements/MiscStatements.h>
#include <slang/ast/symbols/VariableSymbols.h>

namespace lyra::lowering::ast_to_hir {

namespace {

// Walks the body in source order. At each enclosing detached fork it records
// the count of automatics declared so far as a boundary; a reference inside
// such a branch to an automatic whose declaration index is below that boundary
// -- declared before the fork, so enclosing it -- is a borrow. The fork's own
// block-item locals and a branch's own locals are declared at or after the
// boundary and so are excluded.
struct LifetimeExtendedCollector
    : slang::ast::ASTVisitor<
          LifetimeExtendedCollector, slang::ast::VisitFlags::AllGood> {
  std::unordered_map<const slang::ast::VariableSymbol*, std::size_t>
      declared_index;
  std::size_t declared_count = 0;
  std::vector<std::size_t> detached_boundaries;
  std::unordered_set<const slang::ast::VariableSymbol*> result;

  void handle(const slang::ast::VariableDeclStatement& s) {
    if (s.symbol.lifetime == slang::ast::VariableLifetime::Automatic) {
      declared_index.emplace(&s.symbol, declared_count++);
    }
    visitDefault(s);
  }

  void handle(const slang::ast::BlockStatement& b) {
    const bool detached =
        b.blockKind == slang::ast::StatementBlockKind::JoinNone ||
        b.blockKind == slang::ast::StatementBlockKind::JoinAny;
    if (detached) {
      detached_boundaries.push_back(declared_count);
    }
    visitDefault(b);
    if (detached) {
      detached_boundaries.pop_back();
    }
  }

  void handle(const slang::ast::NamedValueExpression& e) {
    if (!detached_boundaries.empty()) {
      if (const auto* var = e.symbol.as_if<slang::ast::VariableSymbol>()) {
        const auto it = declared_index.find(var);
        if (it != declared_index.end() &&
            it->second < detached_boundaries.back()) {
          result.insert(var);
        }
      }
    }
    visitDefault(e);
  }
};

}  // namespace

auto CollectLifetimeExtendedVars(const slang::ast::Statement& body)
    -> std::unordered_set<const slang::ast::VariableSymbol*> {
  LifetimeExtendedCollector collector;
  body.visit(collector);
  return std::move(collector.result);
}

}  // namespace lyra::lowering::ast_to_hir
