#include "lyra/lowering/ast_to_hir/lifetime_extension.hpp"

#include <cstddef>
#include <span>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

#include <slang/ast/ASTVisitor.h>
#include <slang/ast/Statement.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/statements/LoopStatements.h>
#include <slang/ast/statements/MiscStatements.h>
#include <slang/ast/symbols/VariableSymbols.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/lowering/ast_to_hir/unit_identity.hpp"

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

  void Declare(const slang::ast::VariableSymbol& var) {
    if (!HasStaticStorage(var)) {
      declared_index.emplace(&var, declared_count++);
    }
  }

  void handle(const slang::ast::VariableDeclStatement& s) {
    Declare(s.symbol);
    // A declaration's initializer is one of the expressions the body evaluates,
    // and it hangs off the declared symbol rather than under the statement, so
    // the walk reaches it only by descending through the symbol. LRM 9.3.2
    // makes that the interesting position: a branch's own declaration is
    // initialized when the branch starts running, so an enclosing automatic it
    // names has to outlive the scope that declared it.
    s.symbol.visit(*this);
    visitDefault(s);
  }

  // LRM 12.7.3: a foreach declares its loop variables itself, automatic, with
  // no declaration statement, so the loop is where each one is declared.
  void handle(const slang::ast::ForeachLoopStatement& s) {
    for (const auto& dim : s.loopDims) {
      if (dim.loopVar != nullptr) {
        Declare(*dim.loopVar);
      }
    }
    visitDefault(s);
  }

  // A fork's block items initialize in the forking process, so a name in one
  // is no borrow; they are declared at or after the boundary, so a branch
  // reading one, which takes its own copy, borrows nothing either.
  void handle(const slang::ast::BlockStatement& b) {
    if (!BranchesMayOutliveBlock(b.blockKind)) {
      visitDefault(b);
      return;
    }
    const std::size_t boundary = declared_count;
    const auto visit_child = [&](const slang::ast::Statement& child) {
      if (child.kind == slang::ast::StatementKind::VariableDeclaration) {
        child.visit(*this);
        return;
      }
      detached_boundaries.push_back(boundary);
      child.visit(*this);
      detached_boundaries.pop_back();
    };
    if (b.body.kind == slang::ast::StatementKind::List) {
      for (const slang::ast::Statement* child :
           b.body.as<slang::ast::StatementList>().list) {
        visit_child(*child);
      }
    } else {
      visit_child(b.body);
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

auto BranchesMayOutliveBlock(slang::ast::StatementBlockKind kind) -> bool {
  switch (kind) {
    case slang::ast::StatementBlockKind::Sequential:
    case slang::ast::StatementBlockKind::JoinAll:
      return false;
    case slang::ast::StatementBlockKind::JoinAny:
    case slang::ast::StatementBlockKind::JoinNone:
      return true;
  }
  throw InternalError("BranchesMayOutliveBlock: unknown StatementBlockKind");
}

auto CollectLifetimeExtendedVars(
    std::span<const slang::ast::VariableSymbol* const> declared_ahead,
    const slang::ast::Statement& body)
    -> std::unordered_set<const slang::ast::VariableSymbol*> {
  LifetimeExtendedCollector collector;
  for (const slang::ast::VariableSymbol* var : declared_ahead) {
    collector.Declare(*var);
  }
  body.visit(collector);
  return std::move(collector.result);
}

}  // namespace lyra::lowering::ast_to_hir
