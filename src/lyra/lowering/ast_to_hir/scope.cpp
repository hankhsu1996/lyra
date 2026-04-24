#include "scope.hpp"

#include <unordered_set>
#include <utility>
#include <vector>

#include <slang/ast/Scope.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/syntax/SyntaxNode.h>

#include "facts.hpp"
#include "generate.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/support/unsupported.hpp"
#include "process.hpp"
#include "state.hpp"
#include "type.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

auto IsCaseConstruct(
    const std::vector<const slang::ast::GenerateBlockSymbol*>& siblings)
    -> bool {
  for (const auto* block : siblings) {
    switch (block->branchKind) {
      case slang::ast::GenerateBranchKind::CaseItem:
      case slang::ast::GenerateBranchKind::CaseDefault:
        return true;
      default:
        break;
    }
  }
  return false;
}

}  // namespace

void LowerScope(
    UnitLoweringState& unit, hir::StructuralScope& scope,
    const slang::ast::Scope& slang_scope, ScopeStack& stack) {
  const ScopeLoweringFacts facts(slang_scope);
  const ScopeStackGuard guard(stack, scope);
  ScopeLoweringState scope_state(unit, scope, guard.Frame());

  for (const auto& member : slang_scope.members()) {
    if (member.kind != slang::ast::SymbolKind::Variable) {
      continue;
    }
    const auto& var = member.as<slang::ast::VariableSymbol>();
    if (var.lifetime != slang::ast::VariableLifetime::Static) {
      support::Unsupported("LowerScope: only static vars supported");
    }
    const hir::TypeId type_id = LowerType(unit, var.getType());
    scope_state.AddVarDecl(var, type_id);
  }

  for (const auto& member : slang_scope.members()) {
    if (member.kind != slang::ast::SymbolKind::ProceduralBlock) {
      continue;
    }
    const auto& proc = member.as<slang::ast::ProceduralBlockSymbol>();
    if (proc.procedureKind != slang::ast::ProceduralBlockKind::Initial) {
      support::Unsupported("LowerScope: only `initial` processes supported");
    }
    scope_state.AddProcess(LowerProcess(facts, scope_state, stack, proc));
  }

  std::unordered_set<const slang::syntax::SyntaxNode*> consumed;
  for (const auto& member : slang_scope.members()) {
    switch (member.kind) {
      case slang::ast::SymbolKind::GenerateBlockArray:
        support::Unsupported(
            "LowerScope: for-generate not supported in this cut");

      case slang::ast::SymbolKind::GenerateBlock: {
        const auto& block = member.as<slang::ast::GenerateBlockSymbol>();
        const auto* syntax = block.generateConstructSyntax;
        if (syntax == nullptr) {
          support::Unsupported(
              "LowerScope: generate block missing construct syntax");
        }
        if (!consumed.insert(syntax).second) {
          continue;
        }

        std::vector<const slang::ast::GenerateBlockSymbol*> siblings;
        for (const auto& candidate : slang_scope.members()) {
          if (candidate.kind != slang::ast::SymbolKind::GenerateBlock) {
            continue;
          }
          const auto& sibling = candidate.as<slang::ast::GenerateBlockSymbol>();
          if (sibling.generateConstructSyntax == syntax) {
            siblings.push_back(&sibling);
          }
        }

        if (IsCaseConstruct(siblings)) {
          scope_state.AddGenerate(
              BuildCaseGenerate(unit, scope_state, stack, siblings));
        } else {
          scope_state.AddGenerate(
              BuildIfGenerate(unit, scope_state, stack, siblings));
        }
        break;
      }

      default:
        break;
    }
  }
}

}  // namespace lyra::lowering::ast_to_hir
