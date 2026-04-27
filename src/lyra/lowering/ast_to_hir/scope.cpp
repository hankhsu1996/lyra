#include "scope.hpp"

#include <expected>
#include <unordered_set>
#include <utility>
#include <variant>
#include <vector>

#include <slang/ast/Scope.h>
#include <slang/ast/SemanticFacts.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/syntax/SyntaxNode.h>

#include "facts.hpp"
#include "generate.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/support/internal_error.hpp"
#include "process.hpp"
#include "state.hpp"
#include "type.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

auto FromSlangSubroutineKind(slang::ast::SubroutineKind k)
    -> hir::SubroutineKind {
  switch (k) {
    case slang::ast::SubroutineKind::Function:
      return hir::SubroutineKind::kFunction;
    case slang::ast::SubroutineKind::Task:
      return hir::SubroutineKind::kTask;
  }
  throw support::InternalError(
      "FromSlangSubroutineKind: unknown SubroutineKind");
}

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

auto LowerScopeInto(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    hir::StructuralScope& scope, const slang::ast::Scope& slang_scope,
    ScopeStack& stack) -> diag::Result<void> {
  const auto& mapper = unit_facts.SourceMapper();
  const ScopeStackGuard guard(stack);
  ScopeLoweringState scope_state(unit_state, scope, guard.Frame());

  for (const auto& member : slang_scope.members()) {
    if (member.kind != slang::ast::SymbolKind::Variable) {
      continue;
    }
    const auto& var = member.as<slang::ast::VariableSymbol>();
    if (var.lifetime != slang::ast::VariableLifetime::Static) {
      return diag::Unsupported(
          mapper.PointSpanOf(var.location),
          diag::DiagCode::kUnsupportedNonStaticVariableLifetime,
          "only static variables are supported",
          diag::UnsupportedCategory::kFeature);
    }
    auto type_data =
        LowerTypeData(var.getType(), mapper.PointSpanOf(var.location));
    if (!type_data) return std::unexpected(std::move(type_data.error()));
    // Slang rejects `void` in any variable-declaration position before
    // elaboration, so a void-typed VariableSymbol can only reach this path
    // via a slang/Lyra integration bug.
    if (std::holds_alternative<hir::VoidType>(*type_data)) {
      throw support::InternalError(
          "LowerScopeInto: variable declaration produced void type");
    }
    const auto type_id = unit_state.AddType(*std::move(type_data));
    scope_state.AddMemberVar(var, type_id);
  }

  for (const auto& member : slang_scope.members()) {
    if (member.kind != slang::ast::SymbolKind::Subroutine) {
      continue;
    }
    const auto& sym = member.as<slang::ast::SubroutineSymbol>();
    auto return_type_data =
        LowerTypeData(sym.getReturnType(), mapper.PointSpanOf(sym.location));
    if (!return_type_data) {
      return std::unexpected(std::move(return_type_data.error()));
    }
    const auto return_type_id =
        unit_state.AddType(*std::move(return_type_data));
    scope_state.AddSubroutine(
        sym, hir::UserSubroutineDecl{
                 .name = std::string{sym.name},
                 .kind = FromSlangSubroutineKind(sym.subroutineKind),
                 .result_type = return_type_id});
  }

  for (const auto& member : slang_scope.members()) {
    if (member.kind != slang::ast::SymbolKind::ProceduralBlock) {
      continue;
    }
    const auto& proc = member.as<slang::ast::ProceduralBlockSymbol>();
    if (proc.procedureKind != slang::ast::ProceduralBlockKind::Initial) {
      return diag::Unsupported(
          mapper.PointSpanOf(proc.location),
          diag::DiagCode::kUnsupportedNonInitialProcedure,
          "only `initial` procedural blocks are supported",
          diag::UnsupportedCategory::kFeature);
    }
    auto p = LowerProcess(unit_facts, scope_state, stack, proc);
    if (!p) return std::unexpected(std::move(p.error()));
    scope_state.AddProcess(*std::move(p));
  }

  std::unordered_set<const slang::syntax::SyntaxNode*> consumed;
  for (const auto& member : slang_scope.members()) {
    switch (member.kind) {
      case slang::ast::SymbolKind::GenerateBlockArray:
        return diag::Unsupported(
            mapper.PointSpanOf(member.location),
            diag::DiagCode::kUnsupportedForGenerate,
            "for-generate is not supported yet",
            diag::UnsupportedCategory::kFeature);

      case slang::ast::SymbolKind::GenerateBlock: {
        const auto& block = member.as<slang::ast::GenerateBlockSymbol>();
        const auto* syntax = block.generateConstructSyntax;
        if (syntax == nullptr) {
          throw support::InternalError(
              "LowerScopeInto: generate block has no construct syntax");
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
          auto g = BuildCaseGenerate(
              unit_facts, unit_state, scope_state, stack, siblings);
          if (!g) return std::unexpected(std::move(g.error()));
          scope_state.AddGenerate(*std::move(g));
        } else {
          auto g = BuildIfGenerate(
              unit_facts, unit_state, scope_state, stack, siblings);
          if (!g) return std::unexpected(std::move(g.error()));
          scope_state.AddGenerate(*std::move(g));
        }
        break;
      }

      default:
        break;
    }
  }

  return {};
}

}  // namespace lyra::lowering::ast_to_hir
