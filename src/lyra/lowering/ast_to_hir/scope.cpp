#include "lyra/lowering/ast_to_hir/scope.hpp"

#include <cstdint>
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
#include <slang/ast/symbols/ValueSymbol.h>
#include <slang/ast/symbols/VariableSymbols.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/loop_var.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/lowering/ast_to_hir/facts.hpp"
#include "lyra/lowering/ast_to_hir/generate.hpp"
#include "lyra/lowering/ast_to_hir/process.hpp"
#include "lyra/lowering/ast_to_hir/state.hpp"
#include "lyra/lowering/ast_to_hir/time_resolution.hpp"
#include "lyra/lowering/ast_to_hir/type.hpp"

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
  throw InternalError("FromSlangSubroutineKind: unknown SubroutineKind");
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

auto LowerScopeMembersInto(
    const UnitLoweringFacts& unit_facts, ScopeLoweringState& scope_state,
    ScopeStack& stack, const slang::ast::Scope& slang_scope)
    -> diag::Result<void> {
  const auto& mapper = unit_facts.SourceMapper();
  auto& unit_state = scope_state.UnitState();

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
      throw InternalError(
          "LowerScopeMembersInto: variable declaration produced void type");
    }
    const auto type_id = unit_state.AddType(*std::move(type_data));
    scope_state.AddStructuralVar(var, type_id);
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
    scope_state.AddStructuralSubroutine(
        sym, hir::StructuralSubroutineDecl{
                 .name = std::string{sym.name},
                 .kind = FromSlangSubroutineKind(sym.subroutineKind),
                 .result_type = return_type_id});
  }

  for (const auto& member : slang_scope.members()) {
    if (member.kind != slang::ast::SymbolKind::ProceduralBlock) {
      continue;
    }
    const auto& proc = member.as<slang::ast::ProceduralBlockSymbol>();
    auto p = LowerProcess(unit_facts, scope_state, stack, proc);
    if (!p) return std::unexpected(std::move(p.error()));
    scope_state.AddProcess(*std::move(p));
  }

  // slang assigns constructIndex per direct generate construct in the
  // containing scope (Scope.cpp:927-1033): incremented after each construct,
  // shared across siblings of one if/case generate. So within this scope's
  // members it is unique per construct and serves as the sibling-grouping key.
  std::unordered_set<std::uint32_t> consumed_construct_indices;
  for (const auto& member : slang_scope.members()) {
    switch (member.kind) {
      case slang::ast::SymbolKind::GenerateBlockArray: {
        const auto& array = member.as<slang::ast::GenerateBlockArraySymbol>();
        auto g = BuildLoopGenerate(
            unit_facts, unit_state, scope_state, stack, array);
        if (!g) return std::unexpected(std::move(g.error()));
        scope_state.AddGenerate(*std::move(g));
        break;
      }

      case slang::ast::SymbolKind::GenerateBlock: {
        const auto& block = member.as<slang::ast::GenerateBlockSymbol>();
        if (!consumed_construct_indices.insert(block.constructIndex).second) {
          continue;
        }

        std::vector<const slang::ast::GenerateBlockSymbol*> siblings;
        for (const auto& candidate : slang_scope.members()) {
          if (candidate.kind != slang::ast::SymbolKind::GenerateBlock) {
            continue;
          }
          const auto& sibling = candidate.as<slang::ast::GenerateBlockSymbol>();
          if (sibling.constructIndex == block.constructIndex) {
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

}  // namespace

auto LowerScopeInto(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    hir::StructuralScope& scope, const slang::ast::Scope& slang_scope,
    ScopeStack& stack,
    std::span<const ScopeEntryLoopVarBinding> entry_loop_var_bindings)
    -> diag::Result<void> {
  const ScopeStackGuard guard(stack);
  ScopeLoweringState scope_state(unit_state, scope, guard.Frame());
  scope.time_resolution = ResolveTimeResolution(slang_scope.getTimeScale());
  for (const auto& binding : entry_loop_var_bindings) {
    if (binding.symbol == nullptr) {
      throw InternalError(
          "LowerScopeInto: null scope-entry loop-var binding symbol");
    }
    unit_state.MapLoopVarBinding(
        *binding.symbol, binding.home_frame, binding.loop_var, binding.type);
  }
  return LowerScopeMembersInto(unit_facts, scope_state, stack, slang_scope);
}

}  // namespace lyra::lowering::ast_to_hir
