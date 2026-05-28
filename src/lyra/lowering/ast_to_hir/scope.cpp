#include "lyra/lowering/ast_to_hir/scope.hpp"

#include <cstdint>
#include <expected>
#include <unordered_set>
#include <utility>
#include <vector>

#include <slang/ast/Scope.h>
#include <slang/ast/SemanticFacts.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/ValueSymbol.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/AllTypes.h>

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

auto LowerTypeAliasMemberInto(
    const UnitLoweringFacts& unit_facts, ScopeLoweringState& scope_state,
    const slang::ast::TypeAliasType& alias) -> diag::Result<void> {
  const auto& mapper = unit_facts.SourceMapper();
  auto target_or = scope_state.UnitState().GetTypeId(
      alias.targetType.getType(), mapper.PointSpanOf(alias.location));
  if (!target_or) return std::unexpected(std::move(target_or.error()));
  scope_state.AddTypeAlias(
      hir::TypeAliasDecl{
          .name = std::string{alias.name}, .target = *target_or});
  return {};
}

auto LowerVariableMemberInto(
    const UnitLoweringFacts& unit_facts, ScopeLoweringState& scope_state,
    const slang::ast::VariableSymbol& var) -> diag::Result<void> {
  const auto& mapper = unit_facts.SourceMapper();
  auto& unit_state = scope_state.UnitState();
  if (var.lifetime != slang::ast::VariableLifetime::Static) {
    return diag::Unsupported(
        mapper.PointSpanOf(var.location),
        diag::DiagCode::kUnsupportedNonStaticVariableLifetime,
        "only static variables are supported",
        diag::UnsupportedCategory::kFeature);
  }
  auto type_id_or =
      unit_state.GetTypeId(var.getType(), mapper.PointSpanOf(var.location));
  if (!type_id_or) return std::unexpected(std::move(type_id_or.error()));
  // Slang rejects `void` in any variable-declaration position before
  // elaboration, so a void-typed VariableSymbol can only reach this path
  // via a slang/Lyra integration bug.
  if (std::holds_alternative<hir::VoidType>(
          unit_state.GetType(*type_id_or).data)) {
    throw InternalError(
        "LowerVariableMemberInto: variable declaration produced void type");
  }
  scope_state.AddStructuralVar(var, *type_id_or);
  return {};
}

auto LowerSubroutineMemberInto(
    const UnitLoweringFacts& unit_facts, ScopeLoweringState& scope_state,
    const slang::ast::SubroutineSymbol& sym) -> diag::Result<void> {
  const auto& mapper = unit_facts.SourceMapper();
  auto return_type_id_or = scope_state.UnitState().GetTypeId(
      sym.getReturnType(), mapper.PointSpanOf(sym.location));
  if (!return_type_id_or) {
    return std::unexpected(std::move(return_type_id_or.error()));
  }
  scope_state.AddStructuralSubroutine(
      sym, hir::StructuralSubroutineDecl{
               .name = std::string{sym.name},
               .kind = FromSlangSubroutineKind(sym.subroutineKind),
               .result_type = *return_type_id_or});
  return {};
}

auto LowerProceduralBlockMemberInto(
    const UnitLoweringFacts& unit_facts, ScopeLoweringState& scope_state,
    ScopeStack& stack, const slang::ast::ProceduralBlockSymbol& proc)
    -> diag::Result<void> {
  auto p = LowerProcess(unit_facts, scope_state, stack, proc);
  if (!p) return std::unexpected(std::move(p.error()));
  scope_state.AddProcess(*std::move(p));
  return {};
}

auto LowerLoopGenerateMemberInto(
    const UnitLoweringFacts& unit_facts, ScopeLoweringState& scope_state,
    ScopeStack& stack, const slang::ast::GenerateBlockArraySymbol& array)
    -> diag::Result<void> {
  auto g = BuildLoopGenerate(
      unit_facts, scope_state.UnitState(), scope_state, stack, array);
  if (!g) return std::unexpected(std::move(g.error()));
  scope_state.AddGenerate(*std::move(g));
  return {};
}

auto LowerIfOrCaseGenerateMemberInto(
    const UnitLoweringFacts& unit_facts, ScopeLoweringState& scope_state,
    ScopeStack& stack, const slang::ast::GenerateBlockSymbol& block,
    const slang::ast::Scope& slang_scope) -> diag::Result<void> {
  // slang assigns constructIndex per direct generate construct in the
  // containing scope (Scope.cpp:927-1033): incremented after each construct,
  // shared across siblings of one if/case generate. The sibling-collect loop
  // below groups the members that belong to the same construct.
  std::vector<const slang::ast::GenerateBlockSymbol*> siblings;
  for (const auto& candidate : slang_scope.members()) {
    if (candidate.kind != slang::ast::SymbolKind::GenerateBlock) continue;
    const auto& sibling = candidate.as<slang::ast::GenerateBlockSymbol>();
    if (sibling.constructIndex == block.constructIndex) {
      siblings.push_back(&sibling);
    }
  }
  auto g = IsCaseConstruct(siblings) ? BuildCaseGenerate(
                                           unit_facts, scope_state.UnitState(),
                                           scope_state, stack, siblings)
                                     : BuildIfGenerate(
                                           unit_facts, scope_state.UnitState(),
                                           scope_state, stack, siblings);
  if (!g) return std::unexpected(std::move(g.error()));
  scope_state.AddGenerate(*std::move(g));
  return {};
}

auto LowerScopeMemberInto(
    const UnitLoweringFacts& unit_facts, ScopeLoweringState& scope_state,
    ScopeStack& stack, const slang::ast::Symbol& member,
    const slang::ast::Scope& slang_scope) -> diag::Result<void> {
  switch (member.kind) {
    case slang::ast::SymbolKind::TypeAlias:
      return LowerTypeAliasMemberInto(
          unit_facts, scope_state, member.as<slang::ast::TypeAliasType>());
    case slang::ast::SymbolKind::Variable:
      return LowerVariableMemberInto(
          unit_facts, scope_state, member.as<slang::ast::VariableSymbol>());
    case slang::ast::SymbolKind::Subroutine:
      return LowerSubroutineMemberInto(
          unit_facts, scope_state, member.as<slang::ast::SubroutineSymbol>());
    case slang::ast::SymbolKind::ProceduralBlock:
      return LowerProceduralBlockMemberInto(
          unit_facts, scope_state, stack,
          member.as<slang::ast::ProceduralBlockSymbol>());
    case slang::ast::SymbolKind::GenerateBlockArray:
      return LowerLoopGenerateMemberInto(
          unit_facts, scope_state, stack,
          member.as<slang::ast::GenerateBlockArraySymbol>());
    case slang::ast::SymbolKind::GenerateBlock:
      return LowerIfOrCaseGenerateMemberInto(
          unit_facts, scope_state, stack,
          member.as<slang::ast::GenerateBlockSymbol>(), slang_scope);
    default:
      return {};
  }
}

auto LowerScopeMembersInto(
    const UnitLoweringFacts& unit_facts, ScopeLoweringState& scope_state,
    ScopeStack& stack, const slang::ast::Scope& slang_scope)
    -> diag::Result<void> {
  // GenerateBlock siblings of one if/case generate share a constructIndex
  // (slang Scope.cpp:927-1033); we hand the dispatcher only the first sibling
  // and skip the rest, so the per-construct sibling-collect loop inside
  // LowerIfOrCaseGenerateMemberInto runs exactly once per construct.
  std::unordered_set<std::uint32_t> consumed_construct_indices;
  for (const auto& member : slang_scope.members()) {
    if (member.kind == slang::ast::SymbolKind::GenerateBlock) {
      const auto& block = member.as<slang::ast::GenerateBlockSymbol>();
      if (!consumed_construct_indices.insert(block.constructIndex).second) {
        continue;
      }
    }
    auto r = LowerScopeMemberInto(
        unit_facts, scope_state, stack, member, slang_scope);
    if (!r) return std::unexpected(std::move(r.error()));
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
