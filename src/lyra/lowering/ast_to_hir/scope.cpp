#include "lyra/lowering/ast_to_hir/scope.hpp"

#include <cstdint>
#include <expected>
#include <unordered_set>
#include <utility>
#include <vector>

#include <slang/ast/Scope.h>
#include <slang/ast/SemanticFacts.h>
#include <slang/ast/Statement.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
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
#include "lyra/lowering/ast_to_hir/continuous_assign.hpp"
#include "lyra/lowering/ast_to_hir/expression/lower.hpp"
#include "lyra/lowering/ast_to_hir/facts.hpp"
#include "lyra/lowering/ast_to_hir/generate.hpp"
#include "lyra/lowering/ast_to_hir/port_connection.hpp"
#include "lyra/lowering/ast_to_hir/process.hpp"
#include "lyra/lowering/ast_to_hir/state.hpp"
#include "lyra/lowering/ast_to_hir/statement/lower.hpp"
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
    ScopeStack& stack, const slang::ast::VariableSymbol& var)
    -> diag::Result<void> {
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
  std::optional<hir::ExprId> initializer_id;
  if (const auto* init = var.getInitializer(); init != nullptr) {
    auto init_or =
        LowerStructuralExpr(unit_facts, unit_state, scope_state, stack, *init);
    if (!init_or) return std::unexpected(std::move(init_or.error()));
    initializer_id = scope_state.AddExpr(*std::move(init_or));
  }
  scope_state.AddStructuralVar(var, *type_id_or, initializer_id);
  return {};
}

// slang has no ConstRef direction (LRM 13.5.2): a `const ref` formal carries
// direction Ref with the Const variable flag, so the const-ness must be read
// off the formal rather than the direction enum alone.
auto ParamDirectionOf(const slang::ast::FormalArgumentSymbol& formal)
    -> hir::ParamDirection {
  switch (formal.direction) {
    case slang::ast::ArgumentDirection::In:
      return hir::ParamDirection::kInput;
    case slang::ast::ArgumentDirection::Out:
      return hir::ParamDirection::kOutput;
    case slang::ast::ArgumentDirection::InOut:
      return hir::ParamDirection::kInOut;
    case slang::ast::ArgumentDirection::Ref:
      return formal.flags.has(slang::ast::VariableFlags::Const)
                 ? hir::ParamDirection::kConstRef
                 : hir::ParamDirection::kRef;
  }
  throw InternalError("ParamDirectionOf: unknown ArgumentDirection");
}

auto LowerSubroutineMemberInto(
    const UnitLoweringFacts& unit_facts, ScopeLoweringState& scope_state,
    ScopeStack& stack, const slang::ast::SubroutineSymbol& sym)
    -> diag::Result<void> {
  const auto& mapper = unit_facts.SourceMapper();
  auto& unit_state = scope_state.UnitState();
  auto return_type_id_or = unit_state.GetTypeId(
      sym.getReturnType(), mapper.PointSpanOf(sym.location));
  if (!return_type_id_or) {
    return std::unexpected(std::move(return_type_id_or.error()));
  }

  ProcessLoweringState sub_state(sym);

  std::vector<hir::SubroutineParam> params;
  params.reserve(sym.getArguments().size());
  for (const auto* formal : sym.getArguments()) {
    auto formal_type_or = unit_state.GetTypeId(
        formal->getType(), mapper.PointSpanOf(formal->location));
    if (!formal_type_or) {
      return std::unexpected(std::move(formal_type_or.error()));
    }
    const hir::ProceduralVarId var =
        sub_state.AddProceduralVar(*formal, *formal_type_or);
    params.push_back(
        hir::SubroutineParam{
            .var = var, .direction = ParamDirectionOf(*formal)});
  }

  // LRM 13.4.1: a non-void function implicitly declares a body-local variable
  // of the return type, named after the function, that the body reads and
  // writes to produce the return value. slang exposes it as `returnValVar`;
  // void functions and tasks have none. Bind it after the formals so its
  // procedural-var id follows them in declaration order.
  std::optional<hir::ProceduralVarId> result_var;
  if (sym.returnValVar != nullptr) {
    result_var =
        sub_state.AddProceduralVar(*sym.returnValVar, *return_type_id_or);
  }

  auto body_or =
      LowerStatement(unit_facts, sub_state, scope_state, stack, sym.getBody());
  if (!body_or) return std::unexpected(std::move(body_or.error()));
  const hir::StmtId root = sub_state.AddStmt(*std::move(body_or));

  scope_state.AddStructuralSubroutine(
      sym, hir::StructuralSubroutineDecl{
               .name = std::string{sym.name},
               .kind = FromSlangSubroutineKind(sym.subroutineKind),
               .result_type = *return_type_id_or,
               .params = std::move(params),
               .result_var = result_var,
               .body = sub_state.FinalizeBody(root)});
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

auto LowerContinuousAssignMemberInto(
    const UnitLoweringFacts& unit_facts, ScopeLoweringState& scope_state,
    ScopeStack& stack, const slang::ast::ContinuousAssignSymbol& sym)
    -> diag::Result<void> {
  auto ca = LowerContinuousAssign(unit_facts, scope_state, stack, sym);
  if (!ca) return std::unexpected(std::move(ca.error()));
  scope_state.AddContinuousAssign(*std::move(ca));
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

auto LowerInstanceMemberInto(
    ScopeLoweringState& scope_state, const slang::ast::InstanceSymbol& inst)
    -> diag::Result<void> {
  const hir::InstanceMemberId member_id = scope_state.AddInstanceMember(
      hir::InstanceMemberDecl{
          .instance_name = std::string{inst.name},
          .target_unit = std::string{inst.getDefinition().name},
          .array_dims = {}});
  // A downward cross-unit reference (`c.x`) resolves the leading `c` to this
  // member; the binding lets process-body lowering find it regardless of
  // source order.
  scope_state.UnitState().MapInstanceMemberBinding(
      inst, scope_state.Frame(), member_id);
  return {};
}

auto LowerInstanceArrayMemberInto(
    ScopeLoweringState& scope_state,
    const slang::ast::InstanceArraySymbol& array) -> diag::Result<void> {
  // Each nested InstanceArray level contributes one dimension; the descent
  // bottoms out at the per-element instance, which names the target unit. A
  // zero-element level (`Child c[0:-1]`, LRM 23.3.2) has no element to descend
  // into or to name the unit from and constructs nothing, so it contributes no
  // member.
  std::vector<std::uint32_t> dims;
  const slang::ast::Symbol* level = &array;
  while (level->kind == slang::ast::SymbolKind::InstanceArray) {
    const auto& arr = level->as<slang::ast::InstanceArraySymbol>();
    if (arr.elements.empty()) {
      return {};
    }
    dims.push_back(static_cast<std::uint32_t>(arr.elements.size()));
    level = arr.elements.front();
  }
  const auto& leaf = level->as<slang::ast::InstanceSymbol>();
  const hir::InstanceMemberId member_id = scope_state.AddInstanceMember(
      hir::InstanceMemberDecl{
          .instance_name = std::string{array.name},
          .target_unit = std::string{leaf.getDefinition().name},
          .array_dims = std::move(dims)});
  // A downward cross-unit reference whose path indexes this array (`c[i].x`)
  // resolves the leading `c` to this member; the binding lets process-body
  // lowering find it regardless of source order.
  scope_state.UnitState().MapInstanceMemberBinding(
      array, scope_state.Frame(), member_id);
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
          unit_facts, scope_state, stack,
          member.as<slang::ast::VariableSymbol>());
    case slang::ast::SymbolKind::Subroutine:
      return LowerSubroutineMemberInto(
          unit_facts, scope_state, stack,
          member.as<slang::ast::SubroutineSymbol>());
    case slang::ast::SymbolKind::ProceduralBlock:
      return LowerProceduralBlockMemberInto(
          unit_facts, scope_state, stack,
          member.as<slang::ast::ProceduralBlockSymbol>());
    case slang::ast::SymbolKind::ContinuousAssign:
      return LowerContinuousAssignMemberInto(
          unit_facts, scope_state, stack,
          member.as<slang::ast::ContinuousAssignSymbol>());
    case slang::ast::SymbolKind::GenerateBlockArray:
      return LowerLoopGenerateMemberInto(
          unit_facts, scope_state, stack,
          member.as<slang::ast::GenerateBlockArraySymbol>());
    case slang::ast::SymbolKind::GenerateBlock:
      return LowerIfOrCaseGenerateMemberInto(
          unit_facts, scope_state, stack,
          member.as<slang::ast::GenerateBlockSymbol>(), slang_scope);
    default:
      // Instance / InstanceArray members are lowered in a pre-pass (see
      // LowerScopeMembersInto); every other member kind is not modeled.
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
  // Forward-declare every subroutine's binding before lowering any body so a
  // call resolves regardless of source order: direct self-recursion, mutual
  // recursion, and forward references to a later-defined subroutine (LRM
  // 13.4.2). Bodies are added in the same order by the main pass below.
  for (const auto& member : slang_scope.members()) {
    if (member.kind == slang::ast::SymbolKind::Subroutine) {
      scope_state.ReserveSubroutineBinding(
          member.as<slang::ast::SubroutineSymbol>());
    }
  }

  // Instance members are lowered ahead of process bodies so a downward
  // cross-unit reference (`c.x`) resolves its leading `c` even when the
  // referencing process precedes the instance in source order (declarations
  // are scope-wide). The main pass below skips them.
  for (const auto& member : slang_scope.members()) {
    if (member.kind == slang::ast::SymbolKind::Instance) {
      auto r = LowerInstanceMemberInto(
          scope_state, member.as<slang::ast::InstanceSymbol>());
      if (!r) return std::unexpected(std::move(r.error()));
    } else if (member.kind == slang::ast::SymbolKind::InstanceArray) {
      auto r = LowerInstanceArrayMemberInto(
          scope_state, member.as<slang::ast::InstanceArraySymbol>());
      if (!r) return std::unexpected(std::move(r.error()));
    }
  }

  std::unordered_set<std::uint32_t> consumed_construct_indices;
  for (const auto& member : slang_scope.members()) {
    if (member.kind == slang::ast::SymbolKind::Instance ||
        member.kind == slang::ast::SymbolKind::InstanceArray) {
      continue;
    }
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

  // A variable port connection is an implied continuous assignment
  // (LRM 23.3.3), synthesized after every variable and instance binding exists
  // so its source and child-side endpoint resolve regardless of source order.
  auto pc = LowerScopePortConnectionsInto(
      unit_facts, scope_state, stack, slang_scope);
  if (!pc) return std::unexpected(std::move(pc.error()));
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
  auto r = LowerScopeMembersInto(unit_facts, scope_state, stack, slang_scope);
  if (!r) return std::unexpected(std::move(r.error()));
  scope.cross_unit_refs = unit_state.TakeCrossUnitRefsForFrame(guard.Frame());
  return {};
}

}  // namespace lyra::lowering::ast_to_hir
