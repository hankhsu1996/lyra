#include "lyra/lowering/ast_to_hir/scope_lowerer.hpp"

#include <cstdint>
#include <expected>
#include <optional>
#include <span>
#include <string>
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
#include "lyra/hir/continuous_assign.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/loop_var.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/structural_var.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/lowering/ast_to_hir/module_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/time_resolution.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

auto ToHirSubroutineKind(slang::ast::SubroutineKind k) -> hir::SubroutineKind {
  switch (k) {
    case slang::ast::SubroutineKind::Function:
      return hir::SubroutineKind::kFunction;
    case slang::ast::SubroutineKind::Task:
      return hir::SubroutineKind::kTask;
  }
  throw InternalError("ToHirSubroutineKind: unknown SubroutineKind");
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

}  // namespace

ScopeLowerer::ScopeLowerer(
    ModuleLowerer& module, hir::StructuralScope& scope,
    const slang::ast::Scope& slang_scope,
    std::vector<ScopeEntryLoopVarBinding> entry_loop_var_bindings)
    : module_(&module),
      slang_scope_(&slang_scope),
      frame_(module.NextScopeFrameId()),
      entry_loop_var_bindings_(std::move(entry_loop_var_bindings)),
      scope_(&scope) {
}

// Builder API.

auto ScopeLowerer::AddStructuralVar(
    const slang::ast::VariableSymbol& var, hir::TypeId type,
    std::optional<hir::ExprId> initializer) -> hir::StructuralVarId {
  const hir::StructuralVarId local{
      static_cast<std::uint32_t>(scope_->structural_vars.size())};
  scope_->structural_vars.push_back(
      hir::StructuralVarDecl{
          .name = std::string{var.name},
          .type = type,
          .initializer = initializer});
  module_->MapStructuralVarBinding(var, frame_, local, type);
  return local;
}

void ScopeLowerer::AddTypeAlias(hir::TypeAliasDecl decl) {
  scope_->type_aliases.push_back(std::move(decl));
}

auto ScopeLowerer::AddLoopVarDecl(
    const slang::ast::ValueSymbol& sym, hir::TypeId type)
    -> hir::LoopVarDeclId {
  const hir::LoopVarDeclId id{
      static_cast<std::uint32_t>(scope_->loop_var_decls.size())};
  scope_->loop_var_decls.push_back(
      hir::LoopVarDecl{.name = std::string{sym.name}, .type = type});
  module_->MapLoopVarBinding(sym, frame_, id, type);
  return id;
}

auto ScopeLowerer::AddExpr(hir::Expr expr) -> hir::ExprId {
  const hir::ExprId id{static_cast<std::uint32_t>(scope_->exprs.size())};
  scope_->exprs.push_back(std::move(expr));
  return id;
}

auto ScopeLowerer::AddProcess(hir::Process process) -> hir::ProcessId {
  const hir::ProcessId id{static_cast<std::uint32_t>(scope_->processes.size())};
  scope_->processes.push_back(std::move(process));
  return id;
}

auto ScopeLowerer::AddContinuousAssign(hir::ContinuousAssign ca)
    -> hir::ContinuousAssignId {
  const hir::ContinuousAssignId id{
      static_cast<std::uint32_t>(scope_->continuous_assigns.size())};
  scope_->continuous_assigns.push_back(std::move(ca));
  return id;
}

auto ScopeLowerer::AddGenerate(hir::Generate generate) -> hir::GenerateId {
  const hir::GenerateId id{
      static_cast<std::uint32_t>(scope_->generates.size())};
  scope_->generates.push_back(std::move(generate));
  return id;
}

auto ScopeLowerer::NextGenerateId() const -> hir::GenerateId {
  return hir::GenerateId{static_cast<std::uint32_t>(scope_->generates.size())};
}

auto ScopeLowerer::AddInstanceMember(hir::InstanceMemberDecl decl)
    -> hir::InstanceMemberId {
  const hir::InstanceMemberId id{
      static_cast<std::uint32_t>(scope_->instance_members.size())};
  scope_->instance_members.push_back(std::move(decl));
  return id;
}

void ScopeLowerer::ReserveSubroutineBinding(
    const slang::ast::SubroutineSymbol& sym) {
  const hir::StructuralSubroutineId local{reserved_subroutine_count_++};
  module_->MapSubroutineBinding(sym, frame_, local);
}

void ScopeLowerer::AddStructuralSubroutine(
    const slang::ast::SubroutineSymbol& sym,
    hir::StructuralSubroutineDecl decl) {
  const auto binding = module_->LookupSubroutineBinding(sym);
  if (!binding.has_value() ||
      binding->subroutine_id.value !=
          static_cast<std::uint32_t>(scope_->structural_subroutines.size())) {
    throw InternalError(
        "ScopeLowerer::AddStructuralSubroutine: subroutine added out of "
        "reserved order; ReserveSubroutineBinding must run first in the same "
        "order");
  }
  scope_->structural_subroutines.push_back(std::move(decl));
}

// Run: scope-level member-by-member dispatch.

auto ScopeLowerer::Run(WalkFrame parent_frame) -> diag::Result<void> {
  const WalkFrame frame = parent_frame.WithStructuralFrame(frame_);
  scope_->time_resolution = ResolveTimeResolution(slang_scope_->getTimeScale());
  // Apply any loop-generate entry bindings (loop-generate body inherits the
  // loop var from its parent's frame so body refs compute correct hops up).
  for (const auto& binding : entry_loop_var_bindings_) {
    if (binding.symbol == nullptr) {
      throw InternalError(
          "ScopeLowerer::Run: null scope-entry loop-var "
          "binding symbol");
    }
    module_->MapLoopVarBinding(
        *binding.symbol, binding.home_frame, binding.loop_var, binding.type);
  }

  // Forward-declare every subroutine's binding before lowering any body so a
  // call resolves regardless of source order: direct self-recursion, mutual
  // recursion, and forward references (LRM 13.4.2).
  for (const auto& member : slang_scope_->members()) {
    if (member.kind == slang::ast::SymbolKind::Subroutine) {
      ReserveSubroutineBinding(member.as<slang::ast::SubroutineSymbol>());
    }
  }

  // Instance members are lowered ahead of process bodies so a downward
  // cross-unit reference (`c.x`) resolves its leading `c` even when the
  // referencing process precedes the instance in source order (declarations
  // are scope-wide).
  for (const auto& member : slang_scope_->members()) {
    if (member.kind == slang::ast::SymbolKind::Instance) {
      auto r = PopulateInstanceMember(member.as<slang::ast::InstanceSymbol>());
      if (!r) return std::unexpected(std::move(r.error()));
    } else if (member.kind == slang::ast::SymbolKind::InstanceArray) {
      auto r = PopulateInstanceArrayMember(
          member.as<slang::ast::InstanceArraySymbol>());
      if (!r) return std::unexpected(std::move(r.error()));
    }
  }

  // Structural members (variables, generates, subroutine bodies) are lowered
  // before behavioral ones (processes, continuous assigns), so a process or
  // continuous assign resolves a downward reference into a generate block it
  // textually precedes -- declarations are scope-wide (LRM 27), the same
  // reason instances are bound in the pre-pass above.
  std::unordered_set<std::uint32_t> consumed_construct_indices;
  for (const auto& member : slang_scope_->members()) {
    if (member.kind == slang::ast::SymbolKind::Instance ||
        member.kind == slang::ast::SymbolKind::InstanceArray ||
        member.kind == slang::ast::SymbolKind::ProceduralBlock ||
        member.kind == slang::ast::SymbolKind::ContinuousAssign) {
      continue;
    }
    if (member.kind == slang::ast::SymbolKind::GenerateBlock) {
      const auto& block = member.as<slang::ast::GenerateBlockSymbol>();
      if (!consumed_construct_indices.insert(block.constructIndex).second) {
        continue;
      }
    }
    auto r = PopulateMember(member, frame);
    if (!r) return std::unexpected(std::move(r.error()));
  }
  for (const auto& member : slang_scope_->members()) {
    if (member.kind != slang::ast::SymbolKind::ProceduralBlock &&
        member.kind != slang::ast::SymbolKind::ContinuousAssign) {
      continue;
    }
    auto r = PopulateMember(member, frame);
    if (!r) return std::unexpected(std::move(r.error()));
  }

  // A variable port connection is an implied continuous assignment
  // (LRM 23.3.3), synthesized after every variable and instance binding
  // exists so its source and child-side endpoint resolve regardless of source
  // order.
  auto pc = PopulatePortConnections(*slang_scope_, frame);
  if (!pc) return std::unexpected(std::move(pc.error()));

  scope_->cross_unit_refs = module_->TakeCrossUnitRefsForFrame(frame_);
  return {};
}

auto ScopeLowerer::PopulateMember(
    const slang::ast::Symbol& member, WalkFrame frame) -> diag::Result<void> {
  switch (member.kind) {
    case slang::ast::SymbolKind::TypeAlias:
      return PopulateTypeAliasMember(member.as<slang::ast::TypeAliasType>());
    case slang::ast::SymbolKind::Variable:
      return PopulateVariableMember(
          member.as<slang::ast::VariableSymbol>(), frame);
    case slang::ast::SymbolKind::Subroutine:
      return PopulateSubroutineMember(
          member.as<slang::ast::SubroutineSymbol>(), frame);
    case slang::ast::SymbolKind::ProceduralBlock:
      return PopulateProceduralBlockMember(
          member.as<slang::ast::ProceduralBlockSymbol>(), frame);
    case slang::ast::SymbolKind::ContinuousAssign:
      return PopulateContinuousAssignMember(
          member.as<slang::ast::ContinuousAssignSymbol>(), frame);
    case slang::ast::SymbolKind::GenerateBlockArray:
      return PopulateLoopGenerateMember(
          member.as<slang::ast::GenerateBlockArraySymbol>(), frame);
    case slang::ast::SymbolKind::GenerateBlock:
      return PopulateIfOrCaseGenerateMember(
          member.as<slang::ast::GenerateBlockSymbol>(), frame);
    default:
      return {};
  }
}

auto ScopeLowerer::PopulateTypeAliasMember(
    const slang::ast::TypeAliasType& alias) -> diag::Result<void> {
  const auto& mapper = module_->SourceMapper();
  auto target_or = module_->GetTypeId(
      alias.targetType.getType(), mapper.PointSpanOf(alias.location));
  if (!target_or) return std::unexpected(std::move(target_or.error()));
  AddTypeAlias(
      hir::TypeAliasDecl{
          .name = std::string{alias.name}, .target = *target_or});
  return {};
}

auto ScopeLowerer::PopulateVariableMember(
    const slang::ast::VariableSymbol& var, WalkFrame frame)
    -> diag::Result<void> {
  const auto& mapper = module_->SourceMapper();
  if (var.lifetime != slang::ast::VariableLifetime::Static) {
    return diag::Unsupported(
        mapper.PointSpanOf(var.location),
        diag::DiagCode::kUnsupportedNonStaticVariableLifetime,
        "only static variables are supported",
        diag::UnsupportedCategory::kFeature);
  }
  auto type_id_or =
      module_->GetTypeId(var.getType(), mapper.PointSpanOf(var.location));
  if (!type_id_or) return std::unexpected(std::move(type_id_or.error()));
  // Slang rejects `void` in any variable-declaration position before
  // elaboration, so a void-typed VariableSymbol can only reach this path
  // via a slang/Lyra integration bug.
  if (std::holds_alternative<hir::VoidType>(
          module_->GetType(*type_id_or).data)) {
    throw InternalError(
        "ScopeLowerer::PopulateVariableMember: variable declaration produced "
        "void type");
  }
  std::optional<hir::ExprId> initializer_id;
  if (const auto* init = var.getInitializer(); init != nullptr) {
    auto init_or = LowerExpr(*init, frame);
    if (!init_or) return std::unexpected(std::move(init_or.error()));
    initializer_id = AddExpr(*std::move(init_or));
  }
  AddStructuralVar(var, *type_id_or, initializer_id);
  return {};
}

auto ScopeLowerer::PopulateSubroutineMember(
    const slang::ast::SubroutineSymbol& sym, WalkFrame frame)
    -> diag::Result<void> {
  const auto& mapper = module_->SourceMapper();
  auto return_type_id_or =
      module_->GetTypeId(sym.getReturnType(), mapper.PointSpanOf(sym.location));
  if (!return_type_id_or) {
    return std::unexpected(std::move(return_type_id_or.error()));
  }

  ProcessLowerer sub_lowerer(*module_, sym);

  std::vector<hir::SubroutineParam> params;
  params.reserve(sym.getArguments().size());
  for (const auto* formal : sym.getArguments()) {
    auto formal_type_or = module_->GetTypeId(
        formal->getType(), mapper.PointSpanOf(formal->location));
    if (!formal_type_or) {
      return std::unexpected(std::move(formal_type_or.error()));
    }
    const hir::ProceduralVarId var =
        sub_lowerer.AddProceduralVar(*formal, *formal_type_or);
    params.push_back(
        hir::SubroutineParam{
            .var = var, .direction = ParamDirectionOf(*formal)});
  }

  // LRM 13.4.1: a non-void function implicitly declares a body-local
  // variable of the return type, named after the function, that the body
  // reads and writes to produce the return value.
  std::optional<hir::ProceduralVarId> result_var;
  if (sym.returnValVar != nullptr) {
    result_var =
        sub_lowerer.AddProceduralVar(*sym.returnValVar, *return_type_id_or);
  }

  auto body_stmt_or = sub_lowerer.LowerStmt(sym.getBody(), frame);
  if (!body_stmt_or) return std::unexpected(std::move(body_stmt_or.error()));
  const auto root_id_or = sub_lowerer.AddStmt(*std::move(body_stmt_or));

  AddStructuralSubroutine(
      sym, hir::StructuralSubroutineDecl{
               .name = std::string{sym.name},
               .kind = ToHirSubroutineKind(sym.subroutineKind),
               .result_type = *return_type_id_or,
               .params = std::move(params),
               .result_var = result_var,
               .body = sub_lowerer.FinalizeBody(root_id_or)});
  return {};
}

auto ScopeLowerer::PopulateProceduralBlockMember(
    const slang::ast::ProceduralBlockSymbol& proc, WalkFrame frame)
    -> diag::Result<void> {
  ProcessLowerer proc_lowerer(*module_, proc);
  auto p = proc_lowerer.Run(proc, frame);
  if (!p) return std::unexpected(std::move(p.error()));
  AddProcess(*std::move(p));
  return {};
}

auto ScopeLowerer::PopulateContinuousAssignMember(
    const slang::ast::ContinuousAssignSymbol& sym, WalkFrame frame)
    -> diag::Result<void> {
  auto ca = LowerContinuousAssign(sym, frame);
  if (!ca) return std::unexpected(std::move(ca.error()));
  AddContinuousAssign(*std::move(ca));
  return {};
}

auto ScopeLowerer::PopulateLoopGenerateMember(
    const slang::ast::GenerateBlockArraySymbol& array, WalkFrame frame)
    -> diag::Result<void> {
  auto g = BuildLoopGenerate(array, frame);
  if (!g) return std::unexpected(std::move(g.error()));
  AddGenerate(*std::move(g));
  return {};
}

auto ScopeLowerer::PopulateIfOrCaseGenerateMember(
    const slang::ast::GenerateBlockSymbol& block, WalkFrame frame)
    -> diag::Result<void> {
  // slang assigns constructIndex per direct generate construct in the
  // containing scope (Scope.cpp:927-1033): incremented after each construct,
  // shared across siblings of one if/case generate.
  std::vector<const slang::ast::GenerateBlockSymbol*> siblings;
  for (const auto& candidate : slang_scope_->members()) {
    if (candidate.kind != slang::ast::SymbolKind::GenerateBlock) continue;
    const auto& sibling = candidate.as<slang::ast::GenerateBlockSymbol>();
    if (sibling.constructIndex == block.constructIndex) {
      siblings.push_back(&sibling);
    }
  }
  auto g = IsCaseConstruct(siblings) ? BuildCaseGenerate(siblings, frame)
                                     : BuildIfGenerate(siblings, frame);
  if (!g) return std::unexpected(std::move(g.error()));
  AddGenerate(*std::move(g));
  return {};
}

auto ScopeLowerer::PopulateInstanceMember(
    const slang::ast::InstanceSymbol& inst) -> diag::Result<void> {
  const hir::InstanceMemberId member_id = AddInstanceMember(
      hir::InstanceMemberDecl{
          .instance_name = std::string{inst.name},
          .target_unit = std::string{inst.getDefinition().name},
          .array_dims = {}});
  // A downward cross-unit reference (`c.x`) resolves the leading `c` to
  // this member; the binding lets process-body lowering find it regardless
  // of source order.
  module_->MapOwnedChildBinding(
      inst, frame_, hir::DownwardHead{.child = member_id});
  return {};
}

auto ScopeLowerer::PopulateInstanceArrayMember(
    const slang::ast::InstanceArraySymbol& array) -> diag::Result<void> {
  // Each nested InstanceArray level contributes one dimension; the descent
  // bottoms out at the per-element instance, which names the target unit.
  // A zero-element level (`Child c[0:-1]`, LRM 23.3.2) has no element to
  // descend into or to name the unit from and constructs nothing, so it
  // contributes no member.
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
  const hir::InstanceMemberId member_id = AddInstanceMember(
      hir::InstanceMemberDecl{
          .instance_name = std::string{array.name},
          .target_unit = std::string{leaf.getDefinition().name},
          .array_dims = std::move(dims)});
  module_->MapOwnedChildBinding(
      array, frame_, hir::DownwardHead{.child = member_id});
  return {};
}

}  // namespace lyra::lowering::ast_to_hir
