#include "lyra/lowering/hir_to_mir/lower_constructor.hpp"

#include <cstddef>
#include <cstdint>
#include <expected>
#include <format>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/hir_to_mir/case_cascade.hpp"
#include "lyra/lowering/hir_to_mir/lower_continuous_assign.hpp"
#include "lyra/lowering/hir_to_mir/lower_expr.hpp"
#include "lyra/lowering/hir_to_mir/lower_process.hpp"
#include "lyra/lowering/hir_to_mir/procedural_scope_helpers.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/procedural_hops.hpp"
#include "lyra/mir/procedural_var.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/structural_scope.hpp"
#include "lyra/mir/structural_var.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto ChildScopeNameFor(std::size_t gen_index, std::string_view arm_tag)
    -> std::string {
  return std::format("gen{}_{}", gen_index, arm_tag);
}

auto CompanionVarNameFor(std::string_view child_scope_name) -> std::string {
  return std::string(child_scope_name) + "_obj";
}

void CheckNoNameCollision(
    const mir::StructuralScope& owner_scope, std::string_view child_scope_name,
    std::string_view companion_var_name) {
  for (const auto& v : owner_scope.structural_vars) {
    if (v.name == companion_var_name || v.name == child_scope_name) {
      throw InternalError(
          "child scope or companion var name collides with an existing "
          "structural var declaration in the enclosing scope");
    }
  }
  for (const auto& c : owner_scope.child_structural_scopes) {
    if (c.name == child_scope_name) {
      throw InternalError(
          "child scope name collides with an existing nested structural "
          "scope declaration in the enclosing scope");
    }
  }
}

struct GenerateChildSpec {
  hir::StructuralScopeId scope_id;
  const hir::StructuralScope* scope;
  std::string scope_name;
  bool is_repeated;
  std::vector<ScopeEntryStructuralParamBinding> entry_bindings;
};

auto EnumerateGenerateChildSpecs(
    const hir::Generate& gen, std::size_t gen_index,
    const hir::StructuralScope& enclosing_scope, UnitLoweringState& unit_state)
    -> std::vector<GenerateChildSpec> {
  std::vector<GenerateChildSpec> specs;
  std::visit(
      Overloaded{
          [&](const hir::IfGenerate& if_gen) {
            const auto& then_scope = gen.GetChildScope(if_gen.then_scope);
            specs.push_back(
                {.scope_id = if_gen.then_scope,
                 .scope = &then_scope,
                 .scope_name = ChildScopeNameFor(gen_index, "then"),
                 .is_repeated = false,
                 .entry_bindings = {}});
            if (if_gen.else_scope.has_value()) {
              const auto& else_scope = gen.GetChildScope(*if_gen.else_scope);
              specs.push_back(
                  {.scope_id = *if_gen.else_scope,
                   .scope = &else_scope,
                   .scope_name = ChildScopeNameFor(gen_index, "else"),
                   .is_repeated = false,
                   .entry_bindings = {}});
            }
          },
          [&](const hir::CaseGenerate& case_gen) {
            for (std::size_t k = 0; k < case_gen.items.size(); ++k) {
              const auto& item_scope =
                  gen.GetChildScope(case_gen.items[k].scope);
              specs.push_back(
                  {.scope_id = case_gen.items[k].scope,
                   .scope = &item_scope,
                   .scope_name =
                       ChildScopeNameFor(gen_index, std::format("case{}", k)),
                   .is_repeated = false,
                   .entry_bindings = {}});
            }
            if (case_gen.default_scope.has_value()) {
              const auto& default_scope =
                  gen.GetChildScope(*case_gen.default_scope);
              specs.push_back(
                  {.scope_id = *case_gen.default_scope,
                   .scope = &default_scope,
                   .scope_name = ChildScopeNameFor(gen_index, "default"),
                   .is_repeated = false,
                   .entry_bindings = {}});
            }
          },
          [&](const hir::LoopGenerate& loop_gen) {
            const auto& loop_scope = gen.GetChildScope(loop_gen.scope);
            const auto& var_decl =
                enclosing_scope.GetLoopVarDecl(loop_gen.loop_var);
            std::vector<ScopeEntryStructuralParamBinding> bindings;
            bindings.push_back(
                ScopeEntryStructuralParamBinding{
                    .param =
                        mir::StructuralParamDecl{
                            .name = var_decl.name,
                            .type = unit_state.TranslateType(var_decl.type)},
                    .source_loop_var = loop_gen.loop_var});
            specs.push_back(
                {.scope_id = loop_gen.scope,
                 .scope = &loop_scope,
                 .scope_name = ChildScopeNameFor(gen_index, "loop"),
                 .is_repeated = true,
                 .entry_bindings = std::move(bindings)});
          },
      },
      gen.data);
  return specs;
}

auto MakeOwnedObjectType(
    UnitLoweringState& unit_state, mir::StructuralScopeId child_id)
    -> mir::TypeId {
  const mir::TypeId object_type =
      unit_state.AddType(mir::ObjectType{.target = child_id});
  return unit_state.AddType(mir::OwningPtrType{.pointee = object_type});
}

auto MakeVectorOfOwnedObjectType(
    UnitLoweringState& unit_state, mir::StructuralScopeId child_id)
    -> mir::TypeId {
  const mir::TypeId owned_type = MakeOwnedObjectType(unit_state, child_id);
  return unit_state.AddType(mir::VectorType{.element = owned_type});
}

void ValidateConstructOwnedObjectStmt(
    const mir::CompilationUnit& unit, const mir::StructuralScope& owner_scope,
    const ProceduralScopeLoweringState& proc_scope_state,
    const mir::ConstructOwnedObjectStmt& stmt) {
  if (stmt.scope_id.value >= owner_scope.child_structural_scopes.size()) {
    throw InternalError(
        "ConstructOwnedObjectStmt: scope_id is not a direct child of the "
        "enclosing scope");
  }
  if (stmt.target.value >= owner_scope.structural_vars.size()) {
    throw InternalError(
        "ConstructOwnedObjectStmt: target is out of range in the enclosing "
        "scope");
  }
  const auto& var = owner_scope.GetStructuralVar(stmt.target);
  const auto target = mir::GetOwnedObjectTarget(unit, var.type);
  if (!target.has_value() || *target != stmt.scope_id) {
    throw InternalError(
        "ConstructOwnedObjectStmt: target var does not own the requested "
        "scope");
  }
  const auto& child_scope = owner_scope.GetChildStructuralScope(stmt.scope_id);
  if (stmt.args.size() != child_scope.structural_params.size()) {
    throw InternalError(
        "ConstructOwnedObjectStmt: args count does not match child scope "
        "structural params count");
  }
  for (std::size_t i = 0; i < stmt.args.size(); ++i) {
    const auto& arg = proc_scope_state.GetExpr(stmt.args[i]);
    const auto& param = child_scope.structural_params[i];
    if (arg.type != param.type) {
      throw InternalError(
          "ConstructOwnedObjectStmt: arg type does not match structural "
          "param type");
    }
  }
}

// The for-stmt body procedural scope is one level below the parent
// constructor scope where the induction var was declared, so a
// `ProceduralVarRef` to that var read from inside the body always hops up
// once. This helper documents the +1 instead of writing it inline.
auto MakeForBodyInductionVarArg(
    mir::ProceduralVarId induction_var_id, mir::TypeId type) -> mir::Expr {
  return mir::Expr{
      .data =
          mir::ProceduralVarRef{
              .hops = mir::ProceduralHops{.value = 1}, .var = induction_var_id},
      .type = type};
}

auto BuildGenerateArmBody(
    const mir::CompilationUnit& unit, const GenerateBindings& gen_bindings,
    const mir::StructuralScope& owner_scope,
    hir::StructuralScopeId arm_scope_id, std::vector<mir::Expr> args)
    -> mir::ProceduralScope {
  const auto& binding = gen_bindings.by_scope_id.at(arm_scope_id.value);

  ProceduralScopeLoweringState proc_scope_state;
  std::vector<mir::ExprId> arg_ids;
  arg_ids.reserve(args.size());
  for (auto& arg : args) {
    arg_ids.push_back(proc_scope_state.AddExpr(std::move(arg)));
  }

  const mir::ConstructOwnedObjectStmt construct_stmt{
      .target = binding.var_id,
      .scope_id = binding.scope_id,
      .args = std::move(arg_ids)};
  ValidateConstructOwnedObjectStmt(
      unit, owner_scope, proc_scope_state, construct_stmt);

  const mir::StmtId stmt_id = proc_scope_state.AddStmt(
      mir::Stmt{
          .label = std::nullopt,
          .data = construct_stmt,
          .child_procedural_scopes = {}});
  proc_scope_state.AddRootStmt(stmt_id);
  return proc_scope_state.Finish();
}

auto LowerIfGenerate(
    UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const hir::StructuralScope& enclosing_scope,
    const GenerateBindings& gen_bindings,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::IfGenerate& if_gen) -> diag::Result<mir::Stmt> {
  const mir::CompilationUnit& unit = unit_state.Unit();
  const mir::StructuralScope& owner_scope = scope_state.Scope();

  auto cond_or = LowerStructuralExpr(
      unit_state, scope_state, proc_scope_state, enclosing_scope,
      enclosing_scope.GetExpr(if_gen.condition));
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  const mir::ExprId cond_id = proc_scope_state.AddExpr(*std::move(cond_or));

  std::vector<mir::ProceduralScope> child_scopes;
  const mir::ProceduralScopeId then_id = AddChildProceduralScope(
      child_scopes,
      BuildGenerateArmBody(
          unit, gen_bindings, owner_scope, if_gen.then_scope, {}));

  std::optional<mir::ProceduralScopeId> else_id;
  if (if_gen.else_scope.has_value()) {
    else_id = AddChildProceduralScope(
        child_scopes,
        BuildGenerateArmBody(
            unit, gen_bindings, owner_scope, *if_gen.else_scope, {}));
  }

  return mir::Stmt{
      .label = std::nullopt,
      .data =
          mir::IfStmt{
              .condition = cond_id,
              .then_scope = then_id,
              .else_scope = else_id},
      .child_procedural_scopes = std::move(child_scopes)};
}

auto LowerCaseGenerate(
    UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const hir::StructuralScope& enclosing_scope,
    const GenerateBindings& gen_bindings, const hir::CaseGenerate& case_gen)
    -> diag::Result<mir::Stmt> {
  const mir::CompilationUnit& unit = unit_state.Unit();
  const mir::StructuralScope& owner_scope = scope_state.Scope();
  const mir::TypeId bit_type = unit_state.Builtins().bit1;

  ProceduralScopeLoweringState wrapper_state;

  auto cond_or = LowerStructuralExpr(
      unit_state, scope_state, wrapper_state, enclosing_scope,
      enclosing_scope.GetExpr(case_gen.condition));
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  const mir::ExprId cond_expr_id = wrapper_state.AddExpr(*std::move(cond_or));

  const CaseSnapshotRefs snapshot =
      AppendCaseSnapshot(wrapper_state, cond_expr_id);

  std::vector<mir::ProceduralScope> body_scopes;
  body_scopes.reserve(case_gen.items.size());
  for (const auto& item : case_gen.items) {
    body_scopes.push_back(
        BuildGenerateArmBody(unit, gen_bindings, owner_scope, item.scope, {}));
  }

  std::optional<mir::ProceduralScope> default_scope;
  if (case_gen.default_scope.has_value()) {
    default_scope = BuildGenerateArmBody(
        unit, gen_bindings, owner_scope, *case_gen.default_scope, {});
  }

  auto build_predicate = [&](ProceduralScopeLoweringState& enc,
                             std::size_t item_idx, std::uint32_t sel_hops) {
    return BuildEqualityChain(
        enc, snapshot, bit_type, mir::BinaryOp::kEquality, sel_hops,
        case_gen.items[item_idx].labels.size(),
        [&](ProceduralScopeLoweringState& es,
            std::size_t li) -> diag::Result<mir::ExprId> {
          auto lab_or = LowerStructuralExpr(
              unit_state, scope_state, es, enclosing_scope,
              enclosing_scope.GetExpr(case_gen.items[item_idx].labels[li]));
          if (!lab_or) {
            return std::unexpected(std::move(lab_or.error()));
          }
          return es.AddExpr(*std::move(lab_or));
        });
  };

  return BuildCaseCascade(
      std::move(wrapper_state), std::nullopt, case_gen.items.size(),
      std::move(body_scopes), std::move(default_scope), build_predicate);
}

auto LowerLoopGenerate(
    UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const hir::StructuralScope& enclosing_scope,
    const GenerateBindings& gen_bindings,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::LoopGenerate& loop) -> diag::Result<mir::Stmt> {
  const mir::CompilationUnit& unit = unit_state.Unit();
  const mir::StructuralScope& owner_scope = scope_state.Scope();

  const auto& var_decl = enclosing_scope.GetLoopVarDecl(loop.loop_var);
  const mir::TypeId genvar_type = unit_state.TranslateType(var_decl.type);

  const mir::ProceduralVarId loop_local_id = proc_scope_state.AddProceduralVar(
      mir::ProceduralVarDecl{.name = var_decl.name, .type = genvar_type});
  const mir::ProceduralVarRef loop_local{
      .hops = mir::ProceduralHops{.value = 0}, .var = loop_local_id};

  ConstructorLoweringState ctor_state;
  ctor_state.MapLoopVar(loop.loop_var, loop_local);

  auto init_or = LowerProceduralExpr(
      unit_state, scope_state, ctor_state, proc_scope_state, enclosing_scope,
      enclosing_scope.GetExpr(loop.initial));
  if (!init_or) return std::unexpected(std::move(init_or.error()));
  const mir::ExprId init_id = proc_scope_state.AddExpr(*std::move(init_or));

  auto cond_or = LowerProceduralExpr(
      unit_state, scope_state, ctor_state, proc_scope_state, enclosing_scope,
      enclosing_scope.GetExpr(loop.stop));
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  const mir::ExprId cond_id = proc_scope_state.AddExpr(*std::move(cond_or));

  // HIR carries the iter as the next-value expression for the loop variable;
  // the loop semantic (this lowering) owns the actual write back.
  auto step_value_or = LowerProceduralExpr(
      unit_state, scope_state, ctor_state, proc_scope_state, enclosing_scope,
      enclosing_scope.GetExpr(loop.iter));
  if (!step_value_or) {
    return std::unexpected(std::move(step_value_or.error()));
  }
  const mir::TypeId step_type = (*step_value_or).type;
  const mir::ExprId step_value_id =
      proc_scope_state.AddExpr(*std::move(step_value_or));
  const mir::ExprId step_target_id = proc_scope_state.AddExpr(
      mir::Expr{.data = loop_local, .type = genvar_type});
  const mir::ExprId step_id = proc_scope_state.AddExpr(
      mir::Expr{
          .data =
              mir::AssignExpr{.target = step_target_id, .value = step_value_id},
          .type = step_type});

  std::vector<mir::Expr> body_args;
  body_args.push_back(MakeForBodyInductionVarArg(loop_local_id, genvar_type));

  std::vector<mir::ProceduralScope> child_scopes;
  const mir::ProceduralScopeId loop_scope_id = AddChildProceduralScope(
      child_scopes,
      BuildGenerateArmBody(
          unit, gen_bindings, owner_scope, loop.scope, std::move(body_args)));

  return mir::Stmt{
      .label = std::nullopt,
      .data =
          mir::ForStmt{
              .init = {mir::ForInitDecl{
                  .induction_var = loop_local, .init = init_id}},
              .condition = cond_id,
              .step = {step_id},
              .scope = loop_scope_id},
      .child_procedural_scopes = std::move(child_scopes)};
}

auto LowerGenerateAsStmt(
    UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const hir::StructuralScope& enclosing_scope, const hir::Generate& gen,
    const GenerateBindings& gen_bindings,
    ProceduralScopeLoweringState& proc_scope_state) -> diag::Result<mir::Stmt> {
  return std::visit(
      Overloaded{
          [&](const hir::IfGenerate& if_gen) {
            return LowerIfGenerate(
                unit_state, scope_state, enclosing_scope, gen_bindings,
                proc_scope_state, if_gen);
          },
          [&](const hir::CaseGenerate& case_gen) {
            return LowerCaseGenerate(
                unit_state, scope_state, enclosing_scope, gen_bindings,
                case_gen);
          },
          [&](const hir::LoopGenerate& loop) {
            return LowerLoopGenerate(
                unit_state, scope_state, enclosing_scope, gen_bindings,
                proc_scope_state, loop);
          },
      },
      gen.data);
}

auto InstallGenerateOwnedChildScopes(
    UnitLoweringState& unit_state, StructuralScopeLoweringState& scope_state,
    ScopeStack& stack, const hir::StructuralScope& scope)
    -> diag::Result<std::vector<GenerateBindings>> {
  std::vector<GenerateBindings> bindings_by_generate;
  bindings_by_generate.reserve(scope.generates.size());

  for (std::size_t gen_idx = 0; gen_idx < scope.generates.size(); ++gen_idx) {
    const auto& gen = scope.generates[gen_idx];
    GenerateBindings gen_bindings;
    gen_bindings.by_scope_id.resize(gen.child_scopes.size());

    auto specs = EnumerateGenerateChildSpecs(gen, gen_idx, scope, unit_state);
    for (auto& spec : specs) {
      const auto companion_name = CompanionVarNameFor(spec.scope_name);
      CheckNoNameCollision(
          scope_state.Scope(), spec.scope_name, companion_name);

      auto child_r = LowerStructuralScope(
          unit_state, &scope_state, stack, *spec.scope,
          std::move(spec.scope_name), spec.entry_bindings);
      if (!child_r) return std::unexpected(std::move(child_r.error()));

      const mir::StructuralScopeId child_id =
          scope_state.AddChildStructuralScope(*std::move(child_r));
      const mir::TypeId var_type =
          spec.is_repeated ? MakeVectorOfOwnedObjectType(unit_state, child_id)
                           : MakeOwnedObjectType(unit_state, child_id);
      const mir::StructuralVarId var_id = scope_state.AddStructuralVar(
          mir::StructuralVarDecl{
              .name = companion_name,
              .type = var_type,
              .initializer = std::nullopt});

      gen_bindings.by_scope_id.at(spec.scope_id.value) =
          ChildStructuralScopeBinding{.scope_id = child_id, .var_id = var_id};
    }

    bindings_by_generate.push_back(std::move(gen_bindings));
  }
  return bindings_by_generate;
}

}  // namespace

auto LowerStructuralScope(
    UnitLoweringState& unit_state,
    const StructuralScopeLoweringState* parent_scope_state, ScopeStack& stack,
    const hir::StructuralScope& scope, std::string name,
    std::span<const ScopeEntryStructuralParamBinding> entry_bindings)
    -> diag::Result<mir::StructuralScope> {
  mir::StructuralScope mir_scope{
      .name = std::move(name),
      .structural_params = {},
      .structural_vars = {},
      .constructor_scope = {},
      .processes = {},
      .child_structural_scopes = {},
      .structural_subroutines = {},
      .type_aliases = {}};
  for (const auto& alias : scope.type_aliases) {
    mir_scope.type_aliases.push_back(
        mir::TypeAliasDecl{
            .name = alias.name,
            .target = unit_state.TranslateType(alias.target)});
  }
  StructuralScopeLoweringState scope_state(parent_scope_state, mir_scope);
  const ScopeStackGuard guard(stack, scope);

  for (const auto& binding : entry_bindings) {
    const mir::StructuralParamId mir_id =
        scope_state.AddStructuralParam(binding.param);
    scope_state.MapLoopVarAsStructuralParam(binding.source_loop_var, mir_id);
  }

  ProceduralScopeLoweringState ctor_scope_state;
  for (std::size_t i = 0; i < scope.structural_vars.size(); ++i) {
    const hir::StructuralVarId hir_id{static_cast<std::uint32_t>(i)};
    const auto& d = scope.structural_vars[i];
    std::optional<mir::ExprId> mir_init;
    if (d.initializer.has_value()) {
      auto init_or = LowerStructuralExpr(
          unit_state, scope_state, ctor_scope_state, scope,
          scope.GetExpr(*d.initializer));
      if (!init_or) return std::unexpected(std::move(init_or.error()));
      mir_init = ctor_scope_state.AddExpr(*std::move(init_or));
    }
    const mir::StructuralVarId mir_id = scope_state.AddStructuralVar(
        mir::StructuralVarDecl{
            .name = d.name,
            .type = unit_state.TranslateType(d.type),
            .initializer = mir_init});
    scope_state.MapStructuralVar(hir_id, mir_id);
  }

  // Map every subroutine's identity before lowering any body, so a call in one
  // body resolves a forward or mutual reference to a peer (LRM 13.7). Only the
  // HIR -> MIR id mapping has to precede the bodies; the MIR id is the index
  // each decl will occupy, and the loop below adds the lowered decls in that
  // same order.
  for (std::size_t i = 0; i < scope.structural_subroutines.size(); ++i) {
    scope_state.MapStructuralSubroutine(
        hir::StructuralSubroutineId{static_cast<std::uint32_t>(i)},
        mir::StructuralSubroutineId{static_cast<std::uint32_t>(i)});
  }
  for (std::size_t i = 0; i < scope.structural_subroutines.size(); ++i) {
    auto decl_or = LowerStructuralSubroutine(
        unit_state, scope_state, scope.structural_subroutines[i],
        scope.time_resolution);
    if (!decl_or) return std::unexpected(std::move(decl_or.error()));
    const mir::StructuralSubroutineId added =
        scope_state.AddStructuralSubroutine(*std::move(decl_or));
    if (added.value != i) {
      throw InternalError(
          "LowerStructuralScope: subroutine added out of mapped id order");
    }
  }

  for (const auto& p : scope.processes) {
    auto proc_or =
        LowerProcess(unit_state, scope_state, p, scope.time_resolution);
    if (!proc_or) return std::unexpected(std::move(proc_or.error()));
    scope_state.AddProcess(*std::move(proc_or));
  }

  for (const auto& ca : scope.continuous_assigns) {
    auto proc_or = LowerContinuousAssign(
        unit_state, scope_state, scope, ca, scope.time_resolution);
    if (!proc_or) return std::unexpected(std::move(proc_or.error()));
    scope_state.AddProcess(*std::move(proc_or));
  }

  auto bindings_r =
      InstallGenerateOwnedChildScopes(unit_state, scope_state, stack, scope);
  if (!bindings_r) return std::unexpected(std::move(bindings_r.error()));

  for (std::size_t i = 0; i < scope.generates.size(); ++i) {
    auto stmt = LowerGenerateAsStmt(
        unit_state, scope_state, scope, scope.generates[i], bindings_r->at(i),
        ctor_scope_state);
    if (!stmt) return std::unexpected(std::move(stmt.error()));
    const mir::StmtId sid = ctor_scope_state.AddStmt(*std::move(stmt));
    ctor_scope_state.AddRootStmt(sid);
  }
  scope_state.SetConstructorScope(ctor_scope_state.Finish());

  return mir_scope;
}

}  // namespace lyra::lowering::hir_to_mir
