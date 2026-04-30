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
};

auto EnumerateGenerateChildSpecs(
    const hir::Generate& gen, std::size_t gen_index)
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
                 .is_repeated = false});
            if (if_gen.else_scope.has_value()) {
              const auto& else_scope = gen.GetChildScope(*if_gen.else_scope);
              specs.push_back(
                  {.scope_id = *if_gen.else_scope,
                   .scope = &else_scope,
                   .scope_name = ChildScopeNameFor(gen_index, "else"),
                   .is_repeated = false});
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
                   .is_repeated = false});
            }
            if (case_gen.default_scope.has_value()) {
              const auto& default_scope =
                  gen.GetChildScope(*case_gen.default_scope);
              specs.push_back(
                  {.scope_id = *case_gen.default_scope,
                   .scope = &default_scope,
                   .scope_name = ChildScopeNameFor(gen_index, "default"),
                   .is_repeated = false});
            }
          },
          [&](const hir::LoopGenerate& loop_gen) {
            const auto& loop_scope = gen.GetChildScope(loop_gen.scope);
            specs.push_back(
                {.scope_id = loop_gen.scope,
                 .scope = &loop_scope,
                 .scope_name = ChildScopeNameFor(gen_index, "loop"),
                 .is_repeated = true});
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
}

auto BuildGenerateArmBody(
    const mir::CompilationUnit& unit, const GenerateBindings& gen_bindings,
    const mir::StructuralScope& owner_scope,
    hir::StructuralScopeId arm_scope_id) -> mir::ProceduralScope {
  const auto& binding = gen_bindings.by_scope_id.at(arm_scope_id.value);
  const mir::ConstructOwnedObjectStmt construct_stmt{
      .target = binding.var_id, .scope_id = binding.scope_id};
  ValidateConstructOwnedObjectStmt(unit, owner_scope, construct_stmt);

  ProceduralScopeLoweringState proc_scope_state;
  const mir::StmtId stmt_id = proc_scope_state.AddStmt(
      mir::Stmt{
          .label = std::nullopt,
          .data = construct_stmt,
          .child_procedural_scopes = {}});
  proc_scope_state.AddRootStmt(stmt_id);
  return proc_scope_state.Finish();
}

auto LowerGenerateAsStmt(
    UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const hir::StructuralScope& enclosing_scope, const hir::Generate& gen,
    const GenerateBindings& gen_bindings,
    ProceduralScopeLoweringState& proc_scope_state) -> diag::Result<mir::Stmt> {
  const mir::CompilationUnit& unit = unit_state.Unit();
  const mir::StructuralScope& owner_scope = scope_state.Scope();
  return std::visit(
      Overloaded{
          [&](const hir::IfGenerate& if_gen) -> diag::Result<mir::Stmt> {
            const ConstructorLoweringState ctor_state;
            auto cond_or = LowerExpr(
                unit_state, scope_state, ctor_state, proc_scope_state,
                enclosing_scope, enclosing_scope.GetExpr(if_gen.condition));
            if (!cond_or) return std::unexpected(std::move(cond_or.error()));
            const mir::ExprId cond_id =
                proc_scope_state.AddExpr(*std::move(cond_or));

            std::vector<mir::ProceduralScope> child_scopes;
            const mir::ProceduralScopeId then_id = AddChildProceduralScope(
                child_scopes,
                BuildGenerateArmBody(
                    unit, gen_bindings, owner_scope, if_gen.then_scope));

            std::optional<mir::ProceduralScopeId> else_id;
            if (if_gen.else_scope.has_value()) {
              else_id = AddChildProceduralScope(
                  child_scopes,
                  BuildGenerateArmBody(
                      unit, gen_bindings, owner_scope, *if_gen.else_scope));
            }

            return mir::Stmt{
                .label = std::nullopt,
                .data =
                    mir::IfStmt{
                        .condition = cond_id,
                        .then_scope = then_id,
                        .else_scope = else_id},
                .child_procedural_scopes = std::move(child_scopes)};
          },
          [&](const hir::CaseGenerate& case_gen) -> diag::Result<mir::Stmt> {
            const ConstructorLoweringState ctor_state;
            auto cond_or = LowerExpr(
                unit_state, scope_state, ctor_state, proc_scope_state,
                enclosing_scope, enclosing_scope.GetExpr(case_gen.condition));
            if (!cond_or) return std::unexpected(std::move(cond_or.error()));
            const mir::ExprId cond_id =
                proc_scope_state.AddExpr(*std::move(cond_or));

            std::vector<mir::ProceduralScope> child_scopes;
            std::vector<mir::SwitchCase> cases;
            cases.reserve(case_gen.items.size());

            for (const auto& item : case_gen.items) {
              std::vector<mir::ExprId> labels;
              labels.reserve(item.labels.size());
              for (const hir::ExprId label_hir_id : item.labels) {
                auto label_or = LowerExpr(
                    unit_state, scope_state, ctor_state, proc_scope_state,
                    enclosing_scope, enclosing_scope.GetExpr(label_hir_id));
                if (!label_or) {
                  return std::unexpected(std::move(label_or.error()));
                }
                labels.push_back(
                    proc_scope_state.AddExpr(*std::move(label_or)));
              }
              const mir::ProceduralScopeId item_scope = AddChildProceduralScope(
                  child_scopes,
                  BuildGenerateArmBody(
                      unit, gen_bindings, owner_scope, item.scope));
              cases.push_back(
                  mir::SwitchCase{
                      .labels = std::move(labels), .scope = item_scope});
            }

            std::optional<mir::ProceduralScopeId> default_scope;
            if (case_gen.default_scope.has_value()) {
              default_scope = AddChildProceduralScope(
                  child_scopes, BuildGenerateArmBody(
                                    unit, gen_bindings, owner_scope,
                                    *case_gen.default_scope));
            }

            return mir::Stmt{
                .label = std::nullopt,
                .data =
                    mir::SwitchStmt{
                        .condition = cond_id,
                        .cases = std::move(cases),
                        .default_scope = default_scope},
                .child_procedural_scopes = std::move(child_scopes)};
          },
          [&](const hir::LoopGenerate& loop) -> diag::Result<mir::Stmt> {
            const auto& var_decl =
                enclosing_scope.GetLoopVarDecl(loop.loop_var);
            const mir::TypeId genvar_type =
                unit_state.TranslateType(var_decl.type);

            const mir::ProceduralVarId loop_local_id =
                proc_scope_state.AddProceduralVar(
                    mir::ProceduralVarDecl{
                        .name = var_decl.name, .type = genvar_type});
            const mir::ProceduralVarRef loop_local{
                .hops = mir::ProceduralHops{.value = 0}, .var = loop_local_id};

            ConstructorLoweringState ctor_state;
            ctor_state.MapLoopVar(loop.loop_var, loop_local);

            auto init_or = LowerExpr(
                unit_state, scope_state, ctor_state, proc_scope_state,
                enclosing_scope, enclosing_scope.GetExpr(loop.initial));
            if (!init_or) return std::unexpected(std::move(init_or.error()));
            const mir::ExprId init_id =
                proc_scope_state.AddExpr(*std::move(init_or));

            auto cond_or = LowerExpr(
                unit_state, scope_state, ctor_state, proc_scope_state,
                enclosing_scope, enclosing_scope.GetExpr(loop.stop));
            if (!cond_or) return std::unexpected(std::move(cond_or.error()));
            const mir::ExprId cond_id =
                proc_scope_state.AddExpr(*std::move(cond_or));

            auto step_or = LowerExpr(
                unit_state, scope_state, ctor_state, proc_scope_state,
                enclosing_scope, enclosing_scope.GetExpr(loop.iter));
            if (!step_or) return std::unexpected(std::move(step_or.error()));
            const mir::ExprId step_id =
                proc_scope_state.AddExpr(*std::move(step_or));

            std::vector<mir::ProceduralScope> child_scopes;
            const mir::ProceduralScopeId loop_scope_id =
                AddChildProceduralScope(
                    child_scopes,
                    BuildGenerateArmBody(
                        unit, gen_bindings, owner_scope, loop.scope));

            return mir::Stmt{
                .label = std::nullopt,
                .data =
                    mir::ForStmt{
                        .init = {mir::ForInitDecl{
                            .local = loop_local, .init = init_id}},
                        .condition = cond_id,
                        .step = {step_id},
                        .scope = loop_scope_id},
                .child_procedural_scopes = std::move(child_scopes)};
          },
      },
      gen.data);
}

auto LowerGenerateConstruction(
    UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const hir::StructuralScope& scope,
    const std::vector<GenerateBindings>& bindings_by_generate)
    -> diag::Result<mir::ProceduralScope> {
  ProceduralScopeLoweringState proc_scope_state;
  for (std::size_t i = 0; i < scope.generates.size(); ++i) {
    const auto& gen = scope.generates[i];
    const auto& gen_bindings = bindings_by_generate.at(i);
    auto stmt = LowerGenerateAsStmt(
        unit_state, scope_state, scope, gen, gen_bindings, proc_scope_state);
    if (!stmt) return std::unexpected(std::move(stmt.error()));
    const mir::StmtId sid = proc_scope_state.AddStmt(*std::move(stmt));
    proc_scope_state.AddRootStmt(sid);
  }
  return proc_scope_state.Finish();
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

    auto specs = EnumerateGenerateChildSpecs(gen, gen_idx);
    for (auto& spec : specs) {
      const auto companion_name = CompanionVarNameFor(spec.scope_name);
      CheckNoNameCollision(
          scope_state.Scope(), spec.scope_name, companion_name);

      auto child_r = LowerStructuralScope(
          unit_state, &scope_state, stack, *spec.scope,
          std::move(spec.scope_name));
      if (!child_r) return std::unexpected(std::move(child_r.error()));

      const mir::StructuralScopeId child_id =
          scope_state.AddChildStructuralScope(*std::move(child_r));
      const mir::TypeId var_type =
          spec.is_repeated ? MakeVectorOfOwnedObjectType(unit_state, child_id)
                           : MakeOwnedObjectType(unit_state, child_id);
      const mir::StructuralVarId var_id = scope_state.AddStructuralVar(
          mir::StructuralVarDecl{.name = companion_name, .type = var_type});

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
    const hir::StructuralScope& scope, std::string name)
    -> diag::Result<mir::StructuralScope> {
  mir::StructuralScope mir_scope{
      .name = std::move(name),
      .structural_vars = {},
      .constructor_scope = {},
      .processes = {},
      .child_structural_scopes = {},
      .structural_subroutines = {}};
  StructuralScopeLoweringState scope_state(parent_scope_state, mir_scope);
  const ScopeStackGuard guard(stack, scope);

  for (std::size_t i = 0; i < scope.structural_vars.size(); ++i) {
    const hir::StructuralVarId hir_id{static_cast<std::uint32_t>(i)};
    const auto& d = scope.structural_vars[i];
    const mir::StructuralVarId mir_id = scope_state.AddStructuralVar(
        mir::StructuralVarDecl{
            .name = d.name, .type = unit_state.TranslateType(d.type)});
    scope_state.MapStructuralVar(hir_id, mir_id);
  }

  for (std::size_t i = 0; i < scope.structural_subroutines.size(); ++i) {
    const hir::StructuralSubroutineId hir_id{static_cast<std::uint32_t>(i)};
    const auto& decl = scope.structural_subroutines[i];
    const mir::StructuralSubroutineId mir_id =
        scope_state.AddStructuralSubroutine(
            mir::StructuralSubroutineDecl{.name = decl.name});
    scope_state.MapStructuralSubroutine(hir_id, mir_id);
  }

  for (const auto& p : scope.processes) {
    auto proc_or =
        LowerProcess(unit_state, scope_state, p, scope.time_resolution);
    if (!proc_or) return std::unexpected(std::move(proc_or.error()));
    scope_state.AddProcess(*std::move(proc_or));
  }

  auto bindings_r =
      InstallGenerateOwnedChildScopes(unit_state, scope_state, stack, scope);
  if (!bindings_r) return std::unexpected(std::move(bindings_r.error()));

  auto ctor_r =
      LowerGenerateConstruction(unit_state, scope_state, scope, *bindings_r);
  if (!ctor_r) return std::unexpected(std::move(ctor_r.error()));
  scope_state.SetConstructorScope(*std::move(ctor_r));

  return mir_scope;
}

}  // namespace lyra::lowering::hir_to_mir
