#include "lyra/lowering/hir_to_mir/lower_stmt.hpp"

#include <expected>
#include <memory>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/integral_constant.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/case_cascade.hpp"
#include "lyra/lowering/hir_to_mir/delay_time_resolver.hpp"
#include "lyra/lowering/hir_to_mir/lower_deferred_check.hpp"
#include "lyra/lowering/hir_to_mir/lower_expr.hpp"
#include "lyra/lowering/hir_to_mir/procedural_scope_helpers.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/procedural_hops.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto IntegralConstantToInt64(const hir::IntegralConstant& c) -> std::int64_t {
  if (c.state_kind == hir::IntegralStateKind::kFourState) {
    throw InternalError(
        "IntegralConstantToInt64: 4-state literal in integer-delay context");
  }
  if (c.value_words.empty()) {
    return 0;
  }
  const std::uint64_t raw = c.value_words[0];
  if (c.signedness == hir::Signedness::kSigned && c.width > 0U &&
      c.width < 64U) {
    const std::uint64_t sign_bit = std::uint64_t{1} << (c.width - 1U);
    if ((raw & sign_bit) != 0U) {
      const std::uint64_t fill =
          ~((std::uint64_t{1} << c.width) - std::uint64_t{1});
      return static_cast<std::int64_t>(raw | fill);
    }
  }
  return static_cast<std::int64_t>(raw);
}

auto ResolveDelayDuration(
    const DelayTimeResolver& resolver, const hir::Expr& duration)
    -> diag::Result<SimDuration> {
  if (const auto* primary = std::get_if<hir::PrimaryExpr>(&duration.data)) {
    if (const auto* int_lit =
            std::get_if<hir::IntegerLiteral>(&primary->data)) {
      return resolver.ResolveIntegerDelay(
          IntegralConstantToInt64(int_lit->value), duration.span);
    }
    if (const auto* time_lit = std::get_if<hir::TimeLiteral>(&primary->data)) {
      return resolver.ResolveTimeLiteral(
          time_lit->value, time_lit->scale, duration.span);
    }
  }
  return diag::Unsupported(
      duration.span, diag::DiagCode::kUnsupportedDelayExpressionForm,
      "delay durations beyond an integer or time literal are not yet supported",
      diag::UnsupportedCategory::kFeature);
}

auto LowerTimingControl(
    const ProcessLoweringState& proc_state, const hir::Process& hir_proc,
    const hir::TimingControl& tc, diag::SourceSpan span)
    -> diag::Result<mir::TimingControl> {
  return std::visit(
      Overloaded{
          [&](const hir::DelayControl& d) -> diag::Result<mir::TimingControl> {
            const DelayTimeResolver resolver{proc_state.Resolution()};
            auto ticks_or = ResolveDelayDuration(
                resolver, hir_proc.exprs.at(d.duration.value));
            if (!ticks_or) {
              return std::unexpected(std::move(ticks_or.error()));
            }
            return mir::TimingControl{mir::DelayControl{.duration = *ticks_or}};
          },
          [&](const hir::EventControl&) -> diag::Result<mir::TimingControl> {
            return diag::Unsupported(
                span, diag::DiagCode::kUnsupportedTimingControlKind,
                "event-control timing is not yet supported",
                diag::UnsupportedCategory::kFeature);
          },
      },
      tc);
}

auto LowerEmptyStmt(const hir::Stmt& stmt) -> diag::Result<mir::Stmt> {
  return mir::Stmt{
      .label = stmt.label,
      .data = mir::EmptyStmt{},
      .child_procedural_scopes = {}};
}

auto LowerVarDeclStmt(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::Process& hir_proc, const hir::Stmt& stmt,
    const hir::VarDeclStmt& v) -> diag::Result<mir::Stmt> {
  const auto& hir_local = hir_proc.procedural_vars.at(v.var.value);
  const mir::TypeId type = unit_state.TranslateType(hir_local.type);
  const mir::ProceduralVarId local_id = proc_scope_state.AddProceduralVar(
      mir::ProceduralVarDecl{.name = hir_local.name, .type = type});
  proc_state.MapProceduralVar(
      v.var,
      ProceduralVarBinding{
          .declaration_procedural_depth = proc_state.CurrentProceduralDepth(),
          .var = local_id});
  std::optional<mir::ExprId> init_id;
  if (v.init.has_value()) {
    auto init_or = LowerExpr(
        unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
        hir_proc.exprs.at(v.init->value));
    if (!init_or) {
      return std::unexpected(std::move(init_or.error()));
    }
    init_id = proc_scope_state.AddExpr(*std::move(init_or));
  }
  return mir::Stmt{
      .label = stmt.label,
      .data =
          mir::ProceduralVarDeclStmt{
              .target =
                  mir::ProceduralVarRef{
                      .hops = mir::ProceduralHops{.value = 0}, .var = local_id},
              .init = init_id},
      .child_procedural_scopes = {}};
}

auto LowerExprStmt(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::Process& hir_proc, const hir::Stmt& stmt, const hir::ExprStmt& e)
    -> diag::Result<mir::Stmt> {
  auto expr_or = LowerExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
      hir_proc.exprs.at(e.expr.value));
  if (!expr_or) {
    return std::unexpected(std::move(expr_or.error()));
  }
  return mir::Stmt{
      .label = stmt.label,
      .data =
          mir::ExprStmt{.expr = proc_scope_state.AddExpr(*std::move(expr_or))},
      .child_procedural_scopes = {}};
}

auto LowerBlockStmt(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    ProcessLoweringState& proc_state, const hir::Process& hir_proc,
    const hir::Stmt& stmt, const hir::BlockStmt& b) -> diag::Result<mir::Stmt> {
  ProceduralScopeLoweringState child_proc_scope_state;
  ProceduralDepthGuard depth_guard{proc_state};
  for (const hir::StmtId child_hir_id : b.statements) {
    const hir::Stmt& child = hir_proc.stmts.at(child_hir_id.value);
    auto lowered = LowerStmt(
        unit_state, scope_state, proc_state, child_proc_scope_state, hir_proc,
        child);
    if (!lowered) {
      return std::unexpected(std::move(lowered.error()));
    }
    const mir::StmtId child_id =
        child_proc_scope_state.AddStmt(*std::move(lowered));
    child_proc_scope_state.AddRootStmt(child_id);
  }
  std::vector<mir::ProceduralScope> child_scopes;
  const mir::ProceduralScopeId scope_id =
      AddChildProceduralScope(child_scopes, child_proc_scope_state.Finish());
  return mir::Stmt{
      .label = stmt.label,
      .data = mir::BlockStmt{.scope = scope_id},
      .child_procedural_scopes = std::move(child_scopes)};
}

auto LowerIfStmt(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::Process& hir_proc, const hir::Stmt& stmt, const hir::IfStmt& i)
    -> diag::Result<mir::Stmt> {
  if (i.check.has_value()) {
    return LowerUniqueIfStmt(
        unit_state, scope_state, proc_state, hir_proc, stmt, i);
  }
  auto cond_expr_or = LowerExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
      hir_proc.exprs.at(i.condition.value));
  if (!cond_expr_or) {
    return std::unexpected(std::move(cond_expr_or.error()));
  }
  const mir::ExprId cond_id =
      proc_scope_state.AddExpr(*std::move(cond_expr_or));

  std::vector<mir::ProceduralScope> child_scopes;
  auto then_or = LowerStmtIntoChildScope(
      unit_state, scope_state, proc_state, hir_proc, i.then_stmt);
  if (!then_or) return std::unexpected(std::move(then_or.error()));
  const mir::ProceduralScopeId then_scope_id =
      AddChildProceduralScope(child_scopes, std::move(*then_or));

  std::optional<mir::ProceduralScopeId> else_scope_id;
  if (i.else_stmt.has_value()) {
    auto else_or = LowerStmtIntoChildScope(
        unit_state, scope_state, proc_state, hir_proc, *i.else_stmt);
    if (!else_or) return std::unexpected(std::move(else_or.error()));
    else_scope_id = AddChildProceduralScope(child_scopes, std::move(*else_or));
  }

  return mir::Stmt{
      .label = stmt.label,
      .data =
          mir::IfStmt{
              .condition = cond_id,
              .then_scope = then_scope_id,
              .else_scope = else_scope_id},
      .child_procedural_scopes = std::move(child_scopes)};
}

auto LowerCaseStmt(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    ProcessLoweringState& proc_state, const hir::Process& hir_proc,
    const hir::Stmt& stmt, const hir::CaseStmt& c) -> diag::Result<mir::Stmt> {
  const mir::TypeId bit_type = unit_state.Builtins().bit1;

  ProceduralScopeLoweringState wrapper_state;
  ProceduralDepthGuard wrapper_depth_guard{proc_state};

  auto cond_or = LowerExpr(
      unit_state, scope_state, proc_state, wrapper_state, hir_proc,
      hir_proc.exprs.at(c.condition.value));
  if (!cond_or) {
    return std::unexpected(std::move(cond_or.error()));
  }
  mir::ExprId cond_expr_id = wrapper_state.AddExpr(*std::move(cond_or));

  // Slang's case-context type unification may wrap the selector in a
  // ConversionExpr widening from a built-in (form=int) type to a form=explicit
  // packed type. The cpp backend cannot yet emit an assignment from a form=int
  // source into a form=explicit lvalue, and the cascade builds its own
  // equality comparisons regardless of the snapshot's form. Peel the outer
  // conversion so the snapshot var matches the unwrapped source type.
  if (const auto* cv = std::get_if<mir::ConversionExpr>(
          &wrapper_state.GetExpr(cond_expr_id).data)) {
    cond_expr_id = cv->operand;
  }

  const CaseSnapshotRefs snapshot =
      AppendCaseSnapshot(wrapper_state, cond_expr_id);

  // Each cascade level k > 0 sits one MIR scope deeper than the previous
  // level (it lives inside the prior level's else_scope). Bump proc_state by k
  // extra guards before lowering the contents of level k so procvar refs
  // receive the correct hops.
  auto with_extra_depth = [&](std::size_t extras, auto fn) {
    std::vector<std::unique_ptr<ProceduralDepthGuard>> guards;
    guards.reserve(extras);
    for (std::size_t i = 0; i < extras; ++i) {
      guards.push_back(std::make_unique<ProceduralDepthGuard>(proc_state));
    }
    return fn();
  };

  std::vector<mir::ProceduralScope> body_scopes;
  body_scopes.reserve(c.items.size());
  for (std::size_t i = 0; i < c.items.size(); ++i) {
    auto body_or = with_extra_depth(i, [&] {
      return LowerStmtIntoChildScope(
          unit_state, scope_state, proc_state, hir_proc, c.items[i].stmt);
    });
    if (!body_or) {
      return std::unexpected(std::move(body_or.error()));
    }
    body_scopes.push_back(std::move(*body_or));
  }

  std::optional<mir::ProceduralScope> default_scope;
  if (c.default_stmt.has_value()) {
    auto def_or = with_extra_depth(c.items.size(), [&] {
      return LowerStmtIntoChildScope(
          unit_state, scope_state, proc_state, hir_proc, *c.default_stmt);
    });
    if (!def_or) {
      return std::unexpected(std::move(def_or.error()));
    }
    default_scope = std::move(*def_or);
  }

  if (c.check.has_value()) {
    // All predicates live at wrapper depth (sel_hops=0); bodies were lowered
    // with the per-level depth guards above.
    std::vector<mir::ExprId> predicates;
    predicates.reserve(c.items.size());
    for (std::size_t i = 0; i < c.items.size(); ++i) {
      auto pred_or = BuildEqualityChain(
          wrapper_state, snapshot, bit_type, 0, c.items[i].labels.size(),
          [&](ProceduralScopeLoweringState& es,
              std::size_t li) -> diag::Result<mir::ExprId> {
            auto lab_or = LowerExpr(
                unit_state, scope_state, proc_state, es, hir_proc,
                hir_proc.exprs.at(c.items[i].labels[li].value));
            if (!lab_or) {
              return std::unexpected(std::move(lab_or.error()));
            }
            return es.AddExpr(*std::move(lab_or));
          });
      if (!pred_or) return std::unexpected(std::move(pred_or.error()));
      predicates.push_back(*pred_or);
    }

    std::vector<DeferredCheckBranch> branches;
    branches.reserve(c.items.size());
    for (std::size_t i = 0; i < c.items.size(); ++i) {
      branches.push_back(
          DeferredCheckBranch{
              .predicate = predicates[i], .body = std::move(body_scopes[i])});
    }
    return BuildDeferredCheckCascade(
        unit_state, std::move(wrapper_state), std::move(branches),
        std::move(default_scope), *c.check, stmt.label);
  }

  auto build_predicate = [&](ProceduralScopeLoweringState& enc,
                             std::size_t item_idx, std::uint32_t sel_hops) {
    return with_extra_depth(item_idx, [&] {
      return BuildEqualityChain(
          enc, snapshot, bit_type, sel_hops, c.items[item_idx].labels.size(),
          [&](ProceduralScopeLoweringState& es,
              std::size_t li) -> diag::Result<mir::ExprId> {
            auto lab_or = LowerExpr(
                unit_state, scope_state, proc_state, es, hir_proc,
                hir_proc.exprs.at(c.items[item_idx].labels[li].value));
            if (!lab_or) {
              return std::unexpected(std::move(lab_or.error()));
            }
            return es.AddExpr(*std::move(lab_or));
          });
    });
  };

  return BuildCaseCascade(
      std::move(wrapper_state), stmt.label, c.items.size(),
      std::move(body_scopes), std::move(default_scope), build_predicate);
}

auto LowerForStmt(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::Process& hir_proc, const hir::Stmt& stmt, const hir::ForStmt& f)
    -> diag::Result<mir::Stmt> {
  std::vector<mir::ForInit> mir_init;
  mir_init.reserve(f.init.size());
  for (const auto& hinit : f.init) {
    auto item_or = std::visit(
        Overloaded{
            [&](const hir::ForInitDecl& d) -> diag::Result<mir::ForInit> {
              const auto& hir_local = hir_proc.procedural_vars.at(d.var.value);
              const mir::TypeId type = unit_state.TranslateType(hir_local.type);
              const mir::ProceduralVarId local_id =
                  proc_scope_state.AddProceduralVar(
                      mir::ProceduralVarDecl{
                          .name = hir_local.name, .type = type});
              proc_state.MapProceduralVar(
                  d.var, ProceduralVarBinding{
                             .declaration_procedural_depth =
                                 proc_state.CurrentProceduralDepth(),
                             .var = local_id});
              std::optional<mir::ExprId> init_id;
              if (d.init.has_value()) {
                auto init_or = LowerExpr(
                    unit_state, scope_state, proc_state, proc_scope_state,
                    hir_proc, hir_proc.exprs.at(d.init->value));
                if (!init_or) {
                  return std::unexpected(std::move(init_or.error()));
                }
                init_id = proc_scope_state.AddExpr(*std::move(init_or));
              }
              return mir::ForInit{mir::ForInitDecl{
                  .induction_var =
                      mir::ProceduralVarRef{
                          .hops = mir::ProceduralHops{.value = 0},
                          .var = local_id},
                  .init = init_id}};
            },
            [&](const hir::ForInitExpr& e) -> diag::Result<mir::ForInit> {
              auto expr_or = LowerExpr(
                  unit_state, scope_state, proc_state, proc_scope_state,
                  hir_proc, hir_proc.exprs.at(e.expr.value));
              if (!expr_or) {
                return std::unexpected(std::move(expr_or.error()));
              }
              return mir::ForInit{mir::ForInitExpr{
                  .expr = proc_scope_state.AddExpr(*std::move(expr_or))}};
            },
        },
        hinit);
    if (!item_or) return std::unexpected(std::move(item_or.error()));
    mir_init.push_back(*std::move(item_or));
  }

  std::optional<mir::ExprId> cond_id;
  if (f.condition.has_value()) {
    auto cond_or = LowerExpr(
        unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
        hir_proc.exprs.at(f.condition->value));
    if (!cond_or) {
      return std::unexpected(std::move(cond_or.error()));
    }
    cond_id = proc_scope_state.AddExpr(*std::move(cond_or));
  }

  std::vector<mir::ExprId> step_ids;
  step_ids.reserve(f.step.size());
  for (const hir::ExprId step_hid : f.step) {
    auto step_or = LowerExpr(
        unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
        hir_proc.exprs.at(step_hid.value));
    if (!step_or) {
      return std::unexpected(std::move(step_or.error()));
    }
    step_ids.push_back(proc_scope_state.AddExpr(*std::move(step_or)));
  }

  auto body_or = LowerStmtIntoChildScope(
      unit_state, scope_state, proc_state, hir_proc, f.body);
  if (!body_or) {
    return std::unexpected(std::move(body_or.error()));
  }

  std::vector<mir::ProceduralScope> child_scopes;
  const mir::ProceduralScopeId body_scope_id =
      AddChildProceduralScope(child_scopes, std::move(*body_or));

  return mir::Stmt{
      .label = stmt.label,
      .data =
          mir::ForStmt{
              .init = std::move(mir_init),
              .condition = cond_id,
              .step = std::move(step_ids),
              .scope = body_scope_id},
      .child_procedural_scopes = std::move(child_scopes)};
}

auto LowerWhileStmt(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::Process& hir_proc, const hir::Stmt& stmt,
    const hir::WhileStmt& w) -> diag::Result<mir::Stmt> {
  auto cond_or = LowerExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
      hir_proc.exprs.at(w.condition.value));
  if (!cond_or) {
    return std::unexpected(std::move(cond_or.error()));
  }
  const mir::ExprId cond_id = proc_scope_state.AddExpr(*std::move(cond_or));

  auto body_or = LowerStmtIntoChildScope(
      unit_state, scope_state, proc_state, hir_proc, w.body);
  if (!body_or) {
    return std::unexpected(std::move(body_or.error()));
  }

  std::vector<mir::ProceduralScope> child_scopes;
  const mir::ProceduralScopeId body_scope_id =
      AddChildProceduralScope(child_scopes, std::move(*body_or));

  return mir::Stmt{
      .label = stmt.label,
      .data = mir::WhileStmt{.condition = cond_id, .scope = body_scope_id},
      .child_procedural_scopes = std::move(child_scopes)};
}

auto LowerRepeatStmt(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    ProcessLoweringState& proc_state, const hir::Process& hir_proc,
    const hir::Stmt& stmt, const hir::RepeatStmt& r)
    -> diag::Result<mir::Stmt> {
  const mir::TypeId int_type = unit_state.Builtins().int32;
  const mir::TypeId bit_type = unit_state.Builtins().bit1;

  ProceduralScopeLoweringState wrapper_state;
  ProceduralDepthGuard wrapper_depth_guard{proc_state};

  auto count_or = LowerExpr(
      unit_state, scope_state, proc_state, wrapper_state, hir_proc,
      hir_proc.exprs.at(r.count.value));
  if (!count_or) {
    return std::unexpected(std::move(count_or.error()));
  }
  mir::ExprId count_expr_id = wrapper_state.AddExpr(*std::move(count_or));
  if (wrapper_state.GetExpr(count_expr_id).type != int_type) {
    count_expr_id = wrapper_state.AddExpr(
        mir::Expr{
            .data =
                mir::ConversionExpr{
                    .operand = count_expr_id,
                    .kind = mir::ConversionKind::kImplicit},
            .type = int_type});
  }

  const mir::ProceduralVarId count_var = wrapper_state.AddProceduralVar(
      mir::ProceduralVarDecl{.name = "_lyra_repeat_count", .type = int_type});

  const mir::StmtId count_decl_id = wrapper_state.AddStmt(
      mir::Stmt{
          .label = std::nullopt,
          .data =
              mir::ProceduralVarDeclStmt{
                  .target =
                      mir::ProceduralVarRef{
                          .hops = mir::ProceduralHops{.value = 0},
                          .var = count_var},
                  .init = count_expr_id},
          .child_procedural_scopes = {}});
  wrapper_state.AddRootStmt(count_decl_id);

  const mir::ProceduralVarId idx_var = wrapper_state.AddProceduralVar(
      mir::ProceduralVarDecl{.name = "_lyra_repeat_index", .type = int_type});

  auto make_int32_literal = [&](std::uint64_t v) -> mir::ExprId {
    return wrapper_state.AddExpr(
        mir::Expr{
            .data =
                mir::IntegerLiteral{
                    .value =
                        mir::IntegralConstant{
                            .value_words = {v},
                            .state_words = {},
                            .width = 32,
                            .signedness = mir::Signedness::kSigned,
                            .state_kind = mir::IntegralStateKind::kTwoState}},
            .type = int_type});
  };

  const mir::ExprId zero_id = make_int32_literal(0);
  const mir::ExprId one_id = make_int32_literal(1);

  const mir::ExprId idx_ref_cond = wrapper_state.AddExpr(
      mir::Expr{
          .data =
              mir::ProceduralVarRef{
                  .hops = mir::ProceduralHops{.value = 0}, .var = idx_var},
          .type = int_type});
  const mir::ExprId count_ref_cond = wrapper_state.AddExpr(
      mir::Expr{
          .data =
              mir::ProceduralVarRef{
                  .hops = mir::ProceduralHops{.value = 0}, .var = count_var},
          .type = int_type});
  const mir::ExprId cond_id = wrapper_state.AddExpr(
      mir::Expr{
          .data =
              mir::BinaryExpr{
                  .op = mir::BinaryOp::kLessThan,
                  .lhs = idx_ref_cond,
                  .rhs = count_ref_cond},
          .type = bit_type});

  const mir::ExprId idx_ref_step = wrapper_state.AddExpr(
      mir::Expr{
          .data =
              mir::ProceduralVarRef{
                  .hops = mir::ProceduralHops{.value = 0}, .var = idx_var},
          .type = int_type});
  const mir::ExprId add_id = wrapper_state.AddExpr(
      mir::Expr{
          .data =
              mir::BinaryExpr{
                  .op = mir::BinaryOp::kAdd,
                  .lhs = idx_ref_step,
                  .rhs = one_id},
          .type = int_type});
  const mir::ExprId step_id = wrapper_state.AddExpr(
      mir::Expr{
          .data =
              mir::AssignExpr{
                  .target = mir::Lvalue{mir::ProceduralVarRef{
                      .hops = mir::ProceduralHops{.value = 0}, .var = idx_var}},
                  .value = add_id},
          .type = int_type});

  auto body_or = LowerStmtIntoChildScope(
      unit_state, scope_state, proc_state, hir_proc, r.body);
  if (!body_or) {
    return std::unexpected(std::move(body_or.error()));
  }

  std::vector<mir::ProceduralScope> for_child_scopes;
  const mir::ProceduralScopeId body_scope_id =
      AddChildProceduralScope(for_child_scopes, std::move(*body_or));

  std::vector<mir::ForInit> for_init;
  for_init.emplace_back(
      mir::ForInitDecl{
          .induction_var =
              mir::ProceduralVarRef{
                  .hops = mir::ProceduralHops{.value = 0}, .var = idx_var},
          .init = zero_id});

  const mir::StmtId for_stmt_id = wrapper_state.AddStmt(
      mir::Stmt{
          .label = std::nullopt,
          .data =
              mir::ForStmt{
                  .init = std::move(for_init),
                  .condition = cond_id,
                  .step = {step_id},
                  .scope = body_scope_id},
          .child_procedural_scopes = std::move(for_child_scopes)});
  wrapper_state.AddRootStmt(for_stmt_id);

  std::vector<mir::ProceduralScope> child_scopes;
  const mir::ProceduralScopeId wrapper_scope_id =
      AddChildProceduralScope(child_scopes, wrapper_state.Finish());

  return mir::Stmt{
      .label = stmt.label,
      .data = mir::BlockStmt{.scope = wrapper_scope_id},
      .child_procedural_scopes = std::move(child_scopes)};
}

auto LowerDoWhileStmt(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::Process& hir_proc, const hir::Stmt& stmt,
    const hir::DoWhileStmt& d) -> diag::Result<mir::Stmt> {
  auto body_or = LowerStmtIntoChildScope(
      unit_state, scope_state, proc_state, hir_proc, d.body);
  if (!body_or) {
    return std::unexpected(std::move(body_or.error()));
  }

  auto cond_or = LowerExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
      hir_proc.exprs.at(d.condition.value));
  if (!cond_or) {
    return std::unexpected(std::move(cond_or.error()));
  }
  const mir::ExprId cond_id = proc_scope_state.AddExpr(*std::move(cond_or));

  std::vector<mir::ProceduralScope> child_scopes;
  const mir::ProceduralScopeId body_scope_id =
      AddChildProceduralScope(child_scopes, std::move(*body_or));

  return mir::Stmt{
      .label = stmt.label,
      .data = mir::DoWhileStmt{.condition = cond_id, .scope = body_scope_id},
      .child_procedural_scopes = std::move(child_scopes)};
}

auto LowerForeverStmt(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    ProcessLoweringState& proc_state, const hir::Process& hir_proc,
    const hir::Stmt& stmt, const hir::ForeverStmt& f)
    -> diag::Result<mir::Stmt> {
  auto body_or = LowerStmtIntoChildScope(
      unit_state, scope_state, proc_state, hir_proc, f.body);
  if (!body_or) {
    return std::unexpected(std::move(body_or.error()));
  }

  std::vector<mir::ProceduralScope> child_scopes;
  const mir::ProceduralScopeId body_scope_id =
      AddChildProceduralScope(child_scopes, std::move(*body_or));

  return mir::Stmt{
      .label = stmt.label,
      .data =
          mir::ForStmt{
              .init = {},
              .condition = std::nullopt,
              .step = {},
              .scope = body_scope_id},
      .child_procedural_scopes = std::move(child_scopes)};
}

auto LowerBreakStmt(const hir::Stmt& stmt) -> diag::Result<mir::Stmt> {
  return mir::Stmt{
      .label = stmt.label,
      .data = mir::BreakStmt{},
      .child_procedural_scopes = {}};
}

auto LowerContinueStmt(const hir::Stmt& stmt) -> diag::Result<mir::Stmt> {
  return mir::Stmt{
      .label = stmt.label,
      .data = mir::ContinueStmt{},
      .child_procedural_scopes = {}};
}

auto LowerTimedStmt(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::Process& hir_proc, const hir::Stmt& stmt,
    const hir::TimedStmt& t) -> diag::Result<mir::Stmt> {
  auto timing_or =
      LowerTimingControl(proc_state, hir_proc, t.timing, stmt.span);
  if (!timing_or) {
    return std::unexpected(std::move(timing_or.error()));
  }
  const hir::Stmt& inner_hir = hir_proc.stmts.at(t.stmt.value);
  auto inner_or = LowerStmt(
      unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
      inner_hir);
  if (!inner_or) {
    return std::unexpected(std::move(inner_or.error()));
  }
  const mir::StmtId inner_id = proc_scope_state.AddStmt(*std::move(inner_or));
  return mir::Stmt{
      .label = stmt.label,
      .data = mir::TimedStmt{.timing = *std::move(timing_or), .stmt = inner_id},
      .child_procedural_scopes = {}};
}

}  // namespace

auto LowerStmtIntoChildScope(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    ProcessLoweringState& proc_state, const hir::Process& hir_proc,
    hir::StmtId hir_stmt_id) -> diag::Result<mir::ProceduralScope> {
  ProceduralScopeLoweringState child_state;
  ProceduralDepthGuard depth_guard{proc_state};
  const hir::Stmt& hir_stmt = hir_proc.stmts.at(hir_stmt_id.value);
  auto lowered = LowerStmt(
      unit_state, scope_state, proc_state, child_state, hir_proc, hir_stmt);
  if (!lowered) {
    return std::unexpected(std::move(lowered.error()));
  }
  const mir::StmtId stmt_id = child_state.AddStmt(*std::move(lowered));
  child_state.AddRootStmt(stmt_id);
  return child_state.Finish();
}

auto LowerStmt(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::Process& hir_proc, const hir::Stmt& stmt)
    -> diag::Result<mir::Stmt> {
  return std::visit(
      Overloaded{
          [&](const hir::EmptyStmt&) { return LowerEmptyStmt(stmt); },
          [&](const hir::VarDeclStmt& v) {
            return LowerVarDeclStmt(
                unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
                stmt, v);
          },
          [&](const hir::ExprStmt& e) {
            return LowerExprStmt(
                unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
                stmt, e);
          },
          [&](const hir::BlockStmt& b) {
            return LowerBlockStmt(
                unit_state, scope_state, proc_state, hir_proc, stmt, b);
          },
          [&](const hir::IfStmt& i) {
            return LowerIfStmt(
                unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
                stmt, i);
          },
          [&](const hir::CaseStmt& c) {
            return LowerCaseStmt(
                unit_state, scope_state, proc_state, hir_proc, stmt, c);
          },
          [&](const hir::ForStmt& f) {
            return LowerForStmt(
                unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
                stmt, f);
          },
          [&](const hir::WhileStmt& w) {
            return LowerWhileStmt(
                unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
                stmt, w);
          },
          [&](const hir::RepeatStmt& r) {
            return LowerRepeatStmt(
                unit_state, scope_state, proc_state, hir_proc, stmt, r);
          },
          [&](const hir::DoWhileStmt& d) {
            return LowerDoWhileStmt(
                unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
                stmt, d);
          },
          [&](const hir::ForeverStmt& f) {
            return LowerForeverStmt(
                unit_state, scope_state, proc_state, hir_proc, stmt, f);
          },
          [&](const hir::BreakStmt&) { return LowerBreakStmt(stmt); },
          [&](const hir::ContinueStmt&) { return LowerContinueStmt(stmt); },
          [&](const hir::TimedStmt& t) {
            return LowerTimedStmt(
                unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
                stmt, t);
          },
      },
      stmt.data);
}

}  // namespace lyra::lowering::hir_to_mir
