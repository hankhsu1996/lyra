#include "lyra/lowering/hir_to_mir/lower_stmt.hpp"

#include <expected>
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
#include "lyra/lowering/hir_to_mir/delay_time_resolver.hpp"
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

}  // namespace

auto LowerStmt(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::Process& hir_proc, const hir::Stmt& stmt)
    -> diag::Result<mir::Stmt> {
  return std::visit(
      Overloaded{
          [&](const hir::EmptyStmt&) -> diag::Result<mir::Stmt> {
            return mir::Stmt{
                .label = stmt.label,
                .data = mir::EmptyStmt{},
                .child_procedural_scopes = {}};
          },
          [&](const hir::VarDeclStmt& v) -> diag::Result<mir::Stmt> {
            const auto& hir_local = hir_proc.procedural_vars.at(v.var.value);
            const mir::TypeId type = unit_state.TranslateType(hir_local.type);
            const mir::ProceduralVarId local_id =
                proc_scope_state.AddProceduralVar(
                    mir::ProceduralVarDecl{
                        .name = hir_local.name, .type = type});
            proc_state.MapProceduralVar(
                v.var, ProceduralVarBinding{
                           .declaration_procedural_depth =
                               proc_state.CurrentProceduralDepth(),
                           .var = local_id});
            std::optional<mir::ExprId> init_id;
            if (v.init.has_value()) {
              auto init_or = LowerExpr(
                  unit_state, scope_state, proc_state, proc_scope_state,
                  hir_proc, hir_proc.exprs.at(v.init->value));
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
                                .hops = mir::ProceduralHops{.value = 0},
                                .var = local_id},
                        .init = init_id},
                .child_procedural_scopes = {}};
          },
          [&](const hir::ExprStmt& e) -> diag::Result<mir::Stmt> {
            auto expr_or = LowerExpr(
                unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
                hir_proc.exprs.at(e.expr.value));
            if (!expr_or) {
              return std::unexpected(std::move(expr_or.error()));
            }
            return mir::Stmt{
                .label = stmt.label,
                .data =
                    mir::ExprStmt{
                        .expr = proc_scope_state.AddExpr(*std::move(expr_or))},
                .child_procedural_scopes = {}};
          },
          [&](const hir::BlockStmt& b) -> diag::Result<mir::Stmt> {
            ProceduralScopeLoweringState child_proc_scope_state;
            ProceduralDepthGuard depth_guard{proc_state};
            for (const hir::StmtId child_hir_id : b.statements) {
              const hir::Stmt& child = hir_proc.stmts.at(child_hir_id.value);
              auto lowered = LowerStmt(
                  unit_state, scope_state, proc_state, child_proc_scope_state,
                  hir_proc, child);
              if (!lowered) {
                return std::unexpected(std::move(lowered.error()));
              }
              const mir::StmtId child_id =
                  child_proc_scope_state.AddStmt(*std::move(lowered));
              child_proc_scope_state.AddRootStmt(child_id);
            }
            std::vector<mir::ProceduralScope> child_scopes;
            const mir::ProceduralScopeId scope_id = AddChildProceduralScope(
                child_scopes, child_proc_scope_state.Finish());
            return mir::Stmt{
                .label = stmt.label,
                .data = mir::BlockStmt{.scope = scope_id},
                .child_procedural_scopes = std::move(child_scopes)};
          },
          [&](const hir::ForStmt& f) -> diag::Result<mir::Stmt> {
            std::vector<mir::ForInit> mir_init;
            mir_init.reserve(f.init.size());
            for (const auto& hinit : f.init) {
              auto item_or = std::visit(
                  Overloaded{
                      [&](const hir::ForInitDecl& d)
                          -> diag::Result<mir::ForInit> {
                        const auto& hir_local =
                            hir_proc.procedural_vars.at(d.var.value);
                        const mir::TypeId type =
                            unit_state.TranslateType(hir_local.type);
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
                              unit_state, scope_state, proc_state,
                              proc_scope_state, hir_proc,
                              hir_proc.exprs.at(d.init->value));
                          if (!init_or) {
                            return std::unexpected(std::move(init_or.error()));
                          }
                          init_id =
                              proc_scope_state.AddExpr(*std::move(init_or));
                        }
                        return mir::ForInit{mir::ForInitDecl{
                            .induction_var =
                                mir::ProceduralVarRef{
                                    .hops = mir::ProceduralHops{.value = 0},
                                    .var = local_id},
                            .init = init_id}};
                      },
                      [&](const hir::ForInitExpr& e)
                          -> diag::Result<mir::ForInit> {
                        auto expr_or = LowerExpr(
                            unit_state, scope_state, proc_state,
                            proc_scope_state, hir_proc,
                            hir_proc.exprs.at(e.expr.value));
                        if (!expr_or) {
                          return std::unexpected(std::move(expr_or.error()));
                        }
                        return mir::ForInit{mir::ForInitExpr{
                            .expr =
                                proc_scope_state.AddExpr(*std::move(expr_or))}};
                      },
                  },
                  hinit);
              if (!item_or) return std::unexpected(std::move(item_or.error()));
              mir_init.push_back(*std::move(item_or));
            }

            std::optional<mir::ExprId> cond_id;
            if (f.condition.has_value()) {
              auto cond_or = LowerExpr(
                  unit_state, scope_state, proc_state, proc_scope_state,
                  hir_proc, hir_proc.exprs.at(f.condition->value));
              if (!cond_or) {
                return std::unexpected(std::move(cond_or.error()));
              }
              cond_id = proc_scope_state.AddExpr(*std::move(cond_or));
            }

            std::vector<mir::ExprId> step_ids;
            step_ids.reserve(f.step.size());
            for (const hir::ExprId step_hid : f.step) {
              auto step_or = LowerExpr(
                  unit_state, scope_state, proc_state, proc_scope_state,
                  hir_proc, hir_proc.exprs.at(step_hid.value));
              if (!step_or) {
                return std::unexpected(std::move(step_or.error()));
              }
              step_ids.push_back(proc_scope_state.AddExpr(*std::move(step_or)));
            }

            ProceduralScopeLoweringState body_state;
            ProceduralDepthGuard depth_guard{proc_state};
            const hir::Stmt& body_hir = hir_proc.stmts.at(f.body.value);
            auto body_or = LowerStmt(
                unit_state, scope_state, proc_state, body_state, hir_proc,
                body_hir);
            if (!body_or) {
              return std::unexpected(std::move(body_or.error()));
            }
            const mir::StmtId body_id = body_state.AddStmt(*std::move(body_or));
            body_state.AddRootStmt(body_id);

            std::vector<mir::ProceduralScope> child_scopes;
            const mir::ProceduralScopeId body_scope_id =
                AddChildProceduralScope(child_scopes, body_state.Finish());

            return mir::Stmt{
                .label = stmt.label,
                .data =
                    mir::ForStmt{
                        .init = std::move(mir_init),
                        .condition = cond_id,
                        .step = std::move(step_ids),
                        .scope = body_scope_id},
                .child_procedural_scopes = std::move(child_scopes)};
          },
          [&](const hir::CaseStmt& c) -> diag::Result<mir::Stmt> {
            auto cond_or = LowerExpr(
                unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
                hir_proc.exprs.at(c.condition.value));
            if (!cond_or) {
              return std::unexpected(std::move(cond_or.error()));
            }
            const mir::ExprId cond_id =
                proc_scope_state.AddExpr(*std::move(cond_or));

            auto lower_branch = [&](hir::StmtId branch_hir_id)
                -> diag::Result<mir::ProceduralScope> {
              ProceduralScopeLoweringState branch_state;
              ProceduralDepthGuard depth_guard{proc_state};
              const hir::Stmt& branch_hir =
                  hir_proc.stmts.at(branch_hir_id.value);
              auto branch_or = LowerStmt(
                  unit_state, scope_state, proc_state, branch_state, hir_proc,
                  branch_hir);
              if (!branch_or) {
                return std::unexpected(std::move(branch_or.error()));
              }
              const mir::StmtId stmt_id =
                  branch_state.AddStmt(*std::move(branch_or));
              branch_state.AddRootStmt(stmt_id);
              return branch_state.Finish();
            };

            std::vector<mir::ProceduralScope> child_scopes;
            std::vector<mir::SwitchCase> cases;
            cases.reserve(c.items.size());
            for (const auto& item : c.items) {
              std::vector<mir::ExprId> labels;
              labels.reserve(item.labels.size());
              for (const hir::ExprId label_hid : item.labels) {
                auto label_or = LowerExpr(
                    unit_state, scope_state, proc_state, proc_scope_state,
                    hir_proc, hir_proc.exprs.at(label_hid.value));
                if (!label_or) {
                  return std::unexpected(std::move(label_or.error()));
                }
                labels.push_back(
                    proc_scope_state.AddExpr(*std::move(label_or)));
              }
              auto branch_or = lower_branch(item.stmt);
              if (!branch_or) {
                return std::unexpected(std::move(branch_or.error()));
              }
              const mir::ProceduralScopeId item_scope_id =
                  AddChildProceduralScope(child_scopes, std::move(*branch_or));
              cases.push_back(
                  mir::SwitchCase{
                      .labels = std::move(labels), .scope = item_scope_id});
            }

            std::optional<mir::ProceduralScopeId> default_scope_id;
            if (c.default_stmt.has_value()) {
              auto default_or = lower_branch(*c.default_stmt);
              if (!default_or) {
                return std::unexpected(std::move(default_or.error()));
              }
              default_scope_id =
                  AddChildProceduralScope(child_scopes, std::move(*default_or));
            }

            return mir::Stmt{
                .label = stmt.label,
                .data =
                    mir::SwitchStmt{
                        .condition = cond_id,
                        .cases = std::move(cases),
                        .default_scope = default_scope_id},
                .child_procedural_scopes = std::move(child_scopes)};
          },
          [&](const hir::IfStmt& i) -> diag::Result<mir::Stmt> {
            auto cond_expr_or = LowerExpr(
                unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
                hir_proc.exprs.at(i.condition.value));
            if (!cond_expr_or) {
              return std::unexpected(std::move(cond_expr_or.error()));
            }
            const mir::ExprId cond_id =
                proc_scope_state.AddExpr(*std::move(cond_expr_or));

            auto lower_branch = [&](hir::StmtId branch_hir_id)
                -> diag::Result<std::pair<mir::ProceduralScope, mir::StmtId>> {
              ProceduralScopeLoweringState branch_state;
              ProceduralDepthGuard depth_guard{proc_state};
              const hir::Stmt& branch_hir =
                  hir_proc.stmts.at(branch_hir_id.value);
              auto branch_or = LowerStmt(
                  unit_state, scope_state, proc_state, branch_state, hir_proc,
                  branch_hir);
              if (!branch_or) {
                return std::unexpected(std::move(branch_or.error()));
              }
              const mir::StmtId stmt_id =
                  branch_state.AddStmt(*std::move(branch_or));
              branch_state.AddRootStmt(stmt_id);
              return std::pair{branch_state.Finish(), stmt_id};
            };

            std::vector<mir::ProceduralScope> child_scopes;
            auto then_or = lower_branch(i.then_stmt);
            if (!then_or) return std::unexpected(std::move(then_or.error()));
            const mir::ProceduralScopeId then_scope_id =
                AddChildProceduralScope(
                    child_scopes, std::move(then_or->first));

            std::optional<mir::ProceduralScopeId> else_scope_id;
            if (i.else_stmt.has_value()) {
              auto else_or = lower_branch(*i.else_stmt);
              if (!else_or) return std::unexpected(std::move(else_or.error()));
              else_scope_id = AddChildProceduralScope(
                  child_scopes, std::move(else_or->first));
            }

            return mir::Stmt{
                .label = stmt.label,
                .data =
                    mir::IfStmt{
                        .condition = cond_id,
                        .then_scope = then_scope_id,
                        .else_scope = else_scope_id},
                .child_procedural_scopes = std::move(child_scopes)};
          },
          [&](const hir::TimedStmt& t) -> diag::Result<mir::Stmt> {
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
            const mir::StmtId inner_id =
                proc_scope_state.AddStmt(*std::move(inner_or));
            return mir::Stmt{
                .label = stmt.label,
                .data =
                    mir::TimedStmt{
                        .timing = *std::move(timing_or), .stmt = inner_id},
                .child_procedural_scopes = {}};
          },
      },
      stmt.data);
}

}  // namespace lyra::lowering::hir_to_mir
