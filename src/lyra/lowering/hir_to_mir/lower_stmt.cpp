#include "lyra/lowering/hir_to_mir/lower_stmt.hpp"

#include <expected>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/delay_time_resolver.hpp"
#include "lyra/lowering/hir_to_mir/lower_expr.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto ResolveDelayDuration(
    const DelayTimeResolver& resolver, const hir::Expr& duration)
    -> diag::Result<SimDuration> {
  if (const auto* primary = std::get_if<hir::PrimaryExpr>(&duration.data)) {
    if (const auto* int_lit =
            std::get_if<hir::IntegerLiteral>(&primary->data)) {
      return resolver.ResolveIntegerDelay(int_lit->value, duration.span);
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
    const hir::TimingControl& tc) -> diag::Result<mir::TimingControl> {
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
      },
      tc);
}

}  // namespace

auto LowerStmt(
    const UnitLoweringState& unit_state, const ClassLoweringState& class_state,
    const ProcessLoweringState& proc_state, BodyLoweringState& body_state,
    const hir::Process& hir_proc, const hir::Stmt& stmt)
    -> diag::Result<mir::Stmt> {
  auto data = std::visit(
      Overloaded{
          [](const hir::EmptyStmt&) -> diag::Result<mir::StmtData> {
            return mir::EmptyStmt{};
          },
          [&](const hir::VarDeclStmt& v) -> diag::Result<mir::StmtData> {
            return mir::LocalVarDeclStmt{
                .target = proc_state.TranslateLocalVar(v.local_var)};
          },
          [&](const hir::ExprStmt& e) -> diag::Result<mir::StmtData> {
            auto expr_or = LowerExpr(
                unit_state, class_state, proc_state, body_state, hir_proc,
                hir_proc.exprs.at(e.expr.value));
            if (!expr_or) {
              return std::unexpected(std::move(expr_or.error()));
            }
            return mir::ExprStmt{
                .expr = body_state.AddExpr(*std::move(expr_or))};
          },
          [&](const hir::BlockStmt& b) -> diag::Result<mir::StmtData> {
            std::vector<mir::StmtId> children;
            children.reserve(b.statements.size());
            for (const hir::StmtId child_id : b.statements) {
              const hir::Stmt& child = hir_proc.stmts.at(child_id.value);
              auto lowered_child = LowerStmt(
                  unit_state, class_state, proc_state, body_state, hir_proc,
                  child);
              if (!lowered_child) {
                return std::unexpected(std::move(lowered_child.error()));
              }
              children.push_back(body_state.AddStmt(*std::move(lowered_child)));
            }
            return mir::BlockStmt{.statements = std::move(children)};
          },
          [&](const hir::TimedStmt& t) -> diag::Result<mir::StmtData> {
            auto timing_or = LowerTimingControl(proc_state, hir_proc, t.timing);
            if (!timing_or) {
              return std::unexpected(std::move(timing_or.error()));
            }
            const hir::Stmt& body_hir = hir_proc.stmts.at(t.body.value);
            auto body_or = LowerStmt(
                unit_state, class_state, proc_state, body_state, hir_proc,
                body_hir);
            if (!body_or) {
              return std::unexpected(std::move(body_or.error()));
            }
            const mir::StmtId body_id = body_state.AddStmt(*std::move(body_or));
            return mir::TimedStmt{
                .timing = *std::move(timing_or), .body = body_id};
          },
      },
      stmt.data);
  if (!data) return std::unexpected(std::move(data.error()));
  return mir::Stmt{
      .label = stmt.label, .data = *std::move(data), .child_bodies = {}};
}

}  // namespace lyra::lowering::hir_to_mir
