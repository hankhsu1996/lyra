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
#include "lyra/lowering/hir_to_mir/body_helpers.hpp"
#include "lyra/lowering/hir_to_mir/delay_time_resolver.hpp"
#include "lyra/lowering/hir_to_mir/lower_expr.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/body_hops.hpp"
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
    const UnitLoweringState& unit_state, const ClassLoweringState& class_state,
    ProcessLoweringState& proc_state, BodyLoweringState& body_state,
    const hir::Process& hir_proc, const hir::Stmt& stmt)
    -> diag::Result<mir::Stmt> {
  return std::visit(
      Overloaded{
          [&](const hir::EmptyStmt&) -> diag::Result<mir::Stmt> {
            return mir::Stmt{
                .label = stmt.label,
                .data = mir::EmptyStmt{},
                .child_bodies = {}};
          },
          [&](const hir::VarDeclStmt& v) -> diag::Result<mir::Stmt> {
            const auto& hir_local = hir_proc.local_vars.at(v.local_var.value);
            const mir::TypeId type = unit_state.TranslateType(hir_local.type);
            const mir::LocalVarId local_id = body_state.AddLocal(
                mir::LocalVar{.name = hir_local.name, .type = type});
            proc_state.MapLocalVar(
                v.local_var,
                LocalBinding{
                    .declaration_body_depth = proc_state.CurrentBodyDepth(),
                    .local = local_id});
            return mir::Stmt{
                .label = stmt.label,
                .data =
                    mir::LocalVarDeclStmt{
                        .target =
                            mir::LocalVarRef{
                                .body_hops = mir::BodyHops{.value = 0},
                                .local = local_id}},
                .child_bodies = {}};
          },
          [&](const hir::ExprStmt& e) -> diag::Result<mir::Stmt> {
            auto expr_or = LowerExpr(
                unit_state, class_state, proc_state, body_state, hir_proc,
                hir_proc.exprs.at(e.expr.value));
            if (!expr_or) {
              return std::unexpected(std::move(expr_or.error()));
            }
            return mir::Stmt{
                .label = stmt.label,
                .data =
                    mir::ExprStmt{
                        .expr = body_state.AddExpr(*std::move(expr_or))},
                .child_bodies = {}};
          },
          [&](const hir::BlockStmt& b) -> diag::Result<mir::Stmt> {
            BodyLoweringState child_body_state;
            BodyDepthGuard depth_guard{proc_state};
            for (const hir::StmtId child_hir_id : b.statements) {
              const hir::Stmt& child = hir_proc.stmts.at(child_hir_id.value);
              auto lowered = LowerStmt(
                  unit_state, class_state, proc_state, child_body_state,
                  hir_proc, child);
              if (!lowered) {
                return std::unexpected(std::move(lowered.error()));
              }
              const mir::StmtId child_id =
                  child_body_state.AddStmt(*std::move(lowered));
              child_body_state.AddRootStmt(child_id);
            }
            std::vector<mir::Body> child_bodies;
            const mir::BodyId body_id =
                AddChildBody(child_bodies, child_body_state.Finish());
            return mir::Stmt{
                .label = stmt.label,
                .data = mir::BlockStmt{.body = body_id},
                .child_bodies = std::move(child_bodies)};
          },
          [&](const hir::TimedStmt& t) -> diag::Result<mir::Stmt> {
            auto timing_or =
                LowerTimingControl(proc_state, hir_proc, t.timing, stmt.span);
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
            return mir::Stmt{
                .label = stmt.label,
                .data =
                    mir::TimedStmt{
                        .timing = *std::move(timing_or), .body = body_id},
                .child_bodies = {}};
          },
      },
      stmt.data);
}

}  // namespace lyra::lowering::hir_to_mir
