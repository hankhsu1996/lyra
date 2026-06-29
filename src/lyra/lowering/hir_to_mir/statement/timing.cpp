#include "lyra/lowering/hir_to_mir/statement/timing.hpp"

#include <cstdint>
#include <expected>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/integral_constant.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/delay_time_resolver.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/sensitivity_wait.hpp"
#include "lyra/lowering/hir_to_mir/services_call.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/builtin_fn.hpp"

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
  return diag::Fail(
      duration.span, diag::DiagCode::kUnsupportedDelayExpressionForm,
      "delay durations beyond an integer or time literal are not yet "
      "supported");
}

auto ResolveDelayTicks(ProcessLowerer& process, const hir::DelayControl& d)
    -> diag::Result<SimDuration> {
  const DelayTimeResolver resolver{process.Resolution()};
  return ResolveDelayDuration(
      resolver, process.HirBody().exprs.Get(d.duration));
}

// LRM 9.4.2.2 `@*`-shaped timed statement: a fresh child scope holding a
// prepended wait / control statement followed by the lowered body. The four
// timing forms differ only in that control statement; `build_wait` produces it
// and may lower sub-expressions into the child block (the named-event form
// awaits a lowered event expression).
auto LowerTimedWaitWrapper(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    hir::StmtId inner_stmt, auto build_wait) -> diag::Result<mir::Stmt> {
  mir::Block child_block;
  const WalkFrame child_frame = frame.WithBlock(&child_block);
  auto wait_or = build_wait(child_block, child_frame);
  if (!wait_or) return std::unexpected(std::move(wait_or.error()));
  child_block.AppendStmt(*std::move(wait_or));
  const hir::Stmt& inner_hir = process.HirBody().stmts.Get(inner_stmt);
  auto inner_or = process.LowerStmt(inner_hir, child_frame);
  if (!inner_or) return std::unexpected(std::move(inner_or.error()));
  child_block.AppendStmt(*std::move(inner_or));
  const mir::BlockId scope_id =
      frame.current_block->child_scopes.Add(std::move(child_block));
  return mir::Stmt{
      .label = std::move(label), .data = mir::BlockStmt{.scope = scope_id}};
}

// LRM 15.5.2 `@e body;`: a suspending named-event await in place of the `@*`
// SensitivityWaitStmt.
auto LowerNamedEventTimedStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::TimedStmt& t, const hir::NamedEventControl& nec)
    -> diag::Result<mir::Stmt> {
  return LowerTimedWaitWrapper(
      process, frame, std::move(label), t.stmt,
      [&](mir::Block& child_block,
          WalkFrame child_frame) -> diag::Result<mir::Stmt> {
        auto receiver_or = process.LowerExpr(
            process.HirBody().exprs.Get(nec.event), child_frame);
        if (!receiver_or) {
          return std::unexpected(std::move(receiver_or.error()));
        }
        const mir::ExprId receiver_id =
            child_block.exprs.Add(*std::move(receiver_or));
        mir::Expr await_call{
            .data =
                mir::CallExpr{
                    .callee = mir::Direct{.target = support::BuiltinFn::kAwait},
                    .arguments = {receiver_id},
                },
            .type = process.Module().Unit().builtins.void_type};
        const mir::ExprId await_id =
            child_block.exprs.Add(std::move(await_call));
        const mir::ExprId await_expr_id = child_block.exprs.Add(
            mir::Expr{
                .data = mir::AwaitExpr{.awaitable = await_id},
                .type = process.Module().Unit().builtins.void_type});
        return mir::Stmt{
            .label = std::nullopt,
            .data = mir::ExprStmt{.expr = await_expr_id}};
      });
}

// LRM 9.4.2 `@(...) body`. Single-leaf trigger expressions lower directly to
// a SensitivityWaitStmt over the union of leaves; multi-leaf forms require a
// snapshot wrapper not yet implemented.
auto LowerEventTimedStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    diag::SourceSpan span, const hir::TimedStmt& t, const hir::EventControl& ec)
    -> diag::Result<mir::Stmt> {
  return LowerTimedWaitWrapper(
      process, frame, std::move(label), t.stmt,
      [&](mir::Block& child_block,
          WalkFrame child_frame) -> diag::Result<mir::Stmt> {
        std::vector<hir::SensitivityEntry> union_reads;
        union_reads.reserve(ec.triggers.size());
        for (const auto& trigger : ec.triggers) {
          if (trigger.sensitivity_list.size() > 1) {
            return diag::Fail(
                span, diag::DiagCode::kUnsupportedEventTriggerForm,
                "compound event expressions (concatenation, arithmetic, "
                "dynamic index) are not yet supported");
          }
          for (auto leaf : trigger.sensitivity_list) {
            // LRM 9.4.2 LSB-reduce: an edge event monitors only the LSB of
            // the expression. A whole-signal footprint already reduces to the
            // LSB at the runtime trigger, so only a bit-addressed footprint
            // needs collapsing.
            if (leaf.edge_kind != hir::EventEdge::kAnyChange &&
                leaf.footprint.has_value()) {
              leaf.footprint = {{leaf.footprint->first, leaf.footprint->first}};
            }
            union_reads.push_back(leaf);
          }
        }
        return MakeSensitivityWaitStmt(
            child_block, child_frame, process.EnclosingScopeLowerer(),
            union_reads);
      });
}

// LRM 9.4.2.2 `@* body`.
auto LowerImplicitEventTimedStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::TimedStmt& t, const hir::ImplicitEventControl& ie)
    -> diag::Result<mir::Stmt> {
  return LowerTimedWaitWrapper(
      process, frame, std::move(label), t.stmt,
      [&](mir::Block& child_block,
          WalkFrame child_frame) -> diag::Result<mir::Stmt> {
        return MakeSensitivityWaitStmt(
            child_block, child_frame, process.EnclosingScopeLowerer(),
            ie.sensitivity_list);
      });
}

// LRM 9.4.1 `#N body`. The wait lowers to a coroutine-suspending free-function
// call whose argument vector states services, the literal tick count in the
// enclosing scope's precision, and that scope's precision power (LRM 3.14.2);
// the runtime scales from precision to the design-global tick (LRM 3.14.3).
auto LowerDelayTimedStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::TimedStmt& t, const hir::DelayControl& d)
    -> diag::Result<mir::Stmt> {
  return LowerTimedWaitWrapper(
      process, frame, std::move(label), t.stmt,
      [&](mir::Block& child_block,
          WalkFrame child_frame) -> diag::Result<mir::Stmt> {
        auto ticks_or = ResolveDelayTicks(process, d);
        if (!ticks_or) return std::unexpected(std::move(ticks_or.error()));
        const auto& builtins = process.Module().Unit().builtins;
        const mir::ExprId services_id = child_block.exprs.Add(
            BuildServicesCallExpr(process.Module(), child_frame));
        const mir::ExprId duration_id = child_block.exprs.Add(
            mir::MakeInt32Literal(
                builtins.int32, static_cast<std::int64_t>(*ticks_or)));
        const mir::ExprId precision_id = child_block.exprs.Add(
            mir::MakeInt32Literal(
                builtins.int32, static_cast<std::int64_t>(
                                    process.Resolution().precision_power)));
        const mir::ExprId call_id = child_block.exprs.Add(
            mir::Expr{
                .data =
                    mir::CallExpr{
                        .callee =
                            mir::Direct{.target = support::BuiltinFn::kDelay},
                        .arguments = {services_id, duration_id, precision_id}},
                .type = builtins.void_type});
        const mir::ExprId await_expr_id = child_block.exprs.Add(
            mir::Expr{
                .data = mir::AwaitExpr{.awaitable = call_id},
                .type = builtins.void_type});
        return mir::Stmt{
            .label = std::nullopt,
            .data = mir::ExprStmt{.expr = await_expr_id}};
      });
}

}  // namespace

auto LowerTimedStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    diag::SourceSpan span, const hir::TimedStmt& t) -> diag::Result<mir::Stmt> {
  if (const auto* ie = std::get_if<hir::ImplicitEventControl>(&t.timing)) {
    return LowerImplicitEventTimedStmt(
        process, frame, std::move(label), t, *ie);
  }
  if (const auto* nec = std::get_if<hir::NamedEventControl>(&t.timing)) {
    return LowerNamedEventTimedStmt(process, frame, std::move(label), t, *nec);
  }
  if (const auto* ec = std::get_if<hir::EventControl>(&t.timing)) {
    return LowerEventTimedStmt(process, frame, std::move(label), span, t, *ec);
  }
  const auto* d = std::get_if<hir::DelayControl>(&t.timing);
  if (d == nullptr) {
    throw InternalError("LowerTimedStmt: unknown hir::TimingControl variant");
  }
  return LowerDelayTimedStmt(process, frame, std::move(label), t, *d);
}

// LRM 15.5.1 `-> e;`.
auto LowerEventTriggerStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::EventTriggerStmt& et) -> diag::Result<mir::Stmt> {
  auto& block = *frame.current_block;
  auto receiver_or =
      process.LowerExpr(process.HirBody().exprs.Get(et.event), frame);
  if (!receiver_or) return std::unexpected(std::move(receiver_or.error()));
  const mir::ExprId receiver_id = block.exprs.Add(*std::move(receiver_or));
  // LRM 15.5.1: triggering reaches RuntimeServices to wake subscribers. The
  // engine handle is a real trailing argument, threaded the same way every
  // runtime effect threads it.
  const mir::ExprId services_id =
      block.exprs.Add(BuildServicesCallExpr(process.Module(), frame));
  mir::Expr call{
      .data =
          mir::CallExpr{
              .callee = mir::Direct{.target = support::BuiltinFn::kTrigger},
              .arguments = {receiver_id, services_id},
          },
      .type = process.Module().Unit().builtins.void_type};
  const mir::ExprId call_id = block.exprs.Add(std::move(call));
  return mir::Stmt{
      .label = std::move(label), .data = mir::ExprStmt{.expr = call_id}};
}

// LRM 9.4.3 `wait (cond) body`.
auto LowerWaitStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::WaitStmt& w) -> diag::Result<mir::Stmt> {
  const hir::ProceduralBody& hir_proc = process.HirBody();
  mir::Block wrapper;
  const WalkFrame wrapper_frame = frame.WithBlock(&wrapper);

  const hir::Expr& hir_cond = hir_proc.exprs.Get(w.cond);
  auto cond_or = process.LowerExpr(hir_cond, wrapper_frame);
  if (!cond_or) {
    return std::unexpected(std::move(cond_or.error()));
  }
  const mir::ExprId cond_id = wrapper.exprs.Add(*std::move(cond_or));
  const mir::TypeId cond_type = wrapper.exprs.Get(cond_id).type;

  const mir::ExprId not_cond_id = wrapper.exprs.Add(
      mir::Expr{
          .data =
              mir::UnaryExpr{
                  .op = mir::UnaryOp::kLogicalNot, .operand = cond_id},
          .type = cond_type});

  const auto& reads = w.sensitivity_list;

  mir::Block inner_block;
  const WalkFrame inner_frame = wrapper_frame.WithBlock(&inner_block);
  inner_block.AppendStmt(MakeSensitivityWaitStmt(
      inner_block, inner_frame, process.EnclosingScopeLowerer(), reads));

  const mir::BlockId inner_scope_id =
      wrapper.child_scopes.Add(std::move(inner_block));

  wrapper.AppendStmt(
      mir::WhileStmt{.condition = not_cond_id, .scope = inner_scope_id});

  const hir::Stmt& body_hir = hir_proc.stmts.Get(w.body);
  auto body_or = process.LowerStmt(body_hir, wrapper_frame);
  if (!body_or) {
    return std::unexpected(std::move(body_or.error()));
  }
  wrapper.AppendStmt(*std::move(body_or));

  const mir::BlockId wrapper_scope_id =
      frame.current_block->child_scopes.Add(std::move(wrapper));

  return mir::Stmt{
      .label = std::move(label),
      .data = mir::BlockStmt{.scope = wrapper_scope_id}};
}

}  // namespace lyra::lowering::hir_to_mir
