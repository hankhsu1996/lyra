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

auto ResolveDelayTicks(ProcessLowerer& process, const hir::DelayControl& d)
    -> diag::Result<SimDuration> {
  const DelayTimeResolver resolver{process.Resolution()};
  return ResolveDelayDuration(
      resolver, process.HirBody().exprs.at(d.duration.value));
}

// LRM 15.5.2 `@e body;`. HIR -> MIR expands the timed statement into a
// Block { AwaitStmt(MethodCall(Await)); body; } inside a fresh child scope --
// the same shape used for `@*` (LRM 9.4.2.2), substituting a suspending
// named-event await in place of SensitivityWaitStmt.
auto LowerNamedEventTimedStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::TimedStmt& t, const hir::NamedEventControl& nec)
    -> diag::Result<mir::Stmt> {
  const hir::ProceduralBody& hir_proc = process.HirBody();
  mir::ProceduralScope child_proc_scope;
  const WalkFrame child_frame =
      frame.WithProceduralScope(&child_proc_scope).Deeper();
  auto receiver_or =
      process.LowerExpr(hir_proc.exprs.at(nec.event.value), child_frame);
  if (!receiver_or) return std::unexpected(std::move(receiver_or.error()));
  const mir::ExprId receiver_id =
      child_proc_scope.AddExpr(*std::move(receiver_or));
  mir::Expr await_call{
      .data =
          mir::CallExpr{
              .callee =
                  mir::BuiltinMethodCallee{
                      .method =
                          mir::EventMethodInfo{
                              .kind = mir::EventMethodKind::kAwait}},
              .arguments = {receiver_id},
          },
      .type = process.Module().Unit().builtins.void_type};
  const mir::ExprId await_id = child_proc_scope.AddExpr(std::move(await_call));
  child_proc_scope.AppendStmt(
      mir::Stmt{
          .label = std::nullopt,
          .data = mir::AwaitStmt{.awaitable = await_id}});
  const hir::Stmt& inner_hir = hir_proc.stmts.at(t.stmt.value);
  auto inner_or = process.LowerStmt(inner_hir, child_frame);
  if (!inner_or) {
    return std::unexpected(std::move(inner_or.error()));
  }
  child_proc_scope.AppendStmt(*std::move(inner_or));
  const mir::ProceduralScopeId scope_id =
      frame.current_procedural_scope->AddChildScope(
          std::move(child_proc_scope));
  return mir::Stmt{
      .label = std::move(label), .data = mir::BlockStmt{.scope = scope_id}};
}

// LRM 9.4.2 `@(...) body`. Single-leaf trigger expressions lower directly to
// a SensitivityWaitStmt over the union of leaves; multi-leaf forms require a
// snapshot wrapper not yet implemented.
auto LowerEventTimedStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    diag::SourceSpan span, const hir::TimedStmt& t, const hir::EventControl& ec)
    -> diag::Result<mir::Stmt> {
  std::vector<hir::SensitivityEntry> union_reads;
  union_reads.reserve(ec.triggers.size());
  for (const auto& trigger : ec.triggers) {
    if (trigger.sensitivity_list.size() > 1) {
      return diag::Unsupported(
          span, diag::DiagCode::kUnsupportedEventTriggerForm,
          "compound event expressions (concatenation, arithmetic, dynamic "
          "index) are not yet supported",
          diag::UnsupportedCategory::kFeature);
    }
    for (auto leaf : trigger.sensitivity_list) {
      // LRM 9.4.2 LSB-reduce: an edge event monitors only the LSB of the
      // expression.
      if (leaf.edge_kind != hir::EventEdge::kAnyChange) {
        leaf.bit_range = {leaf.bit_range.first, leaf.bit_range.first};
      }
      union_reads.push_back(leaf);
    }
  }

  mir::ProceduralScope child_proc_scope;
  const WalkFrame child_frame =
      frame.WithProceduralScope(&child_proc_scope).Deeper();
  child_proc_scope.AppendStmt(
      BuildSensitivityWaitStmt(process.Scope(), union_reads));
  const hir::Stmt& inner_hir = process.HirBody().stmts.at(t.stmt.value);
  auto inner_or = process.LowerStmt(inner_hir, child_frame);
  if (!inner_or) {
    return std::unexpected(std::move(inner_or.error()));
  }
  child_proc_scope.AppendStmt(*std::move(inner_or));
  const mir::ProceduralScopeId scope_id =
      frame.current_procedural_scope->AddChildScope(
          std::move(child_proc_scope));
  return mir::Stmt{
      .label = std::move(label), .data = mir::BlockStmt{.scope = scope_id}};
}

// LRM 9.4.2.2 `@* body`.
auto LowerImplicitEventTimedStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::TimedStmt& t, const hir::ImplicitEventControl& ie)
    -> diag::Result<mir::Stmt> {
  mir::ProceduralScope child_proc_scope;
  const WalkFrame child_frame =
      frame.WithProceduralScope(&child_proc_scope).Deeper();
  child_proc_scope.AppendStmt(
      BuildSensitivityWaitStmt(process.Scope(), ie.sensitivity_list));
  const hir::Stmt& inner_hir = process.HirBody().stmts.at(t.stmt.value);
  auto inner_or = process.LowerStmt(inner_hir, child_frame);
  if (!inner_or) {
    return std::unexpected(std::move(inner_or.error()));
  }
  child_proc_scope.AppendStmt(*std::move(inner_or));
  const mir::ProceduralScopeId scope_id =
      frame.current_procedural_scope->AddChildScope(
          std::move(child_proc_scope));
  return mir::Stmt{
      .label = std::move(label), .data = mir::BlockStmt{.scope = scope_id}};
}

// LRM 9.4.1 `#N body`.
auto LowerDelayTimedStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::TimedStmt& t, const hir::DelayControl& d)
    -> diag::Result<mir::Stmt> {
  auto ticks_or = ResolveDelayTicks(process, d);
  if (!ticks_or) {
    return std::unexpected(std::move(ticks_or.error()));
  }
  mir::ProceduralScope child_proc_scope;
  const WalkFrame child_frame =
      frame.WithProceduralScope(&child_proc_scope).Deeper();
  child_proc_scope.AppendStmt(
      mir::Stmt{
          .label = std::nullopt,
          .data = mir::DelayStmt{.duration = *ticks_or}});
  const hir::Stmt& inner_hir = process.HirBody().stmts.at(t.stmt.value);
  auto inner_or = process.LowerStmt(inner_hir, child_frame);
  if (!inner_or) {
    return std::unexpected(std::move(inner_or.error()));
  }
  child_proc_scope.AppendStmt(*std::move(inner_or));
  const mir::ProceduralScopeId scope_id =
      frame.current_procedural_scope->AddChildScope(
          std::move(child_proc_scope));
  return mir::Stmt{
      .label = std::move(label), .data = mir::BlockStmt{.scope = scope_id}};
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
  auto& proc_scope = *frame.current_procedural_scope;
  auto receiver_or =
      process.LowerExpr(process.HirBody().exprs.at(et.event.value), frame);
  if (!receiver_or) return std::unexpected(std::move(receiver_or.error()));
  const mir::ExprId receiver_id = proc_scope.AddExpr(*std::move(receiver_or));
  // LRM 15.5.1: triggering reaches RuntimeServices to wake subscribers. The
  // engine handle is a real trailing argument, threaded the same way every
  // runtime effect threads it
  // (docs/decisions/runtime-effects-as-generic-calls.md).
  const mir::ExprId services_id =
      proc_scope.AddExpr(BuildServicesCallExpr(process, frame));
  mir::Expr call{
      .data =
          mir::CallExpr{
              .callee =
                  mir::BuiltinMethodCallee{
                      .method =
                          mir::EventMethodInfo{
                              .kind = mir::EventMethodKind::kTrigger}},
              .arguments = {receiver_id, services_id},
          },
      .type = process.Module().Unit().builtins.void_type};
  const mir::ExprId call_id = proc_scope.AddExpr(std::move(call));
  return mir::Stmt{
      .label = std::move(label), .data = mir::ExprStmt{.expr = call_id}};
}

// LRM 9.4.3 `wait (cond) body`.
auto LowerWaitStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::WaitStmt& w) -> diag::Result<mir::Stmt> {
  const hir::ProceduralBody& hir_proc = process.HirBody();
  mir::ProceduralScope wrapper;
  const WalkFrame wrapper_frame = frame.WithProceduralScope(&wrapper).Deeper();

  const hir::Expr& hir_cond = hir_proc.exprs.at(w.cond.value);
  auto cond_or = process.LowerExpr(hir_cond, wrapper_frame);
  if (!cond_or) {
    return std::unexpected(std::move(cond_or.error()));
  }
  const mir::ExprId cond_id = wrapper.AddExpr(*std::move(cond_or));
  const mir::TypeId cond_type = wrapper.GetExpr(cond_id).type;

  const mir::ExprId not_cond_id = wrapper.AddExpr(
      mir::Expr{
          .data =
              mir::UnaryExpr{
                  .op = mir::UnaryOp::kLogicalNot, .operand = cond_id},
          .type = cond_type});

  const auto& reads = w.sensitivity_list;

  mir::ProceduralScope inner_scope;
  inner_scope.AppendStmt(BuildSensitivityWaitStmt(process.Scope(), reads));

  const mir::ProceduralScopeId inner_scope_id =
      wrapper.AddChildScope(std::move(inner_scope));

  wrapper.AppendStmt(
      mir::Stmt{
          .label = std::nullopt,
          .data = mir::WhileStmt{
              .condition = not_cond_id, .scope = inner_scope_id}});

  const hir::Stmt& body_hir = hir_proc.stmts.at(w.body.value);
  auto body_or = process.LowerStmt(body_hir, wrapper_frame);
  if (!body_or) {
    return std::unexpected(std::move(body_or.error()));
  }
  wrapper.AppendStmt(*std::move(body_or));

  const mir::ProceduralScopeId wrapper_scope_id =
      frame.current_procedural_scope->AddChildScope(std::move(wrapper));

  return mir::Stmt{
      .label = std::move(label),
      .data = mir::BlockStmt{.scope = wrapper_scope_id}};
}

}  // namespace lyra::lowering::hir_to_mir
