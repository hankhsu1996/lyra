#include "lyra/lowering/hir_to_mir/statement/timing.hpp"

#include <array>
#include <cstdint>
#include <expected>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/closure_builder.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/deferred_effect.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/sensitivity_wait.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// The shape every timed statement takes (LRM 9.4): a fresh child scope holding
// a prepended wait or control statement followed by the lowered body. The four
// timing forms differ only in that control statement; `build_wait` produces it
// and may lower whatever it needs into the child block, which is where a
// controlled body's own wait keeps the values it evaluated on the way in.
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

// The LRM 9.4.2.3 `iff` qualifier, as the closure that answers it where the
// change happens. It answers a one-bit value the standard's own truth rule
// (LRM 12.4) has already decided, because that rule is the language's and
// belongs where the expression is compiled rather than in the runtime that
// reads the answer.
template <ExprLowerer Lowerer>
auto BuildConditionClosure(
    Lowerer& lowerer, WalkFrame frame, mir::Block& block, hir::ExprId condition)
    -> diag::Result<mir::ExprId> {
  auto& unit = lowerer.Owner().Unit();
  ClosureBuilder closure(unit, frame);
  auto cond_or =
      lowerer.LowerExpr(lowerer.HirExprs().Get(condition), closure.Frame());
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  mir::Block& body = closure.Body();
  const mir::ExprId raw_id = body.exprs.Add(*std::move(cond_or));
  const mir::ExprId held_id = body.exprs.Add(
      mir::Expr{
          .data =
              mir::ConditionalExpr{
                  .condition = ReduceToCondition(unit, body, raw_id),
                  .then_value = BuildBit1Literal(unit, body, true),
                  .else_value = BuildBit1Literal(unit, body, false)},
          .type = unit.builtins.bit1});
  return block.exprs.Add(closure.Build(held_id));
}

// The observation one event expression is watched through (LRM 9.4.2): a
// closure that answers what the expression is worth now, armed with what it is
// worth here, and the `iff` qualifier where the source wrote one. It is one
// value every leaf of that expression names, since the value being watched is
// the expression's and there is one of it.
template <ExprLowerer Lowerer>
auto BuildObservationLocal(
    Lowerer& lowerer, WalkFrame frame, mir::Block& block,
    const hir::EventTrigger& trigger) -> diag::Result<mir::LocalId> {
  auto& unit = lowerer.Owner().Unit();

  ClosureBuilder closure(unit, frame);
  auto value_or = lowerer.LowerExpr(
      lowerer.HirExprs().Get(trigger.signal), closure.Frame());
  if (!value_or) return std::unexpected(std::move(value_or.error()));
  const mir::ExprId value_id = closure.Body().exprs.Add(*std::move(value_or));

  std::vector<mir::ExprId> arguments{
      block.exprs.Add(closure.Build(value_id)),
      BuildIntLiteral(unit, block, static_cast<std::int64_t>(trigger.edge))};
  if (!trigger.condition.has_value()) {
    return DeclareObservation(
        unit, frame, block, support::BuiltinFn::kObservationOfValue,
        std::move(arguments));
  }
  auto condition =
      BuildConditionClosure(lowerer, frame, block, *trigger.condition);
  if (!condition) return std::unexpected(std::move(condition.error()));
  arguments.push_back(*condition);
  return DeclareObservation(
      unit, frame, block, support::BuiltinFn::kObservationOfValueQualified,
      std::move(arguments));
}

// What a wait carries where the only thing that can hold it back is an `iff`
// qualifier (LRM 9.4.2.3): a named event's trigger is the event, so there is no
// value to have moved. Without a qualifier nothing further decides, and being
// reached is the whole condition.
auto BuildQualifierObservationLocal(
    ProcessLowerer& process, WalkFrame frame, mir::Block& block,
    std::optional<hir::ExprId> condition) -> diag::Result<mir::LocalId> {
  auto& unit = process.Owner().Unit();
  if (!condition.has_value()) {
    return DeclareObservation(
        unit, frame, block, support::BuiltinFn::kObservationOnReaching, {});
  }
  auto closure = BuildConditionClosure(process, frame, block, *condition);
  if (!closure) return std::unexpected(std::move(closure.error()));
  return DeclareObservation(
      unit, frame, block, support::BuiltinFn::kObservationQualified,
      {*closure});
}

// LRM 9.4.2.2 `@*`: the standard makes the wait sensitive to the variables the
// controlled statement reads rather than to the value of an expression, so
// being reached is the whole of the condition.
auto BuildImplicitEventWaitStmt(
    ProcessLowerer& process, WalkFrame frame, mir::Block& block,
    const hir::ImplicitEventControl& ie) -> mir::Stmt {
  return BuildValueChangeWaitStmt(
      block, frame, process.EnclosingScopeLowerer(), ie.sensitivity_list);
}

// LRM 9.4.1 `#N`. The wait lowers to a coroutine-suspending free-function
// call whose argument vector states the runtime handle, the amount of time the
// design asked to wait, and the enclosing scope's time unit and precision
// powers (LRM 3.14.2); the runtime rounds that amount to the scope's precision
// (LRM 3.14.1) and scales it to the design-global tick (LRM 3.14.3). The amount
// is evaluated here, where the statement is reached, so a later write to
// anything it read does not reach a wait already under way.
//
// Which entry carries it follows the amount's own type, because the language
// reads the same written value differently in each: an integral amount counts
// whole time units and gives its unknown and negative values meanings of their
// own, while a real one may name a fraction of a unit. The front end has
// already refused an amount that is neither (a delay expression must be
// numeric), so the two together are the whole of what arrives.
auto BuildDelayWaitStmt(
    ProcessLowerer& process, WalkFrame frame, mir::Block& block,
    const hir::DelayControl& d) -> diag::Result<mir::Stmt> {
  auto& unit = process.Owner().Unit();
  auto duration_or =
      process.LowerExpr(process.HirBody().exprs.Get(d.duration), frame);
  if (!duration_or) return std::unexpected(std::move(duration_or.error()));
  mir::ExprId duration_id = block.exprs.Add(*std::move(duration_or));

  const mir::Type& duration_type =
      unit.types.Get(block.exprs.Get(duration_id).type);
  const bool is_real = duration_type.IsRealFamily();
  if (duration_type.Is<mir::ShortRealType>()) {
    // LRM 6.12.1: `real` and `realtime` are one type, and a `shortreal` differs
    // from them only in host precision, so the entry takes the wider and the
    // narrower reshapes into it.
    duration_id =
        ConvertToType(unit, block, duration_id, unit.builtins.realtime);
  }

  const mir::ExprId runtime_id =
      block.exprs.Add(BuildCurrentRuntimeCallExpr(process.Owner()));
  const mir::ExprId unit_power_id = BuildIntLiteral(
      unit, block, static_cast<std::int64_t>(process.Resolution().unit_power));
  const mir::ExprId precision_power_id = BuildIntLiteral(
      unit, block,
      static_cast<std::int64_t>(process.Resolution().precision_power));
  const mir::ExprId call_id = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = is_real ? support::BuiltinFn::kDelayReal
                                            : support::BuiltinFn::kDelay},
                  .arguments =
                      {runtime_id, duration_id, unit_power_id,
                       precision_power_id}},
          .type = unit.builtins.void_type});
  const mir::ExprId await_expr_id = block.exprs.Add(
      mir::Expr{
          .data = mir::AwaitExpr{.awaitable = call_id},
          .type = unit.builtins.void_type});
  return mir::Stmt{
      .label = std::nullopt, .data = mir::ExprStmt{.expr = await_expr_id}};
}

// LRM 15.5.1: triggering reaches RuntimeEffects to wake subscribers. The engine
// handle is a real trailing argument, threaded the same way every runtime
// effect threads it.
auto BuildTriggerCallExpr(
    UnitLowerer& unit_lowerer, mir::Block& block, mir::ExprId event_id)
    -> mir::Expr {
  const mir::ExprId runtime_id =
      block.exprs.Add(BuildCurrentRuntimeCallExpr(unit_lowerer));
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Direct{
                      .target = support::BuiltinFn::kTrigger,
                      .receiver = event_id},
              .arguments = {runtime_id},
          },
      .type = unit_lowerer.Unit().builtins.void_type};
}

}  // namespace

template <ExprLowerer Lowerer>
auto BuildEventWaitStmt(
    Lowerer& lowerer, const StructuralScopeLowerer& scope, WalkFrame frame,
    mir::Block& block, const hir::EventControl& ec) -> diag::Result<mir::Stmt> {
  std::vector<ObservedLeaf> leaves;
  for (const hir::EventTrigger& trigger : ec.triggers) {
    auto observation = BuildObservationLocal(lowerer, frame, block, trigger);
    if (!observation) {
      return std::unexpected(std::move(observation.error()));
    }
    for (const hir::SensitivityEntry& leaf : trigger.sensitivity_list) {
      leaves.push_back(
          ObservedLeaf{.entry = leaf, .observation = *observation});
    }
  }
  return BuildWaitStmt(block, frame, scope, leaves);
}

template auto BuildEventWaitStmt(
    ProcessLowerer&, const StructuralScopeLowerer&, WalkFrame, mir::Block&,
    const hir::EventControl&) -> diag::Result<mir::Stmt>;
template auto BuildEventWaitStmt(
    const StructuralScopeLowerer&, const StructuralScopeLowerer&, WalkFrame,
    mir::Block&, const hir::EventControl&) -> diag::Result<mir::Stmt>;

auto BuildNamedEventWaitStmt(
    ProcessLowerer& process, WalkFrame frame, mir::Block& block,
    const hir::NamedEventControl& nec) -> diag::Result<mir::Stmt> {
  // A trigger is the event itself, so there is nothing to have moved and the
  // observation carries the `iff` qualifier alone (LRM 9.4.2.3, 15.5).
  auto observation =
      BuildQualifierObservationLocal(process, frame, block, nec.condition);
  if (!observation) return std::unexpected(std::move(observation.error()));
  const std::array<ObservedLeaf, 1> leaves{
      ObservedLeaf{.entry = nec.event, .observation = *observation}};
  return BuildWaitStmt(block, frame, process.EnclosingScopeLowerer(), leaves);
}

auto BuildAnyEventWaitStmt(
    ProcessLowerer& process, WalkFrame frame, mir::Block& block,
    const hir::AnyEventControl& event) -> diag::Result<mir::Stmt> {
  return std::visit(
      Overloaded{
          [&](const hir::EventControl& ec) {
            return BuildEventWaitStmt(
                process, process.EnclosingScopeLowerer(), frame, block, ec);
          },
          [&](const hir::NamedEventControl& nec) {
            return BuildNamedEventWaitStmt(process, frame, block, nec);
          }},
      event);
}

auto LowerTimedStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::TimedStmt& t) -> diag::Result<mir::Stmt> {
  return LowerTimedWaitWrapper(
      process, frame, std::move(label), t.stmt,
      [&](mir::Block& block, WalkFrame inner) -> diag::Result<mir::Stmt> {
        return std::visit(
            Overloaded{
                [&](const hir::DelayControl& d) {
                  return BuildDelayWaitStmt(process, inner, block, d);
                },
                [&](const hir::EventControl& ec) {
                  return BuildEventWaitStmt(
                      process, process.EnclosingScopeLowerer(), inner, block,
                      ec);
                },
                [&](const hir::NamedEventControl& nec) {
                  return BuildNamedEventWaitStmt(process, inner, block, nec);
                },
                [&](const hir::ImplicitEventControl& ie)
                    -> diag::Result<mir::Stmt> {
                  return BuildImplicitEventWaitStmt(process, inner, block, ie);
                }},
            t.timing);
      });
}

// LRM 15.5.1 `-> e;` and `->> [ delay_or_event_control ] e;`. The nonblocking
// form is the trigger made a deferred effect: nothing about the event is read
// where the statement stands, since a named event reference designates the same
// storage whenever it is reached, so what the carrier holds is the way it
// reaches that storage.
auto LowerEventTriggerStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::EventTriggerStmt& et) -> diag::Result<mir::Stmt> {
  auto& block = *frame.current_block;
  const hir::Expr& event_hir = process.HirBody().exprs.Get(et.event);

  auto effect_or = std::visit(
      Overloaded{
          [&](const hir::ImmediateEffect&) -> diag::Result<mir::Expr> {
            auto event_or = process.LowerExpr(event_hir, frame);
            if (!event_or) return std::unexpected(std::move(event_or.error()));
            return BuildTriggerCallExpr(
                process.Owner(), block, block.exprs.Add(*std::move(event_or)));
          },
          [&](const hir::NonBlockingEffect& deferred)
              -> diag::Result<mir::Expr> {
            return BuildDeferredEffect(
                process, frame, deferred.control,
                [&](ClosureBuilder& closure) -> diag::Result<mir::ExprId> {
                  auto event_or = process.LowerExpr(event_hir, closure.Frame());
                  if (!event_or) {
                    return std::unexpected(std::move(event_or.error()));
                  }
                  return closure.Body().exprs.Add(*std::move(event_or));
                },
                [&](mir::Block& body, const mir::ExprId& event) {
                  body.AppendStmt(
                      mir::ExprStmt{
                          .expr = body.exprs.Add(BuildTriggerCallExpr(
                              process.Owner(), body, event))});
                });
          }},
      et.timing);
  if (!effect_or) return std::unexpected(std::move(effect_or.error()));
  return mir::Stmt{
      .label = std::move(label),
      .data = mir::ExprStmt{.expr = block.exprs.Add(*std::move(effect_or))}};
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

  // The loop runs while the condition is not true, and "not true" is decided
  // after the condition has been reduced to a predicate, never before. A
  // four-state `!` answers unknown for an unknown operand, and reducing that
  // answer yields false -- so negating first would run no iteration at all for
  // exactly the condition LRM 9.4.3 says must block.
  const mir::ExprId waiting_id = wrapper.exprs.Add(
      mir::Expr{
          .data =
              mir::UnaryExpr{
                  .op = mir::UnaryOp::kLogicalNot,
                  .operand = ReduceToCondition(
                      process.Owner().Unit(), wrapper, cond_id)},
          .type = process.Owner().Unit().builtins.machine_bool});

  const auto& reads = w.sensitivity_list;

  mir::Block inner_block;
  const WalkFrame inner_frame = wrapper_frame.WithBlock(&inner_block);
  inner_block.AppendStmt(BuildValueChangeWaitStmt(
      inner_block, inner_frame, process.EnclosingScopeLowerer(), reads));

  const mir::BlockId inner_scope_id =
      wrapper.child_scopes.Add(std::move(inner_block));

  wrapper.AppendStmt(
      mir::WhileStmt{.condition = waiting_id, .scope = inner_scope_id});

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
