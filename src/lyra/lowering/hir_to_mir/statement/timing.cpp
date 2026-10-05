#include "lyra/lowering/hir_to_mir/statement/timing.hpp"

#include <algorithm>
#include <array>
#include <cstdint>
#include <expected>
#include <optional>
#include <span>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/reads_storage_only.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/block_builder.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/closure_builder.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/deferred_effect.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/sensitivity_wait.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
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

// The LRM 9.4.2.3 `iff` qualifier, as the closure that answers it whenever what
// it gates moves. It answers a one-bit value the standard's own truth rule
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
// closure that answers what the expression is worth now, and the `iff`
// qualifier where the source wrote one. It is one value every leaf of that
// expression names, since the value being watched is the expression's and
// there is one of it.
//
// Where the waiting process decides the wait, `report` holds a pointer to the
// report each evaluation states what it reaches in, and the closure states
// there every cell the expression reads and every place it reaches beyond
// them, as it reaches each.
template <ExprLowerer Lowerer>
auto BuildObservationLocal(
    Lowerer& lowerer, WalkFrame frame, mir::Block& block,
    const hir::EventTrigger& trigger, std::optional<mir::LocalId> report)
    -> diag::Result<mir::LocalId> {
  auto& unit = lowerer.Owner().Unit();

  ClosureBuilder closure(unit, frame);
  WalkFrame evaluation = closure.Frame();
  if (report.has_value()) {
    const mir::ExprId held = SnapshotIntoClosure(
        lowerer.Owner(), frame, closure,
        block.exprs.Add(
            mir::MakeLocalRefExpr(*report, unit.builtins.read_report_ptr)));
    const mir::LocalId reported_to =
        evaluation.bindings->DeclareAnonymous(unit.builtins.read_report_ptr);
    closure.Body().AppendStmt(
        mir::LocalDeclStmt{.target = reported_to, .init = held});
    evaluation = evaluation.WithReportingReachedTo(reported_to);
    auto cells = ReportCells(lowerer, evaluation, reported_to, trigger.cells);
    if (!cells) return std::unexpected(std::move(cells.error()));
  }
  auto value_or =
      lowerer.LowerExpr(lowerer.HirExprs().Get(trigger.signal), evaluation);
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
    const hir::ImplicitEventControl& ie) -> diag::Result<mir::Stmt> {
  return BuildValueChangeWaitStmt(
      block, frame, process, ie.sensitivity_list, support::BuiltinFn::kWaitAny);
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
    duration_id = ConvertToType(unit, block, duration_id, unit.builtins.real);
  }

  const mir::ExprId runtime_id =
      block.exprs.Add(BuildCurrentRuntimeCallExpr(process.Owner()));
  const mir::ExprId unit_power_id = BuildIntLiteral(
      unit, block, static_cast<std::int64_t>(process.Resolution().unit_power));
  const mir::ExprId precision_power_id = BuildIntLiteral(
      unit, block,
      static_cast<std::int64_t>(process.Resolution().precision_power));
  const support::BuiltinFn entry =
      is_real ? support::BuiltinFn::kDelayReal : support::BuiltinFn::kDelay;
  const mir::ExprId call_id = block.exprs.Add(
      mir::MakeCallExpr(
          mir::Direct{.target = entry},
          {runtime_id, duration_id, unit_power_id, precision_power_id},
          unit.builtins.machine_bool));
  return BuildWaitStmt(process.Owner(), block, call_id);
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

// Whether every expression deciding a wait -- what it watches and the qualifier
// gating it, each where the source wrote one -- only reads storage, so that a
// change may evaluate them where it happens: that is a schedule LRM 4.7
// permits, and nothing such an evaluation does can tell it from the waiting
// process evaluating them. Anything else is the waiting process's own to
// evaluate (LRM 4.5).
auto DecidedWhereItChanges(
    const base::Arena<hir::Expr, hir::ExprId>& exprs,
    std::initializer_list<std::optional<hir::ExprId>> deciding) -> bool {
  return std::ranges::all_of(
      deciding, [&](const std::optional<hir::ExprId>& expr) {
        return !expr.has_value() || hir::ReadsStorageOnly(exprs, *expr);
      });
}

// `entry` acting on the observation `observation` holds, answering `type`.
auto ObservationCall(
    const mir::CompilationUnit& unit, mir::Block& block,
    support::BuiltinFn entry, mir::LocalId observation, mir::TypeId type)
    -> mir::ExprId {
  const mir::ExprId held = block.exprs.Add(
      mir::MakeLocalRefExpr(observation, unit.builtins.observation));
  return block.exprs.Add(
      mir::MakeCallExpr(
          mir::Direct{.target = entry, .receiver = held}, {}, type));
}

// Arms `observation` with what its expression is worth where the wait begins.
void ArmObservation(
    const mir::CompilationUnit& unit, mir::Block& block,
    mir::LocalId observation) {
  block.AppendStmt(
      mir::ExprStmt{
          .expr = ObservationCall(
              unit, block, support::BuiltinFn::kObservationArm, observation,
              unit.builtins.void_type)});
}

// `while (none of observations fires) { waiting }`: the waiting process asks
// each observation after every candidacy, evaluating it there (LRM 4.5). Each
// answers one or zero, so their union is nonzero exactly where one of them
// fired; every one is asked, since each is a part of one event expression and
// each moves what it measures from.
auto WaitUntilOneFires(
    const mir::CompilationUnit& unit, mir::Block& block,
    std::span<const mir::LocalId> observations, mir::Block waiting)
    -> mir::Stmt {
  const auto fires = [&](mir::LocalId observation) {
    return ObservationCall(
        unit, block, support::BuiltinFn::kObservationFires, observation,
        unit.builtins.machine_int64);
  };
  mir::ExprId fired = fires(observations.front());
  for (const mir::LocalId observation : observations.subspan(1)) {
    fired = block.exprs.Add(
        mir::Expr{
            .data =
                mir::BinaryExpr{
                    .op = mir::BinaryOp::kBitwiseOr,
                    .lhs = fired,
                    .rhs = fires(observation)},
            .type = unit.builtins.machine_int64});
  }
  const mir::ExprId none = block.exprs.Add(
      mir::Expr{
          .data =
              mir::BinaryExpr{
                  .op = mir::BinaryOp::kEquality,
                  .lhs = fired,
                  .rhs = BuildMachineIntLiteral(unit, block, 0)},
          .type = unit.builtins.machine_bool});
  return mir::Stmt{
      .label = std::nullopt,
      .data = mir::WhileStmt{
          .condition = none,
          .scope = block.child_scopes.Add(std::move(waiting))}};
}

// A report nothing has been stated into, declared in `frame`'s block, and a
// local holding a pointer to it, which is what an evaluation states into and
// what a wait takes.
auto DeclareReport(const mir::CompilationUnit& unit, const WalkFrame& frame)
    -> mir::LocalId {
  mir::Block& block = *frame.current_block;
  const mir::LocalId report = DeclareLocal(
      frame,
      block.exprs.Add(
          mir::MakeCallExpr(
              mir::Direct{.target = support::BuiltinFn::kReadReportEmpty}, {},
              unit.builtins.read_report)));
  const mir::ExprId held =
      block.exprs.Add(mir::MakeLocalRefExpr(report, unit.builtins.read_report));
  return DeclareLocal(
      frame, block.exprs.Add(
                 mir::MakeAddressOfExpr(held, unit.builtins.read_report_ptr)));
}

// `elements`, each built in `block` from one local, as a machine array of
// `element` -- the form a span crosses into a runtime entry as.
auto LocalsAsArray(
    mir::CompilationUnit& unit, mir::Block& block, mir::TypeId element,
    std::span<const mir::LocalId> locals, const auto& element_of)
    -> mir::ExprId {
  std::vector<mir::ExprId> parts;
  parts.reserve(locals.size());
  for (const mir::LocalId local : locals) {
    parts.push_back(element_of(local));
  }
  return block.exprs.Add(
      mir::Expr{
          .data = mir::CompositeExpr{.parts = std::move(parts)},
          .type = mir::MachineArrayOf(unit.types, element, locals.size())});
}

// An event control its process decides (LRM 4.5, 9.4.2): a change to anything
// an evaluation reached resumes the process, which evaluates every expression
// of the control once more -- learning whether that was an event and what each
// reaches now -- and waits again on what they reached where it was not. The
// first evaluation is made before any wait and gives what later ones are
// measured from, so it is no event.
template <ExprLowerer Lowerer>
auto BuildEvaluatedEventWaitStmt(
    Lowerer& lowerer, WalkFrame frame, mir::Block& block,
    const hir::EventControl& ec) -> diag::Result<mir::Stmt> {
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  std::vector<mir::LocalId> reports;
  std::vector<mir::LocalId> observations;
  reports.reserve(ec.triggers.size());
  observations.reserve(ec.triggers.size());
  for (const hir::EventTrigger& trigger : ec.triggers) {
    const mir::LocalId report = DeclareReport(unit, frame);
    auto observation =
        BuildObservationLocal(lowerer, frame, block, trigger, report);
    if (!observation) return std::unexpected(std::move(observation.error()));
    reports.push_back(report);
    observations.push_back(*observation);
  }

  mir::Block waiting;
  const mir::ExprId reports_id = LocalsAsArray(
      unit, waiting, unit.builtins.read_report_ptr, reports,
      [&](mir::LocalId report) {
        return waiting.exprs.Add(
            mir::MakeLocalRefExpr(report, unit.builtins.read_report_ptr));
      });
  const mir::TypeId observation_ptr = unit.types.Intern(
      mir::Type{mir::PointerType{
          .pointee = unit.builtins.observation,
          .ownership = mir::PointerOwnership::kBorrowed,
          .mutability = mir::Mutability::kReadOnly}});
  const mir::ExprId observations_id = LocalsAsArray(
      unit, waiting, observation_ptr, observations,
      [&](mir::LocalId observation) {
        return waiting.exprs.Add(
            mir::MakeAddressOfExpr(
                waiting.exprs.Add(
                    mir::MakeLocalRefExpr(
                        observation, unit.builtins.observation)),
                observation_ptr));
      });
  const mir::ExprId runtime_id =
      waiting.exprs.Add(BuildCurrentRuntimeCallExpr(lowerer.Owner()));
  const mir::ExprId call_id = waiting.exprs.Add(
      mir::MakeCallExpr(
          mir::Direct{.target = support::BuiltinFn::kWaitRecollecting},
          {runtime_id, reports_id, observations_id},
          unit.builtins.machine_bool));
  waiting.AppendStmt(BuildWaitStmt(lowerer.Owner(), waiting, call_id));
  return WaitUntilOneFires(unit, block, observations, std::move(waiting));
}

}  // namespace

// An event control a change decides registers its leaves once, each watching
// for what its observation decides, armed where the control is reached; any
// other is decided by its process (LRM 9.4.2).
template <ExprLowerer Lowerer>
auto BuildEventWaitStmt(
    Lowerer& lowerer, WalkFrame frame, mir::Block& block,
    const hir::EventControl& ec) -> diag::Result<mir::Stmt> {
  if (!std::ranges::all_of(ec.triggers, [&](const hir::EventTrigger& t) {
        return DecidedWhereItChanges(
            lowerer.HirExprs(), {t.signal, t.condition});
      })) {
    return BuildEvaluatedEventWaitStmt(lowerer, frame, block, ec);
  }
  const mir::CompilationUnit& unit = lowerer.Owner().Unit();
  std::vector<ObservedLeaf> leaves;
  for (const hir::EventTrigger& trigger : ec.triggers) {
    auto observation =
        BuildObservationLocal(lowerer, frame, block, trigger, std::nullopt);
    if (!observation) return std::unexpected(std::move(observation.error()));
    ArmObservation(unit, block, *observation);
    for (const hir::SensitivityEntry& cell : trigger.cells) {
      leaves.push_back(
          ObservedLeaf{.entry = cell, .observation = *observation});
    }
  }
  return BuildWaitStmt(
      block, frame, lowerer, leaves, support::BuiltinFn::kWaitAny);
}

template auto BuildEventWaitStmt(
    ProcessLowerer&, WalkFrame, mir::Block&, const hir::EventControl&)
    -> diag::Result<mir::Stmt>;
template auto BuildEventWaitStmt(
    const StructuralScopeLowerer&, WalkFrame, mir::Block&,
    const hir::EventControl&) -> diag::Result<mir::Stmt>;

auto BuildNamedEventWaitStmt(
    ProcessLowerer& process, WalkFrame frame, mir::Block& block,
    const hir::NamedEventControl& nec) -> diag::Result<mir::Stmt> {
  // A trigger is the event itself, so there is nothing to have moved and the
  // observation carries the `iff` qualifier alone (LRM 9.4.2.3, 15.5).
  const mir::CompilationUnit& unit = process.Owner().Unit();
  auto observation =
      BuildQualifierObservationLocal(process, frame, block, nec.condition);
  if (!observation) return std::unexpected(std::move(observation.error()));
  if (DecidedWhereItChanges(process.HirExprs(), {nec.condition})) {
    ArmObservation(unit, block, *observation);
    const std::array<ObservedLeaf, 1> leaves{
        ObservedLeaf{.entry = nec.event, .observation = *observation}};
    return BuildWaitStmt(
        block, frame, process, leaves, support::BuiltinFn::kWaitAny);
  }
  // A qualifier that can act is the waiting process's to evaluate, each time
  // the trigger resumes it; the trigger alone is what the wait watches.
  mir::Block waiting;
  const std::array<ObservedLeaf, 1> leaves{ObservedLeaf{
      .entry = nec.event,
      .observation = DeclareObservation(
          unit, frame, block, support::BuiltinFn::kObservationOnReaching, {})}};
  auto wait = BuildWaitStmt(
      waiting, frame.WithBlock(&waiting), process, leaves,
      support::BuiltinFn::kWaitAny);
  if (!wait) return std::unexpected(std::move(wait.error()));
  waiting.AppendStmt(*std::move(wait));
  const std::array<mir::LocalId, 1> observations{*observation};
  return WaitUntilOneFires(unit, block, observations, std::move(waiting));
}

auto BuildAnyEventWaitStmt(
    ProcessLowerer& process, WalkFrame frame, mir::Block& block,
    const hir::AnyEventControl& event) -> diag::Result<mir::Stmt> {
  return std::visit(
      Overloaded{
          [&](const hir::EventControl& ec) {
            return BuildEventWaitStmt(process, frame, block, ec);
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
                  return BuildEventWaitStmt(process, inner, block, ec);
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
  mir::CompilationUnit& unit = process.Owner().Unit();
  mir::Block wrapper;
  const WalkFrame wrapper_frame = frame.WithBlock(&wrapper);

  // The loop is what reads the condition, so what this waits for is the loop
  // getting to read it again -- which is also the rule LRM 9.7 gives a `wait`
  // that is started after being stopped, and why it is not the wait an event
  // control makes over the same reads. Each test states what it reached, and
  // the loop waits on that.
  const mir::LocalId report = DeclareReport(unit, wrapper_frame);

  // The loop runs while the condition is not true, and "not true" is decided
  // after the condition has been reduced to a predicate, never before. A
  // four-state `!` answers unknown for an unknown operand, and reducing that
  // answer yields false -- so negating first would run no iteration at all for
  // exactly the condition LRM 9.4.3 says must block.
  BlockBuilder test(wrapper_frame);
  const WalkFrame test_frame = test.Frame().WithReportingReachedTo(report);
  auto cells = ReportCells(process, test_frame, report, w.cells);
  if (!cells) return std::unexpected(std::move(cells.error()));
  auto cond_or = process.LowerExpr(hir_proc.exprs.Get(w.cond), test_frame);
  if (!cond_or) {
    return std::unexpected(std::move(cond_or.error()));
  }
  mir::Block& test_block = test.Body();
  const mir::ExprId cond_id = test_block.exprs.Add(*std::move(cond_or));
  const mir::ExprId not_yet = test_block.exprs.Add(
      mir::Expr{
          .data =
              mir::UnaryExpr{
                  .op = mir::UnaryOp::kLogicalNot,
                  .operand = ReduceToCondition(unit, test_block, cond_id)},
          .type = unit.builtins.machine_bool});
  const mir::ExprId waiting_id = wrapper.exprs.Add(test.Build(not_yet));

  mir::Block inner_block;
  const std::array<mir::LocalId, 1> reports{report};
  const mir::ExprId reports_id = LocalsAsArray(
      unit, inner_block, unit.builtins.read_report_ptr, reports,
      [&](mir::LocalId reported) {
        return inner_block.exprs.Add(
            mir::MakeLocalRefExpr(reported, unit.builtins.read_report_ptr));
      });
  const mir::ExprId runtime_id =
      inner_block.exprs.Add(BuildCurrentRuntimeCallExpr(process.Owner()));
  const mir::ExprId call_id = inner_block.exprs.Add(
      mir::MakeCallExpr(
          mir::Direct{.target = support::BuiltinFn::kWaitUntil},
          {runtime_id, reports_id}, unit.builtins.machine_bool));
  inner_block.AppendStmt(BuildWaitStmt(process.Owner(), inner_block, call_id));

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
