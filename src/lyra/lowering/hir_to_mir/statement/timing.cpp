#include "lyra/lowering/hir_to_mir/statement/timing.hpp"

#include <algorithm>
#include <array>
#include <cstdint>
#include <expected>
#include <functional>
#include <optional>
#include <span>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/base/internal_error.hpp"
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
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/deferred_effect.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/select_position.hpp"
#include "lyra/lowering/hir_to_mir/sensitivity_wait.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/lowering/hir_to_mir/value_operations.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/callable_bounds.hpp"
#include "lyra/support/event_edge.hpp"

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
// it gates moves. It answers whether the qualifier holds, by the standard's own
// truth rule (LRM 12.4), because that rule is the language's and belongs where
// the expression is compiled rather than in the runtime that reads the answer.
template <ExprLowerer Lowerer>
auto BuildConditionClosure(
    Lowerer& lowerer, WalkFrame frame, mir::Block& block, hir::ExprId condition)
    -> diag::Result<mir::ExprId> {
  auto& unit = lowerer.Owner().Unit();
  ClosureBuilder closure(unit, frame, support::ClosureKind::kFn);
  auto cond_or =
      lowerer.LowerExpr(lowerer.HirExprs().Get(condition), closure.Frame());
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  mir::Block& body = closure.Body();
  const mir::ExprId raw_id = body.exprs.Add(*std::move(cond_or));
  return BuildKeptClosure(
      unit, block, closure.Build(ReduceToCondition(unit, body, raw_id)));
}

// Whether the event `edge` names occurred between `before` and `now` (LRM
// 9.4.2): any change of its bits, or an edge of the one bit an edge
// reads, which LRM 9.4.2 Table 9-2 states: leaving 0 is a posedge and leaving
// 1 a negedge whatever the destination, and from x or z only arrival at 1 or
// 0 names one.
auto BuildEventOccurred(
    const mir::CompilationUnit& unit, mir::Block& block,
    support::EventEdge edge, mir::LocalId before, mir::LocalId now,
    mir::TypeId type) -> mir::ExprId {
  const auto is = [&](mir::LocalId bit, bool one) {
    return BuildBitIdentity(
        unit, block, block.ReadLocal(bit, type),
        ConvertToType(
            unit, block, mir::BuildBit1Literal(unit, block, one), type));
  };
  const auto leaves = [&](bool one) {
    const std::array left{
        is(before, one), BuildConditionNot(unit, block, is(now, one))};
    const std::array arrived{
        BuildConditionNot(unit, block, is(before, !one)), is(now, !one)};
    const std::array either{
        AllHold(unit, block, left), AllHold(unit, block, arrived)};
    return AnyHolds(unit, block, either);
  };
  switch (edge) {
    case support::EventEdge::kAnyChange:
      return BuildConditionNot(
          unit, block,
          BuildBitIdentity(
              unit, block, block.ReadLocal(before, type),
              block.ReadLocal(now, type)));
    case support::EventEdge::kPosedge:
      return leaves(false);
    case support::EventEdge::kNegedge:
      return leaves(true);
    case support::EventEdge::kBothEdges: {
      const std::array either{leaves(false), leaves(true)};
      return AnyHolds(unit, block, either);
    }
  }
  throw InternalError("BuildEventOccurred: unknown event edge");
}

// What an event control reads of the expression it watches (LRM 9.4.2), as a
// value of the block the expression was lowered into: an edge is a transition
// of the least significant bit, which is that bit as a `logic` whatever type
// the expression has, and any other event is a change anywhere in the value.
auto BuildWatchedPart(
    const mir::CompilationUnit& unit, mir::Block& block,
    support::EventEdge edge, mir::ExprId value) -> mir::ExprId {
  switch (edge) {
    case support::EventEdge::kAnyChange:
      return value;
    case support::EventEdge::kPosedge:
    case support::EventEdge::kNegedge:
    case support::EventEdge::kBothEdges: {
      const std::optional<mir::TypeId> bit_type =
          LeastSignificantBitType(unit, block.exprs.Get(value).type);
      if (!bit_type.has_value()) {
        throw InternalError(
            "BuildWatchedPart: an edge is watched on a value that is not "
            "integral, which the front end refuses -- please report this as a "
            "bug");
      }
      return ConvertToType(
          unit, block, BuildLeastSignificantBit(unit, block, value, *bit_type),
          mir::PackedVectorOf(
              unit.types, 1, mir::IntegralStateKind::kFourState));
    }
  }
  throw InternalError("BuildWatchedPart: unknown event edge");
}

// The value `lower_value` builds in the frame it is handed, evaluated as the
// evaluation that states what it reaches in `report`. Foreign code reads
// SystemVerilog data only through the exported functions it calls (LRM
// 35.5.3), so an exported function called while the evaluation runs reports
// there too; on every way out of the evaluation it stops being the one in
// progress.
auto EvaluatedReporting(
    const UnitLowerer& unit_lowerer, const WalkFrame& frame,
    mir::LocalId report, auto lower_value) -> diag::Result<mir::ExprId> {
  const mir::CompilationUnit& unit = unit_lowerer.Unit();
  const auto on_the_report = [&](mir::Block& at, support::BuiltinFn entry) {
    at.AppendStmt(
        mir::ExprStmt{
            .expr = BuildReportCall(
                unit, at, report, entry,
                {at.exprs.Add(BuildCurrentRuntimeCallExpr(unit_lowerer))},
                unit.builtins.void_type)});
  };
  mir::Block& block = *frame.current_block;
  on_the_report(block, support::BuiltinFn::kReadReportBeginEvaluation);
  mir::Block evaluated;
  auto value = lower_value(frame.WithBlock(&evaluated));
  if (!value) return std::unexpected(std::move(value.error()));
  const mir::ExprId answer = evaluated.AsValue(unit.types, *value);
  const mir::TypeId type = evaluated.exprs.Get(answer).type;
  const mir::LocalId held = DeclareLocal(
      frame, block.exprs.Add(BuildDefaultValueExpr(unit, block, type)));
  evaluated.AppendStmt(
      mir::ExprStmt{
          .expr = evaluated.exprs.Add(
              mir::MakeAssignExpr(
                  unit.builtins, evaluated.LocalPlace(held, type), answer))});
  mir::Block cleanup;
  on_the_report(cleanup, support::BuiltinFn::kReadReportEndEvaluation);
  block.AppendFinally(std::move(evaluated), std::move(cleanup));
  return block.ReadLocal(held, type);
}

// What an entry's leaves watch: the cells a value change reads, or the object
// an event named where the control was reached.
using EntryWatches =
    std::variant<std::span<const hir::SensitivityEntry>, HeldEvent>;

// What the watch of one entry reads where it is asked, evaluated in the
// closure's body (`now`); what it keeps before it is first asked, built where
// the wait is; what it keeps of what it read; and whether what it kept and what
// it reads now differ by an occurrence of the entry.
struct EntryReading {
  EntryWatches watches;
  mir::ExprId now;
  mir::ExprId kept_first;
  std::function<mir::ExprId(mir::Block&, mir::ExprId)> kept_of;
  std::function<mir::ExprId(mir::Block&, mir::LocalId, mir::LocalId)>
      occurred_between;
};

// What a watch keeps of a value it read where it keeps the value itself.
const auto kKeepsTheValue = [](mir::Block&, mir::ExprId value) {
  return value;
};

// A value change's reading: the expression's value, or the least significant
// bit of it where an edge was written, with every cell it reads and every place
// it reaches beyond them stated in the report where the waiting process decides
// the wait, as it reaches each.
template <ExprLowerer Lowerer>
auto ReadingOfValueChange(
    Lowerer& lowerer, const WalkFrame& frame, const WalkFrame& evaluation,
    const hir::ValueChange& change) -> diag::Result<EntryReading> {
  auto& unit = lowerer.Owner().Unit();
  mir::Block& block = *frame.current_block;
  const auto watched_part =
      [&](const WalkFrame& at) -> diag::Result<mir::ExprId> {
    auto value_or =
        lowerer.LowerExpr(lowerer.HirExprs().Get(change.signal), at);
    if (!value_or) return std::unexpected(std::move(value_or.error()));
    mir::Block& at_block = *at.current_block;
    return BuildWatchedPart(
        unit, at_block, change.edge, at_block.exprs.Add(*std::move(value_or)));
  };
  diag::Result<mir::ExprId> now = [&]() -> diag::Result<mir::ExprId> {
    if (!evaluation.reports_reached_to.has_value()) {
      return watched_part(evaluation);
    }
    const mir::LocalId reported_to = *evaluation.reports_reached_to;
    auto cells = ReportCells(lowerer, evaluation, reported_to, change.cells);
    if (!cells) return std::unexpected(std::move(cells.error()));
    return EvaluatedReporting(
        lowerer.Owner(), evaluation, reported_to, watched_part);
  }();
  if (!now) return std::unexpected(std::move(now.error()));
  const mir::TypeId type = evaluation.current_block->exprs.Get(*now).type;
  // A handle is watched for which object it names (LRM 9.4.2), and the watch
  // keeps that as a weak reference, so that watching an object does not keep
  // it (LRM 8.29): one that expires has changed into whatever is read next.
  if (const auto* handle =
          unit.types.Get(type).template As<mir::ManagedRefType>();
      handle != nullptr && change.edge == support::EventEdge::kAnyChange) {
    const mir::TypeId weak =
        unit.types.Intern(mir::Type{mir::WeakManagedRefType{handle->pointee}});
    return EntryReading{
        .watches = std::span<const hir::SensitivityEntry>{change.cells},
        .now = *now,
        .kept_first = block.exprs.Add(BuildDefaultValueExpr(unit, block, weak)),
        .kept_of =
            [weak](mir::Block& at, mir::ExprId value) {
              return at.exprs.Add(
                  mir::MakeCallExpr(
                      mir::Direct{
                          .target = support::BuiltinFn::kWeakDowngrade,
                          .receiver = value},
                      {}, weak));
            },
        .occurred_between =
            [unit = &unit, weak, type](
                mir::Block& at, mir::LocalId before, mir::LocalId now) {
              return BuildConditionNot(
                  *unit, at,
                  at.exprs.Add(
                      mir::MakeCallExpr(
                          mir::Direct{
                              .target = support::BuiltinFn::kWeakRefersTo,
                              .receiver = at.ReadLocal(before, weak)},
                          {at.ReadLocal(now, type)},
                          unit->builtins.machine_bool)));
            },
    };
  }
  return EntryReading{
      .watches = std::span<const hir::SensitivityEntry>{change.cells},
      .now = *now,
      .kept_first = block.exprs.Add(BuildDefaultValueExpr(unit, block, type)),
      .kept_of = kKeepsTheValue,
      .occurred_between =
          [unit = &unit, edge = change.edge, type](
              mir::Block& at, mir::LocalId before, mir::LocalId now) {
            return BuildEventOccurred(*unit, at, edge, before, now, type);
          },
  };
}

// An event's reading: how many times the object the event named where the
// control was reached has been triggered, which moves with each trigger (LRM
// 15.5.2). The event is evaluated once, there, and the closure holds a share
// of what it named, so the object lasts as long as the wait does (LRM
// 15.5.5.2) and an assignment to the event afterwards reaches only a later
// wait (LRM 15.5.5.1). Where the waiting process decides the wait, reading the
// count states the object in the report, so the wait is put on it.
auto ReadingOfHeldEvent(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, ClosureBuilder& closure,
    const WalkFrame& evaluation, mir::ExprId event) -> EntryReading {
  auto& unit = unit_lowerer.Unit();
  mir::Block& block = *frame.current_block;
  const mir::LocalId handle = DeclareLocal(frame, event);
  const BodyBindingRef held = SnapshotBindingIntoClosure(
      unit_lowerer, frame, closure,
      block.ReadLocal(handle, unit.builtins.event));
  mir::Block& body = closure.Body();
  const mir::TypeId count = unit.builtins.machine_int64;
  return EntryReading{
      .watches = HeldEvent{.handle = handle},
      .now = body.exprs.Add(
          mir::MakeCallExpr(
              mir::Direct{
                  .target = support::BuiltinFn::kEventOccurrences,
                  .receiver = body.exprs.Add(
                      closure.Bindings().MakeBindingPlace(held, body))},
              {BuildReportHandedOn(unit, evaluation)}, count)),
      .kept_first = mir::BuildMachineIntLiteral(unit, block, 0),
      .kept_of = kKeepsTheValue,
      .occurred_between =
          [unit = &unit, count](
              mir::Block& at, mir::LocalId before, mir::LocalId now) {
            return at.exprs.Add(
                mir::MakeBinary(
                    *unit, at, mir::BinaryOp::kInequality,
                    at.ReadLocal(now, count), at.ReadLocal(before, count),
                    unit->builtins.machine_bool));
          },
  };
}

template <ExprLowerer Lowerer>
auto ReadingOfNamedEventTrigger(
    Lowerer& lowerer, const WalkFrame& frame, ClosureBuilder& closure,
    const WalkFrame& evaluation, const hir::NamedEventTrigger& trigger)
    -> diag::Result<EntryReading> {
  auto event_or =
      lowerer.LowerExpr(lowerer.HirExprs().Get(trigger.event), frame);
  if (!event_or) return std::unexpected(std::move(event_or.error()));
  return ReadingOfHeldEvent(
      lowerer.Owner(), frame, closure, evaluation,
      frame.current_block->exprs.Add(*std::move(event_or)));
}

// One entry of an event control, armed where its wait is built: what its leaves
// watch, and the observation it is watched through (LRM 9.4.2), which every
// leaf of the entry names, since what is being watched for is one thing.
struct ArmedEntry {
  EntryWatches watches;
  mir::LocalId observation;
};

// Reads an entry's occurrence, in the closure that watches it and that
// closure's evaluation frame.
using OccurrenceReader = std::function<diag::Result<EntryReading>(
    ClosureBuilder& closure, const WalkFrame& evaluation)>;

// Arms an entry whose occurrence `read` reads. Its observation holds the
// entry's watch and, where the source wrote one, its `iff` qualifier
// `condition`. The watch keeps what it last read, and is asked either to arm
// -- to take what it reads now as that -- or whether an occurrence of the
// entry happened between what it keeps and what it reads now; either way what
// it read becomes what it keeps. It compares as what it reads does, so the
// runtime asking it knows no type.
//
// Where the waiting process decides the wait, `report` holds a pointer to the
// report each evaluation states what it reaches in.
template <ExprLowerer Lowerer>
auto ArmOccurrence(
    Lowerer& lowerer, WalkFrame frame, const OccurrenceReader& read,
    std::optional<hir::ExprId> condition, std::optional<mir::LocalId> report)
    -> diag::Result<ArmedEntry> {
  auto& unit = lowerer.Owner().Unit();
  mir::Block& block = *frame.current_block;

  ClosureBuilder closure(unit, frame, support::ClosureKind::kFnMut);
  const mir::LocalId arming = closure.AddParam(unit.builtins.machine_bool);
  const WalkFrame evaluation =
      ClosureFrameReportingTo(lowerer.Owner(), frame, closure, report);
  auto reading = read(closure, evaluation);
  if (!reading) return std::unexpected(std::move(reading.error()));
  mir::Block& body = closure.Body();
  const mir::TypeId watched = body.exprs.Get(reading->now).type;

  const BodyBindingRef previous = SnapshotBindingIntoClosure(
      lowerer.Owner(), frame, closure, reading->kept_first);
  const mir::LocalId now = DeclareLocal(evaluation, reading->now);
  const mir::LocalId before = DeclareLocal(
      evaluation,
      body.exprs.Add(closure.Bindings().MakeBindingPlace(previous, body)));
  const mir::LocalId occurred = DeclareLocal(
      evaluation,
      body.exprs.Add(
          mir::Expr{
              .data =
                  mir::ConditionalExpr{
                      .condition =
                          body.ReadLocal(arming, unit.builtins.machine_bool),
                      .then_value = BuildMachineBool(unit, body, false),
                      .else_value =
                          reading->occurred_between(body, before, now)},
              .type = unit.builtins.machine_bool}));
  body.AppendStmt(
      mir::ExprStmt{
          .expr = body.exprs.Add(
              mir::MakeAssignExpr(
                  unit.builtins,
                  body.exprs.Add(
                      closure.Bindings().MakeBindingPlace(previous, body)),
                  reading->kept_of(body, body.ReadLocal(now, watched))))});

  std::vector<mir::ExprId> arguments{BuildKeptClosure(
      unit, block,
      closure.Build(body.ReadLocal(occurred, unit.builtins.machine_bool)))};
  support::BuiltinFn observation = support::BuiltinFn::kObservationOfWatch;
  if (condition.has_value()) {
    auto qualifier = BuildConditionClosure(lowerer, frame, block, *condition);
    if (!qualifier) return std::unexpected(std::move(qualifier.error()));
    arguments.push_back(*qualifier);
    observation = support::BuiltinFn::kObservationOfWatchQualified;
  }
  return ArmedEntry{
      .watches = reading->watches,
      .observation = DeclareObservation(
          unit, frame, block, observation, std::move(arguments))};
}

// Arms `entry`, an entry of an event control the source wrote.
template <ExprLowerer Lowerer>
auto ArmEntry(
    Lowerer& lowerer, WalkFrame frame, const hir::EventEntry& entry,
    std::optional<mir::LocalId> report) -> diag::Result<ArmedEntry> {
  return ArmOccurrence(
      lowerer, frame,
      [&](ClosureBuilder& closure,
          const WalkFrame& evaluation) -> diag::Result<EntryReading> {
        return std::visit(
            Overloaded{
                [&](const hir::ValueChange& change) {
                  return ReadingOfValueChange(
                      lowerer, frame, evaluation, change);
                },
                [&](const hir::NamedEventTrigger& trigger) {
                  return ReadingOfNamedEventTrigger(
                      lowerer, frame, closure, evaluation, trigger);
                }},
            entry.occurrence);
      },
      entry.condition, report);
}

// LRM 9.4.1 `#N`. The wait is built at the stop by a free-function call whose
// argument vector states the runtime handle, the amount of time the design
// asked to wait, and the enclosing scope's time unit and precision powers (LRM
// 3.14.2); the runtime rounds that amount to the scope's precision (LRM
// 3.14.1) and scales it to the design-global tick (LRM 3.14.3). The amount is
// evaluated here, where the statement is reached, so a later write to anything
// it read does not reach a wait already under way.
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
  const mir::ExprId unit_power_id = mir::BuildMachineIntLiteral(
      unit, block, static_cast<std::int64_t>(process.Resolution().unit_power));
  const mir::ExprId precision_power_id = mir::BuildMachineIntLiteral(
      unit, block,
      static_cast<std::int64_t>(process.Resolution().precision_power));
  const support::BuiltinFn entry =
      is_real ? support::BuiltinFn::kDelayReal : support::BuiltinFn::kDelay;
  const mir::ExprId wait_id = block.exprs.Add(
      mir::MakeCallExpr(
          mir::Direct{.target = entry},
          {runtime_id, duration_id, unit_power_id, precision_power_id},
          unit.builtins.wait));
  return BuildStopStmt(process.Owner(), frame.WithBlock(&block), wait_id);
}

// The expressions an event control evaluates when what it watches moves: what
// each value change watches, and the qualifier gating each entry where the
// source wrote one (LRM 9.4.2, 9.4.2.3). An event is evaluated where the
// control is reached and not again (LRM 15.5.1, 15.5.5.1), so its qualifier is
// the whole of what its entry evaluates then.
auto EvaluatedBy(const hir::EventControl& ec) -> std::vector<hir::ExprId> {
  std::vector<hir::ExprId> evaluated;
  for (const hir::EventEntry& entry : ec.entries) {
    std::visit(
        Overloaded{
            [&](const hir::ValueChange& change) {
              evaluated.push_back(change.signal);
            },
            [](const hir::NamedEventTrigger&) {}},
        entry.occurrence);
    if (entry.condition.has_value()) {
      evaluated.push_back(*entry.condition);
    }
  }
  return evaluated;
}

// Who evaluates an event control's expressions when what it watches changes,
// which is the one question that separates the two ways an event control is
// built. The waiting process does (LRM 4.5), and that is what the control
// means. Where every one of them only reads storage, the change may evaluate
// them where it happens instead: a schedule LRM 4.7 permits, which nothing
// such an evaluation does can tell from the process evaluating them, and which
// spares resuming the process to learn that a change was no event.
auto ChangeDecides(
    const base::Arena<hir::Expr, hir::ExprId>& exprs,
    std::span<const hir::ExprId> evaluated) -> bool {
  return std::ranges::all_of(evaluated, [&](hir::ExprId expr) {
    return hir::ReadsStorageOnly(exprs, expr);
  });
}

// `entry` acting on the observation `observation` holds, answering `type`.
auto ObservationCall(
    const mir::CompilationUnit& unit, mir::Block& block,
    support::BuiltinFn entry, mir::LocalId observation, mir::TypeId type)
    -> mir::ExprId {
  const mir::ExprId held =
      block.LocalPlace(observation, unit.builtins.observation);
  return block.exprs.Add(
      mir::MakeCallExpr(
          mir::Direct{.target = entry, .receiver = held}, {}, type));
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
        mir::MakeBinary(
            unit, block, mir::BinaryOp::kBitwiseOr, fired, fires(observation),
            unit.builtins.machine_int64));
  }
  const mir::ExprId none = block.exprs.Add(
      mir::MakeBinary(
          unit, block, mir::BinaryOp::kEquality, fired,
          mir::BuildMachineIntLiteral(unit, block, 0),
          unit.builtins.machine_bool));
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
  const mir::ExprId held = block.LocalPlace(report, unit.builtins.read_report);
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
auto BuildProcessDecidedEventWaitStmt(
    Lowerer& lowerer, WalkFrame frame, mir::Block& block,
    const hir::EventControl& ec) -> diag::Result<mir::Stmt> {
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  // The reports are the wait's own and end where the wait does, so nothing an
  // evaluation read is held while the statement the control governs runs.
  mir::Block own;
  const WalkFrame own_frame = frame.WithBlock(&own);
  std::vector<mir::LocalId> reports;
  std::vector<mir::LocalId> observations;
  reports.reserve(ec.entries.size());
  observations.reserve(ec.entries.size());
  for (const hir::EventEntry& entry : ec.entries) {
    const mir::LocalId report = DeclareReport(unit, own_frame);
    auto armed = ArmEntry(lowerer, own_frame, entry, report);
    if (!armed) return std::unexpected(std::move(armed.error()));
    reports.push_back(report);
    observations.push_back(armed->observation);
  }

  mir::Block waiting;
  const mir::ExprId reports_id = LocalsAsArray(
      unit, waiting, unit.builtins.read_report_ptr, reports,
      [&](mir::LocalId report) {
        return waiting.ReadLocal(report, unit.builtins.read_report_ptr);
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
                waiting.LocalPlace(observation, unit.builtins.observation),
                observation_ptr));
      });
  const mir::ExprId wait_id = waiting.exprs.Add(
      mir::MakeCallExpr(
          mir::Direct{.target = support::BuiltinFn::kWaitRecollecting},
          {reports_id, observations_id}, unit.builtins.wait));
  waiting.AppendStmt(
      BuildStopStmt(lowerer.Owner(), frame.WithBlock(&waiting), wait_id));
  own.AppendStmt(
      WaitUntilOneFires(unit, own, observations, std::move(waiting)));
  return mir::Stmt{
      .label = std::nullopt,
      .data = mir::BlockStmt{.scope = block.child_scopes.Add(std::move(own))}};
}

// An event control a change decides (LRM 4.7, 9.4.2): one wait, built where
// everything it watches and `evaluated` -- the expressions its observations
// evaluate at the change -- exists, each leaf watching for what the
// observation of its entry decides.
template <ExprLowerer Lowerer>
auto BuildChangeDecidedEventWaitStmt(
    Lowerer& lowerer, WalkFrame frame, mir::Block& block,
    const hir::EventControl& ec, std::span<const hir::ExprId> evaluated)
    -> diag::Result<mir::Stmt> {
  std::vector<hir::SensitivityEntry> cells;
  bool evaluates_where_reached = false;
  for (const hir::EventEntry& entry : ec.entries) {
    std::visit(
        Overloaded{
            [&](const hir::ValueChange& change) {
              cells.insert(
                  cells.end(), change.cells.begin(), change.cells.end());
            },
            [&](const hir::NamedEventTrigger&) {
              evaluates_where_reached = true;
            }},
        entry.occurrence);
  }
  mir::Block& storage = WaitStorageBlock(
      frame, block, cells, lowerer.HirExprs(), evaluated,
      evaluates_where_reached);
  auto leaves = BuildEventLeaves(lowerer, frame.WithBlock(&storage), ec);
  if (!leaves) return std::unexpected(std::move(leaves.error()));
  return BuildWaitOnStmt(storage, block, frame, lowerer, *leaves);
}

}  // namespace

// LRM 15.5.1: triggering reaches RuntimeEffects to wake what waits on the
// object the event names. The engine handle rides after the event, the way
// every runtime effect threads it.
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

template <ExprLowerer Lowerer>
auto BuildEventObjectWaitStmt(
    Lowerer& lowerer, WalkFrame frame, mir::Block& block, mir::ExprId event)
    -> diag::Result<mir::Stmt> {
  auto armed = ArmOccurrence(
      lowerer, frame,
      [&](ClosureBuilder& closure,
          const WalkFrame& evaluation) -> diag::Result<EntryReading> {
        return ReadingOfHeldEvent(
            lowerer.Owner(), frame, closure, evaluation, event);
      },
      std::nullopt, std::nullopt);
  if (!armed) return std::unexpected(std::move(armed.error()));
  const std::array leaves{ObservedLeaf{
      .watched = std::get<HeldEvent>(armed->watches),
      .observation = armed->observation}};
  return BuildWaitOnStmt(block, block, frame, lowerer, leaves);
}

template auto BuildEventObjectWaitStmt(
    ProcessLowerer&, WalkFrame, mir::Block&, mir::ExprId)
    -> diag::Result<mir::Stmt>;

auto ChangeDecides(
    const base::Arena<hir::Expr, hir::ExprId>& exprs,
    const hir::EventControl& ec) -> bool {
  return ChangeDecides(exprs, EvaluatedBy(ec));
}

template <ExprLowerer Lowerer>
auto BuildEventLeaves(
    Lowerer& lowerer, const WalkFrame& frame, const hir::EventControl& ec)
    -> diag::Result<std::vector<ObservedLeaf>> {
  std::vector<ObservedLeaf> leaves;
  for (const hir::EventEntry& entry : ec.entries) {
    auto armed = ArmEntry(lowerer, frame, entry, std::nullopt);
    if (!armed) return std::unexpected(std::move(armed.error()));
    std::visit(
        Overloaded{
            [&](std::span<const hir::SensitivityEntry> cells) {
              for (const hir::SensitivityEntry& cell : cells) {
                leaves.push_back(
                    ObservedLeaf{
                        .watched = cell, .observation = armed->observation});
              }
            },
            [&](const HeldEvent& event) {
              leaves.push_back(
                  ObservedLeaf{
                      .watched = event, .observation = armed->observation});
            }},
        armed->watches);
  }
  return leaves;
}

template auto BuildEventLeaves(
    ProcessLowerer&, const WalkFrame&, const hir::EventControl&)
    -> diag::Result<std::vector<ObservedLeaf>>;
template auto BuildEventLeaves(
    const StructuralScopeLowerer&, const WalkFrame&, const hir::EventControl&)
    -> diag::Result<std::vector<ObservedLeaf>>;

template <ExprLowerer Lowerer>
auto BuildEventWaitStmt(
    Lowerer& lowerer, WalkFrame frame, mir::Block& block,
    const hir::EventControl& ec) -> diag::Result<mir::Stmt> {
  const std::vector<hir::ExprId> evaluated = EvaluatedBy(ec);
  if (ChangeDecides(lowerer.HirExprs(), evaluated)) {
    return BuildChangeDecidedEventWaitStmt(
        lowerer, frame, block, ec, evaluated);
  }
  return BuildProcessDecidedEventWaitStmt(lowerer, frame, block, ec);
}

template auto BuildEventWaitStmt(
    ProcessLowerer&, WalkFrame, mir::Block&, const hir::EventControl&)
    -> diag::Result<mir::Stmt>;
template auto BuildEventWaitStmt(
    const StructuralScopeLowerer&, WalkFrame, mir::Block&,
    const hir::EventControl&) -> diag::Result<mir::Stmt>;

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
                // LRM 9.4.2.2 `@*`: the standard makes the wait sensitive to
                // the variables the controlled statement reads rather than to
                // the value of an expression, so being reached is the whole of
                // the condition.
                [&](const hir::ImplicitEventControl& ie) {
                  return BuildValueChangeWaitStmt(
                      block, inner, process, ie.sensitivity_list);
                }},
            t.timing);
      });
}

// LRM 15.5.1 `-> e;` and `->> [ delay_or_event_control ] e;`. The nonblocking
// form is the trigger made a deferred effect, and like every one it reads what
// it needs where the statement is reached: the object the event names then is
// the one triggered where the effect lands, whatever the event names by then.
auto LowerEventTriggerStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::EventTriggerStmt& et) -> diag::Result<mir::Stmt> {
  auto& block = *frame.current_block;
  const hir::Expr& event_hir = process.HirBody().exprs.Get(et.event);

  auto effect_or = std::visit(
      Overloaded{
          [&](const hir::ImmediateEffect&) -> diag::Result<mir::Expr> {
            auto event_or = process.LowerAsPlace(event_hir, frame);
            if (!event_or) return std::unexpected(std::move(event_or.error()));
            return BuildTriggerCallExpr(
                process.Owner(), block, block.exprs.Add(*std::move(event_or)));
          },
          [&](const hir::NonBlockingEffect& deferred)
              -> diag::Result<mir::Expr> {
            return BuildDeferredEffect(
                process, frame, deferred.control,
                [&](ClosureBuilder& closure) -> diag::Result<mir::ExprId> {
                  auto event_or = process.LowerExpr(event_hir, frame);
                  if (!event_or) {
                    return std::unexpected(std::move(event_or.error()));
                  }
                  const BodyBindingRef held = SnapshotBindingIntoClosure(
                      process.Owner(), frame, closure,
                      block.exprs.Add(*std::move(event_or)));
                  return closure.Body().exprs.Add(
                      closure.Bindings().MakeBindingPlace(
                          held, closure.Body()));
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
  auto cond_id = EvaluatedReporting(
      process.Owner(), test_frame, report,
      [&](const WalkFrame& at) -> diag::Result<mir::ExprId> {
        auto cond_or = process.LowerExpr(hir_proc.exprs.Get(w.cond), at);
        if (!cond_or) return std::unexpected(std::move(cond_or.error()));
        return at.current_block->exprs.Add(*std::move(cond_or));
      });
  if (!cond_id) return std::unexpected(std::move(cond_id.error()));
  mir::Block& test_block = test.Body();
  const mir::ExprId not_yet = BuildConditionNot(
      unit, test_block, ReduceToCondition(unit, test_block, *cond_id));
  const mir::ExprId waiting_id = wrapper.exprs.Add(test.Build(not_yet));

  mir::Block inner_block;
  const std::array<mir::LocalId, 1> reports{report};
  const mir::ExprId reports_id = LocalsAsArray(
      unit, inner_block, unit.builtins.read_report_ptr, reports,
      [&](mir::LocalId reported) {
        return inner_block.ReadLocal(reported, unit.builtins.read_report_ptr);
      });
  const mir::ExprId wait_id = inner_block.exprs.Add(
      mir::MakeCallExpr(
          mir::Direct{.target = support::BuiltinFn::kWaitUntil}, {reports_id},
          unit.builtins.wait));
  inner_block.AppendStmt(BuildStopStmt(
      process.Owner(), wrapper_frame.WithBlock(&inner_block), wait_id));

  const mir::BlockId inner_scope_id =
      wrapper.child_scopes.Add(std::move(inner_block));

  wrapper.AppendStmt(
      mir::WhileStmt{.condition = waiting_id, .scope = inner_scope_id});

  // The report is the wait's and ends where the wait does, so nothing the last
  // test read is held while the body runs.
  mir::Block whole;
  whole.AppendStmt(
      mir::BlockStmt{.scope = whole.child_scopes.Add(std::move(wrapper))});
  const hir::Stmt& body_hir = hir_proc.stmts.Get(w.body);
  auto body_or = process.LowerStmt(body_hir, frame.WithBlock(&whole));
  if (!body_or) {
    return std::unexpected(std::move(body_or.error()));
  }
  whole.AppendStmt(*std::move(body_or));

  const mir::BlockId whole_scope_id =
      frame.current_block->child_scopes.Add(std::move(whole));

  return mir::Stmt{
      .label = std::move(label),
      .data = mir::BlockStmt{.scope = whole_scope_id}};
}

}  // namespace lyra::lowering::hir_to_mir
