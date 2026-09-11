#include "lyra/lowering/hir_to_mir/concurrent_assertion.hpp"

#include <cstddef>
#include <cstdint>
#include <expected>
#include <format>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/overloaded.hpp"
#include "lyra/lowering/hir_to_mir/assertion_automaton.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/closure_builder.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/sensitivity_wait.hpp"
#include "lyra/lowering/hir_to_mir/statement/assertions.hpp"
#include "lyra/lowering/hir_to_mir/statement/timing.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/evaluation_outcome.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto AssertionName(hir::ConcurrentAssertionId id, std::string_view part)
    -> std::string {
  return std::format("concurrent_assertion_{}__{}", id.value, part);
}

// A call on the storage holding one assertion's attempts. Every one of them is
// reached through its address, so the receiver is the member place and nothing
// reads it as a value.
auto AttemptsCall(
    mir::Block& block, support::BuiltinFn fn, mir::ExprId attempts,
    std::vector<mir::ExprId> arguments, mir::TypeId result) -> mir::ExprId {
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Direct{.target = fn, .receiver = attempts},
                  .arguments = std::move(arguments)},
          .type = result});
}

auto Word(mir::Block& block, mir::TypeId type, std::uint64_t value)
    -> mir::ExprId {
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::MachineIntLiteral{.value = static_cast<std::int64_t>(value)},
          .type = type});
}

auto Op(
    mir::Block& block, mir::TypeId type, mir::BinaryOp op, mir::ExprId lhs,
    mir::ExprId rhs) -> mir::ExprId {
  return block.exprs.Add(
      mir::Expr{
          .data = mir::BinaryExpr{.op = op, .lhs = lhs, .rhs = rhs},
          .type = type});
}

auto Read(mir::Block& block, mir::LocalId local, mir::TypeId type)
    -> mir::ExprId {
  return block.exprs.Add(mir::MakeLocalRefExpr(local, type));
}

void Assign(
    mir::Block& block, mir::LocalId local, mir::TypeId type,
    mir::ExprId value) {
  block.AppendStmt(
      mir::ExprStmt{
          .expr = block.exprs.Add(
              mir::MakeAssignExpr(Read(block, local, type), value, type))});
}

// One position set as body locals, one per word of the automaton's width. A
// set operation is then the same run of word operations wherever it appears,
// and a one-word automaton is that run at length one.
auto DeclareWords(
    CallableBindings& bindings, mir::Block& block, mir::TypeId type,
    std::string_view name, const PositionSet& initial)
    -> std::vector<mir::LocalId> {
  std::vector<mir::LocalId> locals;
  locals.reserve(initial.size());
  for (std::size_t word = 0; word < initial.size(); ++word) {
    const mir::LocalId local = bindings.DeclareAnonymous(
        mir::LocalDecl{.name = std::format("{}_{}", name, word), .type = type});
    block.AppendStmt(
        mir::LocalDeclStmt{
            .target = local, .init = Word(block, type, initial[word])});
    locals.push_back(local);
  }
  return locals;
}

// Whether any position of `mask` is set in `words`, as one predicate over every
// word of the set.
auto AnyOf(
    mir::Block& block, const mir::CompilationUnit& unit,
    const std::vector<mir::LocalId>& words, const PositionSet& mask)
    -> mir::ExprId {
  const mir::TypeId type = unit.builtins.machine_word;
  mir::ExprId combined = Word(block, type, 0);
  for (std::size_t word = 0; word < words.size(); ++word) {
    combined =
        Op(block, type, mir::BinaryOp::kBitwiseOr, combined,
           Op(block, type, mir::BinaryOp::kBitwiseAnd,
              Read(block, words[word], type), Word(block, type, mask[word])));
  }
  return Op(
      block, unit.builtins.machine_bool, mir::BinaryOp::kInequality, combined,
      Word(block, type, 0));
}

// `words |= mask`, one word at a time.
void OrInto(
    mir::Block& block, mir::TypeId type, const std::vector<mir::LocalId>& words,
    const PositionSet& mask) {
  for (std::size_t word = 0; word < words.size(); ++word) {
    Assign(
        block, words[word], type,
        Op(block, type, mir::BinaryOp::kBitwiseOr,
           Read(block, words[word], type), Word(block, type, mask[word])));
  }
}

void AppendIf(mir::Block& block, mir::ExprId condition, mir::Block then_block) {
  const mir::BlockId scope = block.child_scopes.Add(std::move(then_block));
  block.AppendStmt(
      mir::IfStmt{
          .condition = condition,
          .then_scope = scope,
          .else_scope = std::nullopt});
}

// The two dispositions a concurrent assertion carries, read as what this
// lowering needs of them: an assert or assume owes the tool's own report where
// the source wrote no fail statement, and a coverage goal owes none and demands
// a match of a result no tick settled (LRM 16.14.1, 16.14.3, 16.12.2).
struct AssertionParts {
  const hir::PropertySpec* spec = nullptr;
  std::optional<hir::StmtId> pass_stmt;
  std::optional<hir::StmtId> fail_stmt;
  std::optional<hir::AssertionDirective> reports_as;
  bool pending_holds = true;
};

auto PartsOf(const hir::ConcurrentAssertion& assertion) -> AssertionParts {
  return std::visit(
      Overloaded{
          [](const hir::ConcurrentAssertStmt& check) {
            return AssertionParts{
                .spec = &check.spec,
                .pass_stmt = check.pass_stmt,
                .fail_stmt = check.fail_stmt,
                .reports_as = check.directive,
                .pending_holds = true};
          },
          [](const hir::ConcurrentCoverStmt& cover) {
            return AssertionParts{
                .spec = &cover.spec,
                .pass_stmt = cover.pass_stmt,
                .fail_stmt = std::nullopt,
                .reports_as = std::nullopt,
                .pending_holds = false};
          }},
      assertion);
}

// What one outcome selects (LRM 16.14.1): the statements the source wrote for
// it, the tool's own report where a false result reached none, or nothing.
struct RunsStatements {
  hir::StmtId stmt;
};
struct RunsDefaultReport {
  hir::AssertionDirective directive;
};
struct RunsNothing {};
using ActionArm = std::variant<RunsStatements, RunsDefaultReport, RunsNothing>;

// A pass statement is what a true result selects, and nothing stands in for it.
auto PassArmOf(const hir::ProceduralBody& body, const AssertionParts& parts)
    -> ActionArm {
  if (HasRealArm(body, parts.pass_stmt)) {
    return RunsStatements{.stmt = *parts.pass_stmt};
  }
  return RunsNothing{};
}

// A fail statement takes the place of the tool's own report, which is what a
// false result selects where the source wrote none; a coverage goal has
// neither, because an attempt that does not succeed is not a failure
// (LRM 16.14.3).
auto FailArmOf(const hir::ProceduralBody& body, const AssertionParts& parts)
    -> ActionArm {
  if (HasRealArm(body, parts.fail_stmt)) {
    return RunsStatements{.stmt = *parts.fail_stmt};
  }
  if (parts.reports_as.has_value()) {
    return RunsDefaultReport{.directive = *parts.reports_as};
  }
  return RunsNothing{};
}

// One arm of the action block as a callable of its own. Both arms are the same
// for every attempt -- an assertion local variable, which is what would make
// two attempts' statements differ, is refused where it is read -- so an outcome
// selects a call and never a per-attempt closure.
auto LowerActionArm(
    ProcessLowerer& action, const WalkFrame& parent, const ActionArm& arm,
    diag::SourceSpan span) -> diag::Result<mir::CallableCode> {
  mir::CompilationUnit& unit = action.Owner().Unit();
  mir::CallableCode code = mir::CallableCode::Defined();
  CallableBindings bindings(unit, code);
  const mir::LocalId self_id = bindings.Declare(
      BindingOriginId::Receiver(),
      mir::LocalDecl{
          .name = "self", .type = parent.current_class->self_pointer_type});
  code.params = {self_id};
  code.result_type = unit.builtins.void_type;

  const WalkFrame frame =
      parent.WithBlock(&code.Body())
          .WithBindings(&bindings)
          .WithScopeNameBorrowedHandle(action.RootScope().NameBorrowedHandle());

  auto appended = std::visit(
      Overloaded{
          [&](const RunsStatements& statements) -> diag::Result<void> {
            auto lowered = action.LowerStmt(
                action.HirBody().stmts.Get(statements.stmt), frame);
            if (!lowered) return std::unexpected(std::move(lowered.error()));
            code.Body().AppendStmt(*std::move(lowered));
            return {};
          },
          [&](const RunsDefaultReport& report) -> diag::Result<void> {
            AppendDefaultReport(action, code.Body(), report.directive, span);
            return {};
          },
          [](const RunsNothing&) -> diag::Result<void> { return {}; }},
      arm);
  if (!appended) return std::unexpected(std::move(appended.error()));
  code.Body().AppendStmt(mir::ReturnStmt{.value = std::nullopt});
  return code;
}

// What the tick left one evaluation in, as the answer the generated transition
// hands back (Annex F.5.3.2). Which positions carry something the attempt owes
// and which carry an antecedent is settled where the automaton was built, so
// this is a chain of constant mask tests and nothing the runtime decides.
auto BuildOutcome(
    mir::Block& block, const mir::CompilationUnit& unit,
    const AssertionAutomaton& automaton, const std::vector<mir::LocalId>& bits,
    const std::vector<mir::LocalId>& matched,
    const std::vector<mir::LocalId>& next) -> mir::ExprId {
  const mir::TypeId type = unit.builtins.machine_word;
  const auto answer = [&](support::EvaluationOutcome outcome) {
    return Word(block, type, static_cast<std::uint64_t>(outcome));
  };
  const mir::ExprId exhausted = Op(
      block, unit.builtins.machine_bool, mir::BinaryOp::kEquality,
      [&] {
        mir::ExprId combined = Word(block, type, 0);
        for (const mir::LocalId word : next) {
          combined =
              Op(block, type, mir::BinaryOp::kBitwiseOr, combined,
                 Read(block, word, type));
        }
        return combined;
      }(),
      Word(block, type, 0));

  const mir::ExprId as_obligation = block.exprs.Add(
      mir::Expr{
          .data =
              mir::ConditionalExpr{
                  .condition =
                      AnyOf(block, unit, matched, automaton.satisfying),
                  .then_value = answer(support::EvaluationOutcome::kSatisfied),
                  .else_value = block.exprs.Add(
                      mir::Expr{
                          .data =
                              mir::ConditionalExpr{
                                  .condition = exhausted,
                                  .then_value = answer(
                                      support::EvaluationOutcome::kFailed),
                                  .else_value = answer(
                                      support::EvaluationOutcome::kLive)},
                          .type = type})},
          .type = type});

  const mir::ExprId as_trigger = block.exprs.Add(
      mir::Expr{
          .data =
              mir::ConditionalExpr{
                  .condition = exhausted,
                  .then_value = answer(support::EvaluationOutcome::kVacuous),
                  .else_value = answer(support::EvaluationOutcome::kLive)},
          .type = type});

  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::ConditionalExpr{
                  .condition = AnyOf(block, unit, bits, automaton.trigger),
                  .then_value = as_trigger,
                  .else_value = as_obligation},
          .type = type});
}

// The whole of one tick, as the callable an Observed submission runs. Reading
// the design's Boolean expressions and stepping the positions belong to one
// body because the second bounds the first: only a position some live
// evaluation can reach this tick is read, and each is read once however many
// evaluations reach it.
auto LowerAdvance(
    ProcessLowerer& action, const WalkFrame& parent,
    const StructuralScopeLowerer& lowerer, hir::ConcurrentAssertionId id,
    const hir::PropertySpec& spec, const AssertionAutomaton& automaton)
    -> diag::Result<mir::CallableCode> {
  mir::CompilationUnit& unit = action.Owner().Unit();
  const mir::TypeId word_type = unit.builtins.machine_word;
  const mir::TypeId index_type = unit.builtins.machine_int64;
  const mir::TypeId void_type = unit.builtins.void_type;
  const PositionSet empty(automaton.words, 0);

  mir::CallableCode code = mir::CallableCode::Defined();
  CallableBindings bindings(unit, code);
  const mir::LocalId self_id = bindings.Declare(
      BindingOriginId::Receiver(),
      mir::LocalDecl{
          .name = "self", .type = parent.current_class->self_pointer_type});
  code.params = {self_id};
  code.result_type = void_type;

  mir::Block& body = code.Body();
  const WalkFrame frame =
      parent.WithBlock(&body)
          .WithBindings(&bindings)
          .WithScopeNameBorrowedHandle(action.RootScope().NameBorrowedHandle());
  const auto call = [&](mir::Block& block, support::BuiltinFn fn,
                        std::vector<mir::ExprId> arguments,
                        mir::TypeId result) {
    return AttemptsCall(
        block, fn, BuildEvaluationAttemptsExpr(block, frame, lowerer, id),
        std::move(arguments), result);
  };
  const auto call_stmt = [&](mir::Block& block, support::BuiltinFn fn,
                             std::vector<mir::ExprId> arguments) {
    block.AppendStmt(
        mir::ExprStmt{
            .expr = call(block, fn, std::move(arguments), void_type)});
  };
  const auto bit_at = [&](std::size_t position) {
    return PositionSetOf(static_cast<std::uint32_t>(position), automaton.words);
  };

  // LRM 16.12: an attempt that would begin at a tick where the disable
  // condition already holds is disabled at its own start. One already in
  // flight is preempted where the condition changes instead, because the
  // interval it is tested across is not made of ticks.
  if (spec.disable.has_value()) {
    auto condition = action.LowerExpr(
        action.HirBody().exprs.Get(spec.disable->condition), frame);
    if (!condition) return std::unexpected(std::move(condition.error()));
    const mir::ExprId reduced =
        ReduceToCondition(unit, body, body.exprs.Add(*std::move(condition)));
    mir::Block preempted;
    call_stmt(
        preempted, support::BuiltinFn::kEvaluationAttemptsDisableTick, {});
    preempted.AppendStmt(mir::ReturnStmt{.value = std::nullopt});
    AppendIf(body, reduced, std::move(preempted));
  }

  // LRM 16.14.5: an attempt begins at every tick, the enabling condition of an
  // assertion nothing has to reach being 1.
  for (std::uint32_t word = 0; word < automaton.words; ++word) {
    call_stmt(
        body, support::BuiltinFn::kEvaluationAttemptsSeedWord,
        {Word(body, word_type, word),
         Word(body, word_type, automaton.start[word])});
  }
  call_stmt(body, support::BuiltinFn::kEvaluationAttemptsBeginTick, {});

  // What this tick has to read: every position an unstepped evaluation is live
  // at, and the ones an implication would seed at this same tick. Seeds are
  // recorded outermost-first, so one forward pass over them is the whole
  // cascade however deep the implications nest.
  const std::vector<mir::LocalId> need =
      DeclareWords(bindings, body, word_type, "need", empty);
  for (std::uint32_t word = 0; word < automaton.words; ++word) {
    Assign(
        body, need[word], word_type,
        call(
            body, support::BuiltinFn::kEvaluationAttemptsLiveWord,
            {Word(body, word_type, word)}, word_type));
  }
  for (const ImplicationSeed& seed : automaton.seeds) {
    if (!seed.same_tick) {
      continue;
    }
    mir::Block reached;
    OrInto(reached, word_type, need, seed.consequent_start);
    AppendIf(
        body, AnyOf(body, unit, need, seed.antecedent_last),
        std::move(reached));
  }

  // The design's own Boolean expressions, as of the Preponed region of this
  // tick's time step (LRM 16.5.1), and only where something waits on them.
  const std::vector<mir::LocalId> hold =
      DeclareWords(bindings, body, word_type, "hold", empty);
  for (std::size_t position = 0; position < automaton.positions.size();
       ++position) {
    const PositionSet at = bit_at(position);
    mir::Block reached;
    const WalkFrame sampled =
        frame.WithBlock(&reached).WithReadsAsOf(ReadsAsOf::kPreponed);
    std::optional<mir::ExprId> condition;
    for (const hir::ExprId conjunct : automaton.positions[position].conjuncts) {
      auto lowered =
          action.LowerExpr(action.HirBody().exprs.Get(conjunct), sampled);
      if (!lowered) return std::unexpected(std::move(lowered.error()));
      const mir::ExprId reduced = ReduceToCondition(
          unit, reached, reached.exprs.Add(*std::move(lowered)));
      condition = condition.has_value()
                      ? Op(reached, unit.builtins.machine_bool,
                           mir::BinaryOp::kLogicalAnd, *condition, reduced)
                      : reduced;
    }
    // A position with no conjunct is a tick a delay measures across, which
    // matches whatever the design does there, so its bit is set outright.
    if (condition.has_value()) {
      mir::Block held;
      OrInto(held, word_type, hold, at);
      AppendIf(reached, *condition, std::move(held));
    } else {
      OrInto(reached, word_type, hold, at);
    }
    AppendIf(body, AnyOf(body, unit, need, at), std::move(reached));
  }

  // Every evaluation this tick has not stepped, including the ones an
  // overlapped implication seeds during the sweep, which is what reads a
  // consequent at the very tick its antecedent matched (LRM 16.12.7).
  const mir::LocalId cursor = bindings.DeclareAnonymous(
      mir::LocalDecl{.name = "evaluation", .type = index_type});
  body.AppendStmt(
      mir::LocalDeclStmt{
          .target = cursor,
          .init = call(
              body, support::BuiltinFn::kEvaluationAttemptsNextUnstepped, {},
              index_type)});

  mir::Block sweep;
  const std::vector<mir::LocalId> bits =
      DeclareWords(bindings, sweep, word_type, "live", empty);
  for (std::uint32_t word = 0; word < automaton.words; ++word) {
    Assign(
        sweep, bits[word], word_type,
        call(
            sweep, support::BuiltinFn::kEvaluationAttemptsBitsAt,
            {Read(sweep, cursor, index_type), Word(sweep, word_type, word)},
            word_type));
  }
  const std::vector<mir::LocalId> matched =
      DeclareWords(bindings, sweep, word_type, "matched", empty);
  for (std::uint32_t word = 0; word < automaton.words; ++word) {
    Assign(
        sweep, matched[word], word_type,
        Op(sweep, word_type, mir::BinaryOp::kBitwiseAnd,
           Read(sweep, bits[word], word_type),
           Read(sweep, hold[word], word_type)));
  }
  const std::vector<mir::LocalId> next =
      DeclareWords(bindings, sweep, word_type, "next", empty);
  for (std::size_t position = 0; position < automaton.positions.size();
       ++position) {
    mir::Block advanced;
    OrInto(advanced, word_type, next, automaton.follow[position]);
    AppendIf(
        sweep, AnyOf(sweep, unit, matched, bit_at(position)),
        std::move(advanced));
  }
  for (std::uint32_t word = 0; word < automaton.words; ++word) {
    call_stmt(
        sweep, support::BuiltinFn::kEvaluationAttemptsSetWord,
        {Read(sweep, cursor, index_type), Word(sweep, word_type, word),
         Read(sweep, next[word], word_type)});
  }
  call_stmt(
      sweep, support::BuiltinFn::kEvaluationAttemptsStep,
      {Read(sweep, cursor, index_type),
       BuildOutcome(sweep, unit, automaton, bits, matched, next)});
  for (const ImplicationSeed& seed : automaton.seeds) {
    mir::Block seeded;
    for (std::uint32_t word = 0; word < automaton.words; ++word) {
      call_stmt(
          seeded, support::BuiltinFn::kEvaluationAttemptsSeedWord,
          {Word(seeded, word_type, word),
           Word(seeded, word_type, seed.consequent_start[word])});
    }
    call_stmt(
        seeded, support::BuiltinFn::kEvaluationAttemptsSeed,
        {Read(seeded, cursor, index_type),
         seeded.exprs.Add(
             mir::Expr{
                 .data = mir::MachineBoolLiteral{.value = seed.same_tick},
                 .type = unit.builtins.machine_bool})});
    AppendIf(
        sweep, AnyOf(sweep, unit, matched, seed.antecedent_last),
        std::move(seeded));
  }
  Assign(
      sweep, cursor, index_type,
      call(
          sweep, support::BuiltinFn::kEvaluationAttemptsNextUnstepped, {},
          index_type));
  body.AppendStmt(
      mir::WhileStmt{
          .condition =
              Op(body, unit.builtins.machine_bool, mir::BinaryOp::kGreaterEqual,
                 Read(body, cursor, index_type), Word(body, index_type, 0)),
          .scope = body.child_scopes.Add(std::move(sweep))});

  call_stmt(
      body, support::BuiltinFn::kEvaluationAttemptsSettle,
      {body.exprs.Add(BuildCurrentRuntimeCallExpr(action.Owner()))});
  body.AppendStmt(mir::ReturnStmt{.value = std::nullopt});
  return code;
}

// LRM 16.12 disables an evaluation attempt if the disable condition is true at
// any time between the start of that attempt and its end, both inclusive. That
// interval is not made of ticks, so the condition is watched for change rather
// than read at the clock: a reset that rises and falls between two ticks is low
// at every tick an attempt reads, and still preempts every attempt it spans.
auto LowerDisableWatcher(
    ProcessLowerer& action, const WalkFrame& ctor_frame,
    const StructuralScopeLowerer& lowerer, hir::ConcurrentAssertionId id,
    const hir::DisableCondition& disable) -> diag::Result<mir::CallableDecl> {
  mir::CompilationUnit& unit = action.Owner().Unit();
  const mir::TypeId void_type = unit.builtins.void_type;

  mir::CallableCode code = mir::CallableCode::Defined();
  CallableBindings bindings(unit, code);
  const mir::LocalId self_id = bindings.Declare(
      BindingOriginId::Receiver(),
      mir::LocalDecl{
          .name = "self", .type = ctor_frame.current_class->self_pointer_type});

  mir::Block body_block;
  const WalkFrame body_frame =
      ctor_frame.WithBindings(&bindings).WithBlock(&body_block);
  body_block.AppendStmt(BuildValueChangeWaitStmt(
      body_block, body_frame, lowerer, disable.sensitivity));

  auto condition = action.LowerExpr(
      action.HirBody().exprs.Get(disable.condition), body_frame);
  if (!condition) return std::unexpected(std::move(condition.error()));
  const mir::ExprId reduced = ReduceToCondition(
      unit, body_block, body_block.exprs.Add(*std::move(condition)));

  mir::Block preempted;
  preempted.AppendStmt(
      mir::ExprStmt{
          .expr = AttemptsCall(
              preempted, support::BuiltinFn::kEvaluationAttemptsDisableTick,
              BuildEvaluationAttemptsExpr(preempted, body_frame, lowerer, id),
              {}, void_type)});
  AppendIf(body_block, reduced, std::move(preempted));

  const mir::BlockId body_scope_id =
      code.Body().child_scopes.Add(std::move(body_block));
  code.Body().AppendStmt(
      mir::ForStmt{
          .init = {},
          .condition = std::nullopt,
          .step = {},
          .scope = body_scope_id});
  code.Body().AppendStmt(mir::ReturnStmt{.value = std::nullopt});
  code.params = {self_id};
  code.result_type = unit.builtins.coroutine_void;
  return mir::CallableDecl{
      .name = AssertionName(id, "disable"),
      .code = std::move(code),
      .foreign = std::nullopt,
      .virtual_dispatch = std::nullopt};
}

// LRM 16.14.5 gives a declarative concurrent assertion always semantics, and
// LRM 16.5 evaluates it in the Observed region. Those are two facts about
// different things: the process waits wherever any value-change wait waits, and
// what the tick has to do goes to Observed as a deferred effect -- so nothing
// in the engine learns what a concurrent assertion is.
auto LowerProcess(
    ProcessLowerer& action, const StructuralScopeLowerer& lowerer,
    const WalkFrame& ctor_frame, hir::ConcurrentAssertionId id,
    const hir::EventControl& clock, mir::CallableId advance)
    -> diag::Result<mir::CallableDecl> {
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  const mir::TypeId void_type = unit.builtins.void_type;
  const mir::TypeId self_ptr_type = ctor_frame.current_class->self_pointer_type;

  mir::CallableCode code = mir::CallableCode::Defined();
  CallableBindings bindings(unit, code);
  const mir::LocalId self_id = bindings.Declare(
      BindingOriginId::Receiver(),
      mir::LocalDecl{.name = "self", .type = self_ptr_type});

  mir::Block body_block;
  const WalkFrame body_frame =
      ctor_frame.WithBindings(&bindings).WithBlock(&body_block);

  // The clock's own expression lives in the assertion's body, beside the
  // Booleans the property reads, so it is that body's lowering that resolves
  // it.
  auto wait =
      BuildEventWaitStmt(action, lowerer, body_frame, body_block, clock);
  if (!wait) return std::unexpected(std::move(wait.error()));
  body_block.AppendStmt(*std::move(wait));

  ClosureBuilder closure(unit, body_frame);
  closure.Body().AppendStmt(
      mir::ExprStmt{
          .expr = closure.Body().exprs.Add(
              mir::Expr{
                  .data =
                      mir::CallExpr{
                          .callee =
                              mir::Direct{
                                  .target =
                                      mir::CallableTarget{
                                          .owner = body_frame.current_class_id,
                                          .slot = advance},
                                  .receiver =
                                      closure.Body().exprs.Add(MakeSelfRefExpr(
                                          closure.Frame(), self_ptr_type))},
                          .arguments = {}},
                  .type = void_type})});

  const mir::ExprId closure_id = body_block.exprs.Add(closure.BuildVoid());
  const mir::ExprId runtime_id =
      body_block.exprs.Add(BuildCurrentRuntimeCallExpr(lowerer.Owner()));
  body_block.AppendStmt(
      mir::ExprStmt{
          .expr = body_block.exprs.Add(
              mir::Expr{
                  .data =
                      mir::CallExpr{
                          .callee =
                              mir::Direct{
                                  .target = support::BuiltinFn::kSubmitObserved,
                                  .receiver = runtime_id},
                          .arguments = {closure_id}},
                  .type = void_type})});

  const mir::BlockId body_scope_id =
      code.Body().child_scopes.Add(std::move(body_block));
  code.Body().AppendStmt(
      mir::ForStmt{
          .init = {},
          .condition = std::nullopt,
          .step = {},
          .scope = body_scope_id});
  code.Body().AppendStmt(mir::ReturnStmt{.value = std::nullopt});
  code.params = {self_id};
  code.result_type = unit.builtins.coroutine_void;
  return mir::CallableDecl{
      .name = AssertionName(id, "process"),
      .code = std::move(code),
      .foreign = std::nullopt,
      .virtual_dispatch = std::nullopt};
}

}  // namespace

auto BuildEvaluationAttemptsExpr(
    mir::Block& block, const WalkFrame& frame,
    const StructuralScopeLowerer& lowerer, hir::ConcurrentAssertionId id)
    -> mir::ExprId {
  const mir::FieldId field = lowerer.TranslateConcurrentAssertion(id);
  const mir::ExprId self = block.exprs.Add(
      MakeSelfRefExpr(frame, frame.current_class->self_pointer_type));
  return block.exprs.Add(
      mir::MakeFieldAccessExpr(
          self,
          mir::ClassFieldTarget{.owner = frame.current_class_id, .slot = field},
          frame.current_class->fields.Get(field).type));
}

auto LowerConcurrentAssertion(
    StructuralScopeLowerer& lowerer, mir::Class& mir_class,
    const WalkFrame& ctor_frame, const DeclaredScopes& scopes,
    hir::ConcurrentAssertionId id, const hir::ConcurrentAssertionDecl& decl)
    -> diag::Result<InstalledConcurrentAssertion> {
  const AssertionParts parts = PartsOf(decl.assertion);
  const AssertionAutomaton automaton =
      BuildAssertionAutomaton(decl.action, *parts.spec);

  ProcessLowerer action(
      lowerer.Owner(), &lowerer, lowerer.HirScope().time_resolution,
      decl.action, std::nullopt, AssertionName(id, "action"), ctor_frame,
      scopes, {});

  auto pass = LowerActionArm(
      action, ctor_frame, PassArmOf(decl.action, parts), decl.span);
  if (!pass) return std::unexpected(std::move(pass.error()));

  auto fail = LowerActionArm(
      action, ctor_frame, FailArmOf(decl.action, parts), decl.span);
  if (!fail) return std::unexpected(std::move(fail.error()));

  auto advance =
      LowerAdvance(action, ctor_frame, lowerer, id, *parts.spec, automaton);
  if (!advance) return std::unexpected(std::move(advance.error()));

  const mir::CallableId pass_id = mir_class.callables.Add(
      mir::CallableDecl{
          .name = AssertionName(id, "pass"),
          .code = *std::move(pass),
          .foreign = std::nullopt,
          .virtual_dispatch = std::nullopt});
  const mir::CallableId fail_id = mir_class.callables.Add(
      mir::CallableDecl{
          .name = AssertionName(id, "fail"),
          .code = *std::move(fail),
          .foreign = std::nullopt,
          .virtual_dispatch = std::nullopt});
  const mir::CallableId advance_id = mir_class.callables.Add(
      mir::CallableDecl{
          .name = AssertionName(id, "advance"),
          .code = *std::move(advance),
          .foreign = std::nullopt,
          .virtual_dispatch = std::nullopt});

  auto process = LowerProcess(
      action, lowerer, ctor_frame, id, parts.spec->clock, advance_id);
  if (!process) return std::unexpected(std::move(process.error()));

  std::vector<mir::CallableId> processes;
  processes.push_back(mir_class.callables.Add(*std::move(process)));
  if (parts.spec->disable.has_value()) {
    auto watcher = LowerDisableWatcher(
        action, ctor_frame, lowerer, id, *parts.spec->disable);
    if (!watcher) return std::unexpected(std::move(watcher.error()));
    processes.push_back(mir_class.callables.Add(*std::move(watcher)));
  }

  return InstalledConcurrentAssertion{
      .processes = std::move(processes),
      .pass_action = pass_id,
      .fail_action = fail_id,
      .words = automaton.words,
      .pending_holds = parts.pending_holds};
}

void AppendConcurrentAssertionInstall(
    const StructuralScopeLowerer& lowerer, const WalkFrame& activate_frame,
    hir::ConcurrentAssertionId id,
    const InstalledConcurrentAssertion& installed) {
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  mir::Block& block = *activate_frame.current_block;
  const mir::TypeId void_type = unit.builtins.void_type;
  const mir::TypeId self_ptr_type =
      activate_frame.current_class->self_pointer_type;

  // Both arms are the same for every attempt, so the storage holds one of each
  // and calls it once per attempt an outcome selects rather than being handed a
  // closure per attempt.
  const auto arm = [&](mir::CallableId target) {
    ClosureBuilder closure(unit, activate_frame);
    closure.Body().AppendStmt(
        mir::ExprStmt{
            .expr = closure.Body().exprs.Add(
                mir::Expr{
                    .data =
                        mir::CallExpr{
                            .callee =
                                mir::Direct{
                                    .target =
                                        mir::CallableTarget{
                                            .owner =
                                                activate_frame.current_class_id,
                                            .slot = target},
                                    .receiver = closure.Body().exprs.Add(
                                        MakeSelfRefExpr(
                                            closure.Frame(), self_ptr_type))},
                            .arguments = {}},
                    .type = void_type})});
    return block.exprs.Add(closure.BuildVoid());
  };

  block.AppendStmt(
      mir::ExprStmt{
          .expr = AttemptsCall(
              block, support::BuiltinFn::kEvaluationAttemptsInstall,
              BuildEvaluationAttemptsExpr(block, activate_frame, lowerer, id),
              {block.exprs.Add(BuildCurrentRuntimeCallExpr(lowerer.Owner())),
               Word(block, unit.builtins.machine_word, installed.words),
               block.exprs.Add(
                   mir::Expr{
                       .data =
                           mir::MachineBoolLiteral{
                               .value = installed.pending_holds},
                       .type = unit.builtins.machine_bool}),
               arm(installed.pass_action), arm(installed.fail_action)},
              void_type)});
}

}  // namespace lyra::lowering::hir_to_mir
