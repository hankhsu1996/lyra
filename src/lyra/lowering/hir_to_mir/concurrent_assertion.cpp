#include "lyra/lowering/hir_to_mir/concurrent_assertion.hpp"

#include <cstddef>
#include <cstdint>
#include <expected>
#include <functional>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/diag_message.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/hir/case_keyword.hpp"
#include "lyra/hir/timing.hpp"
#include "lyra/lowering/hir_to_mir/assertion_automaton.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/closure_builder.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/declaration_initializer.hpp"
#include "lyra/lowering/hir_to_mir/predicate.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/sensitivity_wait.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/lowering/hir_to_mir/statement/assertions.hpp"
#include "lyra/lowering/hir_to_mir/statement/blocks.hpp"
#include "lyra/lowering/hir_to_mir/statement/timing.hpp"
#include "lyra/lowering/hir_to_mir/switch_stmt.hpp"
#include "lyra/lowering/hir_to_mir/value_operations.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/callable_bounds.hpp"
#include "lyra/support/property_answer.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

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
    const mir::CompilationUnit& unit, mir::Block& block, mir::TypeId type,
    mir::BinaryOp op, mir::ExprId lhs, mir::ExprId rhs) -> mir::ExprId {
  return block.exprs.Add(mir::MakeBinary(unit, block, op, lhs, rhs, type));
}

void Assign(
    const mir::CompilationUnit& unit, mir::Block& block, mir::LocalId local,
    mir::TypeId type, mir::ExprId value) {
  block.AppendStmt(
      mir::ExprStmt{
          .expr = block.exprs.Add(
              mir::MakeAssignExpr(
                  unit.builtins, block.LocalPlace(local, type), value))});
}

// One position set as body locals, one per word of the automaton's width. A
// set operation is then the same sequence of word operations wherever it
// appears, and a one-word automaton is that sequence at length one.
auto DeclareWords(
    CallableBindings& bindings, mir::Block& block, mir::TypeId type,
    const PositionSet& initial) -> std::vector<mir::LocalId> {
  std::vector<mir::LocalId> locals;
  locals.reserve(initial.size());
  for (const auto& word : initial) {
    const mir::LocalId local = bindings.DeclareAnonymous(type);
    block.AppendStmt(
        mir::LocalDeclStmt{.target = local, .init = Word(block, type, word)});
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
    combined = Op(
        unit, block, type, mir::BinaryOp::kBitwiseOr, combined,
        Op(unit, block, type, mir::BinaryOp::kBitwiseAnd,
           block.ReadLocal(words[word], type), Word(block, type, mask[word])));
  }
  return Op(
      unit, block, unit.builtins.machine_bool, mir::BinaryOp::kInequality,
      combined, Word(block, type, 0));
}

// `words |= mask`, one word at a time.
void OrInto(
    const mir::CompilationUnit& unit, mir::Block& block, mir::TypeId type,
    const std::vector<mir::LocalId>& words, const PositionSet& mask) {
  for (std::size_t word = 0; word < words.size(); ++word) {
    Assign(
        unit, block, words[word], type,
        Op(unit, block, type, mir::BinaryOp::kBitwiseOr,
           block.ReadLocal(words[word], type), Word(block, type, mask[word])));
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
// the source wrote no fail statement, and a coverage goal owes none (LRM
// 16.14.1, 16.14.3).
struct AssertionDispositions {
  const hir::PropertySpec* spec = nullptr;
  std::optional<hir::StmtId> pass_stmt;
  std::optional<hir::StmtId> fail_stmt;
  std::optional<hir::AssertionDirective> reports_as;
};

auto DispositionsOf(const hir::ConcurrentAssertion& assertion)
    -> AssertionDispositions {
  return std::visit(
      Overloaded{
          [](const hir::ConcurrentAssertStmt& check) {
            return AssertionDispositions{
                .spec = &check.spec,
                .pass_stmt = check.pass_stmt,
                .fail_stmt = check.fail_stmt,
                .reports_as = check.directive};
          },
          [](const hir::ConcurrentCoverStmt& cover) {
            return AssertionDispositions{
                .spec = &cover.spec,
                .pass_stmt = cover.pass_stmt,
                .fail_stmt = std::nullopt,
                .reports_as = std::nullopt};
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
auto PassArmOf(
    const hir::ProceduralBody& body, const AssertionDispositions& dispositions)
    -> ActionArm {
  if (HasRealArm(body, dispositions.pass_stmt)) {
    return RunsStatements{.stmt = *dispositions.pass_stmt};
  }
  return RunsNothing{};
}

// A fail statement takes the place of the tool's own report, which is what a
// false result selects where the source wrote no else clause (LRM 16.14.1); a
// null one runs nothing. A coverage goal has neither, because an attempt that
// does not succeed is not a failure (LRM 16.14.3).
auto FailArmOf(
    const hir::ProceduralBody& body, const AssertionDispositions& dispositions)
    -> ActionArm {
  if (dispositions.fail_stmt.has_value()) {
    if (HasRealArm(body, dispositions.fail_stmt)) {
      return RunsStatements{.stmt = *dispositions.fail_stmt};
    }
    return RunsNothing{};
  }
  if (dispositions.reports_as.has_value()) {
    return RunsDefaultReport{.directive = *dispositions.reports_as};
  }
  return RunsNothing{};
}

// One arm of the action block as a callable of its own, answering the execution
// it runs as: an arm is any procedural statement, so it may stop and resume
// (LRM 16.3, 16.14.1). Both arms are the same for every attempt -- an assertion
// local variable, which is what would make two attempts' statements differ, is
// refused where it is read -- so an outcome starts one of two bodies and never
// a per-attempt closure.
auto LowerActionArm(
    ProcessLowerer& action, const WalkFrame& parent, const ActionArm& arm,
    diag::SourceSpan span) -> diag::Result<mir::CallableCode> {
  mir::CompilationUnit& unit = action.Owner().Unit();
  mir::CallableCode code = mir::CallableCode::Defined();
  CallableBindings bindings(unit, code);
  const mir::LocalId self_id = bindings.Declare(
      ReceiverOrigin{}, parent.current_class.cls->self_pointer_type);
  code.params = {self_id};
  code.result_type = unit.builtins.coroutine_void;

  const WalkFrame frame =
      action.InRootScope(parent.WithBlock(&code.Body()).WithBindings(&bindings))
          .WithRunsAsFunctionCall(false);

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

auto Choose(
    mir::Block& block, mir::TypeId type, mir::ExprId condition,
    mir::ExprId then_value, mir::ExprId else_value) -> mir::ExprId {
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::ConditionalExpr{
                  .condition = condition,
                  .then_value = then_value,
                  .else_value = else_value},
          .type = type});
}

auto AnswerWord(
    mir::Block& block, mir::TypeId type, support::PropertyAnswer answer)
    -> mir::ExprId {
  return Word(block, type, static_cast<std::uint64_t>(answer));
}

// The least answer an implication's instances reached, for the evaluation the
// code being built answers.
using ImplicationAnswerOf =
    std::function<mir::ExprId(mir::Block&, std::uint32_t)>;

// What one part answers for one evaluation (Annex F.5.3.2), from the positions
// `bits` it is live at once the tick has stepped it and from what its
// implications' instances answered. The answers are ordered, so negation is the
// order reversed and a conjunction and a disjunction are the least and the
// greatest of two (LRM 16.12.3-16.12.5). A term may be read by two later ones
// -- `iff` reads each operand twice -- so each is held in a local of its own.
auto BuildPartAnswer(
    const mir::CompilationUnit& unit, CallableBindings& bindings,
    mir::Block& block, const AutomatonPart& part,
    const std::vector<mir::LocalId>& bits,
    const ImplicationAnswerOf& implication_answer) -> mir::LocalId {
  const mir::TypeId type = unit.builtins.machine_word;
  const auto hold = [&](mir::ExprId value) {
    const mir::LocalId local = bindings.DeclareAnonymous(type);
    block.AppendStmt(mir::LocalDeclStmt{.target = local, .init = value});
    return local;
  };
  const auto extreme = [&](mir::BinaryOp keeps_lhs, mir::LocalId lhs,
                           mir::LocalId rhs) {
    return Choose(
        block, type,
        Op(unit, block, unit.builtins.machine_bool, keeps_lhs,
           block.ReadLocal(lhs, type), block.ReadLocal(rhs, type)),
        block.ReadLocal(lhs, type), block.ReadLocal(rhs, type));
  };
  const auto answer = [&](support::PropertyAnswer value) {
    return AnswerWord(block, type, value);
  };

  std::vector<mir::LocalId> held;
  held.reserve(part.terms.size());
  for (const AnswerTerm& term : part.terms) {
    const mir::ExprId value = std::visit(
        Overloaded{
            [&](const SequenceTerm& sequence) {
              const support::PropertyAnswer while_live =
                  sequence.strength == hir::SequenceStrength::kWeak
                      ? support::PropertyAnswer::kHolds
                      : support::PropertyAnswer::kPending;
              return Choose(
                  block, type, AnyOf(block, unit, bits, sequence.kept),
                  answer(support::PropertyAnswer::kHoldsStrongly),
                  Choose(
                      block, type, AnyOf(block, unit, bits, sequence.live),
                      answer(while_live),
                      answer(support::PropertyAnswer::kFails)));
            },
            [&](const NegatedTerm& negated) {
              return Op(
                  unit, block, type, mir::BinaryOp::kSub,
                  answer(support::PropertyAnswer::kHoldsStrongly),
                  block.ReadLocal(held[negated.operand], type));
            },
            [&](const LeastTerm& least) {
              return extreme(
                  mir::BinaryOp::kLessThan, held[least.lhs], held[least.rhs]);
            },
            [&](const GreatestTerm& greatest) {
              return extreme(
                  mir::BinaryOp::kGreaterThan, held[greatest.lhs],
                  held[greatest.rhs]);
            },
            [&](const ImplicationTerm& implication) {
              const mir::LocalId antecedent = hold(Choose(
                  block, type, AnyOf(block, unit, bits, implication.antecedent),
                  answer(support::PropertyAnswer::kHolds),
                  answer(support::PropertyAnswer::kHoldsStrongly)));
              const mir::LocalId instances =
                  hold(implication_answer(block, implication.implication));
              return extreme(mir::BinaryOp::kLessThan, antecedent, instances);
            }},
        term);
    held.push_back(hold(value));
  }
  if (held.empty()) {
    throw InternalError(
        "BuildPartAnswer: a part with no term answers for no property");
  }
  return held.back();
}

// Every part a seed may start, as the positions a tick it seeds at has to read.
auto StartsOf(const ImplicationSeed& seed, const AssertionAutomaton& automaton)
    -> PositionSet {
  PositionSet starts(automaton.words, 0);
  const auto add = [&](std::uint32_t part) {
    for (std::uint32_t word = 0; word < automaton.words; ++word) {
      starts[word] |= automaton.parts[part].start[word];
    }
  };
  std::visit(
      Overloaded{
          [&](const MatchSeed& match) { add(match.part); },
          [&](const ConditionSeed& condition) {
            add(condition.then_part);
            if (condition.else_part.has_value()) {
              add(*condition.else_part);
            }
          },
          [&](const CaseSeed& selection) {
            for (const CaseSeedItem& item : selection.items) {
              add(item.part);
            }
            if (selection.default_part.has_value()) {
              add(*selection.default_part);
            }
          }},
      seed.starts);
  return starts;
}

// The storage holding the attempts a tick steps, as the place a call on it acts
// through, built in whichever block the call is made in.
using AttemptsPlace = std::function<mir::ExprId(mir::Block&)>;

// One tick of every evaluation in flight, as a block run in the Observed
// region. Reading the design's Boolean expressions and stepping the positions
// belong to one body because the second bounds the first: only a position some
// live evaluation can reach this tick is read, and each is read once however
// many evaluations reach it. The tick ends by settling every attempt it
// answered.
auto LowerTick(
    ProcessLowerer& action, const WalkFrame& parent, CallableBindings& bindings,
    const AttemptsPlace& attempts, const AssertionAutomaton& automaton)
    -> diag::Result<mir::Block> {
  mir::Block body;
  const WalkFrame frame = parent.WithBlock(&body);
  mir::CompilationUnit& unit = action.Owner().Unit();
  const mir::TypeId word_type = unit.builtins.machine_word;
  const mir::TypeId index_type = unit.builtins.machine_int64;
  const mir::TypeId void_type = unit.builtins.void_type;
  const PositionSet empty(automaton.words, 0);

  const auto call = [&](mir::Block& block, support::BuiltinFn fn,
                        std::vector<mir::ExprId> arguments,
                        mir::TypeId result) {
    return AttemptsCall(
        block, fn, attempts(block), std::move(arguments), result);
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

  call_stmt(body, support::BuiltinFn::kEvaluationAttemptsBeginTick, {});

  // What this tick has to read: every position an unstepped evaluation is live
  // at, and the ones a seed would start, since every seed starts its instance
  // at the tick of the match.
  const std::vector<mir::LocalId> need =
      DeclareWords(bindings, body, word_type, empty);
  for (std::uint32_t word = 0; word < automaton.words; ++word) {
    Assign(
        unit, body, need[word], word_type,
        call(
            body, support::BuiltinFn::kEvaluationAttemptsLiveWord,
            {Word(body, word_type, word)}, word_type));
  }
  for (const ImplicationSeed& seed : automaton.seeds) {
    mir::Block reached;
    OrInto(unit, reached, word_type, need, StartsOf(seed, automaton));
    AppendIf(body, AnyOf(body, unit, need, seed.matches), std::move(reached));
  }

  // The design's own Boolean expressions, as of the Preponed region of this
  // tick's time step (LRM 16.5.1), and only where something waits on them.
  const std::vector<mir::LocalId> hold =
      DeclareWords(bindings, body, word_type, empty);
  for (std::size_t position = 0; position < automaton.positions.size();
       ++position) {
    const PositionSet at = bit_at(position);
    mir::Block reached;
    const WalkFrame sampled =
        frame.WithBlock(&reached).WithReadsAsOf(ReadsAsOf::kPreponed);
    // Each conjunct is a step that runs while the ones before it held, so the
    // steps stand beside each other however many conjuncts a position has.
    std::vector<Evaluation> conjuncts;
    for (const hir::ExprId conjunct : automaton.positions[position].conjuncts) {
      conjuncts.emplace_back(
          [&action,
           conjunct](const WalkFrame& tried) -> diag::Result<mir::ExprId> {
            auto lowered =
                action.LowerExpr(action.HirBody().exprs.Get(conjunct), tried);
            if (!lowered) return std::unexpected(std::move(lowered.error()));
            return tried.current_block->exprs.Add(*std::move(lowered));
          });
    }
    const mir::LocalId all_hold =
        DeclareLocal(sampled, BuildMachineBool(unit, reached, true));
    auto assigned = AssignAllHoldInOrder(unit, sampled, all_hold, conjuncts);
    if (!assigned) return std::unexpected(std::move(assigned.error()));
    // A position with no conjunct matches whatever the design does there, so
    // its bit is set whenever it is reached.
    mir::Block held;
    OrInto(unit, held, word_type, hold, at);
    AppendIf(
        reached, reached.ReadLocal(all_hold, unit.builtins.machine_bool),
        std::move(held));
    AppendIf(body, AnyOf(body, unit, need, at), std::move(reached));
  }

  // Every evaluation this tick has not stepped, including the ones seeded
  // during the sweep, which is what reads a consequent at the very tick its
  // antecedent matched (LRM 16.12.7).
  const mir::LocalId cursor = bindings.DeclareAnonymous(index_type);
  body.AppendStmt(
      mir::LocalDeclStmt{
          .target = cursor,
          .init = call(
              body, support::BuiltinFn::kEvaluationAttemptsNextUnstepped, {},
              index_type)});

  mir::Block sweep;
  const std::vector<mir::LocalId> bits =
      DeclareWords(bindings, sweep, word_type, empty);
  for (std::uint32_t word = 0; word < automaton.words; ++word) {
    Assign(
        unit, sweep, bits[word], word_type,
        call(
            sweep, support::BuiltinFn::kEvaluationAttemptsBitsAt,
            {sweep.ReadLocal(cursor, index_type), Word(sweep, word_type, word)},
            word_type));
  }
  const std::vector<mir::LocalId> matched =
      DeclareWords(bindings, sweep, word_type, empty);
  for (std::uint32_t word = 0; word < automaton.words; ++word) {
    Assign(
        unit, sweep, matched[word], word_type,
        Op(unit, sweep, word_type, mir::BinaryOp::kBitwiseAnd,
           sweep.ReadLocal(bits[word], word_type),
           sweep.ReadLocal(hold[word], word_type)));
  }
  const std::vector<mir::LocalId> next =
      DeclareWords(bindings, sweep, word_type, empty);
  for (std::size_t position = 0; position < automaton.positions.size();
       ++position) {
    mir::Block advanced;
    OrInto(unit, advanced, word_type, next, automaton.follow[position]);
    AppendIf(
        sweep, AnyOf(sweep, unit, matched, bit_at(position)),
        std::move(advanced));
  }
  for (std::uint32_t word = 0; word < automaton.words; ++word) {
    call_stmt(
        sweep, support::BuiltinFn::kEvaluationAttemptsSetWord,
        {sweep.ReadLocal(cursor, index_type), Word(sweep, word_type, word),
         sweep.ReadLocal(next[word], word_type)});
  }
  call_stmt(
      sweep, support::BuiltinFn::kEvaluationAttemptsStep,
      {sweep.ReadLocal(cursor, index_type)});

  // An instance of `part` for `implication` of the evaluation just stepped,
  // read at this same tick.
  const auto seed_into = [&](mir::Block& block, std::uint32_t implication,
                             std::uint32_t part) {
    for (std::uint32_t word = 0; word < automaton.words; ++word) {
      call_stmt(
          block, support::BuiltinFn::kEvaluationAttemptsSeedWord,
          {Word(block, word_type, word),
           Word(block, word_type, automaton.parts[part].start[word])});
    }
    call_stmt(
        block, support::BuiltinFn::kEvaluationAttemptsSeed,
        {block.ReadLocal(cursor, index_type),
         Word(block, word_type, implication), Word(block, word_type, part)});
  };
  // A selection reads what it selects by as of the Preponed region of this
  // tick, as every Boolean of the property does (LRM 16.5.1).
  const auto lower_sampled =
      [&](const WalkFrame& at, hir::ExprId expr) -> diag::Result<mir::ExprId> {
    auto lowered = action.LowerExpr(action.HirBody().exprs.Get(expr), at);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    return at.current_block->exprs.Add(*std::move(lowered));
  };
  for (const ImplicationSeed& seed : automaton.seeds) {
    mir::Block seeded;
    const WalkFrame at =
        frame.WithBlock(&seeded).WithReadsAsOf(ReadsAsOf::kPreponed);
    auto filled = std::visit(
        Overloaded{
            [&](const MatchSeed& match) -> diag::Result<void> {
              seed_into(seeded, seed.implication, match.part);
              return {};
            },
            [&](const ConditionSeed& condition) -> diag::Result<void> {
              auto value = lower_sampled(at, condition.condition);
              if (!value) return std::unexpected(std::move(value.error()));
              mir::Block then_block;
              seed_into(then_block, seed.implication, condition.then_part);
              std::optional<mir::BlockId> else_scope;
              if (condition.else_part.has_value()) {
                mir::Block else_block;
                seed_into(else_block, seed.implication, *condition.else_part);
                else_scope = seeded.child_scopes.Add(std::move(else_block));
              }
              seeded.AppendStmt(
                  mir::IfStmt{
                      .condition = ReduceToCondition(unit, seeded, *value),
                      .then_scope =
                          seeded.child_scopes.Add(std::move(then_block)),
                      .else_scope = else_scope});
              return {};
            },
            [&](const CaseSeed& selection) -> diag::Result<void> {
              auto subject = lower_sampled(at, selection.subject);
              if (!subject) return std::unexpected(std::move(subject.error()));
              const mir::TypeId subject_type = seeded.exprs.Get(*subject).type;
              const mir::LocalId subject_local = SnapshotExprToLocal(
                  action.Owner(), at, seeded, subject_type, *subject);
              const auto starting =
                  [&, implication = seed.implication](std::uint32_t part) {
                    return [&, implication, part](const WalkFrame&) {
                      mir::Block block;
                      seed_into(block, implication, part);
                      return diag::Result<mir::Block>{std::move(block)};
                    };
                  };
              std::vector<SearchedArm> items;
              items.reserve(selection.items.size());
              for (const CaseSeedItem& item : selection.items) {
                SearchedArm arm{.conditions = {}, .body = starting(item.part)};
                for (const hir::ExprId label : item.labels) {
                  arm.conditions.emplace_back(
                      [&, label](
                          const WalkFrame& tried) -> diag::Result<mir::ExprId> {
                        auto written = lower_sampled(tried, label);
                        if (!written) return written;
                        mir::Block& block = *tried.current_block;
                        return BuildCaseComparison(
                            unit, block, hir::CaseKeyword::kCase,
                            block.LocalPlace(subject_local, subject_type),
                            OperandAtHandleType(
                                unit, block, *written, subject_type));
                      });
                }
                items.push_back(std::move(arm));
              }
              std::optional<mir::Block> otherwise;
              if (selection.default_part.has_value()) {
                otherwise = mir::Block{};
                seed_into(
                    *otherwise, seed.implication, *selection.default_part);
              }
              auto search =
                  BuildArmSearch(unit, at, items, std::move(otherwise));
              if (!search) return std::unexpected(std::move(search.error()));
              seeded.AppendStmt(*std::move(search));
              return {};
            }},
        seed.starts);
    if (!filled) return std::unexpected(std::move(filled.error()));
    AppendIf(
        sweep, AnyOf(sweep, unit, matched, seed.matches), std::move(seeded));
  }
  Assign(
      unit, sweep, cursor, index_type,
      call(
          sweep, support::BuiltinFn::kEvaluationAttemptsNextUnstepped, {},
          index_type));
  body.AppendStmt(
      mir::WhileStmt{
          .condition =
              Op(unit, body, unit.builtins.machine_bool,
                 mir::BinaryOp::kGreaterEqual,
                 body.ReadLocal(cursor, index_type), Word(body, index_type, 0)),
          .scope = body.child_scopes.Add(std::move(sweep))});

  // Every evaluation answers, from the last back, so the instances of an
  // implication have answered before the evaluation holding it does.
  const mir::LocalId answering = bindings.DeclareAnonymous(index_type);
  body.AppendStmt(
      mir::LocalDeclStmt{
          .target = answering,
          .init =
              Op(unit, body, index_type, mir::BinaryOp::kSub,
                 call(
                     body, support::BuiltinFn::kEvaluationAttemptsCount, {},
                     index_type),
                 Word(body, index_type, 1))});
  mir::Block resolve;
  const WalkFrame resolve_frame = frame.WithBlock(&resolve);
  const std::vector<mir::LocalId> stepped =
      DeclareWords(bindings, resolve, word_type, empty);
  for (std::uint32_t word = 0; word < automaton.words; ++word) {
    Assign(
        unit, resolve, stepped[word], word_type,
        call(
            resolve, support::BuiltinFn::kEvaluationAttemptsBitsAt,
            {resolve.ReadLocal(answering, index_type),
             Word(resolve, word_type, word)},
            word_type));
  }
  const ImplicationAnswerOf implication_answer =
      [&](mir::Block& block, std::uint32_t implication) {
        return call(
            block, support::BuiltinFn::kEvaluationAttemptsImplicationAnswer,
            {block.ReadLocal(answering, index_type),
             Word(block, word_type, implication)},
            word_type);
      };
  std::vector<ValueArm> arms;
  arms.reserve(automaton.parts.size());
  for (std::size_t part = 0; part < automaton.parts.size(); ++part) {
    arms.push_back(
        ValueArm{
            .values = {static_cast<std::int64_t>(part)},
            .body = [&, part](const WalkFrame&) -> diag::Result<mir::Block> {
              mir::Block block;
              const mir::LocalId answer = BuildPartAnswer(
                  unit, bindings, block, automaton.parts[part], stepped,
                  implication_answer);
              call_stmt(
                  block, support::BuiltinFn::kEvaluationAttemptsResolve,
                  {block.ReadLocal(answering, index_type),
                   block.ReadLocal(answer, word_type)});
              return block;
            }});
  }
  auto dispatched = BuildValueSwitch(
      resolve_frame,
      call(
          resolve, support::BuiltinFn::kEvaluationAttemptsPartOf,
          {resolve.ReadLocal(answering, index_type)}, word_type),
      arms, std::nullopt);
  if (!dispatched) return std::unexpected(std::move(dispatched.error()));
  resolve.AppendStmt(*std::move(dispatched));
  Assign(
      unit, resolve, answering, index_type,
      Op(unit, resolve, index_type, mir::BinaryOp::kSub,
         resolve.ReadLocal(answering, index_type),
         Word(resolve, index_type, 1)));
  body.AppendStmt(
      mir::WhileStmt{
          .condition = Op(
              unit, body, unit.builtins.machine_bool,
              mir::BinaryOp::kGreaterEqual,
              body.ReadLocal(answering, index_type), Word(body, index_type, 0)),
          .scope = body.child_scopes.Add(std::move(resolve))});

  call_stmt(
      body, support::BuiltinFn::kEvaluationAttemptsSettle,
      {body.exprs.Add(BuildCurrentRuntimeCallExpr(action.Owner()))});
  return body;
}

// `block` as a statement of `into`, in a scope of its own.
void AppendBlock(mir::Block& into, mir::Block block) {
  into.AppendStmt(
      mir::BlockStmt{.scope = into.child_scopes.Add(std::move(block))});
}

// Opens an attempt on the start of the whole property, as a tick of a declared
// assertion and the reaching of an expect statement each do.
void AppendOpenAttempt(
    mir::Block& body, const mir::CompilationUnit& unit,
    const AttemptsPlace& attempts, const AssertionAutomaton& automaton) {
  const mir::TypeId word_type = unit.builtins.machine_word;
  const mir::TypeId void_type = unit.builtins.void_type;
  for (std::uint32_t word = 0; word < automaton.words; ++word) {
    body.AppendStmt(
        mir::ExprStmt{
            .expr = AttemptsCall(
                body, support::BuiltinFn::kEvaluationAttemptsSeedWord,
                attempts(body),
                {Word(body, word_type, word),
                 Word(body, word_type, automaton.parts.front().start[word])},
                void_type)});
  }
  body.AppendStmt(
      mir::ExprStmt{
          .expr = AttemptsCall(
              body, support::BuiltinFn::kEvaluationAttemptsOpenAttempt,
              attempts(body), {}, void_type)});
}

// The whole of one tick of an assertion the scope declares, as the callable an
// Observed submission runs.
auto LowerAdvance(
    ProcessLowerer& action, const WalkFrame& parent,
    const StructuralScopeLowerer& lowerer, hir::ConcurrentAssertionId id,
    const hir::PropertySpec& spec, const AssertionAutomaton& automaton)
    -> diag::Result<mir::CallableCode> {
  mir::CompilationUnit& unit = action.Owner().Unit();
  const mir::TypeId void_type = unit.builtins.void_type;

  mir::CallableCode code = mir::CallableCode::Defined();
  CallableBindings bindings(unit, code);
  const mir::LocalId self_id = bindings.Declare(
      ReceiverOrigin{}, parent.current_class.cls->self_pointer_type);
  code.params = {self_id};
  code.result_type = void_type;

  mir::Block& body = code.Body();
  const WalkFrame frame =
      action.InRootScope(parent.WithBlock(&body).WithBindings(&bindings));
  const AttemptsPlace attempts = [&](mir::Block& block) {
    return BuildEvaluationAttemptsExpr(block, frame, lowerer, id);
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
    preempted.AppendStmt(
        mir::ExprStmt{
            .expr = AttemptsCall(
                preempted, support::BuiltinFn::kEvaluationAttemptsDisableTick,
                attempts(preempted), {}, void_type)});
    preempted.AppendStmt(mir::ReturnStmt{.value = std::nullopt});
    AppendIf(body, reduced, std::move(preempted));
  }

  // LRM 16.14.5: an attempt begins at every tick, the enabling condition of an
  // assertion nothing has to reach being 1.
  AppendOpenAttempt(body, unit, attempts, automaton);
  auto ticked = LowerTick(action, frame, bindings, attempts, automaton);
  if (!ticked) return std::unexpected(std::move(ticked.error()));
  AppendBlock(body, *std::move(ticked));
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
      ReceiverOrigin{}, ctor_frame.current_class.cls->self_pointer_type);

  mir::Block body_block;
  const WalkFrame body_frame =
      ctor_frame.WithBindings(&bindings).WithBlock(&body_block);
  // What the condition reads is stated beside it, in the action's own body.
  auto waited = BuildValueChangeWaitStmt(
      body_block, body_frame, action, disable.sensitivity);
  if (!waited) return std::unexpected(std::move(waited.error()));
  body_block.AppendStmt(*std::move(waited));

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
      .code = std::move(code),
      .foreign = std::nullopt,
      .virtual_dispatch = std::nullopt,
      .defining_unit = std::nullopt};
}

// LRM 16.14.5 gives a declarative concurrent assertion always semantics, and
// LRM 16.5 evaluates it in the Observed region. Those are two facts about
// different things: the process waits wherever any value-change wait waits, and
// what the tick has to do goes to Observed as a deferred effect -- so nothing
// in the engine learns what a concurrent assertion is.
auto LowerProcess(
    ProcessLowerer& action, const StructuralScopeLowerer& lowerer,
    const WalkFrame& ctor_frame, const hir::EventControl& clock,
    mir::CallableId advance) -> diag::Result<mir::CallableDecl> {
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  const mir::TypeId void_type = unit.builtins.void_type;
  const mir::TypeId self_ptr_type =
      ctor_frame.current_class.cls->self_pointer_type;

  mir::CallableCode code = mir::CallableCode::Defined();
  CallableBindings bindings(unit, code);
  const mir::LocalId self_id =
      bindings.Declare(ReceiverOrigin{}, self_ptr_type);

  mir::Block body_block;
  const WalkFrame body_frame =
      ctor_frame.WithBindings(&bindings).WithBlock(&body_block);

  // The clock's own expression lives in the assertion's body, beside the
  // Booleans the property reads, so it is that body's lowering that resolves
  // it.
  auto wait = BuildEventWaitStmt(action, body_frame, body_block, clock);
  if (!wait) return std::unexpected(std::move(wait.error()));
  body_block.AppendStmt(*std::move(wait));

  ClosureBuilder closure(unit, body_frame, support::ClosureKind::kFn);
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
                                          .owner = body_frame.current_class.id,
                                          .slot = advance},
                                  .receiver = BuildObjectDeref(
                                      unit, closure.Body(),
                                      closure.Body().exprs.Add(
                                          MakeSelfRefExpr(closure.Frame())))},
                          .arguments = {}},
                  .type = void_type})});

  mir::Expr keeper = BuildRegionEffectsCallExpr(
      lowerer.Owner(), body_block, support::BuiltinFn::kObservedEffects);
  const mir::ExprId kept = body_block.exprs.Add(BuildEmplacedClosure(
      lowerer.Owner().Unit(), body_frame, std::move(keeper),
      closure.BuildVoid()));
  body_block.AppendStmt(mir::ExprStmt{.expr = kept});

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
      .code = std::move(code),
      .foreign = std::nullopt,
      .virtual_dispatch = std::nullopt,
      .defining_unit = std::nullopt};
}

}  // namespace

auto BuildEvaluationAttemptsExpr(
    mir::Block& block, const WalkFrame& frame,
    const StructuralScopeLowerer& lowerer, hir::ConcurrentAssertionId id)
    -> mir::ExprId {
  const mir::FieldId field = lowerer.TranslateConcurrentAssertion(id);
  const mir::ExprId self = block.exprs.Add(MakeSelfRefExpr(frame));
  return block.exprs.Add(
      mir::MakeFieldAccessExpr(
          BuildObjectDeref(lowerer.Owner().Unit(), block, self),
          mir::ClassFieldTarget{.owner = frame.current_class.id, .slot = field},
          frame.current_class.cls->fields.Get(field).type));
}

auto LowerConcurrentAssertion(
    StructuralScopeLowerer& lowerer, mir::Class& mir_class,
    const WalkFrame& ctor_frame, const StorageBringUp& bring_up,
    const DeclaredScopes& scopes, const StaticVarBindings& statics,
    hir::ConcurrentAssertionId id, const hir::ConcurrentAssertionDecl& decl)
    -> diag::Result<InstalledConcurrentAssertion> {
  const AssertionDispositions dispositions = DispositionsOf(decl.assertion);
  const AssertionAutomaton automaton =
      BuildAssertionAutomaton(decl.action, *dispositions.spec);

  ProcessLowerer action(
      lowerer.Owner(), &lowerer, lowerer.HirScope().time_resolution,
      decl.action, std::nullopt, ctor_frame, scopes, statics);

  auto pass = LowerActionArm(
      action, ctor_frame, PassArmOf(decl.action, dispositions), decl.span);
  if (!pass) return std::unexpected(std::move(pass.error()));

  auto fail = LowerActionArm(
      action, ctor_frame, FailArmOf(decl.action, dispositions), decl.span);
  if (!fail) return std::unexpected(std::move(fail.error()));

  auto advance = LowerAdvance(
      action, ctor_frame, lowerer, id, *dispositions.spec, automaton);
  if (!advance) return std::unexpected(std::move(advance.error()));

  for (const StaticVarBinding& binding : statics) {
    auto integrated =
        IntegrateStaticInitializer(action, decl.action, bring_up, binding);
    if (!integrated) return std::unexpected(std::move(integrated.error()));
  }

  const mir::CallableId pass_id = mir_class.callables.Add(
      mir::CallableDecl{
          .code = *std::move(pass),
          .foreign = std::nullopt,
          .virtual_dispatch = std::nullopt,
          .defining_unit = std::nullopt});
  const mir::CallableId fail_id = mir_class.callables.Add(
      mir::CallableDecl{
          .code = *std::move(fail),
          .foreign = std::nullopt,
          .virtual_dispatch = std::nullopt,
          .defining_unit = std::nullopt});
  const mir::CallableId advance_id = mir_class.callables.Add(
      mir::CallableDecl{
          .code = *std::move(advance),
          .foreign = std::nullopt,
          .virtual_dispatch = std::nullopt,
          .defining_unit = std::nullopt});

  auto process = LowerProcess(
      action, lowerer, ctor_frame, dispositions.spec->clock, advance_id);
  if (!process) return std::unexpected(std::move(process.error()));

  std::vector<mir::CallableId> processes;
  processes.push_back(mir_class.callables.Add(*std::move(process)));
  if (dispositions.spec->disable.has_value()) {
    auto watcher = LowerDisableWatcher(
        action, ctor_frame, lowerer, id, *dispositions.spec->disable);
    if (!watcher) return std::unexpected(std::move(watcher.error()));
    processes.push_back(mir_class.callables.Add(*std::move(watcher)));
  }

  return InstalledConcurrentAssertion{
      .processes = std::move(processes),
      .pass_action = pass_id,
      .fail_action = fail_id,
      .words = automaton.words,
      .written_at = decl.span};
}

auto LowerExpectStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::ExpectStmt& expect, diag::SourceSpan span)
    -> diag::Result<mir::Stmt> {
  mir::CompilationUnit& unit = process.Owner().Unit();
  const mir::TypeId word_type = unit.builtins.machine_word;
  const mir::TypeId void_type = unit.builtins.void_type;
  const AssertionAutomaton automaton =
      BuildAssertionAutomaton(process.HirBody(), expect.spec);

  // The wait decides each tick of the clock where it happens, so a clock whose
  // occurrence only the waiting process could decide has nothing to watch it.
  if (!ChangeDecides(process.HirBody().exprs, expect.spec.clock)) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedStatementForm,
        "an 'expect' whose clocking event calls a function or reads through a "
        "handle is not yet supported");
  }

  // The wait watches the clock and what the disable condition reads, and
  // holds the one attempt, so the evaluation ends with the scope the wait is a
  // local of, however the process leaves it (LRM 16.17).
  mir::Block scope;
  const WalkFrame inner = frame.WithBlock(&scope);
  auto clock_leaves = BuildEventLeaves(process, inner, expect.spec.clock);
  if (!clock_leaves) return std::unexpected(std::move(clock_leaves.error()));
  auto clock = BuildTriggerArray(scope, inner, process, *clock_leaves);
  if (!clock) return std::unexpected(std::move(clock.error()));
  std::vector<ObservedLeaf> disable_leaves;
  if (expect.spec.disable.has_value()) {
    disable_leaves = BuildValueChangeLeaves(
        unit, inner, scope, expect.spec.disable->sensitivity);
  }
  auto disable_reads = BuildTriggerArray(scope, inner, process, disable_leaves);
  if (!disable_reads) return std::unexpected(std::move(disable_reads.error()));
  const mir::LocalId wait = DeclareLocal(
      inner,
      scope.exprs.Add(
          mir::MakeCallExpr(
              mir::Direct{
                  .target = support::BuiltinFn::kWaitOnPropertyEvaluation},
              {scope.exprs.Add(BuildCurrentRuntimeCallExpr(process.Owner())),
               *clock, *disable_reads, Word(scope, word_type, automaton.words)},
              unit.builtins.wait)));
  const mir::LocalId wait_pointer = DeclareLocal(
      inner, scope.exprs.Add(
                 mir::MakeAddressOfExpr(
                     scope.LocalPlace(wait, unit.builtins.wait),
                     unit.builtins.wait_ptr)));

  const mir::TypeId attempts_type =
      unit.types.Intern(mir::Type{mir::EvaluationAttemptsType{}});
  const mir::TypeId attempts_pointer = unit.types.Intern(
      mir::Type{mir::PointerType{
          .pointee = attempts_type,
          .ownership = mir::PointerOwnership::kBorrowed}});
  // The attempt the wait holds, which stays where it is for as long as the
  // wait does, so it is looked up once.
  const mir::LocalId attempts_held = DeclareLocal(
      inner, scope.exprs.Add(
                 mir::MakeCallExpr(
                     mir::Direct{.target = support::BuiltinFn::kAttemptsOf},
                     {scope.ReadLocal(wait_pointer, unit.builtins.wait_ptr)},
                     attempts_pointer)));
  // The attempt, through the local `pointer` holding its address.
  const auto attempts_at = [&](mir::LocalId pointer) -> AttemptsPlace {
    return
        [&unit, attempts_type, attempts_pointer, pointer](mir::Block& block) {
          return block.exprs.Add(
              mir::MakeDerefExpr(
                  block.DerefOperand(
                      unit.types, block.ReadLocal(pointer, attempts_pointer)),
                  attempts_type));
        };
  };
  const auto call_on_wait = [&](mir::Block& block, support::BuiltinFn fn,
                                mir::LocalId pointer,
                                std::vector<mir::ExprId> rest) {
    std::vector<mir::ExprId> arguments{
        block.ReadLocal(pointer, unit.builtins.wait_ptr)};
    arguments.insert(arguments.end(), rest.begin(), rest.end());
    block.AppendStmt(
        mir::ExprStmt{
            .expr = block.exprs.Add(
                mir::MakeCallExpr(
                    mir::Direct{.target = fn}, std::move(arguments),
                    void_type))});
  };
  // What the wait runs is code of this body and reads its frame -- what an
  // automatic variable or a `ref` formal holds when the Observed region
  // evaluates is what the property reads (LRM 16.5.1) -- so each is a closure
  // holding the addresses it reaches the evaluation through. The wait holds
  // both and is a local of the frame they read, so neither runs once what it
  // reads is gone.
  const auto snapshot_into = [&](ClosureBuilder& closure, mir::LocalId held,
                                 mir::TypeId type) {
    const mir::LocalId pointer = closure.Bindings().DeclareAnonymous(type);
    closure.Body().AppendStmt(
        mir::LocalDeclStmt{
            .target = pointer,
            .init = SnapshotValueIntoClosure(
                process.Owner(), inner, closure, scope.ReadLocal(held, type))});
    return pointer;
  };
  // The disable condition, tested where the closure stands: held, the
  // evaluation is disabled and the closure goes no further (LRM 16.12).
  const auto test_disable = [&](ClosureBuilder& closure) -> diag::Result<void> {
    if (!expect.spec.disable.has_value()) return {};
    const mir::LocalId pointer =
        snapshot_into(closure, wait_pointer, unit.builtins.wait_ptr);
    auto condition = process.LowerExpr(
        process.HirBody().exprs.Get(expect.spec.disable->condition),
        closure.Frame());
    if (!condition) return std::unexpected(std::move(condition.error()));
    const mir::ExprId reduced = ReduceToCondition(
        unit, closure.Body(), closure.Body().exprs.Add(*std::move(condition)));
    mir::Block disabled;
    call_on_wait(
        disabled, support::BuiltinFn::kDisablePropertyEvaluation, pointer, {});
    disabled.AppendStmt(mir::ReturnStmt{.value = std::nullopt});
    AppendIf(closure.Body(), reduced, std::move(disabled));
    return {};
  };

  ClosureBuilder step(unit, inner, support::ClosureKind::kFn);
  if (auto tested = test_disable(step); !tested) {
    return std::unexpected(std::move(tested.error()));
  }
  const mir::LocalId step_attempts =
      snapshot_into(step, attempts_held, attempts_pointer);
  auto ticked = LowerTick(
      process, step.Frame(), step.Bindings(), attempts_at(step_attempts),
      automaton);
  if (!ticked) return std::unexpected(std::move(ticked.error()));
  AppendBlock(step.Body(), *std::move(ticked));
  const mir::ExprId step_closure =
      BuildKeptClosure(unit, scope, step.BuildVoid());

  ClosureBuilder test(unit, inner, support::ClosureKind::kFn);
  if (auto tested = test_disable(test); !tested) {
    return std::unexpected(std::move(tested.error()));
  }
  const mir::ExprId test_closure =
      BuildKeptClosure(unit, scope, test.BuildVoid());

  // The first evaluation is at the next tick of the clock (LRM 16.17).
  AppendOpenAttempt(scope, unit, attempts_at(attempts_held), automaton);
  call_on_wait(
      scope, support::BuiltinFn::kBindPropertyEvaluation, wait_pointer,
      {step_closure, test_closure});
  scope.AppendStmt(BuildParkStmt(process.Owner(), scope, wait));

  const auto answer_is = [&](mir::Block& block, support::AttemptAnswer answer) {
    return Op(
        unit, block, unit.builtins.machine_bool, mir::BinaryOp::kEquality,
        AttemptsCall(
            block, support::BuiltinFn::kEvaluationAttemptsAnswer,
            attempts_at(attempts_held)(block), {}, word_type),
        Word(block, word_type, static_cast<std::uint64_t>(answer)));
  };
  auto pass = HasRealArm(process.HirBody(), expect.pass_stmt)
                  ? LowerStmtIntoChildScope(process, inner, *expect.pass_stmt)
                  : diag::Result<mir::Block>{mir::Block{}};
  if (!pass) return std::unexpected(std::move(pass.error()));
  diag::Result<mir::Block> fail = mir::Block{};
  if (!expect.fail_stmt.has_value()) {
    AppendToolReportStmt(
        process.Owner(), *fail, support::BuiltinFn::kEmitError,
        base::DiagMessage("expect failed"), span);
  } else if (HasRealArm(process.HirBody(), expect.fail_stmt)) {
    fail = LowerStmtIntoChildScope(process, inner, *expect.fail_stmt);
  }
  if (!fail) return std::unexpected(std::move(fail.error()));
  scope.AppendStmt(
      mir::IfStmt{
          .condition = answer_is(scope, support::AttemptAnswer::kFails),
          .then_scope = scope.child_scopes.Add(*std::move(fail)),
          .else_scope = scope.child_scopes.Add(*std::move(pass))});

  return mir::Stmt{
      .label = std::move(label),
      .data = mir::BlockStmt{
          .scope = frame.current_block->child_scopes.Add(std::move(scope))}};
}

void AppendConcurrentAssertionInstall(
    const StructuralScopeLowerer& lowerer, const WalkFrame& activate_frame,
    hir::ConcurrentAssertionId id,
    const InstalledConcurrentAssertion& installed) {
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  mir::Block& block = *activate_frame.current_block;
  const mir::TypeId void_type = unit.builtins.void_type;

  // Both arms are the same for every attempt, so the storage holds one of each
  // and starts it once per attempt an outcome selects rather than being handed
  // a closure per attempt.
  const auto arm = [&](mir::CallableId target) {
    ClosureBuilder closure(unit, activate_frame, support::ClosureKind::kFn);
    closure.Body().AppendStmt(
        mir::ExprStmt{
            .expr = BuildExecutionStart(
                lowerer.Owner(), closure.Frame(), target,
                support::BuiltinFn::kRunAssertionAction,
                installed.written_at)});
    return BuildKeptClosure(unit, block, closure.BuildVoid());
  };

  block.AppendStmt(
      mir::ExprStmt{
          .expr = AttemptsCall(
              block, support::BuiltinFn::kEvaluationAttemptsInstall,
              BuildEvaluationAttemptsExpr(block, activate_frame, lowerer, id),
              {block.exprs.Add(BuildCurrentRuntimeCallExpr(lowerer.Owner())),
               Word(block, unit.builtins.machine_word, installed.words),
               arm(installed.pass_action), arm(installed.fail_action)},
              void_type)});
}

}  // namespace lyra::lowering::hir_to_mir
