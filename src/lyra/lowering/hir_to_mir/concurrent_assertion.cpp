#include "lyra/lowering/hir_to_mir/concurrent_assertion.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <expected>
#include <functional>
#include <optional>
#include <span>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/diag_message.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/hir/case_keyword.hpp"
#include "lyra/hir/operands.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/timing.hpp"
#include "lyra/lowering/hir_to_mir/assertion_automaton.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/closure_builder.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/kept_piece.hpp"
#include "lyra/lowering/hir_to_mir/predicate.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/sensitivity_wait.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/lowering/hir_to_mir/statement/assertions.hpp"
#include "lyra/lowering/hir_to_mir/statement/blocks.hpp"
#include "lyra/lowering/hir_to_mir/statement/timing.hpp"
#include "lyra/lowering/hir_to_mir/static_cell_binding.hpp"
#include "lyra/lowering/hir_to_mir/static_cell_bring_up.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/switch_stmt.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/value_operations.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/callable_bounds.hpp"
#include "lyra/support/library_decl.hpp"
#include "lyra/support/property_answer.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// Visits every expression one evaluated from `expr` is made of, `expr` among
// them, as long as `visit` answers that it goes on below the one it was handed.
// Only what is evaluated where it stands is followed.
struct OperandWalk {
  const hir::ProceduralBody* body;
  const std::function<bool(hir::ExprId)>* visit;

  void operator()(hir::ExprId id) const {
    if (!(*visit)(id)) return;
    hir::ForEachOperand(body->exprs.Get(id).data, Evaluated());
  }
  // The reach handing this walk what the holder it is given evaluates.
  [[nodiscard]] auto Evaluated() const -> hir::EvaluatedOperands<OperandWalk> {
    return {
        .trees =
            hir::TreeArenas{
                .patterns = &body->patterns,
                .properties = &body->property_exprs,
                .sequences = &body->sequence_exprs,
                .constraints = &body->constraints},
        .visit = this};
  }
};

// The same walk over every expression the statement `id` evaluates, its own
// and its children's, a variable's declaration assignment included.
void WalkStmt(const OperandWalk& walk, hir::StmtId id) {
  const hir::Stmt& stmt = walk.body->stmts.Get(id);
  if (const auto* declared = std::get_if<hir::VarDeclStmt>(&stmt.data)) {
    const std::optional<hir::ExprId> init =
        walk.body->procedural_vars.Get(declared->var).init;
    if (init.has_value()) walk(*init);
  }
  hir::ForEachExpr(stmt.data, walk.Evaluated());
  hir::ForEachChildStmt(
      stmt.data, [&](hir::StmtId child) { WalkStmt(walk, child); });
}

auto Word(mir::Builder block, mir::TypeId type, std::uint64_t value)
    -> mir::ExprId {
  return block.Add(
      mir::Expr{
          .data =
              mir::MachineIntLiteral{.value = static_cast<std::int64_t>(value)},
          .type = type});
}

auto Op(
    const mir::CompilationUnit& unit, mir::Builder block, mir::TypeId type,
    mir::BinaryOp op, mir::ExprId lhs, mir::ExprId rhs) -> mir::ExprId {
  return block.Add(mir::MakeBinary(unit, block.Body(), op, lhs, rhs, type));
}

void Assign(
    const mir::CompilationUnit& unit, mir::Builder block, mir::LocalId local,
    mir::TypeId type, mir::ExprId value) {
  block.AppendStmt(
      mir::ExprStmt{
          .expr = block.Add(
              mir::MakeAssignExpr(
                  unit.builtins, block.LocalPlace(local, type), value))});
}

// One position set as body locals, one per word of the automaton's width. A
// set operation is then the same sequence of word operations wherever it
// appears, and a one-word automaton is that sequence at length one.
auto DeclareWords(
    CallableBindings& bindings, mir::Builder block, mir::TypeId type,
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
    mir::Builder block, const mir::CompilationUnit& unit,
    const std::vector<mir::LocalId>& words, const PositionSet& mask)
    -> mir::ExprId {
  const mir::TypeId type = unit.builtins.machine_word;
  mir::ExprId combined = Word(block, type, 0);
  for (std::size_t word = 0; word < words.size(); ++word) {
    const mir::ExprId lhs = block.ReadLocal(words[word], type);
    const mir::ExprId rhs = Word(block, type, mask[word]);
    combined =
        Op(unit, block, type, mir::BinaryOp::kBitwiseOr, combined,
           Op(unit, block, type, mir::BinaryOp::kBitwiseAnd, lhs, rhs));
  }
  return Op(
      unit, block, unit.builtins.machine_bool, mir::BinaryOp::kInequality,
      combined, Word(block, type, 0));
}

// `words |= mask`, one word at a time.
void OrInto(
    const mir::CompilationUnit& unit, mir::Builder block, mir::TypeId type,
    const std::vector<mir::LocalId>& words, const PositionSet& mask) {
  for (std::size_t word = 0; word < words.size(); ++word) {
    const mir::ExprId lhs = block.ReadLocal(words[word], type);
    const mir::ExprId rhs = Word(block, type, mask[word]);
    Assign(
        unit, block, words[word], type,
        Op(unit, block, type, mir::BinaryOp::kBitwiseOr, lhs, rhs));
  }
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
  hir::ControlledAssertionId control;
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
                .reports_as = check.directive,
                .control = check.control};
          },
          [](const hir::ConcurrentCoverStmt& cover) {
            return AssertionDispositions{
                .spec = &cover.spec,
                .pass_stmt = cover.pass_stmt,
                .fail_stmt = std::nullopt,
                .reports_as = std::nullopt,
                .control = cover.control};
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

auto Choose(
    mir::Builder block, mir::TypeId type, mir::ExprId condition,
    mir::ExprId then_value, mir::ExprId else_value) -> mir::ExprId {
  return block.Add(
      mir::Expr{
          .data =
              mir::ConditionalExpr{
                  .condition = condition,
                  .then_value = then_value,
                  .else_value = else_value},
          .type = type});
}

auto AnswerWord(
    mir::Builder block, mir::TypeId type, support::PropertyAnswer answer)
    -> mir::ExprId {
  return Word(block, type, static_cast<std::uint64_t>(answer));
}

// What an implication's instances came to, for the evaluation the code being
// built answers: the least answer any reached, and whether any was
// nonvacuous.
struct ImplicationAnswersOf {
  std::function<mir::ExprId(mir::Builder, std::uint32_t)> answer;
  std::function<mir::ExprId(mir::Builder, std::uint32_t)> nonvacuous;
};

// What one part answers for one evaluation, held in locals of the body.
struct PartAnswer {
  mir::LocalId answer;
  mir::LocalId nonvacuous;
};

// What one part answers for one evaluation (Annex F.5.3.2), from the positions
// `bits` it is live at once the tick has stepped it and from what its
// implications' instances answered, and whether that evaluation is nonvacuous
// (LRM 16.14.8), as a word that is 1 where it is. The answers are ordered, so
// negation is the order reversed and a conjunction and a disjunction are the
// least and the greatest of two (LRM 16.12.3-16.12.5). A term may be read by
// two later ones -- `iff` reads each operand twice -- so each is held in a
// local of its own.
auto BuildPartAnswer(
    const mir::CompilationUnit& unit, CallableBindings& bindings,
    mir::Builder block, const AutomatonPart& part,
    const std::vector<mir::LocalId>& bits,
    const ImplicationAnswersOf& instances) -> PartAnswer {
  const mir::TypeId type = unit.builtins.machine_word;
  const mir::TypeId bool_type = unit.builtins.machine_bool;
  const auto in_local = [&](mir::ExprId value) {
    const mir::LocalId local = bindings.DeclareAnonymous(type);
    block.AppendStmt(mir::LocalDeclStmt{.target = local, .init = value});
    return local;
  };
  const auto read = [&](mir::LocalId local) {
    return block.ReadLocal(local, type);
  };
  const auto extreme = [&](mir::BinaryOp keeps_lhs, mir::LocalId lhs,
                           mir::LocalId rhs) {
    const mir::ExprId condition =
        Op(unit, block, bool_type, keeps_lhs, read(lhs), read(rhs));
    return Choose(block, type, condition, read(lhs), read(rhs));
  };
  const auto answer = [&](support::PropertyAnswer value) {
    return AnswerWord(block, type, value);
  };
  const auto reversed = [&](mir::LocalId operand) {
    return in_local(
        Op(unit, block, type, mir::BinaryOp::kSub,
           answer(support::PropertyAnswer::kHoldsStrongly), read(operand)));
  };
  const auto either = [&](mir::LocalId lhs, mir::LocalId rhs) {
    return Op(
        unit, block, type, mir::BinaryOp::kBitwiseOr, read(lhs), read(rhs));
  };

  std::vector<PartAnswer> held;
  held.reserve(part.terms.size());
  for (const AnswerTerm& term : part.terms) {
    const PartAnswer answered = std::visit(
        Overloaded{
            [&](const SequenceTerm& sequence) {
              const support::PropertyAnswer while_live =
                  sequence.strength == hir::SequenceStrength::kWeak
                      ? support::PropertyAnswer::kHolds
                      : support::PropertyAnswer::kPending;
              const mir::ExprId kept = AnyOf(block, unit, bits, sequence.kept);
              const mir::ExprId holds_strongly =
                  answer(support::PropertyAnswer::kHoldsStrongly);
              const mir::ExprId live = AnyOf(block, unit, bits, sequence.live);
              const mir::ExprId live_answer = answer(while_live);
              const mir::ExprId fails = answer(support::PropertyAnswer::kFails);
              const mir::ExprId otherwise =
                  Choose(block, type, live, live_answer, fails);
              return PartAnswer{
                  .answer = in_local(
                      Choose(block, type, kept, holds_strongly, otherwise)),
                  .nonvacuous = in_local(Word(block, type, 1))};
            },
            [&](const NegatedTerm& negated) {
              const PartAnswer& operand = held[negated.operand];
              return PartAnswer{
                  .answer = reversed(operand.answer),
                  .nonvacuous = operand.nonvacuous};
            },
            [&](const LeastTerm& least) {
              const PartAnswer& lhs = held[least.lhs];
              const PartAnswer& rhs = held[least.rhs];
              return PartAnswer{
                  .answer = in_local(extreme(
                      mir::BinaryOp::kLessThan, lhs.answer, rhs.answer)),
                  .nonvacuous =
                      in_local(either(lhs.nonvacuous, rhs.nonvacuous))};
            },
            [&](const GreatestTerm& greatest) {
              const PartAnswer& lhs = held[greatest.lhs];
              const PartAnswer& rhs = held[greatest.rhs];
              return PartAnswer{
                  .answer = in_local(extreme(
                      mir::BinaryOp::kGreaterThan, lhs.answer, rhs.answer)),
                  .nonvacuous =
                      in_local(either(lhs.nonvacuous, rhs.nonvacuous))};
            },
            [&](const ImpliesTerm& implies) {
              const PartAnswer& from = held[implies.from];
              const PartAnswer& to = held[implies.to];
              // The left operand is true (LRM 16.14.8 z) where the trace so
              // far satisfies it, the reading every answer here takes (Annex
              // F.5.3.1): the success is reported at the tick it is decided,
              // which may come before the left operand is decided.
              const mir::ExprId from_holds = Op(
                  unit, block, bool_type, mir::BinaryOp::kGreaterEqual,
                  read(from.answer), answer(support::PropertyAnswer::kHolds));
              const mir::ExprId both =
                  Op(unit, block, type, mir::BinaryOp::kBitwiseAnd,
                     read(from.nonvacuous), read(to.nonvacuous));
              return PartAnswer{
                  .answer = in_local(extreme(
                      mir::BinaryOp::kGreaterThan, reversed(from.answer),
                      to.answer)),
                  .nonvacuous = in_local(Choose(
                      block, type, from_holds, both, Word(block, type, 0)))};
            },
            [&](const IffTerm& iff) {
              const PartAnswer& lhs = held[iff.lhs];
              const PartAnswer& rhs = held[iff.rhs];
              const mir::LocalId forward = in_local(extreme(
                  mir::BinaryOp::kGreaterThan, reversed(lhs.answer),
                  rhs.answer));
              const mir::LocalId backward = in_local(extreme(
                  mir::BinaryOp::kGreaterThan, reversed(rhs.answer),
                  lhs.answer));
              return PartAnswer{
                  .answer = in_local(
                      extreme(mir::BinaryOp::kLessThan, forward, backward)),
                  .nonvacuous =
                      in_local(either(lhs.nonvacuous, rhs.nonvacuous))};
            },
            [&](const ImplicationTerm& implication) {
              const mir::ExprId matched =
                  AnyOf(block, unit, bits, implication.antecedent);
              const mir::ExprId holds = answer(support::PropertyAnswer::kHolds);
              const mir::ExprId holds_strongly =
                  answer(support::PropertyAnswer::kHoldsStrongly);
              const mir::LocalId antecedent =
                  in_local(Choose(block, type, matched, holds, holds_strongly));
              const mir::LocalId reached =
                  in_local(instances.answer(block, implication.implication));
              return PartAnswer{
                  .answer = in_local(
                      extreme(mir::BinaryOp::kLessThan, antecedent, reached)),
                  .nonvacuous = in_local(
                      instances.nonvacuous(block, implication.implication))};
            }},
        term);
    held.push_back(answered);
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

// What a call on the storage holding an assertion's attempts, or on one of
// them, acts through, built in whichever block the call is made in.
using ReceiverBuilder = std::function<mir::ExprId(mir::Builder)>;

// One word of a position set, as what a call answers in `block`.
using WordBuilder = std::function<mir::ExprId(mir::Builder, std::uint32_t)>;

// The design's own Boolean expressions this body reads at a tick, as of the
// Preponed region of its time step (LRM 16.5.1), and only where something
// waits on them: every position `live` says an unstepped evaluation is at, and
// the ones a seed would start, since every seed starts its instance at the tick
// of the match. `satisfied_before` is what is already known of each word, and
// `reads` says which positions this body reads -- the attempts' shared ones,
// read once for every attempt, or one attempt's own, which read what it saved.
auto DeclareSatisfiedPositions(
    ProcessLowerer& action, const WalkFrame& frame, CallableBindings& bindings,
    const WordBuilder& live, const WordBuilder& satisfied_before,
    const AssertionAutomaton& automaton, const std::vector<bool>& reads)
    -> diag::Result<std::vector<mir::LocalId>> {
  mir::Builder body = frame.Builder();
  mir::CompilationUnit& unit = action.Owner().Unit();
  const mir::TypeId word_type = unit.builtins.machine_word;
  const PositionSet empty(automaton.words, 0);

  const std::vector<mir::LocalId> satisfied =
      DeclareWords(bindings, body, word_type, empty);
  for (std::uint32_t word = 0; word < automaton.words; ++word) {
    Assign(
        unit, body, satisfied[word], word_type, satisfied_before(body, word));
  }
  // The bound is read only by the positions read here, and each word of it is
  // a call of the runtime the optimizer cannot drop, so with none read it is
  // not asked for.
  if (std::ranges::none_of(reads, [](bool read) { return read; })) {
    return satisfied;
  }

  const std::vector<mir::LocalId> need =
      DeclareWords(bindings, body, word_type, empty);
  for (std::uint32_t word = 0; word < automaton.words; ++word) {
    Assign(unit, body, need[word], word_type, live(body, word));
  }
  for (const ImplicationSeed& seed : automaton.seeds) {
    mir::Block reached;
    OrInto(
        unit, body.Into(reached), word_type, need, StartsOf(seed, automaton));
    const mir::ExprId condition = AnyOf(body, unit, need, seed.matches);
    body.AppendIfThen(condition, std::move(reached));
  }

  for (std::size_t position = 0; position < automaton.positions.size();
       ++position) {
    if (!reads[position]) continue;
    const PositionSet at =
        PositionSetOf(static_cast<std::uint32_t>(position), automaton.words);
    mir::Block reached;
    const WalkFrame sampled =
        frame.WithBlock(&reached).WithReadsAsOf(ReadsAsOf::kPreponed);
    mir::Builder reached_builder = sampled.Builder();
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
            return tried.Builder().Add(*std::move(lowered));
          });
    }
    const mir::LocalId all_hold =
        DeclareLocal(sampled, BuildMachineBool(unit, reached_builder, true));
    auto assigned = AssignAllHoldInOrder(unit, sampled, all_hold, conjuncts);
    if (!assigned) return std::unexpected(std::move(assigned.error()));
    // A position with no conjunct matches whatever the design does there, so
    // its bit is set whenever it is reached.
    mir::Block held;
    OrInto(unit, reached_builder.Into(held), word_type, satisfied, at);
    const mir::ExprId all_held =
        reached_builder.ReadLocal(all_hold, unit.builtins.machine_bool);
    reached_builder.AppendIfThen(all_held, std::move(held));
    const mir::ExprId condition = AnyOf(body, unit, need, at);
    body.AppendIfThen(condition, std::move(reached));
  }
  return satisfied;
}

// One tick of one attempt's evaluations, for a tick already begun, as a block
// run in the Observed region. `satisfied` is every position whose Booleans hold
// at this tick, those every attempt shares and the attempt's own, and reading a
// selection's condition or a case's subject belongs here too, since the
// evaluation that matched is the one reading it. The tick ends with every
// evaluation answered, from the last back.
auto LowerStepOfAttempt(
    ProcessLowerer& action, const WalkFrame& parent, CallableBindings& bindings,
    const ReceiverBuilder& attempt, const AssertionAutomaton& automaton,
    const std::vector<mir::LocalId>& satisfied) -> diag::Result<mir::Block> {
  mir::Block body;
  const WalkFrame frame = parent.WithBlock(&body);
  mir::Builder body_builder = frame.Builder();
  mir::CompilationUnit& unit = action.Owner().Unit();
  const mir::TypeId word_type = unit.builtins.machine_word;
  const mir::TypeId index_type = unit.builtins.machine_int64;
  const mir::TypeId void_type = unit.builtins.void_type;
  const PositionSet empty(automaton.words, 0);

  const auto call = [&](mir::Builder block, support::BuiltinFn fn,
                        std::vector<mir::ExprId> arguments,
                        mir::TypeId result) {
    return block.Add(
        mir::MakeCallExpr(
            mir::Direct{.target = fn, .receiver = attempt(block)},
            std::move(arguments), result));
  };
  const auto call_stmt = [&](mir::Builder block, support::BuiltinFn fn,
                             std::vector<mir::ExprId> arguments) {
    block.AppendStmt(
        mir::ExprStmt{
            .expr = call(block, fn, std::move(arguments), void_type)});
  };
  const auto bit_at = [&](std::size_t position) {
    return PositionSetOf(static_cast<std::uint32_t>(position), automaton.words);
  };

  // Every evaluation this tick has not stepped, including the ones seeded
  // during the sweep, which is what reads a consequent at the very tick its
  // antecedent matched (LRM 16.12.7).
  const mir::LocalId cursor = bindings.DeclareAnonymous(index_type);
  body_builder.AppendStmt(
      mir::LocalDeclStmt{
          .target = cursor,
          .init = call(
              body_builder, support::BuiltinFn::kEvaluationAttemptNextUnstepped,
              {}, index_type)});

  mir::Block sweep;
  mir::Builder sweep_builder = body_builder.Into(sweep);
  const std::vector<mir::LocalId> bits =
      DeclareWords(bindings, sweep_builder, word_type, empty);
  for (std::uint32_t word = 0; word < automaton.words; ++word) {
    Assign(
        unit, sweep_builder, bits[word], word_type,
        call(
            sweep_builder, support::BuiltinFn::kEvaluationAttemptBitsAt,
            {sweep_builder.ReadLocal(cursor, index_type),
             Word(sweep_builder, word_type, word)},
            word_type));
  }
  const std::vector<mir::LocalId> matched =
      DeclareWords(bindings, sweep_builder, word_type, empty);
  for (std::uint32_t word = 0; word < automaton.words; ++word) {
    const mir::ExprId lhs = sweep_builder.ReadLocal(bits[word], word_type);
    const mir::ExprId rhs = sweep_builder.ReadLocal(satisfied[word], word_type);
    Assign(
        unit, sweep_builder, matched[word], word_type,
        Op(unit, sweep_builder, word_type, mir::BinaryOp::kBitwiseAnd, lhs,
           rhs));
  }
  const std::vector<mir::LocalId> next =
      DeclareWords(bindings, sweep_builder, word_type, empty);
  for (std::size_t position = 0; position < automaton.positions.size();
       ++position) {
    mir::Block advanced;
    OrInto(
        unit, sweep_builder.Into(advanced), word_type, next,
        automaton.follow[position]);
    const mir::ExprId condition =
        AnyOf(sweep_builder, unit, matched, bit_at(position));
    sweep_builder.AppendIfThen(condition, std::move(advanced));
  }
  for (std::uint32_t word = 0; word < automaton.words; ++word) {
    call_stmt(
        sweep_builder, support::BuiltinFn::kEvaluationAttemptSetWord,
        {sweep_builder.ReadLocal(cursor, index_type),
         Word(sweep_builder, word_type, word),
         sweep_builder.ReadLocal(next[word], word_type)});
  }
  call_stmt(
      sweep_builder, support::BuiltinFn::kEvaluationAttemptStep,
      {sweep_builder.ReadLocal(cursor, index_type)});

  // An instance of `part` for `implication` of the evaluation just stepped,
  // read at this same tick.
  const auto seed_into = [&](mir::Builder block, std::uint32_t implication,
                             std::uint32_t part) {
    for (std::uint32_t word = 0; word < automaton.words; ++word) {
      call_stmt(
          block, support::BuiltinFn::kEvaluationAttemptSeedWord,
          {Word(block, word_type, word),
           Word(block, word_type, automaton.parts[part].start[word])});
    }
    call_stmt(
        block, support::BuiltinFn::kEvaluationAttemptSeed,
        {block.ReadLocal(cursor, index_type),
         Word(block, word_type, implication), Word(block, word_type, part)});
  };
  // A selection reads what it selects by as of the Preponed region of this
  // tick, as every Boolean of the property does (LRM 16.5.1).
  const auto lower_sampled =
      [&](const WalkFrame& at, hir::ExprId expr) -> diag::Result<mir::ExprId> {
    auto lowered = action.LowerExpr(action.HirBody().exprs.Get(expr), at);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    return at.Builder().Add(*std::move(lowered));
  };
  for (const ImplicationSeed& seed : automaton.seeds) {
    mir::Block seeded;
    const WalkFrame at =
        frame.WithBlock(&seeded).WithReadsAsOf(ReadsAsOf::kPreponed);
    mir::Builder seeded_builder = at.Builder();
    auto filled = std::visit(
        Overloaded{
            [&](const MatchSeed& match) -> diag::Result<void> {
              seed_into(seeded_builder, seed.implication, match.part);
              return {};
            },
            [&](const ConditionSeed& condition) -> diag::Result<void> {
              auto value = lower_sampled(at, condition.condition);
              if (!value) return std::unexpected(std::move(value.error()));
              mir::Block then_block;
              seed_into(
                  seeded_builder.Into(then_block), seed.implication,
                  condition.then_part);
              std::optional<mir::BlockId> else_scope;
              if (condition.else_part.has_value()) {
                mir::Block else_block;
                seed_into(
                    seeded_builder.Into(else_block), seed.implication,
                    *condition.else_part);
                else_scope = seeded_builder.AddScope(std::move(else_block));
              }
              seeded_builder.AppendStmt(
                  mir::IfStmt{
                      .condition =
                          ReduceToCondition(unit, seeded_builder, *value),
                      .then_scope =
                          seeded_builder.AddScope(std::move(then_block)),
                      .else_scope = else_scope});
              return {};
            },
            [&](const CaseSeed& selection) -> diag::Result<void> {
              auto subject = lower_sampled(at, selection.subject);
              if (!subject) return std::unexpected(std::move(subject.error()));
              const mir::TypeId subject_type =
                  seeded_builder.Get(*subject).type;
              const mir::LocalId subject_local = SnapshotExprToLocal(
                  action.Owner(), at, seeded_builder, subject_type, *subject);
              const auto starting =
                  [&, implication = seed.implication](std::uint32_t part) {
                    return [&, implication, part](const WalkFrame& in) {
                      mir::Block block;
                      seed_into(in.Builder().Into(block), implication, part);
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
                        mir::Builder block = tried.Builder();
                        const mir::ExprId held_subject =
                            block.LocalPlace(subject_local, subject_type);
                        const mir::ExprId label_value = OperandAtHandleType(
                            unit, block, *written, subject_type);
                        return BuildCaseComparison(
                            unit, block, hir::CaseKeyword::kCase, held_subject,
                            label_value);
                      });
                }
                items.push_back(std::move(arm));
              }
              std::optional<mir::Block> otherwise;
              if (selection.default_part.has_value()) {
                otherwise = mir::Block{};
                seed_into(
                    seeded_builder.Into(*otherwise), seed.implication,
                    *selection.default_part);
              }
              auto search =
                  BuildArmSearch(unit, at, items, std::move(otherwise));
              if (!search) return std::unexpected(std::move(search.error()));
              seeded_builder.AppendStmt(*std::move(search));
              return {};
            }},
        seed.starts);
    if (!filled) return std::unexpected(std::move(filled.error()));
    const mir::ExprId condition =
        AnyOf(sweep_builder, unit, matched, seed.matches);
    sweep_builder.AppendIfThen(condition, std::move(seeded));
  }
  Assign(
      unit, sweep_builder, cursor, index_type,
      call(
          sweep_builder, support::BuiltinFn::kEvaluationAttemptNextUnstepped,
          {}, index_type));
  const mir::ExprId unstepped = body_builder.ReadLocal(cursor, index_type);
  const mir::ExprId first_index = Word(body_builder, index_type, 0);
  body_builder.AppendStmt(
      mir::WhileStmt{
          .condition =
              Op(unit, body_builder, unit.builtins.machine_bool,
                 mir::BinaryOp::kGreaterEqual, unstepped, first_index),
          .scope = body_builder.AddScope(std::move(sweep))});

  // Every evaluation answers, from the last back, so the instances of an
  // implication have answered before the evaluation holding it does.
  const mir::LocalId answering = bindings.DeclareAnonymous(index_type);
  const mir::ExprId count = call(
      body_builder, support::BuiltinFn::kEvaluationAttemptCount, {},
      index_type);
  const mir::ExprId one = Word(body_builder, index_type, 1);
  body_builder.AppendStmt(
      mir::LocalDeclStmt{
          .target = answering,
          .init =
              Op(unit, body_builder, index_type, mir::BinaryOp::kSub, count,
                 one)});
  mir::Block resolve;
  const WalkFrame resolve_frame = frame.WithBlock(&resolve);
  mir::Builder resolve_builder = resolve_frame.Builder();
  const std::vector<mir::LocalId> stepped =
      DeclareWords(bindings, resolve_builder, word_type, empty);
  for (std::uint32_t word = 0; word < automaton.words; ++word) {
    Assign(
        unit, resolve_builder, stepped[word], word_type,
        call(
            resolve_builder, support::BuiltinFn::kEvaluationAttemptBitsAt,
            {resolve_builder.ReadLocal(answering, index_type),
             Word(resolve_builder, word_type, word)},
            word_type));
  }
  const auto of_instances = [&](support::BuiltinFn fn) {
    return [&, fn](mir::Builder block, std::uint32_t implication) {
      return call(
          block, fn,
          {block.ReadLocal(answering, index_type),
           Word(block, word_type, implication)},
          word_type);
    };
  };
  const ImplicationAnswersOf instances{
      .answer =
          of_instances(support::BuiltinFn::kEvaluationAttemptImplicationAnswer),
      .nonvacuous = of_instances(
          support::BuiltinFn::kEvaluationAttemptImplicationNonvacuous)};
  std::vector<ValueArm> arms;
  arms.reserve(automaton.parts.size());
  for (std::size_t part = 0; part < automaton.parts.size(); ++part) {
    arms.push_back(
        ValueArm{
            .values = {static_cast<std::int64_t>(part)},
            .body = [&, part](const WalkFrame& in) -> diag::Result<mir::Block> {
              mir::Block block;
              mir::Builder answered = in.Builder().Into(block);
              const PartAnswer answer = BuildPartAnswer(
                  unit, bindings, answered, automaton.parts[part], stepped,
                  instances);
              call_stmt(
                  answered, support::BuiltinFn::kEvaluationAttemptResolve,
                  {answered.ReadLocal(answering, index_type),
                   answered.ReadLocal(answer.answer, word_type),
                   answered.ReadLocal(answer.nonvacuous, word_type)});
              return block;
            }});
  }
  auto dispatched = BuildValueSwitch(
      resolve_frame,
      call(
          resolve_builder, support::BuiltinFn::kEvaluationAttemptPartOf,
          {resolve_builder.ReadLocal(answering, index_type)}, word_type),
      arms, std::nullopt);
  if (!dispatched) return std::unexpected(std::move(dispatched.error()));
  resolve_builder.AppendStmt(*std::move(dispatched));
  const mir::ExprId lhs = resolve_builder.ReadLocal(answering, index_type);
  const mir::ExprId rhs = Word(resolve_builder, index_type, 1);
  Assign(
      unit, resolve_builder, answering, index_type,
      Op(unit, resolve_builder, index_type, mir::BinaryOp::kSub, lhs, rhs));
  const mir::ExprId unanswered = body_builder.ReadLocal(answering, index_type);
  const mir::ExprId lowest_index = Word(body_builder, index_type, 0);
  body_builder.AppendStmt(
      mir::WhileStmt{
          .condition =
              Op(unit, body_builder, unit.builtins.machine_bool,
                 mir::BinaryOp::kGreaterEqual, unanswered, lowest_index),
          .scope = body_builder.AddScope(std::move(resolve))});
  return body;
}

// `block` as a statement of `into`, in a scope of its own.
void AppendBlock(mir::Builder into, mir::Block block) {
  into.AppendStmt(mir::BlockStmt{.scope = into.AddScope(std::move(block))});
}

// A position set as the machine words a runtime entry reads it in.
auto WordsOf(
    const mir::CompilationUnit& unit, mir::Builder block,
    std::vector<mir::ExprId> words) -> mir::ExprId {
  const mir::TypeId type =
      mir::MachineArrayOf(unit.types, unit.builtins.machine_word, words.size());
  return block.Add(
      mir::Expr{
          .data = mir::CompositeExpr{.parts = std::move(words)}, .type = type});
}

// Where an assertion's attempts lie, reached from `frame`'s object.
auto AttemptsAt(
    const mir::CompilationUnit& unit, const WalkFrame& frame,
    const StaticCellBinding& attempts) -> ReceiverBuilder {
  return [&unit, &frame, &attempts](mir::Builder block) {
    const WalkFrame in = frame.WithBlock(&block.Body());
    return block.Add(BuildStaticStorageAccess(
        unit, in, attempts.home, attempts.cell_type, mir::EnclosingHops{}));
  };
}

// The Booleans every attempt of an assertion shares, as the words a tick hands
// each attempt's closure: the positions no saved value bears on, read once
// at the tick for all of them, bounded by where any attempt is live.
auto BuildSharedSatisfiedPositions(
    ProcessLowerer& action, const WalkFrame& frame, CallableBindings& bindings,
    const ReceiverBuilder& attempts, const AssertionAutomaton& automaton,
    const std::vector<bool>& reads_saved) -> diag::Result<mir::ExprId> {
  mir::CompilationUnit& unit = action.Owner().Unit();
  const mir::TypeId word_type = unit.builtins.machine_word;
  std::vector<bool> shared(reads_saved.size());
  for (std::size_t position = 0; position < reads_saved.size(); ++position) {
    shared[position] = !reads_saved[position];
  }
  auto satisfied = DeclareSatisfiedPositions(
      action, frame, bindings,
      [&](mir::Builder block, std::uint32_t word) {
        return block.Add(
            mir::MakeCallExpr(
                mir::Direct{
                    .target = support::BuiltinFn::kEvaluationAttemptsLiveWord,
                    .receiver = attempts(block)},
                {Word(block, word_type, word)}, word_type));
      },
      [&](mir::Builder block, std::uint32_t) {
        return Word(block, word_type, 0);
      },
      automaton, shared);
  if (!satisfied) return std::unexpected(std::move(satisfied.error()));
  mir::Builder body = frame.Builder();
  std::vector<mir::ExprId> words;
  words.reserve(satisfied->size());
  for (const mir::LocalId word : *satisfied) {
    words.push_back(body.ReadLocal(word, word_type));
  }
  return WordsOf(unit, body, std::move(words));
}

// What a tick runs over an assertion's attempts, a function of the scope that
// the storage runs at the tick and again where an attempt matures later in
// its time step: the Booleans every attempt shares, read once, handed to each
// attempt the tick has not yet stepped.
auto LowerStepOverAttempts(
    ProcessLowerer& action, const WalkFrame& parent,
    const StaticCellBinding& attempts, const AssertionAutomaton& automaton,
    const std::vector<bool>& reads_saved) -> diag::Result<mir::CallableDecl> {
  mir::CompilationUnit& unit = action.Owner().Unit();
  mir::CallableCode code = mir::CallableCode::Defined(parent.span);
  CallableBindings bindings(unit, code);
  const mir::LocalId self_id = bindings.Declare(
      ReceiverOrigin{}, parent.current_class.cls->self_pointer_type);
  code.params = {self_id};
  code.result_type = unit.builtins.void_type;
  const WalkFrame frame = action.InRootScope(
      parent.WithBlock(&code.Body()).WithBindings(&bindings));
  mir::Builder body = frame.Builder();
  const ReceiverBuilder place = AttemptsAt(unit, frame, attempts);
  auto shared = BuildSharedSatisfiedPositions(
      action, frame, bindings, place, automaton, reads_saved);
  if (!shared) return std::unexpected(std::move(shared.error()));
  body.AppendStmt(
      mir::ExprStmt{
          .expr = body.Add(
              mir::MakeCallExpr(
                  mir::Direct{
                      .target = support::BuiltinFn::kEvaluationAttemptsRun,
                      .receiver = place(body)},
                  {*shared}, unit.builtins.void_type))});
  body.AppendStmt(mir::ReturnStmt{.value = std::nullopt});
  return mir::CallableDecl{
      .code = std::move(code),
      .foreign = std::nullopt,
      .virtual_dispatch = std::nullopt,
      .defining_unit = std::nullopt};
}

// Starts the statement `arm` an attempt's answer selects, as an execution of
// its own that may stop and resume (LRM 16.3, 16.14.1), from the attempt's
// closure being lowered into `frame`: a closure of it, which reads what the
// attempt saved as that closure does (LRM 16.14.6.1).
auto LowerActionStartInto(
    ProcessLowerer& action, const WalkFrame& frame, const ActionArm& arm,
    diag::SourceSpan written_at) -> diag::Result<void> {
  if (std::holds_alternative<RunsNothing>(arm)) return {};
  mir::CompilationUnit& unit = action.Owner().Unit();
  ClosureBuilder body(unit, frame, support::ClosureKind::kFnOnce);
  const WalkFrame body_frame = body.Frame().WithRunsAsFunctionCall(false);
  auto appended = std::visit(
      Overloaded{
          [&](const RunsStatements& statements) -> diag::Result<void> {
            auto lowered = action.LowerStmt(
                action.HirBody().stmts.Get(statements.stmt), body_frame);
            if (!lowered) return std::unexpected(std::move(lowered.error()));
            body.Body().AppendStmt(*std::move(lowered));
            return {};
          },
          [&](const RunsDefaultReport& report) -> diag::Result<void> {
            AppendDefaultReport(action, body_frame, report.directive);
            return {};
          },
          [](const RunsNothing&) -> diag::Result<void> { return {}; }},
      arm);
  if (!appended) return std::unexpected(std::move(appended.error()));
  mir::Builder block = frame.Builder();
  const mir::ExprId started = block.Add(body.BuildCallOnce());
  block.AppendStmt(
      mir::ExprStmt{
          .expr = BuildExecutionStart(
              action.Owner(), frame, started,
              support::BuiltinFn::kRunAssertionAction, written_at)});
  return {};
}

// What one attempt's closure does where its assertion is evaluated against
// it (LRM 16.14.6): `reads_saved` names the positions whose Booleans read a
// value the attempt saved, and the statements each outcome selects where the
// storage starts them -- an expect statement's process runs its own.
struct AttemptCode {
  const hir::PropertySpec* spec = nullptr;
  const AssertionAutomaton* automaton = nullptr;
  const std::vector<bool>* reads_saved = nullptr;
  ActionArm pass;
  ActionArm fail;
  diag::SourceSpan written_at;
};

// The closure of one attempt, made where `frame` stands: the code of the
// attempt bound to what it was made with, called for each event its storage
// tells it of (`support::AttemptEvent`). A tick steps the attempt unless the
// disable condition holds, which it answers with 1 (LRM 16.12); a disable test
// answers whether the condition holds; an outcome starts the statement it
// selects (LRM 16.14.1). `saved` is what a procedural assertion saved where
// its instance was queued (LRM 16.14.6.1), read where the source wrote it.
auto BuildAttemptClosure(
    ProcessLowerer& action, const WalkFrame& frame, const AttemptCode& code,
    std::span<const SavedValue> saved) -> diag::Result<mir::Expr> {
  mir::CompilationUnit& unit = action.Owner().Unit();
  const mir::TypeId word_type = unit.builtins.machine_word;
  const mir::TypeId attempt_type =
      mir::LibraryTypeOf(unit.types, support::LibraryDecl::kEvaluationAttempt);
  const mir::TypeId attempt_pointer = unit.types.Intern(
      mir::Type{mir::PointerType{
          .pointee = attempt_type,
          .ownership = mir::PointerOwnership::kBorrowed,
          .mutability = mir::Mutability::kMutable}});

  ClosureBuilder closure(unit, frame, support::ClosureKind::kFn);
  const mir::LocalId event = closure.AddParam(word_type);
  const mir::LocalId attempt_param = closure.AddParam(attempt_pointer);
  const WalkFrame body_frame = closure.Frame().WithSavedValues(saved);
  mir::Builder body = body_frame.Builder();
  const mir::LocalId answer =
      DeclareLocal(body_frame, Word(body, word_type, 0));
  const ReceiverBuilder attempt = [&unit, attempt_type, attempt_pointer,
                                   attempt_param](mir::Builder block) {
    return block.Deref(
        unit.types, block.ReadLocal(attempt_param, attempt_pointer),
        attempt_type);
  };
  // The disable condition, as of where the arm stands (LRM 16.12).
  const auto disabled = [&](const WalkFrame& in) -> diag::Result<mir::ExprId> {
    auto condition = action.LowerExpr(
        action.HirBody().exprs.Get(code.spec->disable->condition), in);
    if (!condition) return std::unexpected(std::move(condition.error()));
    return ReduceToCondition(
        unit, in.Builder(), in.Builder().Add(*std::move(condition)));
  };
  const auto answer_one = [&](mir::Builder block) {
    Assign(unit, block, answer, word_type, Word(block, word_type, 1));
  };

  const ArmBody tick = [&](const WalkFrame& in) -> diag::Result<mir::Block> {
    mir::Block block;
    const WalkFrame at = in.WithBlock(&block);
    mir::Block step;
    const WalkFrame step_frame = at.WithBlock(&step);
    CallableBindings& bindings = closure.Bindings();
    const AssertionAutomaton& automaton = *code.automaton;
    auto satisfied = DeclareSatisfiedPositions(
        action, step_frame, bindings,
        [&](mir::Builder words, std::uint32_t word) {
          return words.Add(
              mir::MakeCallExpr(
                  mir::Direct{
                      .target = support::BuiltinFn::kEvaluationAttemptLiveWord,
                      .receiver = attempt(words)},
                  {Word(words, word_type, word)}, word_type));
        },
        [&](mir::Builder words, std::uint32_t word) {
          return words.Add(
              mir::MakeCallExpr(
                  mir::Direct{
                      .target =
                          support::BuiltinFn::kEvaluationAttemptSharedWord,
                      .receiver = attempt(words)},
                  {Word(words, word_type, word)}, word_type));
        },
        automaton, *code.reads_saved);
    if (!satisfied) return std::unexpected(std::move(satisfied.error()));
    auto ticked = LowerStepOfAttempt(
        action, step_frame, bindings, attempt, automaton, *satisfied);
    if (!ticked) return std::unexpected(std::move(ticked.error()));
    AppendBlock(step_frame.Builder(), *std::move(ticked));
    if (!code.spec->disable.has_value()) {
      AppendBlock(at.Builder(), std::move(step));
      return block;
    }
    // A disabled attempt is discarded and steps no further, and one
    // that would start here starts disabled (LRM 16.12, 16.14.1).
    auto condition = disabled(at);
    if (!condition) return std::unexpected(std::move(condition.error()));
    mir::Block discarded;
    answer_one(at.Builder().Into(discarded));
    mir::Builder at_builder = at.Builder();
    at_builder.AppendStmt(
        mir::IfStmt{
            .condition = *condition,
            .then_scope = at_builder.AddScope(std::move(discarded)),
            .else_scope = at_builder.AddScope(std::move(step))});
    return block;
  };
  const ArmBody disable_test =
      [&](const WalkFrame& in) -> diag::Result<mir::Block> {
    mir::Block block;
    if (!code.spec->disable.has_value()) return block;
    const WalkFrame at = in.WithBlock(&block);
    auto condition = disabled(at);
    if (!condition) return std::unexpected(std::move(condition.error()));
    mir::Block held;
    answer_one(at.Builder().Into(held));
    at.Builder().AppendIfThen(*condition, std::move(held));
    return block;
  };
  const auto outcome = [&](const ActionArm& arm) -> ArmBody {
    return [&](const WalkFrame& in) -> diag::Result<mir::Block> {
      mir::Block block;
      auto started = LowerActionStartInto(
          action, in.WithBlock(&block), arm, code.written_at);
      if (!started) return std::unexpected(std::move(started.error()));
      return block;
    };
  };
  // What each event the storage calls the closure for runs.
  const auto body_for = [&](support::AttemptEvent called) -> ArmBody {
    switch (called) {
      case support::AttemptEvent::kTick:
        return tick;
      case support::AttemptEvent::kDisableTest:
        return disable_test;
      case support::AttemptEvent::kSucceeded:
        return outcome(code.pass);
      case support::AttemptEvent::kFailed:
        return outcome(code.fail);
    }
    throw InternalError("BuildAttemptClosure: unknown support::AttemptEvent");
  };
  std::vector<ValueArm> arms;
  for (const support::AttemptEvent called :
       {support::AttemptEvent::kTick, support::AttemptEvent::kDisableTest,
        support::AttemptEvent::kSucceeded, support::AttemptEvent::kFailed}) {
    arms.push_back(
        ValueArm{
            .values = {static_cast<std::int64_t>(called)},
            .body = body_for(called)});
  }
  auto dispatched = BuildValueSwitch(
      body_frame, body.ReadLocal(event, word_type), arms, std::nullopt);
  if (!dispatched) return std::unexpected(std::move(dispatched.error()));
  body.AppendStmt(*std::move(dispatched));
  return closure.Build(body.ReadLocal(answer, word_type));
}

// An attempt of `code` becoming due (LRM 16.14.5, 16.14.6), as a statement of
// `frame`'s block: its closure, reading `saved`, made in the attempts, which
// open it now or at the next tick of its clock.
auto AppendAttemptBecomingDue(
    ProcessLowerer& action, const WalkFrame& frame, const AttemptCode& code,
    std::span<const SavedValue> saved, const StaticCellBinding& attempts)
    -> diag::Result<void> {
  mir::CompilationUnit& unit = action.Owner().Unit();
  auto made = BuildAttemptClosure(action, frame, code, saved);
  if (!made) return std::unexpected(std::move(made.error()));
  mir::Builder block = frame.Builder();
  block.AppendStmt(
      mir::ExprStmt{
          .expr = block.Add(BuildEmplacedClosure(
              unit, frame,
              BuildStaticStorageAddress(unit, block, frame, attempts),
              *std::move(made)))});
  return {};
}

// LRM 16.12 disables an evaluation attempt if the disable condition is true at
// any time between the start of that attempt and its end, both inclusive. That
// interval is not made of ticks, so the condition is watched for change rather
// than read at the clock: a reset that rises and falls between two ticks is low
// at every tick an attempt reads, and still preempts every attempt it spans.
// Each attempt's closure tests it, since it may read what the attempt
// saved.
auto LowerDisableWatcher(
    ProcessLowerer& action, const WalkFrame& ctor_frame,
    const StaticCellBinding& attempts, const hir::DisableCondition& disable,
    std::span<const hir::ExprId> saved) -> diag::Result<mir::CallableDecl> {
  mir::CompilationUnit& unit = action.Owner().Unit();
  const mir::TypeId void_type = unit.builtins.void_type;

  mir::CallableCode code = mir::CallableCode::Defined(ctor_frame.span);
  CallableBindings bindings(unit, code);
  const mir::LocalId self_id = bindings.Declare(
      ReceiverOrigin{}, ctor_frame.current_class.cls->self_pointer_type);

  mir::Block body_block;
  const WalkFrame body_frame =
      ctor_frame.WithBindings(&bindings).WithBlock(&body_block);
  mir::Builder body_block_builder = body_frame.Builder();
  // An attempt reads its own saved copy of each variable it saved (LRM
  // 16.14.6.1), so a later change to the variable reaches no attempt; only the
  // rest of what the condition reads is watched.
  std::vector<hir::SensitivityEntry> watched;
  for (const hir::SensitivityEntry& entry : disable.sensitivity) {
    const auto* var = std::get_if<hir::ProceduralVarRef>(&entry.cell);
    const bool is_saved =
        var != nullptr && std::ranges::any_of(saved, [&](hir::ExprId id) {
          return ProceduralVarRead(action.HirBody().exprs.Get(id)) == var->var;
        });
    if (!is_saved) watched.push_back(entry);
  }
  auto waited =
      BuildValueChangeWaitStmt(body_block_builder, body_frame, action, watched);
  if (!waited) return std::unexpected(std::move(waited.error()));
  body_block_builder.AppendStmt(*std::move(waited));
  body_block_builder.AppendStmt(
      mir::ExprStmt{
          .expr = body_block_builder.Add(
              mir::MakeCallExpr(
                  mir::Direct{
                      .target =
                          support::BuiltinFn::kEvaluationAttemptsTestDisable,
                      .receiver = AttemptsAt(
                          unit, body_frame, attempts)(body_block_builder)},
                  {}, void_type))});

  mir::Builder entry_builder(code.Body(), body_frame.span);
  const mir::BlockId body_scope_id =
      entry_builder.AddScope(std::move(body_block));
  entry_builder.AppendStmt(
      mir::ForStmt{
          .init = {},
          .condition = std::nullopt,
          .step = {},
          .scope = body_scope_id});
  entry_builder.AppendStmt(mir::ReturnStmt{.value = std::nullopt});
  code.params = {self_id};
  code.result_type = unit.builtins.coroutine_void;
  return mir::CallableDecl{
      .code = std::move(code),
      .foreign = std::nullopt,
      .virtual_dispatch = std::nullopt,
      .defining_unit = std::nullopt};
}

// The process of an assertion's leading clock. LRM 16.5 evaluates an assertion
// in the Observed region, so the process waits wherever any event wait waits
// and the tick goes to Observed as a deferred effect -- nothing in the engine
// learns what a concurrent assertion is. A static assertion is the statement
// reached at every tick of its clock (LRM 16.14.5), so where
// `matured_at_each_tick` is given, an attempt of that code matures at each
// tick, its closure made in the attempts.
auto LowerProcess(
    ProcessLowerer& action, const WalkFrame& ctor_frame,
    const hir::EventControl& clock, const StaticCellBinding& attempts,
    const std::optional<AttemptCode>& matured_at_each_tick)
    -> diag::Result<mir::CallableDecl> {
  mir::CompilationUnit& unit = action.Owner().Unit();
  const mir::TypeId void_type = unit.builtins.void_type;
  const mir::TypeId self_ptr_type =
      ctor_frame.current_class.cls->self_pointer_type;

  mir::CallableCode code = mir::CallableCode::Defined(ctor_frame.span);
  CallableBindings bindings(unit, code);
  const mir::LocalId self_id =
      bindings.Declare(ReceiverOrigin{}, self_ptr_type);

  mir::Block body_block;
  const WalkFrame body_frame = action.InRootScope(
      ctor_frame.WithBindings(&bindings).WithBlock(&body_block));
  mir::Builder body_block_builder = body_frame.Builder();

  // The clock's own expression lives in the assertion's body, beside the
  // Booleans the property reads, so it is that body's lowering that resolves
  // it.
  auto wait = BuildEventWaitStmt(action, body_frame, body_block_builder, clock);
  if (!wait) return std::unexpected(std::move(wait.error()));
  body_block_builder.AppendStmt(*std::move(wait));
  if (matured_at_each_tick.has_value()) {
    if (auto due = AppendAttemptBecomingDue(
            action, body_frame, *matured_at_each_tick, {}, attempts);
        !due) {
      return std::unexpected(std::move(due.error()));
    }
  }

  ClosureBuilder closure(unit, body_frame, support::ClosureKind::kFn);
  RunDeferredBodyOnNoThread(action.Owner(), body_frame, closure);
  closure.Body().AppendStmt(
      mir::ExprStmt{
          .expr = closure.Body().Add(
              mir::MakeCallExpr(
                  mir::Direct{
                      .target = support::BuiltinFn::kEvaluationAttemptsTick,
                      .receiver = AttemptsAt(
                          unit, closure.Frame(), attempts)(closure.Body())},
                  {}, void_type))});

  mir::Expr keeper = BuildRegionEffectsCallExpr(
      action.Owner(), body_block_builder, support::BuiltinFn::kObservedEffects);
  const mir::ExprId kept = body_block_builder.Add(BuildEmplacedClosure(
      unit, body_frame, std::move(keeper), closure.BuildVoid()));
  body_block_builder.AppendStmt(mir::ExprStmt{.expr = kept});

  mir::Builder entry_builder(code.Body(), body_frame.span);
  const mir::BlockId body_scope_id =
      entry_builder.AddScope(std::move(body_block));
  entry_builder.AppendStmt(
      mir::ForStmt{
          .init = {},
          .condition = std::nullopt,
          .step = {},
          .scope = body_scope_id});
  entry_builder.AppendStmt(mir::ReturnStmt{.value = std::nullopt});
  code.params = {self_id};
  code.result_type = unit.builtins.coroutine_void;
  return mir::CallableDecl{
      .code = std::move(code),
      .foreign = std::nullopt,
      .virtual_dispatch = std::nullopt,
      .defining_unit = std::nullopt};
}

// LRM 16.14.6.1: what a procedural concurrent assertion saves where its
// statement is reached -- each read of an automatic variable of the procedure
// that its property, its disable condition or its action block makes, and
// each constant cast in any of them, as the value it makes rather than the
// variables under it. The attempt reads them as they were, every tick and in
// its action block, as a lambda's by-value captures are read.
//
// What it saves of is each automatic variable the scopes around the statement
// declare, from the body's root down to the scope declaring the statement's
// control, which is where the statement stands. A variable its action block
// declares lies in a scope below that and is its own.
auto SavedOperands(
    const ProcessLowerer& process, const hir::ConcurrentAssertion& assertion)
    -> std::vector<hir::ExprId> {
  const hir::ProceduralBody& body = process.HirBody();
  const AssertionDispositions dispositions = DispositionsOf(assertion);
  const auto& control = std::get<AssertionControlCell>(
      process.ControlCellOf(dispositions.control).of);
  const auto& scopes =
      process.EnclosingScopeLowerer().HirScope().procedural_scopes;
  std::vector<hir::ProceduralVarId> around;
  for (const hir::ProceduralScopeId id : control.within) {
    std::ranges::copy(scopes.Get(id).declarations, std::back_inserter(around));
  }
  std::vector<hir::ExprId> saved;
  const std::function<bool(hir::ExprId)> visit = [&](hir::ExprId id) {
    if (const auto* conversion =
            std::get_if<hir::ConversionExpr>(&body.exprs.Get(id).data);
        conversion != nullptr && IsConstCast(conversion->kind)) {
      saved.push_back(id);
      return false;
    }
    const std::optional<hir::ProceduralVarId> var =
        ProceduralVarRead(body.exprs.Get(id));
    if (var.has_value() && std::ranges::find(around, *var) != around.end()) {
      saved.push_back(id);
      return false;
    }
    return true;
  };
  const OperandWalk walk{.body = &body, .visit = &visit};
  walk.Evaluated().ThroughProperty(dispositions.spec->body);
  if (dispositions.spec->disable.has_value()) {
    walk(dispositions.spec->disable->condition);
  }
  for (const std::optional<hir::StmtId>& arm :
       {dispositions.pass_stmt, dispositions.fail_stmt}) {
    if (arm.has_value()) WalkStmt(walk, *arm);
  }
  return saved;
}

// Which positions of `automaton` read a value of `saved`, and so are read by
// each attempt rather than once for every attempt of the assertion.
auto PositionsReadingSaved(
    const hir::ProceduralBody& body, const AssertionAutomaton& automaton,
    std::span<const hir::ExprId> saved) -> std::vector<bool> {
  std::vector<bool> reads_saved(automaton.positions.size(), false);
  for (std::size_t position = 0; position < reads_saved.size(); ++position) {
    bool reads = false;
    const std::function<bool(hir::ExprId)> visit = [&](hir::ExprId id) {
      if (std::ranges::find(saved, id) != saved.end()) reads = true;
      return !reads;
    };
    const OperandWalk walk{.body = &body, .visit = &visit};
    for (const hir::ExprId conjunct : automaton.positions[position].conjuncts) {
      walk(conjunct);
    }
    reads_saved[position] = reads;
  }
  return reads_saved;
}

// What every lowering of one assertion statement reads of it: the property's
// automaton, what a procedural one saves, and which positions read it.
struct AnalyzedAssertion {
  AssertionDispositions dispositions;
  AssertionAutomaton automaton;
  std::vector<hir::ExprId> saved;
  std::vector<bool> reads_saved;
};

auto AnalyzeAssertion(
    const hir::ProceduralBody& body, const hir::ConcurrentAssertion& assertion,
    std::vector<hir::ExprId> saved) -> AnalyzedAssertion {
  const AssertionDispositions dispositions = DispositionsOf(assertion);
  AssertionAutomaton automaton =
      BuildAssertionAutomaton(body, *dispositions.spec);
  std::vector<bool> reads_saved = PositionsReadingSaved(body, automaton, saved);
  return AnalyzedAssertion{
      .dispositions = dispositions,
      .automaton = std::move(automaton),
      .saved = std::move(saved),
      .reads_saved = std::move(reads_saved)};
}

// The same for an assertion a procedure holds as a statement, which saves what
// it reads of the procedure's automatics and its constant casts (LRM
// 16.14.6.1). It is a function of the statement alone, so the scope's
// machinery and the statement, which each ask, are given one answer.
auto AnalyzeProceduralAssertion(
    const ProcessLowerer& process, const hir::ConcurrentAssertion& assertion)
    -> AnalyzedAssertion {
  return AnalyzeAssertion(
      process.HirBody(), assertion, SavedOperands(process, assertion));
}

// The code of each attempt of an installed assertion, which starts the
// statement its outcome selects.
auto InstalledAttemptCode(
    const hir::ProceduralBody& body, const AnalyzedAssertion& analyzed,
    diag::SourceSpan written_at) -> AttemptCode {
  return AttemptCode{
      .spec = analyzed.dispositions.spec,
      .automaton = &analyzed.automaton,
      .reads_saved = &analyzed.reads_saved,
      .pass = PassArmOf(body, analyzed.dispositions),
      .fail = FailArmOf(body, analyzed.dispositions),
      .written_at = written_at};
}

// The machinery of one assertion, from the body its expressions are written
// in, each callable added to `mir_class`: what a tick runs, its clock's
// process, and where the source wrote `disable iff` the process watching that
// condition. A static assertion's process makes the closure of each
// attempt it begins.
auto LowerMachinery(
    ProcessLowerer& action, mir::Class& mir_class, const WalkFrame& ctor_frame,
    const AnalyzedAssertion& analyzed,
    const std::optional<AttemptCode>& matured_at_each_tick)
    -> diag::Result<InstalledConcurrentAssertion> {
  const AssertionDispositions& dispositions = analyzed.dispositions;
  const StaticCellBinding& attempts =
      action.AttemptsCellOf(dispositions.control);

  auto step = LowerStepOverAttempts(
      action, ctor_frame, attempts, analyzed.automaton, analyzed.reads_saved);
  if (!step) return std::unexpected(std::move(step.error()));

  auto process = LowerProcess(
      action, ctor_frame, dispositions.spec->clock, attempts,
      matured_at_each_tick);
  if (!process) return std::unexpected(std::move(process.error()));
  std::vector<mir::CallableId> processes;
  processes.push_back(mir_class.callables.Add(*std::move(process)));
  if (dispositions.spec->disable.has_value()) {
    auto watcher = LowerDisableWatcher(
        action, ctor_frame, attempts, *dispositions.spec->disable,
        analyzed.saved);
    if (!watcher) return std::unexpected(std::move(watcher.error()));
    processes.push_back(mir_class.callables.Add(*std::move(watcher)));
  }

  return InstalledConcurrentAssertion{
      .processes = std::move(processes),
      .step = mir_class.callables.Add(*std::move(step)),
      .start = analyzed.automaton.parts.front().start,
      .control = action.ControlCellOf(dispositions.control),
      .attempts = attempts};
}

}  // namespace

auto LowerConcurrentAssertion(
    StructuralScopeLowerer& lowerer, mir::Class& mir_class,
    const WalkFrame& ctor_frame, const StorageBringUp& bring_up,
    const DeclaredScopes& scopes, const StaticCellBindings& statics,
    const hir::ConcurrentAssertionDecl& decl)
    -> diag::Result<InstalledConcurrentAssertion> {
  ProcessLowerer action(
      lowerer.Owner(), &lowerer, decl.action, std::nullopt, ctor_frame, scopes,
      statics);
  // A static assertion is the statement reached at every tick of its leading
  // clock (LRM 16.14.5), and saves nothing, since nothing it reads is
  // automatic and it is queued nowhere.
  const AnalyzedAssertion analyzed =
      AnalyzeAssertion(decl.action, decl.assertion, {});
  auto built = LowerMachinery(
      action, mir_class, ctor_frame, analyzed,
      InstalledAttemptCode(decl.action, analyzed, decl.span));
  if (!built) return std::unexpected(std::move(built.error()));

  for (const StaticCellBinding& binding : statics) {
    BringUpStaticCell(action, decl.action, bring_up, binding);
  }
  return built;
}

auto LowerProceduralConcurrentAssertion(
    StructuralScopeLowerer& lowerer, mir::Class& mir_class,
    const WalkFrame& ctor_frame, const DeclaredScopes& scopes,
    const StaticCellBindings& statics, const hir::ProceduralBody& body,
    const hir::ConcurrentAssertion& assertion)
    -> diag::Result<InstalledConcurrentAssertion> {
  ProcessLowerer action(
      lowerer.Owner(), &lowerer, body, std::nullopt, ctor_frame, scopes,
      statics);
  const AnalyzedAssertion analyzed =
      AnalyzeProceduralAssertion(action, assertion);
  return LowerMachinery(action, mir_class, ctor_frame, analyzed, std::nullopt);
}

auto LowerExpectStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::ExpectStmt& expect, diag::SourceSpan span)
    -> diag::Result<mir::Stmt> {
  mir::CompilationUnit& unit = process.Owner().Unit();
  const mir::TypeId word_type = unit.builtins.machine_word;
  const mir::TypeId void_type = unit.builtins.void_type;
  // An expect reads the process's automatic variables at their values as of
  // the Observed region (LRM 16.5.1, 16.17), so it saves nothing.
  const AssertionAutomaton automaton =
      BuildAssertionAutomaton(process.HirBody(), expect.spec);
  const std::vector<bool> reads_saved(automaton.positions.size(), false);

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
  mir::Builder scope_builder = inner.Builder();
  auto clock_leaves = BuildEventLeaves(process, inner, expect.spec.clock);
  if (!clock_leaves) return std::unexpected(std::move(clock_leaves.error()));
  auto clock = BuildTriggerArray(scope_builder, inner, process, *clock_leaves);
  if (!clock) return std::unexpected(std::move(clock.error()));
  std::vector<ObservedLeaf> disable_leaves;
  if (expect.spec.disable.has_value()) {
    disable_leaves = BuildValueChangeLeaves(
        unit, inner, scope_builder, expect.spec.disable->sensitivity);
  }
  auto disable_reads =
      BuildTriggerArray(scope_builder, inner, process, disable_leaves);
  if (!disable_reads) return std::unexpected(std::move(disable_reads.error()));
  std::vector<mir::ExprId> start;
  for (const std::uint64_t word : automaton.parts.front().start) {
    start.push_back(Word(scope_builder, word_type, word));
  }
  const mir::LocalId wait = DeclareLocal(
      inner,
      scope_builder.Add(
          mir::MakeCallExpr(
              mir::Direct{
                  .target = support::BuiltinFn::kWaitOnPropertyEvaluation},
              {scope_builder.Add(
                   BuildCurrentRuntimeCallExpr(process.Owner().Unit())),
               *clock, *disable_reads,
               WordsOf(unit, scope_builder, std::move(start)),
               scope_builder.Add(BuildStaticStorageAddress(
                   unit, scope_builder, inner,
                   process.ControlCellOf(expect.control)))},
              unit.builtins.wait)));
  const mir::LocalId wait_pointer = DeclareLocal(
      inner, scope_builder.Add(
                 mir::MakeAddressOfExpr(
                     scope_builder.LocalPlace(wait, unit.builtins.wait),
                     unit.builtins.wait_ptr)));

  const mir::TypeId attempts_type =
      mir::LibraryTypeOf(unit.types, support::LibraryDecl::kEvaluationAttempts);
  const mir::TypeId attempts_pointer = unit.types.Intern(
      mir::Type{mir::PointerType{
          .pointee = attempts_type,
          .ownership = mir::PointerOwnership::kBorrowed}});
  // The attempt the wait holds, which stays where it is for as long as the
  // wait does, so it is looked up once.
  const mir::LocalId attempts_held = DeclareLocal(
      inner,
      scope_builder.Add(
          mir::MakeCallExpr(
              mir::Direct{.target = support::BuiltinFn::kAttemptsOf},
              {scope_builder.ReadLocal(wait_pointer, unit.builtins.wait_ptr)},
              attempts_pointer)));
  // The attempt, through the local `pointer` holding its address.
  const auto attempts_at = [&](mir::LocalId pointer) -> ReceiverBuilder {
    return
        [&unit, attempts_type, attempts_pointer, pointer](mir::Builder block) {
          return block.Deref(
              unit.types, block.ReadLocal(pointer, attempts_pointer),
              attempts_type);
        };
  };

  // What the wait runs is code of this body and reads its frame -- what an
  // automatic variable or a `ref` formal holds when the Observed region
  // evaluates is what the property reads (LRM 16.5.1) -- so each is a closure
  // holding the addresses it reaches the evaluation through. The wait holds
  // both and is a local of the frame they read, so neither runs once what it
  // reads is gone.
  ClosureBuilder step(unit, inner, support::ClosureKind::kFn);
  const mir::LocalId step_attempts =
      step.Bindings().DeclareAnonymous(attempts_pointer);
  step.Body().AppendStmt(
      mir::LocalDeclStmt{
          .target = step_attempts,
          .init = SnapshotValueIntoClosure(
              process.Owner(), inner, step,
              scope_builder.ReadLocal(attempts_held, attempts_pointer))});
  auto shared = BuildSharedSatisfiedPositions(
      process, step.Frame(), step.Bindings(), attempts_at(step_attempts),
      automaton, reads_saved);
  if (!shared) return std::unexpected(std::move(shared.error()));
  step.Body().AppendStmt(
      mir::ExprStmt{
          .expr = step.Body().Add(
              mir::MakeCallExpr(
                  mir::Direct{
                      .target = support::BuiltinFn::kEvaluationAttemptsRun,
                      .receiver = attempts_at(step_attempts)(step.Body())},
                  {*shared}, void_type))});
  const mir::ExprId step_closure =
      BuildKeptClosure(unit, scope_builder, step.BuildVoid());

  auto made = BuildAttemptClosure(
      process, inner,
      AttemptCode{
          .spec = &expect.spec,
          .automaton = &automaton,
          .reads_saved = &reads_saved,
          .pass = RunsNothing{},
          .fail = RunsNothing{},
          .written_at = span},
      {});
  if (!made) return std::unexpected(std::move(made.error()));
  const mir::ExprId attempt_closure =
      BuildKeptClosure(unit, scope_builder, *std::move(made));

  // The first evaluation is at the next tick of the clock (LRM 16.17), which
  // only the wait just made hears.
  scope_builder.AppendStmt(
      mir::ExprStmt{
          .expr = scope_builder.Add(
              mir::MakeCallExpr(
                  mir::Direct{
                      .target = support::BuiltinFn::kBindPropertyEvaluation},
                  {scope_builder.ReadLocal(
                       wait_pointer, unit.builtins.wait_ptr),
                   step_closure, attempt_closure},
                  void_type))});
  scope_builder.AppendStmt(BuildParkStmt(process.Owner(), scope_builder, wait));

  const auto answer_is = [&](mir::Builder block,
                             support::AttemptAnswer answer) {
    const mir::ExprId lhs = block.Add(
        mir::MakeCallExpr(
            mir::Direct{
                .target = support::BuiltinFn::kEvaluationAttemptsAnswer,
                .receiver = attempts_at(attempts_held)(block)},
            {}, word_type));
    const mir::ExprId rhs =
        Word(block, word_type, static_cast<std::uint64_t>(answer));
    return Op(
        unit, block, unit.builtins.machine_bool, mir::BinaryOp::kEquality, lhs,
        rhs);
  };
  auto pass = HasRealArm(process.HirBody(), expect.pass_stmt)
                  ? LowerStmtIntoChildScope(process, inner, *expect.pass_stmt)
                  : diag::Result<mir::Block>{mir::Block{}};
  if (!pass) return std::unexpected(std::move(pass.error()));
  diag::Result<mir::Block> fail = mir::Block{};
  if (!expect.fail_stmt.has_value()) {
    AppendToolReportStmt(
        process.Owner().Unit(), inner.WithBlock(&*fail), support::kErrorReport,
        base::DiagMessage("expect failed"));
  } else if (HasRealArm(process.HirBody(), expect.fail_stmt)) {
    fail = LowerStmtIntoChildScope(process, inner, *expect.fail_stmt);
  }
  if (!fail) return std::unexpected(std::move(fail.error()));
  // The process goes on whatever arm runs, and the arm runs where the control
  // allowed it when the expect began (LRM 20.11).
  mir::Block answered;
  mir::Builder answered_builder = scope_builder.Into(answered);
  answered_builder.AppendStmt(
      mir::IfStmt{
          .condition =
              answer_is(answered_builder, support::AttemptAnswer::kFails),
          .then_scope = answered_builder.AddScope(*std::move(fail)),
          .else_scope = answered_builder.AddScope(*std::move(pass))});
  scope_builder.AppendStmt(
      mir::IfStmt{
          .condition = scope_builder.Add(
              mir::MakeCallExpr(
                  mir::Direct{
                      .target =
                          support::BuiltinFn::kEvaluationAttemptsActionRuns,
                      .receiver = attempts_at(attempts_held)(scope_builder)},
                  {}, unit.builtins.machine_bool)),
          .then_scope = scope_builder.AddScope(std::move(answered)),
          .else_scope = std::nullopt});

  return mir::Stmt{
      .label = std::move(label),
      .data =
          mir::BlockStmt{.scope = frame.Builder().AddScope(std::move(scope))}};
}

void AppendConcurrentAssertionInstall(
    const StructuralScopeLowerer& lowerer, const WalkFrame& activate_frame,
    const InstalledConcurrentAssertion& installed) {
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  mir::Builder block = activate_frame.Builder();
  const mir::TypeId word_type = unit.builtins.machine_word;

  // What a tick runs is the scope's own function, called in place.
  ClosureBuilder step(unit, activate_frame, support::ClosureKind::kFn);
  step.Body().AppendStmt(
      mir::ExprStmt{
          .expr = BuildOwnCallExpr(
              unit, step.Frame(), installed.step, unit.builtins.void_type)});
  const mir::ExprId step_closure =
      BuildKeptClosure(unit, block, step.BuildVoid());

  std::vector<mir::ExprId> start;
  for (const std::uint64_t word : installed.start) {
    start.push_back(Word(block, word_type, word));
  }
  const mir::ExprId attempts =
      AttemptsAt(unit, activate_frame, installed.attempts)(block);
  block.AppendStmt(
      mir::ExprStmt{
          .expr = block.Add(
              mir::MakeCallExpr(
                  mir::Direct{
                      .target = support::BuiltinFn::kEvaluationAttemptsInstall,
                      .receiver = attempts},
                  {block.Add(
                       BuildCurrentRuntimeCallExpr(lowerer.Owner().Unit())),
                   WordsOf(unit, block, std::move(start)), step_closure,
                   block.Add(BuildStaticStorageAddress(
                       unit, block, activate_frame, installed.control))},
                  unit.builtins.void_type))});
}

auto LowerProceduralConcurrentAssertionStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::ConcurrentAssertion& assertion, diag::SourceSpan span)
    -> diag::Result<mir::Stmt> {
  mir::CompilationUnit& unit = process.Owner().Unit();
  const hir::ProceduralBody& body = process.HirBody();
  const AnalyzedAssertion analyzed =
      AnalyzeProceduralAssertion(process, assertion);
  const AssertionDispositions& dispositions = analyzed.dispositions;

  // A pending instance that matures begins an attempt (LRM 16.14.6): its
  // closure, made in the attempts, reading the values saved here.
  mir::Block scope;
  const WalkFrame scope_frame = frame.WithBlock(&scope);
  mir::Builder scope_builder = scope_frame.Builder();
  auto saved = process.SaveValues(analyzed.saved, scope_frame);
  if (!saved) return std::unexpected(std::move(saved.error()));
  ClosureBuilder instance(unit, scope_frame, support::ClosureKind::kFn);
  // It matures in the Observed region, where no process runs it.
  RunDeferredBodyOnNoThread(process.Owner(), scope_frame, instance);
  if (auto due = AppendAttemptBecomingDue(
          process, instance.Frame(), InstalledAttemptCode(body, analyzed, span),
          *saved, process.AttemptsCellOf(dispositions.control));
      !due) {
    return std::unexpected(std::move(due.error()));
  }

  // The control keeps it in the Observed region, on the condition that no
  // flush point, `disable` of its label or of the process, or Kill reaches it
  // first (LRM 16.14.6.2, 16.14.6.4, 20.11).
  mir::Expr keeper = BuildStaticStorageAddress(
      unit, scope_builder, scope_frame,
      process.ControlCellOf(dispositions.control));
  scope_builder.AppendStmt(
      mir::ExprStmt{
          .expr = scope_builder.Add(BuildEmplacedClosure(
              unit, scope_frame, std::move(keeper), instance.BuildVoid()))});

  return mir::Stmt{
      .label = std::move(label),
      .data =
          mir::BlockStmt{.scope = frame.Builder().AddScope(std::move(scope))}};
}

}  // namespace lyra::lowering::hir_to_mir
