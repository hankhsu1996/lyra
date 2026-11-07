#pragma once

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <variant>
#include <vector>

#include "lyra/base/time.hpp"
#include "lyra/runtime/owned_call.hpp"
#include "lyra/support/property_answer.hpp"

namespace lyra::runtime {

class AssertionControl;
class RuntimeEffects;

// Every evaluation attempt of one property that is still in flight (LRM
// 16.14.1, 16.17). A concurrent assertion begins an attempt at every tick of
// its clock and each carries its own result, so what is held here is a list of
// attempts and never one merged state: two attempts that reach their answers at
// the same tick owe two actions. An expect statement begins one, and its
// process waits for that one's answer.
//
// An attempt is a tree of evaluations. Each answers for one part of the
// property -- the whole of it, an instance of what an implication asks once per
// match of its antecedent (LRM 16.12.7), or the arm a selection took (LRM
// 16.12.6, 16.12.16) -- and is never pooled with another: a sequence's
// operators are existential, so two paths through one that reach the same
// position have the same future and their positions may be unioned, but an
// implication quantifies universally over its instances and a conjunction is
// not preserved by union. An instance is a consequent of one implication of the
// evaluation whose antecedent matched or whose selection read its condition,
// and an implication answers the least answer any of its instances reached
// (Annex F.5.3.1).
//
// An evaluation is a set of positions in the automaton the lowering built, held
// here as the words a generated transition reads it in, and its answer is
// computed by generated code from those positions and its implications'
// answers. Nothing here knows what a position means or how a part of the
// property combines its answers: the width arrives at installation, every bit
// pattern crosses opaque, and a part is a number the generated code chose.
//
// Whoever owns the attempts says when one is due and when the clock ticks, and
// this storage alone knows whether the current time step has ticked, which
// decides whether a due attempt begins now or waits (LRM 16.14.6). What a tick
// does over the attempts is generated code, given at installation, and both
// events run it.
class EvaluationAttempts {
 public:
  EvaluationAttempts() = default;

  // Non-movable for the reason every member storage is: a place resolves to
  // this address, so it has to stay put once anything has named it.
  EvaluationAttempts(const EvaluationAttempts&) = delete;
  auto operator=(const EvaluationAttempts&) -> EvaluationAttempts& = delete;
  EvaluationAttempts(EvaluationAttempts&&) = delete;
  auto operator=(EvaluationAttempts&&) -> EvaluationAttempts& = delete;
  ~EvaluationAttempts() = default;

  // Fixes how wide a position set is, what a tick runs over the attempts, the
  // statements an outcome selects, and the control whose state decides whether
  // an attempt begins and which of them its outcome runs (LRM 20.11), which
  // reaches these attempts again to abort them.
  //
  // Registering here is what puts the answers of attempts the run ends in the
  // middle of before the design's `final` procedures, which read what those
  // statements wrote.
  void Install(
      RuntimeEffects& effects, std::uint64_t words, OwnedCall step,
      OwnedCall pass_action, OwnedCall fail_action, AssertionControl* control);

  // Fixes how wide a position set is and what a tick runs for the one attempt
  // of an expect statement, whose process reads the answer itself (LRM 16.17):
  // nothing runs when it settles and nothing is owed when the run ends, since
  // a process still waiting then never goes on. `control` decides which arm an
  // answer may run; nothing turns an expect off or aborts it (LRM 20.11).
  void Hold(
      std::uint64_t words, const AssertionControl* control, OwnedCall step);

  // Stages one word of the position set the next `OpenMatured` or `Seed`
  // starts an evaluation from. A seed is a constant of the automaton, so it is
  // written a word at a time by whoever holds it rather than described here.
  void SeedWord(std::uint64_t word, std::uint64_t bits);

  // An attempt is due: it begins at the tick of the leading clock in this time
  // step, or where that has not come, at the next one (LRM 16.14.6). A static
  // assertion has one due at every tick (LRM 16.14.5), a procedural one where
  // a pending instance matures, and an expect where it is executed (LRM 16.17).
  // Where this time step's tick has already been run, what a tick runs is run
  // again now, which steps only the attempt it opens.
  void BeginAttempt();

  // A tick of the leading clock: every evaluation becomes one this tick has
  // not yet stepped, no implication has heard from a live instance yet, and
  // what a tick runs is run.
  void Tick();

  // Opens an attempt on the staged seed for every one that is due, each only
  // where checking is on now (LRM 20.11) and dropped otherwise. The attempt
  // keeps what its control allowed when it began.
  void OpenMatured();

  // A tick at which the disable condition held. Every attempt is discarded and
  // none begins -- a disabled attempt is neither a success nor a failure and
  // runs no statement, and one that would start here starts disabled, every
  // due one among them (LRM 16.12, 16.14.1).
  void DisableTick();

  // Aborts every attempt in flight, which settles none of them and runs no
  // statement: a Kill (LRM 20.11), and the disable condition coming true
  // between ticks (LRM 16.12). One that is due is no attempt yet and is kept;
  // it begins only where checking is on and the condition is false when its
  // tick comes.
  void Discard();

  // One word of the union of every position any unstepped evaluation is live
  // at, which is what bounds the Boolean expressions this tick has to read.
  [[nodiscard]] auto LiveWord(std::uint64_t word) const -> std::uint64_t;

  // The next evaluation this tick has not stepped, or -1 when none is left.
  // Seeding one during the sweep puts it here too, which is how a consequent
  // is read at the very tick its antecedent matched.
  [[nodiscard]] auto NextUnstepped() -> std::int64_t;

  [[nodiscard]] auto BitsAt(std::int64_t index, std::uint64_t word) const
      -> std::uint64_t;

  // Replaces one word of an evaluation's position set with its successor. Every
  // word of the old set is read before any is replaced, which is the caller's
  // to arrange.
  void SetWord(std::int64_t index, std::uint64_t word, std::uint64_t bits);

  void Step(std::int64_t index);

  // Adds an evaluation on the staged seed as an instance of `implication` of
  // the evaluation at `index`, answering for the part numbered `part`, to be
  // stepped at this same tick.
  void Seed(std::int64_t index, std::uint64_t implication, std::uint64_t part);

  // How many evaluations are held. A child is always held after its parent,
  // so answering them from the last back answers every instance before the
  // evaluation whose implication it is a consequent of.
  [[nodiscard]] auto Count() const -> std::int64_t;

  // The part of the property the evaluation at `index` answers for.
  [[nodiscard]] auto PartOf(std::int64_t index) const -> std::uint64_t;

  // The least answer any instance of `implication` of the evaluation at
  // `index` has reached, as a `support::PropertyAnswer`: every instance still
  // held, answered this tick, and every one already settled. With none, it
  // holds strongly -- an implication with no instance asks nothing.
  [[nodiscard]] auto ImplicationAnswer(
      std::int64_t index, std::uint64_t implication) const -> std::uint64_t;

  // 1 where some instance of `implication` of the evaluation at `index` is
  // nonvacuous, of those held and those settled, and 0 otherwise, including
  // where it has none (LRM 16.14.8).
  [[nodiscard]] auto ImplicationNonvacuous(
      std::int64_t index, std::uint64_t implication) const -> std::uint64_t;

  // Records what the evaluation at `index` answered this tick, a
  // `support::PropertyAnswer`, and whether it is nonvacuous (1) or not (0),
  // and counts both in the implication it is a consequent of.
  void Resolve(
      std::int64_t index, std::uint64_t answer, std::uint64_t nonvacuous);

  // Settles every attempt this tick answered (LRM 16.14.1): an instance whose
  // answer reached either end is folded into its implication and leaves with
  // the instances under it; an attempt whose answer reached either end has
  // failed or succeeded, and leaves too. An installed assertion submits the
  // statements the answer selects to the Reactive region; a held attempt keeps
  // its answer for its process.
  void Settle(RuntimeEffects& effects);

  // What a held attempt has come to, as a `support::AttemptAnswer`.
  [[nodiscard]] auto Answer() const -> std::uint64_t;

  // Whether the arm a held attempt's answer selects may run, as its control
  // allowed when the attempt began and by whether the success was vacuous.
  [[nodiscard]] auto ActionRuns() const -> bool;

  // The run is over, so no tick will settle what is left. An attempt's last
  // answer is what the finished trace says (Annex F.5.3.1): it succeeded where
  // that answer holds, and failed otherwise.
  void SettleAtEndOfRun();

 private:
  // What an answered attempt does. An installed assertion starts the
  // statements its answer selects, lent to each submission: this storage is
  // the design's, which outlives every region queue. A held attempt keeps its
  // answer for the process waiting on it.
  struct Installed {
    OwnedCall pass_action;
    OwnedCall fail_action;
  };
  struct Held {
    support::AttemptAnswer answer;
    bool action_runs;
  };

  // What an evaluation has come to: its answer and whether it is nonvacuous
  // (LRM 16.14.8), which every reader takes together.
  struct Outcome {
    support::PropertyAnswer answer;
    bool nonvacuous;

    // What an implication with no instance comes to: it asks nothing, so it
    // holds strongly, vacuously.
    static constexpr auto OfNoInstance() -> Outcome {
      return {
          .answer = support::PropertyAnswer::kHoldsStrongly,
          .nonvacuous = false};
    }

    // An implication holds where every one of its instances does, so two of
    // them come to the least answer either reached, conjoined, and are
    // nonvacuous where either is (LRM 16.14.8 h).
    [[nodiscard]] auto Conjoined(Outcome other) const -> Outcome {
      return {
          .answer = std::min(answer, other.answer),
          .nonvacuous = nonvacuous || other.nonvacuous};
    }
  };

  // What one implication's instances have come to: those that settled and
  // left, which outlive them, and the live ones this tick.
  struct ImplicationAnswers {
    std::uint64_t implication = 0;
    Outcome settled = Outcome::OfNoInstance();
    Outcome this_tick = Outcome::OfNoInstance();
  };

  // The evaluation an attempt opened, answering for the whole property, and
  // what its control allowed when it began, as `support::kAssertion*` bits.
  struct OpensAttempt {
    std::uint64_t allowed;
  };

  // The evaluation whose antecedent matched or whose selection read its
  // condition, and which of its implications the instance answers for.
  struct ConsequentOf {
    std::size_t parent;
    std::uint64_t implication;
  };

  struct Evaluation {
    std::variant<OpensAttempt, ConsequentOf> role;
    std::uint64_t part = 0;
    bool stepped = false;
    Outcome outcome = {
        .answer = support::PropertyAnswer::kPending, .nonvacuous = false};
    std::vector<ImplicationAnswers> implications;
  };

  // The record of `implication` in `evaluation`, made on first use.
  static auto AnswersOf(Evaluation& evaluation, std::uint64_t implication)
      -> ImplicationAnswers&;
  // What the instances of `implication` of the evaluation at `index` have
  // come to, settled and live together.
  [[nodiscard]] auto ImplicationOutcome(
      std::int64_t index, std::uint64_t implication) const -> Outcome;

  void RequireInstalled() const;
  void Size(std::uint64_t words, OwnedCall step);
  void OpenAttempt();
  void OpenEvaluation(
      std::variant<OpensAttempt, ConsequentOf> role, std::uint64_t part);
  [[nodiscard]] auto IndexOf(std::int64_t index) const -> std::size_t;
  [[nodiscard]] auto EvaluationAt(std::int64_t index) -> Evaluation&;
  [[nodiscard]] auto EvaluationAt(std::int64_t index) const
      -> const Evaluation&;
  [[nodiscard]] auto WordIndex(std::int64_t index, std::uint64_t word) const
      -> std::size_t;
  void Report(const Evaluation& attempt, RuntimeEffects& effects);

  // One position set per evaluation, laid end to end: an evaluation's words are
  // `words_` at its index times the width.
  std::vector<std::uint64_t> words_;
  std::vector<Evaluation> evaluations_;
  std::vector<std::uint64_t> seed_;
  // The matured assertion queue (LRM 16.14.6): how many attempts are due and
  // wait for a tick.
  std::size_t matured_ = 0;
  // The time step the clock last ticked in.
  std::optional<SimTime> ticked_at_;
  // What a tick runs over the attempts.
  OwnedCall step_;
  std::variant<Installed, Held> answered_;
  const AssertionControl* control_ = nullptr;
  std::size_t width_ = 0;
  // Every evaluation before it has been stepped this tick, which is what makes
  // the sweep a forward scan rather than a search.
  std::size_t cursor_ = 0;
  bool installed_ = false;
};

}  // namespace lyra::runtime
