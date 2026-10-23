#pragma once

#include <cstddef>
#include <cstdint>
#include <optional>
#include <variant>
#include <vector>

#include "lyra/runtime/owned_call.hpp"
#include "lyra/support/property_answer.hpp"

namespace lyra::runtime {

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

  // Fixes how wide a position set is and the statements an outcome selects.
  //
  // Registering here is what puts the answers of attempts the run ends in the
  // middle of before the design's `final` procedures, which read what those
  // statements wrote.
  void Install(
      RuntimeEffects& effects, std::uint64_t words, OwnedCall pass_action,
      OwnedCall fail_action);

  // Fixes how wide a position set is for the one attempt of an expect
  // statement, whose process reads the answer itself (LRM 16.17): nothing runs
  // when it settles and nothing is owed when the run ends, since a process
  // still waiting then never goes on.
  void Hold(std::uint64_t words);

  // Stages one word of the position set the next `OpenAttempt` or `Seed`
  // starts an evaluation from. A seed is a constant of the automaton, so it is
  // written a word at a time by whoever holds it rather than described here.
  void SeedWord(std::uint64_t word, std::uint64_t bits);

  // Opens an attempt on the staged seed, answering for the whole property; the
  // next sweep steps it.
  void OpenAttempt();

  // A tick of the clock: every evaluation becomes one this tick has not yet
  // stepped, and no implication has heard from a live instance yet.
  void BeginTick();

  // A tick at which the disable condition held. Every attempt is discarded and
  // none begins -- a disabled attempt is neither a success nor a failure and
  // runs no statement, and one that would start here starts disabled (LRM
  // 16.12, 16.14.1).
  void DisableTick();

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

  // Records what the evaluation at `index` answered this tick, a
  // `support::PropertyAnswer`, and counts it in the implication it is a
  // consequent of.
  void Resolve(std::int64_t index, std::uint64_t answer);

  // Settles every attempt this tick answered (LRM 16.14.1): an instance whose
  // answer reached either end is folded into its implication and leaves with
  // the instances under it; an attempt whose answer reached either end has
  // failed or succeeded, and leaves too. An installed assertion submits the
  // statements the answer selects to the Reactive region; a held attempt keeps
  // its answer for its process.
  void Settle(RuntimeEffects& effects);

  // What a held attempt has come to, as a `support::AttemptAnswer`.
  [[nodiscard]] auto Answer() const -> std::uint64_t;

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
  };

  // What one implication's instances have answered: the least any instance
  // that has settled and left reached, which outlives it, and the least the
  // live ones answered this tick. One no instance has answered holds strongly.
  struct ImplicationAnswers {
    std::uint64_t implication = 0;
    support::PropertyAnswer settled = support::PropertyAnswer::kHoldsStrongly;
    support::PropertyAnswer this_tick = support::PropertyAnswer::kHoldsStrongly;
  };

  // The evaluation whose antecedent matched or whose selection read its
  // condition, and which of its implications the instance answers for.
  struct ConsequentOf {
    std::size_t parent;
    std::uint64_t implication;
  };

  struct Evaluation {
    // Absent for the evaluation an attempt opened.
    std::optional<ConsequentOf> consequent_of;
    std::uint64_t part = 0;
    bool stepped = false;
    support::PropertyAnswer answer = support::PropertyAnswer::kPending;
    std::vector<ImplicationAnswers> implications;
  };

  // The record of `implication` in `evaluation`, made on first use.
  static auto AnswersOf(Evaluation& evaluation, std::uint64_t implication)
      -> ImplicationAnswers&;

  void RequireInstalled() const;
  void Size(std::uint64_t words);
  void OpenEvaluation(
      std::optional<ConsequentOf> consequent_of, std::uint64_t part);
  [[nodiscard]] auto IndexOf(std::int64_t index) const -> std::size_t;
  [[nodiscard]] auto EvaluationAt(std::int64_t index) -> Evaluation&;
  [[nodiscard]] auto EvaluationAt(std::int64_t index) const
      -> const Evaluation&;
  [[nodiscard]] auto WordIndex(std::int64_t index, std::uint64_t word) const
      -> std::size_t;
  void Report(support::PropertyAnswer answer, RuntimeEffects& effects);

  // One position set per evaluation, laid end to end: an evaluation's words are
  // `words_` at its index times the width.
  std::vector<std::uint64_t> words_;
  std::vector<Evaluation> evaluations_;
  std::vector<std::uint64_t> seed_;
  std::variant<Installed, Held> answered_;
  std::size_t width_ = 0;
  // Every evaluation before it has been stepped this tick, which is what makes
  // the sweep a forward scan rather than a search.
  std::size_t cursor_ = 0;
  bool installed_ = false;
};

}  // namespace lyra::runtime
