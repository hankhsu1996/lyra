#pragma once

#include <cstddef>
#include <cstdint>
#include <functional>
#include <vector>

namespace lyra::runtime {

class RuntimeEffects;

// Every evaluation attempt of one concurrent assertion that is still in flight
// (LRM 16.14.1). An attempt begins at every tick of the assertion's clock and
// carries its own result, so what is held here is a list of attempts and never
// one merged state: two attempts that reach their answers at the same tick owe
// two actions.
//
// Inside an attempt is a list of evaluations, and those are never pooled
// either. A sequence's operators are existential, so two paths through one that
// reach the same position have the same future and their positions may be
// unioned; an implication quantifies universally over the ticks its antecedent
// matched, and a conjunction is not preserved by union -- pooled with a
// sibling's, a dead evaluation's positions are indistinguishable from a live
// one's.
//
// A flat list, rather than a tree with an operator at each node, on one
// condition: an attempt's answer is the conjunction of its evaluations', which
// holds while implication is the only operator combining them. A property
// connective that disjoins or negates (LRM Table 16-3) is answered by no flat
// list, and is refused where it is read.
//
// An evaluation is a set of positions in the automaton the lowering built, held
// here as the words a generated transition reads it in. Nothing here knows what
// a position means: the width arrives at installation and every bit pattern
// crosses opaque.
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

  // Fixes how wide a position set is, what a pending attempt is owed when the
  // run ends, and the statements an outcome selects. `pending_holds` is what
  // the statement demanded of a result the trace never settled: an obligation
  // is met by "holds" and runs its pass statements, a coverage goal needed a
  // match and gets nothing (LRM 16.12.2, Annex F.5.3.2).
  //
  // Registering here is what puts the pending answer before the design's
  // `final` procedures, which read what those statements wrote.
  void Install(
      RuntimeEffects& effects, std::uint64_t words, bool pending_holds,
      std::function<void()> pass_action, std::function<void()> fail_action);

  // Stages one word of the position set the next `BeginTick` or `Seed` starts
  // an evaluation from. A seed is a constant of the automaton, so it is written
  // a word at a time by whoever holds it rather than described here.
  void SeedWord(std::uint64_t word, std::uint64_t bits);

  // A tick of the clock: this tick's own attempt opens on the staged seed, and
  // every evaluation still live becomes one this tick has not yet stepped.
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
  // Seeding one during the sweep puts it here too, which is how an overlapped
  // implication's consequent is read at the very tick its antecedent matched.
  [[nodiscard]] auto NextUnstepped() -> std::int64_t;

  [[nodiscard]] auto BitsAt(std::int64_t index, std::uint64_t word) const
      -> std::uint64_t;

  // Replaces one word of an evaluation's position set with its successor. Every
  // word of the old set is read before any is replaced, which is the caller's
  // to arrange.
  void SetWord(std::int64_t index, std::uint64_t word, std::uint64_t bits);

  // Records what the tick left this evaluation in and marks it stepped.
  // `outcome` is a `support::EvaluationOutcome`.
  void Step(std::int64_t index, std::uint64_t outcome);

  // Adds an evaluation on the staged seed to the attempt the evaluation at
  // `index` belongs to. `this_tick` is the overlapped form, whose consequent
  // starts at the end point of the match that triggered it; otherwise it starts
  // at the tick after (LRM 16.12.7).
  void Seed(std::int64_t index, bool this_tick);

  // Settles every attempt the sweep answered and submits its statements to the
  // Reactive region (LRM 16.14.1): an attempt is false as soon as any
  // evaluation of it failed, and true once every evaluation has left, which is
  // one match for each thing it owed and nothing owed by an antecedent that
  // never matched.
  void Settle(RuntimeEffects& effects);

  // The run is over, so no tick will settle what is left. Each attempt gets the
  // answer its statement demanded of a pending result.
  void SettleAtEndOfRun();

 private:
  struct Evaluation {
    std::size_t attempt = 0;
    bool stepped = false;
    bool live = true;
  };

  struct Attempt {
    bool failed = false;
  };

  void RequireInstalled() const;
  void OpenEvaluation(std::size_t attempt, bool stepped);
  [[nodiscard]] auto EvaluationAt(std::int64_t index) -> Evaluation&;
  [[nodiscard]] auto WordSlot(std::int64_t index, std::uint64_t word) const
      -> std::size_t;

  // One position set per evaluation, laid end to end: an evaluation's words are
  // `words_` at its index times the width.
  std::vector<std::uint64_t> words_;
  std::vector<Evaluation> evaluations_;
  std::vector<Attempt> attempts_;
  std::vector<std::uint64_t> seed_;
  std::function<void()> pass_action_;
  std::function<void()> fail_action_;
  std::size_t width_ = 0;
  // Every evaluation before it has been stepped this tick, which is what makes
  // the sweep a forward scan rather than a search.
  std::size_t cursor_ = 0;
  bool pending_holds_ = true;
  bool installed_ = false;
};

}  // namespace lyra::runtime
