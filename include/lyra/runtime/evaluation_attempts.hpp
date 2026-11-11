#pragma once

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <new>
#include <optional>
#include <span>
#include <variant>
#include <vector>

#include "lyra/base/time.hpp"
#include "lyra/runtime/closure.hpp"
#include "lyra/runtime/owned_call.hpp"
#include "lyra/support/property_answer.hpp"

namespace lyra::runtime {

class AssertionControl;
class RuntimeEffects;

// One evaluation attempt of a property (LRM 16.14.1, 16.17): the closure it
// was made from, and the evaluations it carries.
//
// The closure is generated code bound to the values the attempt was made
// with -- what a procedural assertion saved where it was queued (LRM
// 16.14.6.1), and nothing for every other placement -- so whatever reads them
// is a call of it: the step of a tick, the test of the disable condition, and
// the statement the answer selects. Two attempts of one assertion therefore
// read their Booleans apart where those read saved values, and nothing here
// knows which do.
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
class EvaluationAttempt {
 public:
  // The closure as the library holds it, handed the attempt it acts on as
  // the attempt it is, the type the closure's body declares that parameter at.
  using Closure =
      OwnedClosure<std::uint64_t(std::uint64_t, EvaluationAttempt*)>;

  // An attempt opened on `start`, the positions the whole property starts at,
  // keeping `allowed`, what its control allowed as it began (LRM 20.11).
  EvaluationAttempt(
      Closure closure, std::span<const std::uint64_t> start,
      std::uint64_t allowed);

  EvaluationAttempt(const EvaluationAttempt&) = delete;
  auto operator=(const EvaluationAttempt&) -> EvaluationAttempt& = delete;
  EvaluationAttempt(EvaluationAttempt&&) noexcept = default;
  auto operator=(EvaluationAttempt&&) noexcept -> EvaluationAttempt& = default;
  ~EvaluationAttempt() = default;

  // What the closure answers when called for `event`, with `shared`, the
  // Boolean values every attempt of the assertion shares at this tick, for the
  // call to read.
  auto Call(
      support::AttemptEvent event, std::span<const std::uint64_t> shared = {})
      -> std::uint64_t;

  // One word of the Boolean values every attempt shares at this tick, as the
  // call stepping the attempt was handed them.
  [[nodiscard]] auto SharedWord(std::uint64_t word) const -> std::uint64_t;

  // The tick of the leading clock has come: every evaluation becomes one this
  // tick has not yet stepped, and no implication has heard from a live
  // instance yet.
  void BeginTick();

  // Whether this tick has stepped every evaluation the attempt carries.
  [[nodiscard]] auto Stepped() const -> bool;

  // One word of the union of every position an unstepped evaluation is live
  // at, which is what bounds the Boolean expressions this tick has to read.
  [[nodiscard]] auto LiveWord(std::uint64_t word) const -> std::uint64_t;

  // The next evaluation this tick has not stepped, or -1 when none is left.
  // Seeding one during the sweep puts it here too, which is how a consequent
  // is read at the very tick its antecedent matched.
  [[nodiscard]] auto NextUnstepped() -> std::int64_t;

  [[nodiscard]] auto BitsAt(std::int64_t index, std::uint64_t word) const
      -> std::uint64_t;

  // Replaces one word of an evaluation's position set with its successor. Every
  // word of the old set is read before any is replaced, which the caller
  // arranges.
  void SetWord(std::int64_t index, std::uint64_t word, std::uint64_t bits);

  void Step(std::int64_t index);

  // Stages one word of the position set the next instance starts from. A seed
  // is a constant of the automaton, so it is written a word at a time by
  // whoever holds it rather than described here.
  void SeedWord(std::uint64_t word, std::uint64_t bits);

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

  // Folds every instance this tick settled into its implication, and answers
  // whether the attempt as a whole has settled: its answer reached either end
  // of the order, so no continuation of the trace can change it.
  auto Settle() -> bool;

  // Whether the attempt holds as its answer stands, which is what a finished
  // trace says of one still in flight (Annex F.5.3.1).
  [[nodiscard]] auto Holds() const -> bool;

  // Whether the statement the attempt's answer selects may run, as its control
  // allowed when it began and by whether a success was vacuous (LRM 20.11,
  // 16.14.8).
  [[nodiscard]] auto ActionRuns() const -> bool;

 private:
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

  // The evaluation whose antecedent matched or whose selection read its
  // condition, and which of its implications the instance answers for. The
  // evaluation answering for the whole property is the consequent of none.
  struct ConsequentOf {
    std::size_t parent;
    std::uint64_t implication;
  };

  struct Evaluation {
    std::optional<ConsequentOf> consequent_of;
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

  void OpenEvaluation(
      std::optional<ConsequentOf> consequent_of, std::uint64_t part);
  [[nodiscard]] auto IndexOf(std::int64_t index) const -> std::size_t;
  [[nodiscard]] auto EvaluationAt(std::int64_t index) -> Evaluation&;
  [[nodiscard]] auto EvaluationAt(std::int64_t index) const
      -> const Evaluation&;
  [[nodiscard]] auto WordIndex(std::int64_t index, std::uint64_t word) const
      -> std::size_t;

  Closure closure_;
  // What the call in progress was handed to read.
  std::span<const std::uint64_t> shared_;
  // One position set per evaluation, laid end to end: an evaluation's words are
  // the width times its index on.
  std::vector<std::uint64_t> words_;
  std::vector<Evaluation> evaluations_;
  std::vector<std::uint64_t> seed_;
  std::uint64_t allowed_;
  // Every evaluation before it has been stepped this tick, which is what makes
  // the sweep a forward scan rather than a search.
  std::size_t cursor_ = 0;
};

// Every attempt of one property still in flight, and those matured but not yet
// begun. A concurrent assertion begins an attempt wherever its enabling
// condition holds at a tick of its clock and each carries its own result, so
// what is held here is a list of attempts and never one merged state: two
// attempts that reach their answers at the same tick owe two actions. An
// expect statement begins one, and its process waits for that one's answer.
//
// An attempt matures where its closure is made here: where a procedural
// assertion's pending instance matures (LRM 16.14.6), at every tick of a static
// assertion's clock, which LRM 16.14.5 makes the same statement reached in an
// `always` procedure, and where an expect is reached, which begins its one
// attempt at the next tick (LRM 16.17). This storage alone knows whether the
// current time step has ticked, which decides whether the attempt begins now
// or waits for the next tick (LRM 16.14.6).
//
// What a tick runs over the attempts is generated code, given at
// installation: it reads the Booleans no saved value bears on once, for every
// attempt, and hands them to each attempt's closure. The storage runs it at
// the tick, and again where an attempt matures after the tick in the same
// step, which then steps only the attempts this tick has not.
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

  // Fixes the positions the whole property starts at, what a tick runs over
  // the attempts, and the control whose state decides whether an attempt
  // begins and which statement its outcome may run (LRM 20.11), which reaches
  // these attempts again to abort them.
  //
  // Registering here is what puts the answers of attempts the run ends in the
  // middle of before the design's `final` procedures, which read what those
  // statements wrote.
  void Install(
      RuntimeEffects& effects, std::span<const std::uint64_t> start,
      OwnedCall step, AssertionControl* control);

  // Fixes the same for the one attempt of an expect statement, whose process
  // reads the answer itself (LRM 16.17): nothing runs when it settles and
  // nothing is owed when the run ends, since a process still waiting then
  // never goes on. `control` decides which arm an answer may run; nothing
  // turns an expect off or aborts it (LRM 20.11).
  void Hold(
      std::span<const std::uint64_t> start, const AssertionControl* control,
      OwnedCall step);

  // Hands out storage for one attempt's closure, which the code making it
  // builds in place and commits.
  [[nodiscard]] auto Allocate(std::size_t size, std::size_t alignment) const
      -> void*;

  // The closure made in what `Allocate` handed out has matured.
  void Commit(void* made);

  // The attempt `closure` begins has matured: it begins at the tick of the
  // leading clock in this time step, or where that has not come, at the next
  // one (LRM 16.14.6, 16.17).
  void Mature(EvaluationAttempt::Closure closure);

  // A tick of the leading clock: every matured attempt begins where checking is
  // on now (LRM 20.11), and what a tick runs is run.
  void Tick();

  // Steps every attempt this tick has not, handing each the Booleans every
  // attempt shares (`shared`), then settles each one that answered. One whose
  // disable condition holds is discarded: a disabled attempt is neither a
  // success nor a failure and runs no statement (LRM 16.12, 16.14.1).
  void Run(std::span<const std::uint64_t> shared);

  // What the disable condition reads changed between ticks: every attempt
  // whose condition holds is aborted, settling nothing and running no
  // statement (LRM 16.12). One matured is no attempt yet and is kept.
  void TestDisable();

  // Aborts every attempt in flight, a Kill (LRM 20.11). One matured is no
  // attempt yet and is kept; it begins only where checking is on when its tick
  // comes.
  void Discard();

  // One word of the union of every position any attempt's unstepped
  // evaluations are live at, which bounds the Booleans every attempt shares.
  [[nodiscard]] auto LiveWord(std::uint64_t word) const -> std::uint64_t;

  // What the held attempt has come to, as a `support::AttemptAnswer`.
  [[nodiscard]] auto Answer() const -> std::uint64_t;

  // Whether the arm the held attempt's answer selects may run.
  [[nodiscard]] auto ActionRuns() const -> bool;

  // The run is over, so no tick will settle what is left. An attempt's last
  // answer is what the finished trace says (Annex F.5.3.1): it succeeded where
  // that answer holds, and failed otherwise.
  void SettleAtEndOfRun();

 private:
  // Where the storage stands: before the design activates it, nothing is known
  // of how wide a position set is or whose control decides. Once activated,
  // what an answered attempt does depends on the placement. An installed
  // assertion starts the statement its answer selects in the Reactive region
  // of now (LRM 16.14.1), and its control reaches these attempts again to
  // abort them; a held attempt keeps its answer for the process waiting on
  // it, and its control only decides which arm may run.
  struct Uninstalled {};
  struct Installed {
    RuntimeEffects* effects;
    const AssertionControl* control;
  };
  struct Held {
    support::AttemptAnswer answer;
    bool action_runs;
    const AssertionControl* control;
  };

  void RequireInstalled() const;
  // The control of an activated storage.
  [[nodiscard]] auto Control() const -> const AssertionControl&;
  void Prepare(std::span<const std::uint64_t> start, OwnedCall step);
  void Open(EvaluationAttempt::Closure closure);
  void Conclude(EvaluationAttempt attempt);
  void ConcludeDisabled();

  std::vector<EvaluationAttempt> attempts_;
  // The matured assertion queue (LRM 16.14.6): the closures that wait for a
  // tick.
  std::vector<EvaluationAttempt::Closure> matured_;
  std::vector<std::uint64_t> start_;
  // The time step the clock last ticked in.
  std::optional<SimTime> ticked_at_;
  // What a tick runs over the attempts.
  OwnedCall step_;
  std::variant<Uninstalled, Installed, Held> state_;
};

}  // namespace lyra::runtime

// The allocation functions a placement new-expression making an attempt's
// closure in the attempts it begins finds by its argument (LRM 16.14.6).
[[nodiscard]] auto operator new(
    std::size_t size, lyra::runtime::EvaluationAttempts* attempts) -> void*;
[[nodiscard]] auto operator new(
    std::size_t size, std::align_val_t alignment,
    lyra::runtime::EvaluationAttempts* attempts) -> void*;
