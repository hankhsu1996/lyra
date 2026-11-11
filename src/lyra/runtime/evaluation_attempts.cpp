#include "lyra/runtime/evaluation_attempts.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <new>
#include <span>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/runtime/assertion_control.hpp"
#include "lyra/runtime/closure.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/support/assertion_control.hpp"
#include "lyra/support/property_answer.hpp"
#include "lyra/support/region.hpp"

namespace lyra::runtime {

namespace {

// Whether no continuation of the trace can move the answer: the two ends of
// the order are where every way of combining answers keeps them.
auto IsSettled(support::PropertyAnswer answer) -> bool {
  switch (answer) {
    case support::PropertyAnswer::kFails:
    case support::PropertyAnswer::kHoldsStrongly:
      return true;
    case support::PropertyAnswer::kPending:
    case support::PropertyAnswer::kHolds:
      return false;
  }
  throw InternalError("EvaluationAttempts: unknown property answer");
}

auto AnswerOf(std::uint64_t answer) -> support::PropertyAnswer {
  if (answer >
      static_cast<std::uint64_t>(support::PropertyAnswer::kHoldsStrongly)) {
    throw InternalError(
        "EvaluationAttempts: a property answer outside the four it has");
  }
  return static_cast<support::PropertyAnswer>(answer);
}

// The statement an attempt's answer selects, as the event its closure
// starts it at.
auto SelectedBy(bool holds) -> support::AttemptEvent {
  return holds ? support::AttemptEvent::kSucceeded
               : support::AttemptEvent::kFailed;
}

}  // namespace

EvaluationAttempt::EvaluationAttempt(
    Closure closure, std::span<const std::uint64_t> start,
    std::uint64_t allowed)
    : closure_(std::move(closure)),
      seed_(start.begin(), start.end()),
      allowed_(allowed) {
  OpenEvaluation(std::nullopt, 0);
}

auto EvaluationAttempt::Call(
    support::AttemptEvent event, std::span<const std::uint64_t> shared)
    -> std::uint64_t {
  shared_ = shared;
  const std::uint64_t answer =
      closure_(static_cast<std::uint64_t>(event), this);
  shared_ = {};
  return answer;
}

auto EvaluationAttempt::SharedWord(std::uint64_t word) const -> std::uint64_t {
  if (word >= shared_.size()) {
    throw InternalError(
        "EvaluationAttempt::SharedWord: a word past what the call stepping "
        "the attempt was handed");
  }
  return shared_[static_cast<std::size_t>(word)];
}

auto EvaluationAttempt::AnswersOf(
    Evaluation& evaluation, std::uint64_t implication) -> ImplicationAnswers& {
  for (ImplicationAnswers& answers : evaluation.implications) {
    if (answers.implication == implication) {
      return answers;
    }
  }
  return evaluation.implications.emplace_back(
      ImplicationAnswers{.implication = implication});
}

void EvaluationAttempt::BeginTick() {
  for (Evaluation& evaluation : evaluations_) {
    evaluation.stepped = false;
    for (ImplicationAnswers& answers : evaluation.implications) {
      answers.this_tick = Outcome::OfNoInstance();
    }
  }
  cursor_ = 0;
}

auto EvaluationAttempt::Stepped() const -> bool {
  return std::ranges::all_of(evaluations_, [](const Evaluation& evaluation) {
    return evaluation.stepped;
  });
}

auto EvaluationAttempt::LiveWord(std::uint64_t word) const -> std::uint64_t {
  const std::size_t width = seed_.size();
  std::uint64_t live = 0;
  for (std::size_t index = 0; index < evaluations_.size(); ++index) {
    if (!evaluations_[index].stepped) {
      live |= words_[(index * width) + static_cast<std::size_t>(word)];
    }
  }
  return live;
}

auto EvaluationAttempt::NextUnstepped() -> std::int64_t {
  while (cursor_ < evaluations_.size() && evaluations_[cursor_].stepped) {
    ++cursor_;
  }
  if (cursor_ == evaluations_.size()) {
    return -1;
  }
  return static_cast<std::int64_t>(cursor_);
}

auto EvaluationAttempt::BitsAt(std::int64_t index, std::uint64_t word) const
    -> std::uint64_t {
  return words_[WordIndex(index, word)];
}

void EvaluationAttempt::SetWord(
    std::int64_t index, std::uint64_t word, std::uint64_t bits) {
  words_[WordIndex(index, word)] = bits;
}

void EvaluationAttempt::Step(std::int64_t index) {
  EvaluationAt(index).stepped = true;
}

void EvaluationAttempt::SeedWord(std::uint64_t word, std::uint64_t bits) {
  if (word >= seed_.size()) {
    throw InternalError(
        "EvaluationAttempt::SeedWord: a word past the width this assertion "
        "was installed with");
  }
  seed_[static_cast<std::size_t>(word)] = bits;
}

void EvaluationAttempt::Seed(
    std::int64_t index, std::uint64_t implication, std::uint64_t part) {
  OpenEvaluation(
      ConsequentOf{.parent = IndexOf(index), .implication = implication}, part);
}

auto EvaluationAttempt::Count() const -> std::int64_t {
  return static_cast<std::int64_t>(evaluations_.size());
}

auto EvaluationAttempt::PartOf(std::int64_t index) const -> std::uint64_t {
  return EvaluationAt(index).part;
}

auto EvaluationAttempt::ImplicationOutcome(
    std::int64_t index, std::uint64_t implication) const -> Outcome {
  for (const ImplicationAnswers& answers : EvaluationAt(index).implications) {
    if (answers.implication == implication) {
      return answers.settled.Conjoined(answers.this_tick);
    }
  }
  return Outcome::OfNoInstance();
}

auto EvaluationAttempt::ImplicationAnswer(
    std::int64_t index, std::uint64_t implication) const -> std::uint64_t {
  return static_cast<std::uint64_t>(
      ImplicationOutcome(index, implication).answer);
}

auto EvaluationAttempt::ImplicationNonvacuous(
    std::int64_t index, std::uint64_t implication) const -> std::uint64_t {
  return ImplicationOutcome(index, implication).nonvacuous ? 1 : 0;
}

void EvaluationAttempt::Resolve(
    std::int64_t index, std::uint64_t answer, std::uint64_t nonvacuous) {
  Evaluation& evaluation = EvaluationAt(index);
  evaluation.outcome = {
      .answer = AnswerOf(answer), .nonvacuous = nonvacuous != 0};
  if (!evaluation.consequent_of.has_value()) {
    return;
  }
  ImplicationAnswers& answers = AnswersOf(
      evaluations_[evaluation.consequent_of->parent],
      evaluation.consequent_of->implication);
  answers.this_tick = answers.this_tick.Conjoined(evaluation.outcome);
}

auto EvaluationAttempt::Settle() -> bool {
  if (IsSettled(evaluations_.front().outcome.answer)) {
    return true;
  }
  // Parents come before their instances, so one forward pass knows of each
  // evaluation whether something above it has already left.
  const std::size_t width = seed_.size();
  constexpr auto kLeaves = static_cast<std::size_t>(-1);
  std::vector<std::size_t> renumbered(evaluations_.size(), kLeaves);
  std::vector<Evaluation> kept;
  std::vector<std::uint64_t> kept_words;
  for (std::size_t index = 0; index < evaluations_.size(); ++index) {
    Evaluation& evaluation = evaluations_[index];
    const std::optional<ConsequentOf> consequent_of = evaluation.consequent_of;
    if (consequent_of.has_value() &&
        renumbered[consequent_of->parent] == kLeaves) {
      continue;
    }
    if (consequent_of.has_value() && IsSettled(evaluation.outcome.answer)) {
      ImplicationAnswers& answers = AnswersOf(
          kept[renumbered[consequent_of->parent]], consequent_of->implication);
      answers.settled = answers.settled.Conjoined(evaluation.outcome);
      continue;
    }
    renumbered[index] = kept.size();
    Evaluation moved = std::move(evaluation);
    if (moved.consequent_of.has_value()) {
      moved.consequent_of->parent = renumbered[moved.consequent_of->parent];
    }
    kept.push_back(std::move(moved));
    const auto first = static_cast<std::ptrdiff_t>(index * width);
    kept_words.insert(
        kept_words.end(), words_.begin() + first,
        words_.begin() + first + static_cast<std::ptrdiff_t>(width));
  }
  evaluations_ = std::move(kept);
  words_ = std::move(kept_words);
  cursor_ = 0;
  return false;
}

auto EvaluationAttempt::Holds() const -> bool {
  return evaluations_.front().outcome.answer >= support::PropertyAnswer::kHolds;
}

auto EvaluationAttempt::ActionRuns() const -> bool {
  // A success runs the pass statement where passing that way was allowed,
  // vacuously or not (LRM 16.14.8), and a failure the fail statement where
  // failing was.
  if (!Holds()) {
    return (allowed_ & support::kAssertionFails) != 0;
  }
  return (allowed_ & (evaluations_.front().outcome.nonvacuous
                          ? support::kAssertionPassesNonvacuously
                          : support::kAssertionPassesVacuously)) != 0;
}

void EvaluationAttempt::OpenEvaluation(
    std::optional<ConsequentOf> consequent_of, std::uint64_t part) {
  evaluations_.push_back(
      Evaluation{
          .consequent_of = consequent_of,
          .part = part,
          .stepped = false,
          .outcome =
              {.answer = support::PropertyAnswer::kPending,
               .nonvacuous = false},
          .implications = {}});
  words_.insert(words_.end(), seed_.begin(), seed_.end());
}

auto EvaluationAttempt::IndexOf(std::int64_t index) const -> std::size_t {
  if (index < 0 || std::cmp_greater_equal(index, evaluations_.size())) {
    throw InternalError(
        "EvaluationAttempt: an evaluation index no sweep of this tick "
        "produced");
  }
  return static_cast<std::size_t>(index);
}

auto EvaluationAttempt::EvaluationAt(std::int64_t index) -> Evaluation& {
  return evaluations_[IndexOf(index)];
}

auto EvaluationAttempt::EvaluationAt(std::int64_t index) const
    -> const Evaluation& {
  return evaluations_[IndexOf(index)];
}

auto EvaluationAttempt::WordIndex(std::int64_t index, std::uint64_t word) const
    -> std::size_t {
  const std::size_t evaluation = IndexOf(index);
  if (word >= seed_.size()) {
    throw InternalError(
        "EvaluationAttempt: a position word outside the set this assertion "
        "was installed with");
  }
  return (evaluation * seed_.size()) + static_cast<std::size_t>(word);
}

void EvaluationAttempts::Install(
    RuntimeEffects& effects, std::span<const std::uint64_t> start,
    OwnedCall step, AssertionControl* control) {
  Prepare(start, std::move(step));
  state_ = Installed{.effects = &effects, .control = control};
  control->Watch(*this);
  effects.RegisterConcurrentAssertion(*this);
}

void EvaluationAttempts::Hold(
    std::span<const std::uint64_t> start, const AssertionControl* control,
    OwnedCall step) {
  Prepare(start, std::move(step));
  state_ = Held{
      .answer = support::AttemptAnswer::kPending,
      .action_runs = false,
      .control = control};
}

void EvaluationAttempts::Prepare(
    std::span<const std::uint64_t> start, OwnedCall step) {
  if (start.empty()) {
    throw InternalError(
        "EvaluationAttempts::Prepare: a property with no position to be live "
        "at "
        "matches nothing the grammar admits (LRM 16.7)");
  }
  start_.assign(start.begin(), start.end());
  step_ = std::move(step);
}

auto EvaluationAttempts::Allocate(std::size_t size, std::size_t alignment) const
    -> void* {
  RequireInstalled();
  // A closure's storage is given back by whatever ends its box, which
  // frees it as storage of the default alignment.
  if (alignment > __STDCPP_DEFAULT_NEW_ALIGNMENT__) {
    throw InternalError(
        "EvaluationAttempts::Allocate: an attempt's closure asks more "
        "alignment than the storage it is given back as has");
  }
  return ::operator new(size);
}

void EvaluationAttempts::Commit(void* made) {
  Mature(EvaluationAttempt::Closure(static_cast<ClosureValue*>(made)));
}

void EvaluationAttempts::Mature(EvaluationAttempt::Closure closure) {
  RequireInstalled();
  if (ticked_at_ != current_runtime().Now()) {
    matured_.push_back(std::move(closure));
    return;
  }
  // This time step's tick has been run, so the attempt begins at it and what
  // the tick runs is run again, stepping only the attempt just opened.
  Open(std::move(closure));
  step_();
}

void EvaluationAttempts::Open(EvaluationAttempt::Closure closure) {
  // LRM 20.11: with checking off no attempt begins; the attempt keeps what its
  // control allowed as it began.
  const std::uint64_t allowed = Control().Allowed();
  if ((allowed & support::kAssertionChecks) == 0) {
    return;
  }
  attempts_.emplace_back(std::move(closure), start_, allowed);
}

void EvaluationAttempts::Tick() {
  RequireInstalled();
  ticked_at_ = current_runtime().Now();
  for (EvaluationAttempt& attempt : attempts_) {
    attempt.BeginTick();
  }
  std::vector<EvaluationAttempt::Closure> matured = std::move(matured_);
  matured_.clear();
  for (EvaluationAttempt::Closure& closure : matured) {
    Open(std::move(closure));
  }
  step_();
}

void EvaluationAttempts::Run(std::span<const std::uint64_t> shared) {
  RequireInstalled();
  // The attempts still in flight are moved down over those that left, so a
  // tick allocates nothing.
  std::size_t kept = 0;
  for (EvaluationAttempt& attempt : attempts_) {
    if (!attempt.Stepped()) {
      if (attempt.Call(support::AttemptEvent::kTick, shared) != 0) {
        ConcludeDisabled();
        continue;
      }
      if (attempt.Settle()) {
        Conclude(std::move(attempt));
        continue;
      }
    }
    if (&attempts_[kept] != &attempt) attempts_[kept] = std::move(attempt);
    ++kept;
  }
  attempts_.erase(
      attempts_.begin() + static_cast<std::ptrdiff_t>(kept), attempts_.end());
}

void EvaluationAttempts::Conclude(EvaluationAttempt attempt) {
  const bool holds = attempt.Holds();
  const bool runs = attempt.ActionRuns();
  std::visit(
      Overloaded{
          [&](const Installed& installed) {
            if (!runs) return;
            const support::AttemptEvent event = SelectedBy(holds);
            // LRM 16.14.1: the statement runs in the Reactive region, as an
            // execution of no lineage drawing from what the tick deciding it
            // hands over.
            installed.effects->Submit(
                installed.effects->Now(), support::Region::kReactive,
                [answered = std::move(attempt), event,
                 taken = TakeNoThreadContext(*installed.effects)]() mutable {
                  taken.Enter();
                  answered.Call(event);
                });
          },
          [&](Held& held) {
            held.answer = holds ? support::AttemptAnswer::kHolds
                                : support::AttemptAnswer::kFails;
            held.action_runs = runs;
          },
          [](const Uninstalled&) {
            throw InternalError(
                "EvaluationAttempts::Conclude: an attempt settled before the "
                "design activated its storage");
          }},
      state_);
}

void EvaluationAttempts::TestDisable() {
  RequireInstalled();
  std::erase_if(attempts_, [this](EvaluationAttempt& attempt) {
    if (attempt.Call(support::AttemptEvent::kDisableTest) == 0) {
      return false;
    }
    ConcludeDisabled();
    return true;
  });
}

void EvaluationAttempts::ConcludeDisabled() {
  // A held attempt that is disabled answers nothing, so its process is never
  // woken (LRM 16.12, 16.17).
  if (auto* held = std::get_if<Held>(&state_)) {
    held->answer = support::AttemptAnswer::kDisabled;
  }
}

void EvaluationAttempts::Discard() {
  attempts_.clear();
}

auto EvaluationAttempts::LiveWord(std::uint64_t word) const -> std::uint64_t {
  RequireInstalled();
  std::uint64_t live = 0;
  for (const EvaluationAttempt& attempt : attempts_) {
    live |= attempt.LiveWord(word);
  }
  return live;
}

auto EvaluationAttempts::Answer() const -> std::uint64_t {
  RequireInstalled();
  const auto* held = std::get_if<Held>(&state_);
  if (held == nullptr) {
    throw InternalError(
        "EvaluationAttempts::Answer: an installed assertion starts the "
        "statements its answers select, and has no answer to read");
  }
  return static_cast<std::uint64_t>(held->answer);
}

auto EvaluationAttempts::ActionRuns() const -> bool {
  RequireInstalled();
  const auto* held = std::get_if<Held>(&state_);
  if (held == nullptr) {
    throw InternalError(
        "EvaluationAttempts::ActionRuns: an installed assertion starts the "
        "statements its answers select, and has no answer to read");
  }
  return held->action_runs;
}

void EvaluationAttempts::SettleAtEndOfRun() {
  RequireInstalled();
  if (!std::holds_alternative<Installed>(state_)) {
    throw InternalError(
        "EvaluationAttempts::SettleAtEndOfRun: a held attempt is never "
        "registered for the end of the run");
  }
  std::vector<EvaluationAttempt> attempts = std::move(attempts_);
  attempts_.clear();
  matured_.clear();
  for (EvaluationAttempt& attempt : attempts) {
    if (attempt.ActionRuns()) {
      attempt.Call(SelectedBy(attempt.Holds()));
    }
  }
}

void EvaluationAttempts::RequireInstalled() const {
  if (std::holds_alternative<Uninstalled>(state_)) {
    throw InternalError(
        "EvaluationAttempts: an assertion was reached before the design "
        "activated it, so nothing here knows how wide a position set is");
  }
}

auto EvaluationAttempts::Control() const -> const AssertionControl& {
  return *std::visit(
      Overloaded{
          [](const Installed& installed) { return installed.control; },
          [](const Held& held) { return held.control; },
          [](const Uninstalled&) -> const AssertionControl* {
            throw InternalError(
                "EvaluationAttempts: an assertion was reached before the "
                "design activated it, so no control decides what it may do");
          }},
      state_);
}

}  // namespace lyra::runtime

auto operator new(std::size_t size, lyra::runtime::EvaluationAttempts* attempts)
    -> void* {
  return attempts->Allocate(size, __STDCPP_DEFAULT_NEW_ALIGNMENT__);
}

auto operator new(
    std::size_t size, std::align_val_t alignment,
    lyra::runtime::EvaluationAttempts* attempts) -> void* {
  return attempts->Allocate(size, static_cast<std::size_t>(alignment));
}
