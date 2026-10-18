#include "lyra/runtime/evaluation_attempts.hpp"

#include <cstddef>
#include <cstdint>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/runtime/region.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/support/evaluation_outcome.hpp"

namespace lyra::runtime {

void EvaluationAttempts::Install(
    RuntimeEffects& effects, std::uint64_t words, bool pending_holds,
    OwnedCall pass_action, OwnedCall fail_action) {
  Size(words);
  answered_ = Installed{
      .pass_action = std::move(pass_action),
      .fail_action = std::move(fail_action),
      .pending_holds = pending_holds};
  effects.RegisterConcurrentAssertion(*this);
}

void EvaluationAttempts::Hold(std::uint64_t words) {
  Size(words);
  answered_ = Held{.answer = support::AttemptAnswer::kPending};
}

void EvaluationAttempts::Size(std::uint64_t words) {
  if (words == 0) {
    throw InternalError(
        "EvaluationAttempts: a property with no position to be live at "
        "matches nothing the grammar admits (LRM 16.7)");
  }
  width_ = static_cast<std::size_t>(words);
  seed_.assign(width_, 0);
  installed_ = true;
}

void EvaluationAttempts::SeedWord(std::uint64_t word, std::uint64_t bits) {
  RequireInstalled();
  if (word >= width_) {
    throw InternalError(
        "EvaluationAttempts::SeedWord: a word past the width this assertion "
        "was installed with");
  }
  seed_[static_cast<std::size_t>(word)] = bits;
}

void EvaluationAttempts::OpenAttempt() {
  RequireInstalled();
  attempts_.push_back(Attempt{});
  OpenEvaluation(attempts_.size() - 1, false);
}

void EvaluationAttempts::BeginTick() {
  RequireInstalled();
  for (Evaluation& evaluation : evaluations_) {
    evaluation.stepped = false;
  }
  cursor_ = 0;
}

void EvaluationAttempts::DisableTick() {
  RequireInstalled();
  words_.clear();
  evaluations_.clear();
  attempts_.clear();
  cursor_ = 0;
}

auto EvaluationAttempts::LiveWord(std::uint64_t word) const -> std::uint64_t {
  RequireInstalled();
  std::uint64_t live = 0;
  for (std::size_t index = 0; index < evaluations_.size(); ++index) {
    if (!evaluations_[index].stepped) {
      live |= words_[(index * width_) + static_cast<std::size_t>(word)];
    }
  }
  return live;
}

auto EvaluationAttempts::NextUnstepped() -> std::int64_t {
  RequireInstalled();
  while (cursor_ < evaluations_.size() && evaluations_[cursor_].stepped) {
    ++cursor_;
  }
  if (cursor_ == evaluations_.size()) {
    return -1;
  }
  return static_cast<std::int64_t>(cursor_);
}

auto EvaluationAttempts::BitsAt(std::int64_t index, std::uint64_t word) const
    -> std::uint64_t {
  return words_[WordSlot(index, word)];
}

void EvaluationAttempts::SetWord(
    std::int64_t index, std::uint64_t word, std::uint64_t bits) {
  words_[WordSlot(index, word)] = bits;
}

void EvaluationAttempts::Step(std::int64_t index, std::uint64_t outcome) {
  Evaluation& evaluation = EvaluationAt(index);
  evaluation.stepped = true;
  switch (static_cast<support::EvaluationOutcome>(outcome)) {
    case support::EvaluationOutcome::kLive:
      return;
    case support::EvaluationOutcome::kSatisfied:
    case support::EvaluationOutcome::kVacuous:
      evaluation.live = false;
      return;
    case support::EvaluationOutcome::kFailed:
      evaluation.live = false;
      attempts_[evaluation.attempt].failed = true;
      return;
  }
  throw InternalError("EvaluationAttempts::Step: unknown evaluation outcome");
}

void EvaluationAttempts::Seed(std::int64_t index, bool this_tick) {
  const std::size_t attempt = EvaluationAt(index).attempt;
  OpenEvaluation(attempt, !this_tick);
}

void EvaluationAttempts::Settle(RuntimeEffects& effects) {
  RequireInstalled();

  std::vector<std::size_t> live_evaluations(attempts_.size(), 0);
  for (const Evaluation& evaluation : evaluations_) {
    if (evaluation.live) {
      ++live_evaluations[evaluation.attempt];
    }
  }

  // An attempt that neither failed nor ran out of evaluations is still pending,
  // and keeps its place; the others are answered here and leave.
  const auto answer = [&](support::AttemptAnswer answered,
                          OwnedCall Installed::* selects) {
    std::visit(
        Overloaded{
            [&](Installed& installed) {
              effects.Submit(
                  effects.Now(), Region::kReactive,
                  [action = &(installed.*selects)] { (*action)(); });
            },
            [&](Held& held) { held.answer = answered; }},
        answered_);
  };

  constexpr auto kAnswered = static_cast<std::size_t>(-1);
  std::vector<std::size_t> renumbered(attempts_.size(), kAnswered);
  std::vector<Attempt> kept_attempts;
  for (std::size_t attempt = 0; attempt < attempts_.size(); ++attempt) {
    if (attempts_[attempt].failed) {
      answer(support::AttemptAnswer::kFails, &Installed::fail_action);
      continue;
    }
    if (live_evaluations[attempt] == 0) {
      answer(support::AttemptAnswer::kHolds, &Installed::pass_action);
      continue;
    }
    renumbered[attempt] = kept_attempts.size();
    kept_attempts.push_back(attempts_[attempt]);
  }

  std::vector<Evaluation> kept_evaluations;
  std::vector<std::uint64_t> kept_words;
  for (std::size_t index = 0; index < evaluations_.size(); ++index) {
    const Evaluation& evaluation = evaluations_[index];
    if (!evaluation.live || renumbered[evaluation.attempt] == kAnswered) {
      continue;
    }
    Evaluation moved = evaluation;
    moved.attempt = renumbered[evaluation.attempt];
    kept_evaluations.push_back(moved);
    const auto first = static_cast<std::ptrdiff_t>(index * width_);
    kept_words.insert(
        kept_words.end(), words_.begin() + first,
        words_.begin() + first + static_cast<std::ptrdiff_t>(width_));
  }

  attempts_ = std::move(kept_attempts);
  evaluations_ = std::move(kept_evaluations);
  words_ = std::move(kept_words);
  cursor_ = 0;
}

auto EvaluationAttempts::Answer() const -> std::uint64_t {
  RequireInstalled();
  const auto* held = std::get_if<Held>(&answered_);
  if (held == nullptr) {
    throw InternalError(
        "EvaluationAttempts::Answer: an installed assertion starts the "
        "statements its answers select, and has no answer to read");
  }
  return static_cast<std::uint64_t>(held->answer);
}

void EvaluationAttempts::SettleAtEndOfRun() {
  RequireInstalled();
  auto* installed = std::get_if<Installed>(&answered_);
  if (installed == nullptr) {
    throw InternalError(
        "EvaluationAttempts::SettleAtEndOfRun: a held attempt is never "
        "registered for the end of the run");
  }
  const std::size_t answered = installed->pending_holds ? attempts_.size() : 0;
  for (std::size_t i = 0; i < answered; ++i) {
    installed->pass_action();
  }
  words_.clear();
  evaluations_.clear();
  attempts_.clear();
  cursor_ = 0;
}

void EvaluationAttempts::RequireInstalled() const {
  if (!installed_) {
    throw InternalError(
        "EvaluationAttempts: an assertion was reached before the design "
        "activated it, so nothing here knows how wide a position set is");
  }
}

void EvaluationAttempts::OpenEvaluation(std::size_t attempt, bool stepped) {
  evaluations_.push_back(
      Evaluation{.attempt = attempt, .stepped = stepped, .live = true});
  words_.insert(words_.end(), seed_.begin(), seed_.end());
}

auto EvaluationAttempts::EvaluationAt(std::int64_t index) -> Evaluation& {
  RequireInstalled();
  if (index < 0 || std::cmp_greater_equal(index, evaluations_.size())) {
    throw InternalError(
        "EvaluationAttempts: an evaluation index no sweep of this tick "
        "produced");
  }
  return evaluations_[static_cast<std::size_t>(index)];
}

auto EvaluationAttempts::WordSlot(std::int64_t index, std::uint64_t word) const
    -> std::size_t {
  RequireInstalled();
  if (index < 0 || std::cmp_greater_equal(index, evaluations_.size()) ||
      word >= width_) {
    throw InternalError(
        "EvaluationAttempts: a position word outside the set this assertion "
        "was installed with");
  }
  return (static_cast<std::size_t>(index) * width_) +
         static_cast<std::size_t>(word);
}

}  // namespace lyra::runtime
