#include "lyra/runtime/evaluation_attempts.hpp"

#include <cstddef>
#include <cstdint>
#include <functional>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/region.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/support/evaluation_outcome.hpp"

namespace lyra::runtime {

void EvaluationAttempts::Install(
    RuntimeEffects& effects, std::uint64_t words, bool pending_holds,
    std::function<void()> pass_action, std::function<void()> fail_action) {
  if (words == 0) {
    throw InternalError(
        "EvaluationAttempts::Install: a property with no position to be live "
        "at matches nothing the grammar admits (LRM 16.7)");
  }
  width_ = static_cast<std::size_t>(words);
  pending_holds_ = pending_holds;
  pass_action_ = std::move(pass_action);
  fail_action_ = std::move(fail_action);
  seed_.assign(width_, 0);
  installed_ = true;
  effects.RegisterConcurrentAssertion(*this);
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

void EvaluationAttempts::BeginTick() {
  RequireInstalled();
  for (Evaluation& evaluation : evaluations_) {
    evaluation.stepped = false;
  }
  attempts_.push_back(Attempt{});
  OpenEvaluation(attempts_.size() - 1, false);
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
  constexpr auto kAnswered = static_cast<std::size_t>(-1);
  std::vector<std::size_t> renumbered(attempts_.size(), kAnswered);
  std::vector<Attempt> kept_attempts;
  for (std::size_t attempt = 0; attempt < attempts_.size(); ++attempt) {
    if (attempts_[attempt].failed) {
      effects.Submit(effects.Now(), Region::kReactive, fail_action_);
      continue;
    }
    if (live_evaluations[attempt] == 0) {
      effects.Submit(effects.Now(), Region::kReactive, pass_action_);
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

void EvaluationAttempts::SettleAtEndOfRun() {
  RequireInstalled();
  const std::size_t answered = pending_holds_ ? attempts_.size() : 0;
  for (std::size_t i = 0; i < answered; ++i) {
    pass_action_();
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
