#include "lyra/runtime/evaluation_attempts.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/runtime/region.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/support/property_answer.hpp"

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

}  // namespace

auto EvaluationAttempts::AnswersOf(
    Evaluation& evaluation, std::uint64_t implication) -> ImplicationAnswers& {
  for (ImplicationAnswers& answers : evaluation.implications) {
    if (answers.implication == implication) {
      return answers;
    }
  }
  return evaluation.implications.emplace_back(
      ImplicationAnswers{.implication = implication});
}

void EvaluationAttempts::Install(
    RuntimeEffects& effects, std::uint64_t words, OwnedCall pass_action,
    OwnedCall fail_action) {
  Size(words);
  answered_ = Installed{
      .pass_action = std::move(pass_action),
      .fail_action = std::move(fail_action)};
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
  OpenEvaluation(std::nullopt, 0);
}

void EvaluationAttempts::BeginTick() {
  RequireInstalled();
  for (Evaluation& evaluation : evaluations_) {
    evaluation.stepped = false;
    for (ImplicationAnswers& answers : evaluation.implications) {
      answers.this_tick = support::PropertyAnswer::kHoldsStrongly;
    }
  }
  cursor_ = 0;
}

void EvaluationAttempts::DisableTick() {
  RequireInstalled();
  words_.clear();
  evaluations_.clear();
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
  return words_[WordIndex(index, word)];
}

void EvaluationAttempts::SetWord(
    std::int64_t index, std::uint64_t word, std::uint64_t bits) {
  words_[WordIndex(index, word)] = bits;
}

void EvaluationAttempts::Step(std::int64_t index) {
  EvaluationAt(index).stepped = true;
}

void EvaluationAttempts::Seed(
    std::int64_t index, std::uint64_t implication, std::uint64_t part) {
  OpenEvaluation(
      ConsequentOf{.parent = IndexOf(index), .implication = implication}, part);
}

auto EvaluationAttempts::Count() const -> std::int64_t {
  RequireInstalled();
  return static_cast<std::int64_t>(evaluations_.size());
}

auto EvaluationAttempts::PartOf(std::int64_t index) const -> std::uint64_t {
  return EvaluationAt(index).part;
}

auto EvaluationAttempts::ImplicationAnswer(
    std::int64_t index, std::uint64_t implication) const -> std::uint64_t {
  for (const ImplicationAnswers& answers : EvaluationAt(index).implications) {
    if (answers.implication == implication) {
      return static_cast<std::uint64_t>(
          std::min(answers.settled, answers.this_tick));
    }
  }
  return static_cast<std::uint64_t>(support::PropertyAnswer::kHoldsStrongly);
}

void EvaluationAttempts::Resolve(std::int64_t index, std::uint64_t answer) {
  Evaluation& evaluation = EvaluationAt(index);
  evaluation.answer = AnswerOf(answer);
  if (!evaluation.consequent_of.has_value()) {
    return;
  }
  ImplicationAnswers& answers = AnswersOf(
      evaluations_[evaluation.consequent_of->parent],
      evaluation.consequent_of->implication);
  answers.this_tick = std::min(answers.this_tick, evaluation.answer);
}

void EvaluationAttempts::Report(
    support::PropertyAnswer answer, RuntimeEffects& effects) {
  const bool holds = answer == support::PropertyAnswer::kHoldsStrongly;
  std::visit(
      Overloaded{
          [&](Installed& installed) {
            OwnedCall* action =
                holds ? &installed.pass_action : &installed.fail_action;
            effects.Submit(
                effects.Now(), Region::kReactive, [action] { (*action)(); });
          },
          [&](Held& held) {
            held.answer = holds ? support::AttemptAnswer::kHolds
                                : support::AttemptAnswer::kFails;
          }},
      answered_);
}

void EvaluationAttempts::Settle(RuntimeEffects& effects) {
  RequireInstalled();

  // Parents come before their instances, so one forward pass knows of each
  // evaluation whether something above it has already left.
  constexpr auto kLeaves = static_cast<std::size_t>(-1);
  std::vector<std::size_t> renumbered(evaluations_.size(), kLeaves);
  std::vector<Evaluation> kept;
  std::vector<std::uint64_t> kept_words;
  for (std::size_t index = 0; index < evaluations_.size(); ++index) {
    Evaluation& evaluation = evaluations_[index];
    const std::optional<ConsequentOf>& consequent_of = evaluation.consequent_of;
    if (consequent_of.has_value() &&
        renumbered[consequent_of->parent] == kLeaves) {
      continue;
    }
    if (IsSettled(evaluation.answer)) {
      if (consequent_of.has_value()) {
        ImplicationAnswers& answers = AnswersOf(
            kept[renumbered[consequent_of->parent]],
            consequent_of->implication);
        answers.settled = std::min(answers.settled, evaluation.answer);
      } else {
        Report(evaluation.answer, effects);
      }
      continue;
    }
    renumbered[index] = kept.size();
    Evaluation moved = std::move(evaluation);
    if (moved.consequent_of.has_value()) {
      moved.consequent_of->parent = renumbered[moved.consequent_of->parent];
    }
    kept.push_back(std::move(moved));
    const auto first = static_cast<std::ptrdiff_t>(index * width_);
    kept_words.insert(
        kept_words.end(), words_.begin() + first,
        words_.begin() + first + static_cast<std::ptrdiff_t>(width_));
  }

  evaluations_ = std::move(kept);
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
  for (const Evaluation& evaluation : evaluations_) {
    if (evaluation.consequent_of.has_value()) {
      continue;
    }
    if (evaluation.answer >= support::PropertyAnswer::kHolds) {
      installed->pass_action();
    } else {
      installed->fail_action();
    }
  }
  words_.clear();
  evaluations_.clear();
  cursor_ = 0;
}

void EvaluationAttempts::RequireInstalled() const {
  if (!installed_) {
    throw InternalError(
        "EvaluationAttempts: an assertion was reached before the design "
        "activated it, so nothing here knows how wide a position set is");
  }
}

void EvaluationAttempts::OpenEvaluation(
    std::optional<ConsequentOf> consequent_of, std::uint64_t part) {
  evaluations_.push_back(
      Evaluation{
          .consequent_of = consequent_of,
          .part = part,
          .stepped = false,
          .answer = support::PropertyAnswer::kPending,
          .implications = {}});
  words_.insert(words_.end(), seed_.begin(), seed_.end());
}

auto EvaluationAttempts::IndexOf(std::int64_t index) const -> std::size_t {
  RequireInstalled();
  if (index < 0 || std::cmp_greater_equal(index, evaluations_.size())) {
    throw InternalError(
        "EvaluationAttempts: an evaluation index no sweep of this tick "
        "produced");
  }
  return static_cast<std::size_t>(index);
}

auto EvaluationAttempts::EvaluationAt(std::int64_t index) -> Evaluation& {
  return evaluations_[IndexOf(index)];
}

auto EvaluationAttempts::EvaluationAt(std::int64_t index) const
    -> const Evaluation& {
  return evaluations_[IndexOf(index)];
}

auto EvaluationAttempts::WordIndex(std::int64_t index, std::uint64_t word) const
    -> std::size_t {
  const std::size_t evaluation = IndexOf(index);
  if (word >= width_) {
    throw InternalError(
        "EvaluationAttempts: a position word outside the set this assertion "
        "was installed with");
  }
  return (evaluation * width_) + static_cast<std::size_t>(word);
}

}  // namespace lyra::runtime
