#include "lyra/runtime/randomization.hpp"

#include <charconv>
#include <cstddef>
#include <cstdint>
#include <span>
#include <string>
#include <string_view>
#include <system_error>

#include "lyra/runtime/constraint_cycle.hpp"

namespace lyra::runtime {

namespace {

constexpr std::string_view kNodeBudgetOption = "lyra+randomize+nodes=";

}  // namespace

RandomizeSettings::RandomizeSettings(std::span<const std::string> plusargs) {
  for (const std::string& argument : plusargs) {
    const std::string_view text(argument);
    if (!text.starts_with(kNodeBudgetOption)) {
      continue;
    }
    const std::string_view count = text.substr(kNodeBudgetOption.size());
    std::size_t parsed = 0;
    const auto [end, error] =
        std::from_chars(count.data(), count.data() + count.size(), parsed);
    if (error == std::errc{} && end == count.data() + count.size()) {
      node_budget_ = parsed;
    }
  }
}

auto RandomizeSettings::FirstPastBudget() -> bool {
  const bool first = !past_budget_reported_;
  past_budget_reported_ = true;
  return first;
}

auto RandomizationModes::RandEnabled(const RandomVariablePath& variable) const
    -> bool {
  return rand_default_ != rand_flipped_.contains(variable);
}

void RandomizationModes::SetRand(const RandomVariablePath& variable, bool on) {
  if (on == rand_default_) {
    rand_flipped_.erase(variable);
  } else {
    rand_flipped_.insert(variable);
  }
}

void RandomizationModes::SetAllRand(bool on) {
  rand_default_ = on;
  rand_flipped_.clear();
}

auto RandomizationModes::ConstraintEnabled(const std::string& block) const
    -> bool {
  return constraint_default_ != constraint_flipped_.contains(block);
}

void RandomizationModes::SetConstraint(const std::string& block, bool on) {
  if (on == constraint_default_) {
    constraint_flipped_.erase(block);
  } else {
    constraint_flipped_.insert(block);
  }
}

void RandomizationModes::SetAllConstraints(bool on) {
  constraint_default_ = on;
  constraint_flipped_.clear();
}

auto RandomizationModes::CycleOf(
    const RandomVariablePath& variable, std::uint64_t width,
    std::optional<std::uint64_t> count) -> ConstraintCycle& {
  return cycles_.try_emplace(variable, width, count).first->second;
}

auto StaticRandomizationModes() -> RandomizationModes& {
  static RandomizationModes modes;
  return modes;
}

}  // namespace lyra::runtime
