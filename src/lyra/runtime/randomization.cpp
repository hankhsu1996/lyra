#include "lyra/runtime/randomization.hpp"

#include <cstdint>
#include <string>

#include "lyra/runtime/constraint_cycle.hpp"

namespace lyra::runtime {

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
