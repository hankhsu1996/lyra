#include "lyra/runtime/takeover.hpp"

#include <cstdint>
#include <span>
#include <utility>
#include <vector>

#include "lyra/value/integral_words.hpp"

namespace lyra::runtime {

HeldPositions::HeldPositions(value::BitPositions run) : runs_{run} {
}
HeldPositions::HeldPositions(const HeldPositions&) = default;
HeldPositions::HeldPositions(HeldPositions&&) noexcept = default;
auto HeldPositions::operator=(const HeldPositions&) -> HeldPositions& = default;
auto HeldPositions::operator=(HeldPositions&&) noexcept
    -> HeldPositions& = default;
HeldPositions::~HeldPositions() = default;

void HeldPositions::Remove(value::BitPositions removed) {
  std::vector<value::BitPositions> left;
  const std::uint64_t removed_end = removed.lsb + removed.width;
  for (const value::BitPositions run : runs_) {
    const std::uint64_t run_end = run.lsb + run.width;
    if (removed_end <= run.lsb || removed.lsb >= run_end) {
      left.push_back(run);
      continue;
    }
    if (removed.lsb > run.lsb) {
      left.push_back({.lsb = run.lsb, .width = removed.lsb - run.lsb});
    }
    if (removed_end < run_end) {
      left.push_back({.lsb = removed_end, .width = run_end - removed_end});
    }
  }
  runs_ = std::move(left);
}

auto HeldPositions::Empty() const -> bool {
  return runs_.empty();
}

auto HeldPositions::Runs() const -> std::span<const value::BitPositions> {
  return runs_;
}

}  // namespace lyra::runtime
