#pragma once

#include <algorithm>
#include <array>
#include <cstddef>
#include <initializer_list>
#include <span>

#include "lyra/base/internal_error.hpp"

namespace lyra::support {

// What a function states of each of its operands of one sort, in the order it
// takes them, held in place so that the statement is a constant. A list longer
// than any function takes is refused where it is written rather than laid out
// past what the list holds.
template <typename Fact, std::size_t kMost>
class PerOperand {
 public:
  constexpr PerOperand() = default;
  constexpr PerOperand(std::initializer_list<Fact> facts)
      : count_(facts.size()) {
    if (facts.size() > kMost) {
      throw InternalError(
          "a list states more operands than it holds -- please report this as "
          "a bug");
    }
    std::ranges::copy(facts, facts_.begin());
  }

  // The facts are held in place, so a list read from a statement that is
  // about to end would name storage already gone.
  [[nodiscard]] constexpr auto All() const& -> std::span<const Fact> {
    return std::span<const Fact>(facts_).first(count_);
  }
  [[nodiscard]] auto All() const&& -> std::span<const Fact> = delete;

 private:
  std::array<Fact, kMost> facts_{};
  std::size_t count_ = 0;
};

}  // namespace lyra::support
