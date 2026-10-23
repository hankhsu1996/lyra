#pragma once

#include <compare>
#include <cstddef>
#include <cstdint>
#include <vector>

#include "lyra/runtime/rng.hpp"

namespace lyra::runtime {

// A natural number of any size, for counting what a constraint admits: how
// many assignments lie below a node of a decision diagram, or how much weight a
// distribution gives the values of a range (LRM 18.5.3), neither of which a
// machine word bounds.
class Natural {
 public:
  Natural() = default;
  explicit Natural(std::uint64_t value);
  // The number whose bits these are, least significant first.
  [[nodiscard]] static auto FromBits(const std::vector<bool>& bits) -> Natural;

  [[nodiscard]] auto IsZero() const -> bool {
    return words_.empty();
  }
  // The memory the number holds, itself included.
  [[nodiscard]] auto HeldBytes() const -> std::size_t {
    return sizeof(Natural) + (words_.capacity() * sizeof(std::uint64_t));
  }

  [[nodiscard]] auto ShiftedLeft(std::uint32_t shift) const -> Natural;
  // The low `count` bits, least significant first, taken off the number.
  [[nodiscard]] auto TakeLowBits(std::uint32_t count) -> std::vector<bool>;

  friend auto operator+(const Natural& a, const Natural& b) -> Natural;
  // `a` less `b`, where `b` is no larger.
  friend auto operator-(const Natural& a, const Natural& b) -> Natural;
  friend auto operator*(const Natural& a, const Natural& b) -> Natural;
  friend auto operator<=>(const Natural& a, const Natural& b)
      -> std::strong_ordering;
  friend auto operator==(const Natural& a, const Natural& b) -> bool = default;

  // A number drawn uniformly below `bound`, which is not zero.
  [[nodiscard]] static auto DrawBelow(DrawRng& rng, const Natural& bound)
      -> Natural;

 private:
  [[nodiscard]] auto BitLength() const -> std::uint32_t;
  void Trim();

  // Least significant first, with no zero word on top.
  std::vector<std::uint64_t> words_;
};

}  // namespace lyra::runtime
