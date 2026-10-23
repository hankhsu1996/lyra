#pragma once

#include <cstdint>
#include <optional>
#include <unordered_set>

#include "lyra/runtime/rng.hpp"

namespace lyra::runtime {

// The order a `randc` variable runs through its values (LRM 18.4.2): a random
// permutation of its positions -- every value of its width, or the positions
// of the members of an enumeration, which are all a chosen one takes (LRM
// 18.5) -- each taken at most once before a new permutation begins. A
// position the constraints rule out at a turn stays to be taken later in the
// same permutation.
//
// The permutation is a keyed Feistel network over `width` bits, a bijection on
// every width with no table of the values, so a 32-bit variable costs what an
// 8-bit one does; LRM 18.4.2 permits a limit no lower than 8 bits, and this
// one is 64. A count of positions short of what the width holds is permuted by
// walking the network's cycle until it lands below the count (Black and
// Rogaway, CT-RSA 2002), which stays a bijection on those positions.
class ConstraintCycle {
 public:
  ConstraintCycle(std::uint64_t width, std::optional<std::uint64_t> count);

  // What the cycle holds at its position `position`, among `Size()` of them.
  [[nodiscard]] auto ValueAt(std::uint64_t position) const -> std::uint64_t;
  [[nodiscard]] auto Size() const -> std::uint64_t;

  // The first position from the cursor whose value is not yet taken.
  [[nodiscard]] auto Cursor() const -> std::uint64_t {
    return cursor_;
  }
  // The positions this permutation has taken.
  [[nodiscard]] auto Taken() const -> const std::unordered_set<std::uint64_t>& {
    return taken_;
  }
  // Records `value` as taken in this permutation, and starts a new one drawn
  // from `rng` once every value is.
  void Take(std::uint64_t value, DrawRng& rng);
  // Starts a new permutation drawn from `rng`.
  void Restart(DrawRng& rng);
  // The constraints on the variable at this call are those `signature`
  // stands for; where they differ from the last call's, a new permutation is
  // begun (LRM 18.4.2).
  void ConstrainedBy(std::uint64_t signature, DrawRng& rng);

 private:
  // The network's own permutation of every value of the width.
  [[nodiscard]] auto Permute(std::uint64_t value) const -> std::uint64_t;

  std::uint64_t width_;
  std::optional<std::uint64_t> count_;
  std::uint64_t key_ = 0;
  std::uint64_t cursor_ = 0;
  std::unordered_set<std::uint64_t> taken_;
  std::optional<std::uint64_t> constrained_by_;
};

}  // namespace lyra::runtime
