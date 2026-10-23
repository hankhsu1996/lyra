#pragma once

#include <cstddef>
#include <cstdint>
#include <functional>
#include <optional>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "lyra/runtime/constraint_cycle.hpp"

namespace lyra::runtime {

// A random variable as its owner keeps its mode and its cycle: the variable the
// class declares, by the address of its storage, which is its identity for as
// long as its owner lives, and the positions that reach an element of it, at
// any depth -- empty for the variable itself. An element is named by its
// positions rather than its own storage, which moves when an array holding it
// is resized and is then free for another variable (LRM 18.8).
struct RandomVariablePath {
  const void* declared;
  std::vector<std::size_t> positions;

  auto operator==(const RandomVariablePath&) const -> bool = default;

  // The same path one position deeper.
  [[nodiscard]] auto Into(std::size_t position) const -> RandomVariablePath {
    RandomVariablePath deeper = *this;
    deeper.positions.push_back(position);
    return deeper;
  }

  struct Hash {
    auto operator()(const RandomVariablePath& path) const -> std::size_t {
      std::size_t h = std::hash<const void*>{}(path.declared);
      for (const std::size_t position : path.positions) {
        h = (h ^ position) * 0x100000001b3ULL;
      }
      return h;
    }
  };
};

// What one owner keeps of its randomization between calls: which of its
// random variables and constraint blocks are switched off (LRM 18.8, 18.9),
// and where each of its `randc` variables is in its cycle (LRM 18.4.2). An
// object keeps one for its own properties and blocks; the run keeps one for
// every static property and static block, whose state every object of the
// class shares (LRM 18.4.2, 18.5.10).
//
// A variable is named by a `RandomVariablePath`, and a block by its name, which
// is one block's across a class and every class extending it (LRM 18.5.2).
// Every variable and every block starts on, so only those switched away from
// the owner's default are kept.
class RandomizationModes {
 public:
  [[nodiscard]] auto RandEnabled(const RandomVariablePath& variable) const
      -> bool;
  void SetRand(const RandomVariablePath& variable, bool on);
  void SetAllRand(bool on);

  [[nodiscard]] auto ConstraintEnabled(const std::string& block) const -> bool;
  void SetConstraint(const std::string& block, bool on);
  void SetAllConstraints(bool on);

  // The cycle of the `randc` variable `variable`, over the positions `width`
  // bits hold or the first `count` of them.
  [[nodiscard]] auto CycleOf(
      const RandomVariablePath& variable, std::uint64_t width,
      std::optional<std::uint64_t> count) -> ConstraintCycle&;

 private:
  bool rand_default_ = true;
  std::unordered_set<RandomVariablePath, RandomVariablePath::Hash>
      rand_flipped_;
  bool constraint_default_ = true;
  std::unordered_set<std::string> constraint_flipped_;
  std::unordered_map<
      RandomVariablePath, ConstraintCycle, RandomVariablePath::Hash>
      cycles_;
};

// The run's own, for static properties and static blocks.
[[nodiscard]] auto StaticRandomizationModes() -> RandomizationModes&;

}  // namespace lyra::runtime
