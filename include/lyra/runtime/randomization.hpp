#pragma once

#include <cstddef>
#include <cstdint>
#include <functional>
#include <optional>
#include <span>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "lyra/runtime/constraint_cycle.hpp"

namespace lyra::runtime {

// How the source declared a variable a constraint reads (LRM 18.4): not
// random, `rand`, or `randc`.
enum class RandomQualifier : std::uint8_t { kNone, kRand, kRandc };

// Makes a random array `count` elements big, answering how to make it what it
// was again.
using ArrayResizer = std::function<std::function<void()>(std::size_t count)>;

// What a run's command line says of how `randomize()` draws, and whether the
// run has already told the user a call drew past what it can count.
class RandomizeSettings {
 public:
  // How many live nodes counting a constraint's solutions may take before a
  // call draws without counting them, unless the command line says otherwise.
  static constexpr std::size_t kDefaultNodeBudget = std::size_t{1} << 18U;

  explicit RandomizeSettings(std::span<const std::string> plusargs);

  [[nodiscard]] auto NodeBudget() const -> std::size_t {
    return node_budget_;
  }
  // Whether this is the run's first call drawing past the budget, the one it
  // reports.
  [[nodiscard]] auto FirstPastBudget() -> bool;

 private:
  std::size_t node_budget_ = kDefaultNodeBudget;
  bool past_budget_reported_ = false;
};

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
