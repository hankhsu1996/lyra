#pragma once

#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <vector>

#include "lyra/runtime/constraint_circuit.hpp"
#include "lyra/runtime/rng.hpp"

namespace lyra::runtime {

// A variable a draw chooses: its inputs, least significant bit first, and,
// where it may take only some values, those values, each least significant
// bit first.
struct DrawnVariable {
  std::span<const std::uint32_t> inputs;
  std::span<const std::vector<bool>> members;
};

// Answers questions about conditions of one constraint circuit: whether
// anything makes one true, and a value of some variables drawn among those the
// rest can complete into an assignment making it true.
//
// A value is decided bit by bit in a fixed order -- the variables as listed,
// each from its most significant bit -- so what a draw returns follows from the
// condition and the generator alone, never from what was drawn or built before
// (LRM 18.14). Where the condition fits a decision diagram of the budget given,
// each bit is decided by how many completions each side holds, which is exactly
// uniform (LRM 18.5.9). Where it does not, each variable first tries a few
// values drawn at random and keeps the first that something completes, and a
// bit still open takes a side drawn at random among those something completes:
// every value it returns makes the condition true, and none that does is out of
// reach, but where one side of a bit holds far more completions than the other
// the two are still taken equally often. Drawing uniformly among the solutions
// of an arbitrary condition is #P-hard, so no budget makes the exact way serve
// every condition.
class ConstraintSampler {
 public:
  ConstraintSampler(const ConstraintCircuit& circuit, std::size_t node_budget);

  [[nodiscard]] auto Satisfiable(CircuitLiteral condition) const -> bool;

  // A value of each of `drawn`, their inputs' bits one variable after another,
  // such that some value of every other input makes `condition` true; none
  // where nothing makes it true.
  [[nodiscard]] auto Draw(
      CircuitLiteral condition, std::span<const DrawnVariable> drawn,
      DrawRng& rng) -> std::optional<std::vector<bool>>;

  // Whether some draw so far was made past the budget, and so not uniformly.
  [[nodiscard]] auto DrewPastBudget() const -> bool {
    return drew_past_budget_;
  }

 private:
  [[nodiscard]] auto DrawBySearch(
      CircuitLiteral condition, std::span<const DrawnVariable> drawn,
      DrawRng& rng) const -> std::optional<std::vector<bool>>;

  const ConstraintCircuit* circuit_;
  std::size_t node_budget_;
  bool drew_past_budget_ = false;
};

}  // namespace lyra::runtime
