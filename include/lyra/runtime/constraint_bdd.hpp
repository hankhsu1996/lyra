#pragma once

#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <vector>

#include "lyra/runtime/constraint_circuit.hpp"
#include "lyra/runtime/natural.hpp"
#include "lyra/runtime/rng.hpp"

namespace lyra::runtime {

// A condition of a constraint circuit as a draw of some of its inputs sees
// it, numbered by itself alone: the drawn bits in the order a draw decides
// them, then every other input the condition reads in the order a walk from
// the condition first meets it, then each gate in the order that walk
// finishes it. Two conditions that state the same thing about the drawn bits
// the same way are equal, whatever else the circuits around them hold, and
// what the words state is all a diagram is built from.
struct CanonicalCondition {
  std::uint32_t drawn = 0;
  std::uint32_t others = 0;
  // The two inputs of each gate in turn, then the condition, each a literal
  // over the numbering: 0 for false, then the drawn bits, the other inputs and
  // the gates, doubled and plus one where negated.
  std::vector<std::uint32_t> words;

  auto operator==(const CanonicalCondition&) const -> bool = default;
};

// `condition` as a draw of `drawn` sees it.
[[nodiscard]] auto CanonicalConditionOf(
    const ConstraintCircuit& circuit, CircuitLiteral condition,
    std::span<const std::uint32_t> drawn) -> CanonicalCondition;

// Every value of the drawn bits of a condition that some value of its other
// inputs makes true, as a reduced ordered binary decision diagram deciding
// the drawn bits in their order, with how many such values lie below each
// node. A value drawn by walking down with each branch taken in proportion to
// what it holds is drawn uniformly among all of them (Grosse, Ebendt and
// Drechsler, GLSVLSI 2007, section 3.2), and since the diagram of a function
// in a fixed order is unique, so is what a draw returns from a given
// generator. Built once, it is never changed, so any number of draws may read
// it at once.
class ConstraintBdd {
 public:
  // The diagram of `condition`, built by a decision-diagram package (CUDD;
  // Somenzi) that stops once about `node_budget` nodes are live while it is
  // built; none where the condition does not fit, which is a property of the
  // condition and the budget alone.
  [[nodiscard]] static auto Build(
      const CanonicalCondition& condition, std::size_t node_budget)
      -> std::optional<ConstraintBdd>;

  [[nodiscard]] auto Empty() const -> bool;
  // The memory the diagram holds.
  [[nodiscard]] auto HeldBytes() const -> std::size_t;

  // One value of every drawn bit, in their order, uniformly among those the
  // diagram holds. The diagram is not empty.
  [[nodiscard]] auto Draw(DrawRng& rng) const -> std::vector<bool>;

 private:
  // A decision on bit `level`; nodes 0 and 1 are false and true, at the level
  // past the last.
  struct Node {
    std::uint32_t level;
    std::uint32_t low;
    std::uint32_t high;
  };

  std::uint32_t levels_ = 0;
  std::uint32_t root_ = 0;
  std::vector<Node> nodes_;
  // How many values of the bits from a node's level on reach true from it.
  std::vector<Natural> counts_;
};

}  // namespace lyra::runtime
