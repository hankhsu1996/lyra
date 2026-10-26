#pragma once

#include <optional>
#include <vector>

#include "lyra/hir/owned_child_step.hpp"
#include "lyra/hir/specialization_ref.hpp"

namespace lyra::hir {

struct BlockConstruction;

// How one object of a scope is built beyond what its code states: for each
// instance member the scope declares, in the order it declares them, the
// specialization each object it stands for is, in row-major order (LRM 23.3.2);
// and for each generate construct, the block it builds at each place -- a
// loop's blocks in the order it counts them out (LRM 27.4), a conditional's
// alternatives at the positions the source wrote them (LRM 27.5), a block
// standing on its own at its one place. Which body a child is depends on the
// object being built, never on the code compiled for the scope holding it, so
// it is stated where the program is assembled.
struct ScopeConstruction {
  std::vector<std::vector<SpecializationRef>> instances;
  std::vector<std::vector<BlockConstruction>> generates;

  auto operator==(const ScopeConstruction&) const -> bool = default;
};

// The block one place of a generate builds: its body, absent where the object
// builds none there -- an alternative it did not select -- and how that block
// is built in turn.
struct BlockConstruction {
  std::optional<BlockBodyId> body;
  ScopeConstruction built;

  auto operator==(const BlockConstruction&) const -> bool = default;
};

}  // namespace lyra::hir
