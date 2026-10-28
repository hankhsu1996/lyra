#pragma once

#include <optional>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

#include "lyra/hir/published_callable.hpp"
#include "lyra/hir/published_member.hpp"
#include "lyra/support/event_edge.hpp"

namespace lyra::hir {

// An entry whose operand is a value, waited on for a change of it taken at
// `edge`. A call shows nothing to wait on, so the members the operand reads
// cross beside it, as a name a view computes has them.
struct PublishedClockingChange {
  support::EventEdge edge{};
  std::vector<PublishedMemberId> observes;

  auto operator==(const PublishedClockingChange&) const -> bool = default;
};

// An entry whose operand is an event, waited on for a trigger of it (LRM
// 15.5.2).
struct PublishedClockingTrigger {
  auto operator==(const PublishedClockingTrigger&) const -> bool = default;
};

// One event of a clocking block's clocking event, an entry of its event list
// (LRM 14.3, 9.4.2.1). The expression the block wrote names the declarations
// of the scope declaring it, which mean nothing where this signature is read,
// so what crosses is the subroutine that scope evaluates it in, what waiting
// on it waits for, and the subroutine evaluating its `iff` qualifier where it
// wrote one (LRM 9.4.2.3).
struct PublishedClockingEntry {
  PublishedCallableId evaluate;
  std::variant<PublishedClockingChange, PublishedClockingTrigger> occurrence;
  std::optional<PublishedCallableId> condition;

  auto operator==(const PublishedClockingEntry&) const -> bool = default;
};

// A clocking block a scope declares (LRM 14.3), by its name, which another
// unit reaches it by: through an interface port or a hierarchical name, to
// clock a sequence or property the block declares (LRM 14.13, 16.16).
struct PublishedClockingBlock {
  std::string name;
  std::vector<PublishedClockingEntry> events;

  auto operator==(const PublishedClockingBlock&) const -> bool = default;
};

// The clocking block published under `name`, or nothing when the scope
// declares no such block.
[[nodiscard]] inline auto FindClockingBlock(
    const std::vector<PublishedClockingBlock>& blocks, std::string_view name)
    -> const PublishedClockingBlock* {
  for (const PublishedClockingBlock& block : blocks) {
    if (block.name == name) return &block;
  }
  return nullptr;
}

}  // namespace lyra::hir
