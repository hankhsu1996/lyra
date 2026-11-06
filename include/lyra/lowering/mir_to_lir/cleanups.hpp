#pragma once

#include <compare>
#include <cstdint>
#include <map>
#include <optional>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/pool_id.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/mir/block_id.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::mir_to_lir {

// What leaving a scope owes, run on every way out of it -- falling off its end,
// a break or continue, a return, being ended at a suspension, and unwinding --
// as clang's cleanup stack and rustc's scheduled drops are run.
//
// The code a body paired with a cleanup states, with the block whose child
// scopes hold it.
struct RunFinally {
  const mir::Block* owner = nullptr;
  mir::BlockId cleanup{};
};
// The end of an owned value the scope made.
struct EndValue {
  lir::ValueId value;
};
// The end of the owned value a frame slot holds.
struct EndSlot {
  lir::ValueId slot;
};
// The bytes of a frame slot, owed from where the slot comes into use and so
// standing below the end of any value it comes to hold, as rustc schedules a
// local's storage and its value as two drops. A call that would have made a
// value into such bytes owes this alone where it is unwound out of, since no
// value was made, as clang ends a call temporary's lifetime.
struct StorageDead {
  lir::ValueId slot;
};
// A variable among the body's own, which ends with the scope declaring it while
// its storage lasts the whole body.
struct EndVariable {
  lir::VariablePosition position;
};
// Storage a new-expression allocated, while what it is to hold is still being
// initialized there: a way out gives the storage back and ends nothing, since
// nothing whole lies in it yet, as clang's new-expression calls
// `operator delete` when a constructor throws.
struct ReleaseStorage {
  lir::Operand storage;
};
// The handling of what a handler took, which ends on every way out of the
// handler, being unwound out of it included, as clang ends a `catch`.
struct EndHandling {};

using Cleanup = std::variant<
    RunFinally, EndValue, EndSlot, StorageDead, EndVariable, ReleaseStorage,
    EndHandling>;

struct CleanupId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const CleanupId&) const -> std::strong_ordering = default;
};

struct CleanupNodeId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const CleanupNodeId&) const
      -> std::strong_ordering = default;
};

// The cleanups a body owes, and what every way out of it runs: a node is one
// cleanup and the node it continues to, ending at a root, the block the way out
// goes to once nothing is left to run. A node is made once for its cleanup and
// its continuation, however many ways out reach it, as rustc's drop tree is,
// and it never changes, so a way out recorded before a value's end was handed
// on keeps running it and one recorded after does not.
class CleanupTree {
 public:
  auto Add(Cleanup cleanup) -> CleanupId;
  [[nodiscard]] auto Get(CleanupId id) const -> const Cleanup&;

  // The root a way out going to `destination` ends at once nothing is left to
  // run: a block an ordinary way out goes to, or the frame's edge.
  auto Root(lir::BlockId destination) -> CleanupNodeId;
  // The root unwinding ends at where a region's `handler` takes it, which is
  // what tells a landing that this frame takes the raise.
  auto HandlerRoot(lir::BlockId handler) -> CleanupNodeId;
  // The node running `cleanup` and then `next`, where one was made.
  [[nodiscard]] auto Find(CleanupId cleanup, CleanupNodeId next) const
      -> std::optional<CleanupNodeId>;
  // Records the node running `cleanup` and then `next`, entered at `entry`.
  auto Insert(CleanupId cleanup, CleanupNodeId next, lir::BlockId entry)
      -> CleanupNodeId;

  [[nodiscard]] auto EntryOf(CleanupNodeId node) const -> lir::BlockId;
  // Whether the way out from `node` ends at a region's handler.
  [[nodiscard]] auto EndsInHandler(CleanupNodeId node) const -> bool;
  // The landing that enters `node`, shared by every call unwinding into it.
  [[nodiscard]] auto LandingOf(CleanupNodeId node) const
      -> std::optional<lir::BlockId>;
  void SetLanding(CleanupNodeId node, lir::BlockId landing);

 private:
  struct Node {
    lir::BlockId entry;
    bool ends_in_handler = false;
    std::optional<lir::BlockId> landing;
  };

  std::vector<Cleanup> cleanups_;
  std::vector<Node> nodes_;
  std::map<lir::BlockId, CleanupNodeId> roots_;
  std::map<std::pair<CleanupId, CleanupNodeId>, CleanupNodeId> made_;

  auto RootAt(lir::BlockId destination, bool ends_in_handler) -> CleanupNodeId;
};

}  // namespace lyra::lowering::mir_to_lir
