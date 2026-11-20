#include "lyra/lowering/mir_to_lir/cleanups.hpp"

#include <cstdint>
#include <optional>
#include <utility>

#include "lyra/lir/function.hpp"

namespace lyra::lowering::mir_to_lir {

CleanupTree::CleanupTree() {
  nodes_.push_back(Node{.entry = std::nullopt, .way_out = WayOut::kLeaving});
}

auto CleanupTree::Add(Cleanup cleanup) -> CleanupId {
  const CleanupId id{static_cast<std::uint32_t>(cleanups_.size())};
  cleanups_.push_back(std::move(cleanup));
  return id;
}

auto CleanupTree::Get(CleanupId id) const -> const Cleanup& {
  return cleanups_[id.value];
}

auto CleanupTree::Root(lir::BlockId destination) -> CleanupNodeId {
  return RootAt(destination, WayOut::kOrdinary);
}

auto CleanupTree::TakingRoot(lir::BlockId taking_point) -> CleanupNodeId {
  return RootAt(taking_point, WayOut::kTaken);
}

auto CleanupTree::LeavingRoot() -> CleanupNodeId {
  return CleanupNodeId{0};
}

auto CleanupTree::RootAt(lir::BlockId destination, WayOut way_out)
    -> CleanupNodeId {
  if (const auto found = roots_.find(destination); found != roots_.end()) {
    return found->second;
  }
  const CleanupNodeId id{static_cast<std::uint32_t>(nodes_.size())};
  nodes_.push_back(Node{.entry = destination, .way_out = way_out});
  roots_.emplace(destination, id);
  return id;
}

auto CleanupTree::Find(CleanupId cleanup, CleanupNodeId next) const
    -> std::optional<CleanupNodeId> {
  if (const auto found = made_.find({cleanup, next}); found != made_.end()) {
    return found->second;
  }
  return std::nullopt;
}

auto CleanupTree::Insert(
    CleanupId cleanup, CleanupNodeId next, lir::BlockId entry)
    -> CleanupNodeId {
  const CleanupNodeId id{static_cast<std::uint32_t>(nodes_.size())};
  nodes_.push_back(Node{.entry = entry, .way_out = nodes_[next.value].way_out});
  made_.emplace(std::pair{cleanup, next}, id);
  return id;
}

auto CleanupTree::EntryOf(CleanupNodeId node) const
    -> std::optional<lir::BlockId> {
  return nodes_[node.value].entry;
}

auto CleanupTree::Unwinding(CleanupNodeId node) const -> bool {
  switch (nodes_[node.value].way_out) {
    case WayOut::kOrdinary:
      return false;
    case WayOut::kTaken:
    case WayOut::kLeaving:
      return true;
  }
  std::unreachable();
}

auto CleanupTree::LeavesTheBody(CleanupNodeId node) const -> bool {
  switch (nodes_[node.value].way_out) {
    case WayOut::kOrdinary:
    case WayOut::kTaken:
      return false;
    case WayOut::kLeaving:
      return true;
  }
  std::unreachable();
}

}  // namespace lyra::lowering::mir_to_lir
