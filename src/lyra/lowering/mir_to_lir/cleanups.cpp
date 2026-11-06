#include "lyra/lowering/mir_to_lir/cleanups.hpp"

#include <cstdint>
#include <optional>
#include <utility>

#include "lyra/lir/function.hpp"

namespace lyra::lowering::mir_to_lir {

auto CleanupTree::Add(Cleanup cleanup) -> CleanupId {
  const CleanupId id{static_cast<std::uint32_t>(cleanups_.size())};
  cleanups_.push_back(std::move(cleanup));
  return id;
}

auto CleanupTree::Get(CleanupId id) const -> const Cleanup& {
  return cleanups_[id.value];
}

auto CleanupTree::Root(lir::BlockId destination) -> CleanupNodeId {
  return RootAt(destination, false);
}

auto CleanupTree::HandlerRoot(lir::BlockId handler) -> CleanupNodeId {
  return RootAt(handler, true);
}

auto CleanupTree::RootAt(lir::BlockId destination, bool ends_in_handler)
    -> CleanupNodeId {
  if (const auto found = roots_.find(destination); found != roots_.end()) {
    return found->second;
  }
  const CleanupNodeId id{static_cast<std::uint32_t>(nodes_.size())};
  nodes_.push_back(
      Node{
          .entry = destination,
          .ends_in_handler = ends_in_handler,
          .landing = std::nullopt});
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
  nodes_.push_back(
      Node{
          .entry = entry,
          .ends_in_handler = nodes_[next.value].ends_in_handler,
          .landing = std::nullopt});
  made_.emplace(std::pair{cleanup, next}, id);
  return id;
}

auto CleanupTree::EntryOf(CleanupNodeId node) const -> lir::BlockId {
  return nodes_[node.value].entry;
}

auto CleanupTree::EndsInHandler(CleanupNodeId node) const -> bool {
  return nodes_[node.value].ends_in_handler;
}

auto CleanupTree::LandingOf(CleanupNodeId node) const
    -> std::optional<lir::BlockId> {
  return nodes_[node.value].landing;
}

void CleanupTree::SetLanding(CleanupNodeId node, lir::BlockId landing) {
  nodes_[node.value].landing = landing;
}

}  // namespace lyra::lowering::mir_to_lir
