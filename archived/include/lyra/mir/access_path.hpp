#pragma once

#include <cstdint>
#include <optional>
#include <unordered_set>

#include "lyra/mir/place.hpp"
#include "lyra/mir/terminator.hpp"

namespace lyra::mir {

// Symbolic access path for must-def analysis.
// Phase 1: whole-variable only (scope + slot_id).
// Phase 2 will extend with projection segments (field, index, slice).
struct AccessPath {
  SignalRef::Scope scope;
  uint32_t slot_id;

  auto operator==(const AccessPath&) const -> bool = default;
};

struct AccessPathHash {
  auto operator()(const AccessPath& p) const noexcept -> size_t {
    return std::hash<uint64_t>{}(
        (static_cast<uint64_t>(p.scope) << 32) | p.slot_id);
  }
};

// Try to extract a whole-signal access path from a place.
// Phase 1: returns a path only for whole-signal references (no projections).
// Returns nullopt for locals, temps, or any projected place.
// Phase 2 will extend this to return paths for projected places too.
inline auto TryExtractWholeSignalPath(const Place& place)
    -> std::optional<AccessPath> {
  if (!place.projections.empty()) return std::nullopt;

  SignalRef::Scope scope{};
  if (place.root.kind == PlaceRoot::Kind::kModuleSlot) {
    scope = SignalRef::Scope::kModuleLocal;
  } else if (place.root.kind == PlaceRoot::Kind::kDesignGlobal) {
    scope = SignalRef::Scope::kDesignGlobal;
  } else {
    return std::nullopt;
  }

  return AccessPath{
      .scope = scope, .slot_id = static_cast<uint32_t>(place.root.id)};
}

// Set of definitely-written access paths for forward must-def analysis.
//
// Lattice:
//   Top = all paths defined (optimistic initial state for non-entry blocks)
//   Empty = no paths defined (entry block initial state)
//   Meet = intersection (a path is must-def only if defined on ALL
//   predecessors)
class MustDefSet {
 public:
  static auto Empty() -> MustDefSet {
    return MustDefSet{};
  }

  static auto Top() -> MustDefSet {
    MustDefSet s;
    s.is_top_ = true;
    return s;
  }

  auto IsTop() const -> bool {
    return is_top_;
  }

  // Does any prior must-def write cover this read path?
  // Phase 1: whole-variable exact match only.
  // Phase 2 will implement sub-path coverage (e.g., write to `v` covers
  // read of `v.f`).
  auto Covers(const AccessPath& read_path) const -> bool {
    if (is_top_) return true;
    return paths_.contains(read_path);
  }

  void Insert(const AccessPath& path) {
    if (is_top_) return;
    paths_.insert(path);
  }

  void IntersectWith(const MustDefSet& other) {
    if (other.is_top_) return;
    if (is_top_) {
      is_top_ = false;
      paths_ = other.paths_;
      return;
    }
    // Keep only paths present in both sets.
    auto it = paths_.begin();
    while (it != paths_.end()) {
      if (!other.paths_.contains(*it)) {
        it = paths_.erase(it);
      } else {
        ++it;
      }
    }
  }

  auto operator==(const MustDefSet& other) const -> bool {
    if (is_top_ != other.is_top_) return false;
    if (is_top_) return true;
    return paths_ == other.paths_;
  }

 private:
  bool is_top_ = false;
  std::unordered_set<AccessPath, AccessPathHash> paths_;
};

}  // namespace lyra::mir
