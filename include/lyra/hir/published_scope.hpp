#pragma once

#include <algorithm>
#include <compare>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/base/pool_id.hpp"
#include "lyra/hir/published_callable.hpp"
#include "lyra/hir/published_member.hpp"
#include "lyra/hir/published_modport.hpp"

namespace lyra::hir {

// One block a loop generate counted out (LRM 27.4): the value its index stood
// at, which is how a name selects it, and the class it was published as.
struct PublishedLoopBlock {
  std::int64_t index = 0;
  std::string class_name;

  auto operator==(const PublishedLoopBlock&) const -> bool = default;
};

// A loop generate. Its member holds one object per block, in the order the
// blocks are listed here, which is the order the loop counted them out.
struct PublishedLoop {
  std::string name;
  std::vector<PublishedLoopBlock> blocks;

  auto operator==(const PublishedLoop&) const -> bool = default;
};

// One block of a construct that builds at most one, under the name the source
// gave it (LRM 27.6), and the class it was published as.
struct PublishedAlternative {
  std::string name;
  std::string class_name;

  auto operator==(const PublishedAlternative&) const -> bool = default;
};

// A construct that builds at most one block: a block standing on its own, or a
// conditional, of whose alternatives only the one this elaboration selected
// has a scope to publish (LRM 27.5). Its member holds that one object.
struct PublishedChoice {
  std::vector<PublishedAlternative> blocks;

  auto operator==(const PublishedChoice&) const -> bool = default;
};

// One generate construct a scope publishes, which a name steps through into
// one of its blocks (LRM 23.6).
using PublishedGenerate = std::variant<PublishedLoop, PublishedChoice>;

// Which of the generate constructs a scope published, by its position among
// them.
struct PublishedGenerateId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const PublishedGenerateId&) const
      -> std::strong_ordering = default;
};

// What a `disable` of a named block or task terminates (LRM 9.6.2), named by
// the path of named blocks and subroutines that reaches it from the scope,
// outermost first, the block or task itself last.
struct PublishedDisableTarget {
  std::vector<std::string> path;

  auto operator==(const PublishedDisableTarget&) const -> bool = default;
};

// Which of the disable targets a scope published, by its position among them.
struct PublishedDisableTargetId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const PublishedDisableTargetId&) const
      -> std::strong_ordering = default;
};

// The class of a scope of a unit -- an instance of the unit, or one of the
// generate blocks inside it (LRM 23.6 reaches a declaration of either by name):
// the class's own name, the members another unit may name on it, the
// subroutines another unit may call on it, the generate constructs a name steps
// through into its blocks, what a `disable` of one of its named blocks or tasks
// ends, and, for an instance, the views it offers over those members. A unit's
// name and the name of the class it builds are two facts, so a referrer reads
// the class it reaches here rather than deriving it from the unit it reached
// through. Every type here is one of the pool of whoever holds the record: the
// unit's signature, the unit's own scope, or a referrer's record of it.
//
// The class places the members first, then one member per generate construct
// holding what that construct built, then one per disable target, each list in
// the order it is given here: the order is as much a part of what is published
// as the names are, because a field's position is what fixes where its storage
// sits, and the declaring unit and every referrer lay the class out from these
// lists alone.
struct ScopeClassSignature {
  std::string class_name;
  base::Arena<PublishedMember, PublishedMemberId> members;
  base::Arena<PublishedCallable, PublishedCallableId> callables;
  base::Arena<PublishedGenerate, PublishedGenerateId> generates;
  base::Arena<PublishedDisableTarget, PublishedDisableTargetId> disable_targets;
  std::vector<PublishedModport> modports;

  // The member published under `name` inside the named blocks and subroutines
  // `within` names, or nothing when the scope published no such name there.
  [[nodiscard]] auto FindMember(
      std::string_view name, std::span<const std::string> within = {}) const
      -> std::optional<PublishedMemberId> {
    for (const PublishedMemberId id : members.Ids()) {
      const PublishedMember& member = members.Get(id);
      if (member.name == name && std::ranges::equal(member.within, within)) {
        return id;
      }
    }
    return std::nullopt;
  }

  // Which of the scope's disable targets the path names, or nothing where it
  // published none there.
  [[nodiscard]] auto FindDisableTarget(std::span<const std::string> path) const
      -> std::optional<PublishedDisableTargetId> {
    for (const PublishedDisableTargetId id : disable_targets.Ids()) {
      if (std::ranges::equal(disable_targets.Get(id).path, path)) return id;
    }
    return std::nullopt;
  }

  // The callable published under `name`, or nothing when the scope published no
  // such name, which leaves a call on it nothing to compile against.
  [[nodiscard]] auto FindCallable(std::string_view name) const
      -> std::optional<PublishedCallableId> {
    for (const PublishedCallableId id : callables.Ids()) {
      if (callables.Get(id).name == name) return id;
    }
    return std::nullopt;
  }

  auto operator==(const ScopeClassSignature&) const -> bool = default;
};

}  // namespace lyra::hir
