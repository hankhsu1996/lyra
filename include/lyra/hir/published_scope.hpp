#pragma once

#include <compare>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/base/pool_id.hpp"
#include "lyra/hir/published_callable.hpp"
#include "lyra/hir/published_member.hpp"
#include "lyra/hir/published_modport.hpp"
#include "lyra/support/def_path.hpp"

namespace lyra::hir {

// A loop generate whose blocks are all one application of its text: an array
// of block instances of one class (LRM 27.4), which is the case where nothing
// a block declares uses the index, whether its code reads it or not. How many
// blocks there are and which indices they stand at is what each construction
// of the holding scope counts out, so the class says neither, and two objects
// of it may hold different numbers of them. `class_path` is absent where no
// object of the holding class counted a block.
struct PublishedLoopOfOneClass {
  std::string name;
  std::optional<support::DefPath> class_path;

  auto operator==(const PublishedLoopOfOneClass&) const -> bool = default;
};

// One block of a loop whose blocks are of several classes: the value its index
// stood at, which is how a name finds its class, and which of the loop's
// classes it is an object of, by position among them.
struct PublishedLoopBlock {
  std::int64_t index = 0;
  std::uint32_t class_at = 0;

  auto operator==(const PublishedLoopBlock&) const -> bool = default;
};

// A loop generate whose blocks are several applications of its text, so which
// class stands at each index is part of what the holding class declares.
// `blocks` lists every index any object of the holding class counted out, in
// ascending order, and `classes` the applications in the order that list first
// names them.
struct PublishedLoopByIndex {
  std::string name;
  std::vector<support::DefPath> classes;
  std::vector<PublishedLoopBlock> blocks;

  auto operator==(const PublishedLoopByIndex&) const -> bool = default;
};

// One block of a construct that builds at most one: where the source wrote it
// among the construct's alternatives, and the class it was published as.
struct PublishedAlternative {
  std::uint32_t position = 0;
  support::DefPath class_path;

  auto operator==(const PublishedAlternative&) const -> bool = default;
};

// A construct that builds at most one block: a block standing on its own, or a
// conditional (LRM 27.5). Which alternative stands is chosen where the scope
// is built, so the objects of one class may have chosen differently, and the
// class publishes every alternative any of them built, in the order the source
// wrote them. Its member holds the one object its scope built.
//
// `construct` is where the source wrote the construct among the generate
// constructs of its scope. A name finds a block by that and by its position,
// and not by its label: the alternatives of one conditional may share a label,
// and so may two conditionals of which a scope only ever builds one.
struct PublishedChoice {
  std::uint32_t construct = 0;
  std::vector<PublishedAlternative> blocks;

  auto operator==(const PublishedChoice&) const -> bool = default;
};

// A loop generate a scope publishes, which a name steps through into one of
// its blocks (LRM 23.6, 27.4). Its member holds the blocks the object's own
// construction built, each under the value its index stood at, so a name asks
// for a block by that value: where it stands among the loop's blocks is the
// object's to answer, since the object is what ran the loop.
using PublishedLoop =
    std::variant<PublishedLoopOfOneClass, PublishedLoopByIndex>;

// Which of the loops a scope published, by its position among them.
struct PublishedLoopId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const PublishedLoopId&) const
      -> std::strong_ordering = default;
};

// Which of the constructs building at most one block a scope published, by its
// position among them.
struct PublishedChoiceId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const PublishedChoiceId&) const
      -> std::strong_ordering = default;
};

// What a `disable` of a named block or task terminates (LRM 9.6.2): which
// block or task of the unit it is.
struct PublishedDisableTarget {
  support::DefPath path;

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
// which class of the unit it is, the members another unit may name on it, the
// subroutines another unit may call on it, the generate constructs a name steps
// through into their blocks -- its loops, and the constructs that build at most
// one block -- what a `disable` of one of its named blocks or tasks ends, and,
// for an instance, the views it offers over those members. A unit's name and
// the name of the class it builds are two facts, so a referrer reads the class
// it reaches here rather than deriving it from the unit it reached through.
// Every type here is one of the pool of whoever holds the record: the unit's
// signature, the unit's own scope, or a referrer's record of it.
//
// The class places the members first, then one member per loop, holding its
// blocks by the value each one's index stood at, then one per construct
// building at most one block, holding the block it built, then one per disable
// target, each list in the order it is given here: the order is as much a part
// of what is published as the names are, because a field's position is what
// fixes where its storage sits, and the declaring unit and every referrer lay
// the class out from these lists alone.
//
// `class_path` is where the class is declared: a generate block is a
// declaration nested in the scope holding it (LRM 27.3), so its class is one
// block step further in than that scope's, and the class of the unit's instance
// is the path of no steps.
struct ScopeClassSignature {
  support::DefPath class_path;
  base::Arena<PublishedMember, PublishedMemberId> members;
  base::Arena<PublishedCallable, PublishedCallableId> callables;
  base::Arena<PublishedLoop, PublishedLoopId> loops;
  base::Arena<PublishedChoice, PublishedChoiceId> choices;
  base::Arena<PublishedDisableTarget, PublishedDisableTargetId> disable_targets;
  std::vector<PublishedModport> modports;

  // The member the scope published for the declaration `name` of the scope
  // `holder` of the unit, or nothing when it published none.
  [[nodiscard]] auto FindMember(
      std::string_view name, const support::DefPath& holder) const
      -> std::optional<PublishedMemberId> {
    for (const PublishedMemberId id : members.Ids()) {
      const PublishedMember& member = members.Get(id);
      if (member.name == name && member.holder == holder) return id;
    }
    return std::nullopt;
  }

  // Which of the scope's disable targets the block or task `path` of the unit
  // is, or nothing where the scope published none for it.
  [[nodiscard]] auto FindDisableTarget(const support::DefPath& path) const
      -> std::optional<PublishedDisableTargetId> {
    for (const PublishedDisableTargetId id : disable_targets.Ids()) {
      if (disable_targets.Get(id).path == path) return id;
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
