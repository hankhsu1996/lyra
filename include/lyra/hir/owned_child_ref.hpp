#pragma once

#include <compare>
#include <cstdint>
#include <variant>
#include <vector>

#include "lyra/base/pool_id.hpp"

namespace lyra::hir {

struct GenerateId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const GenerateId&) const -> std::strong_ordering = default;
};

struct StructuralScopeId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const StructuralScopeId&) const
      -> std::strong_ordering = default;
};

struct InstanceMemberId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const InstanceMemberId&) const
      -> std::strong_ordering = default;
};

// A loop generate (LRM 27.4), which holds one block per iteration it counted
// out; a path picks one of them with an instance select.
//
// A path names the construct and selects a block of it, never the scope the
// block compiled to: a loop whose blocks agree compiles to one scope and one
// whose blocks disagree to one each, and both reach the same elaborated block.
// Which scope that is stays the construct's own answer, so no name has to be
// spelled before it exists.
struct GenerateLoopRef {
  GenerateId generate;

  auto operator==(const GenerateLoopRef&) const -> bool = default;
};

// The one block a generate construct holds when it builds at most one: a
// block standing on its own, or the one a conditional chose (LRM 27.5),
// identified by the position the source wrote it at among the construct's
// alternatives. That is the same number wherever the construct stands, which
// is what lets a name be resolved before anything knows which alternative a
// particular instance selected; a block standing on its own is the only
// alternative of its construct.
struct GenerateBlockRef {
  GenerateId generate;
  std::uint32_t alternative = 0;

  auto operator==(const GenerateBlockRef&) const -> bool = default;
};

// A child object the referrer's compilation unit declares, named by the
// declaring scope's own identity for it.
using OwnedChildRef =
    std::variant<InstanceMemberId, GenerateLoopRef, GenerateBlockRef>;

// One element of a hierarchical path (LRM 23.6): what it names, and the
// instance selects written after it. Each select picks one instance out of
// what the name stands for -- an instance array (LRM 23.3.2), an interface
// port carrying a range (LRM 25.3), the blocks of a loop generate -- and is
// the position of that instance, the value the source wrote being spent where
// the name resolves, as a declared range is. Something that stands for one
// object takes none, which is the empty case rather than a shape of its own.
template <typename Names>
struct PathElement {
  Names names;
  std::vector<std::uint32_t> selects;

  auto operator==(const PathElement&) const -> bool = default;
};

// An element naming a child of this unit. Every reach into one names it this
// way, whether it is a step of a reference or a hop of the descent a call
// takes to the scope declaring its callee.
using OwnedChildStep = PathElement<OwnedChildRef>;

}  // namespace lyra::hir
