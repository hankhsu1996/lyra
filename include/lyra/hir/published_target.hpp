#pragma once

#include <cstdint>
#include <optional>
#include <variant>
#include <vector>

#include "lyra/hir/published_member.hpp"
#include "lyra/hir/type_id.hpp"

namespace lyra::hir {

// One endpoint pair of a window into a published member, in the form the
// declaration wrote it (LRM 11.5.1). Which endpoint is the physically low one
// follows from the orientation the member's own type carries, so the written
// form is what crosses and the coordinate resolves against that orientation
// where the member is reached.
struct PublishedConstantRange {
  std::int32_t left{};
  std::int32_t right{};

  auto operator==(const PublishedConstantRange&) const -> bool = default;
};

struct PublishedIndexedUpRange {
  std::int32_t base{};
  std::int32_t width{};

  auto operator==(const PublishedIndexedUpRange&) const -> bool = default;
};

struct PublishedIndexedDownRange {
  std::int32_t base{};
  std::int32_t width{};

  auto operator==(const PublishedIndexedDownRange&) const -> bool = default;
};

using PublishedRange = std::variant<
    PublishedConstantRange, PublishedIndexedUpRange, PublishedIndexedDownRange>;

// One coordinate into a homogeneous value: an array element or a bit of a
// packed value (LRM 7.4.5, 11.5.1).
struct PublishedElementSelector {
  std::int32_t index{};
  TypeId projected_type;

  auto operator==(const PublishedElementSelector&) const -> bool = default;
};

// One fixed-width window into a value (LRM 7.4.6, 11.5.1).
struct PublishedSliceSelector {
  PublishedRange range;
  TypeId projected_type;

  auto operator==(const PublishedSliceSelector&) const -> bool = default;
};

// One descent step into the value a published member holds, with the type the
// step lands on. Every coordinate is the source-level one, which is what lets
// the step cross a boundary at all: a storage position would only mean anything
// under the declaring unit's own range, and the range travels on the types
// here.
using PublishedSelector =
    std::variant<PublishedElementSelector, PublishedSliceSelector>;

// Which of a member's own positions the part a port stands for covers, counted
// from the low end. A connection states that runs of positions resolve together
// (LRM 10.11, 23.3.3.7), and only the declaring unit's source can say which run
// its port stands for -- the descent below states it in the coordinates that
// unit declared, and no other unit can turn those into positions, so what
// crosses is the answer rather than the question. A member with one indivisible
// position is covered whole by a run of one.
struct PublishedRun {
  std::uint32_t position{};
  std::uint32_t width{};

  auto operator==(const PublishedRun&) const -> bool = default;
};

// A published member and the descent that reaches the part of it a port stands
// for, in owner-to-leaf order. A port naming the whole declaration has an empty
// path and needs no case of its own, since descending no steps reaches the
// member.
//
// `run` is absent where the part is not a run of the member's own positions at
// all, which is what naming an element of an unpacked declaration is: such a
// part is reachable and readable, and is not something another name's positions
// can be laid over.
struct MemberProjection {
  PublishedMemberId member;
  std::vector<PublishedSelector> path;
  std::optional<PublishedRun> run;

  auto operator==(const MemberProjection&) const -> bool = default;
};

// A point that reaches no storage. LRM 23.2.2.1 admits a port defined so that
// it connects to nothing internal to the unit: data crossing such a point lands
// nowhere, and nothing can be read back through it.
struct NoInternalTarget {
  auto operator==(const NoInternalTarget&) const -> bool = default;
};

// What a port a unit publishes reaches inside that unit. Only the declaring
// unit's own source says which part of a declaration a port stands for (LRM
// 23.2.2.2), so this is carried rather than derived on both sides the way a
// member's position is.
using ConnectionTarget = std::variant<MemberProjection, NoInternalTarget>;

}  // namespace lyra::hir
