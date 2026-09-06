#pragma once

#include <cstdint>
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

// A published member and the descent that reaches the part of it a port stands
// for, in owner-to-leaf order. A port naming the whole declaration has an empty
// path and needs no case of its own, since descending no steps reaches the
// member.
struct MemberProjection {
  PublishedMemberId member;
  std::vector<PublishedSelector> path;

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
