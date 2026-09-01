#pragma once

// Projecting a packed aggregate onto the single vector it is (LRM 7.2.1 /
// 7.3.1 / 7.3.2). HIR carries only what the source declared -- the members,
// their types, their order, and whether a union is tagged -- and the LRM fixes
// where that places each one, so the placement is derived here rather than
// cached beside the declaration, where the two could disagree about the same
// member. These are positions the language defines and a program can address
// (`s[15:8]` names a field), not a storage choice.

#include <cstdint>
#include <vector>

#include "lyra/hir/type.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"

namespace lyra::lowering::hir_to_mir {

// Where one member lands in the vector. A struct's members occupy disjoint
// runs, the first declared at the most significant bits (LRM 7.2.1); a union's
// members overlap at the least significant ones (LRM 7.3.1).
//
// `bit_offset` counts up from the least significant bit, which is also the
// projected vector's own declared coordinate, because a packed aggregate
// projects onto `[width-1:0]`. That is what lets a member access be an
// ordinary part-select at this offset, still resolved by the value against its
// own declared range rather than handed an already-resolved position. A
// projection declared the other way round would break exactly that, so the two
// have to stay agreed.
struct ProjectedMember {
  hir::TypeId type = {};
  std::uint64_t bit_offset = 0;
  std::uint64_t bit_width = 0;
};

// `tag_bits` is the width of the leading run a tagged union carries to name
// the member it currently holds (LRM 7.3.2). It is zero whenever nothing
// distinguishes the members -- a struct, an untagged union, or a tagged union
// declaring a single member -- so a member access is guarded exactly when the
// tag is wide enough to name more than one thing.
struct PackedProjection {
  std::vector<ProjectedMember> members;
  std::uint64_t bit_width = 0;
  std::uint32_t tag_bits = 0;
  mir::IntegralStateKind state_kind = mir::IntegralStateKind::kTwoState;
};

[[nodiscard]] auto ProjectPackedAggregate(
    const UnitLowerer& unit_lowerer, const hir::Type& aggregate)
    -> PackedProjection;

}  // namespace lyra::lowering::hir_to_mir
