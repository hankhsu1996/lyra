#include "lyra/lowering/hir_to_mir/packed_projection.hpp"

#include <algorithm>
#include <bit>
#include <cstddef>
#include <cstdint>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

struct MemberFacts {
  std::uint64_t bit_width = 0;
  bool four_state = false;
};

// What placing one member needs to know, read from its translated type. A
// `void` member, which only a tagged union may declare (LRM 7.3.2),
// contributes nothing: the tag alone says the value is that member.
auto MemberFactsOf(const UnitLowerer& unit_lowerer, hir::TypeId member)
    -> MemberFacts {
  const mir::Type& translated =
      unit_lowerer.Unit().types.Get(unit_lowerer.TranslateType(member));
  if (translated.Kind() == mir::TypeKind::kVoid) return MemberFacts{};
  if (!translated.IsIntegralPacked()) {
    throw InternalError(
        "ProjectPackedAggregate: a packed aggregate member is not integral");
  }
  const mir::PackedArrayType& packed = translated.AsIntegralPacked();
  return MemberFacts{
      .bit_width = packed.BitWidth(), .four_state = packed.IsFourState()};
}

// LRM 7.3.2: the tag names one of the members, so it is as wide as it takes to
// code every member name. One member needs no tag at all, which is why an
// untagged union and a single-member tagged union share a zero-width tag.
auto TagBitsFor(bool tagged, std::size_t member_count) -> std::uint32_t {
  if (!tagged || member_count == 0) return 0;
  return static_cast<std::uint32_t>(std::bit_width(member_count - 1));
}

}  // namespace

auto ProjectPackedAggregate(
    const UnitLowerer& unit_lowerer, const hir::TypeData& aggregate)
    -> PackedProjection {
  const auto* packed_struct = std::get_if<hir::PackedStructType>(&aggregate);
  const auto* packed_union = std::get_if<hir::PackedUnionType>(&aggregate);
  if (packed_struct == nullptr && packed_union == nullptr) {
    throw InternalError(
        "ProjectPackedAggregate: type is not a packed struct or union");
  }
  const std::vector<hir::PackedAggregateField>& fields =
      packed_struct != nullptr ? packed_struct->fields : packed_union->fields;

  PackedProjection projection;
  projection.members.reserve(fields.size());
  for (const auto& field : fields) {
    const MemberFacts facts = MemberFactsOf(unit_lowerer, field.type);
    projection.members.push_back(
        ProjectedMember{
            .type = field.type, .bit_offset = 0, .bit_width = facts.bit_width});
    // LRM 7.2.1 / 7.3.1: an aggregate is 4-state as a whole as soon as one
    // member is, and the mixed-state member conversion happens against that
    // whole-aggregate domain.
    projection.four_state = projection.four_state || facts.four_state;
  }

  if (packed_struct != nullptr) {
    // LRM 7.2.1: "The first member specified is the most significant", so a
    // member starts above everything declared after it.
    for (const auto& member : projection.members) {
      projection.bit_width += member.bit_width;
    }
    std::uint64_t below = projection.bit_width;
    for (auto& member : projection.members) {
      below -= member.bit_width;
      member.bit_offset = below;
    }
    return projection;
  }

  // LRM 7.3.1: members overlap at the least significant bits, so the vector
  // holds the widest of them -- preceded by the tag when there is one.
  projection.tag_bits =
      TagBitsFor(packed_union->tagged, projection.members.size());
  std::uint64_t widest = 0;
  for (const auto& member : projection.members) {
    widest = std::max(widest, member.bit_width);
  }
  projection.bit_width = widest + projection.tag_bits;
  return projection;
}

}  // namespace lyra::lowering::hir_to_mir
