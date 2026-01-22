#include "lyra/mir/place_type.hpp"

#include <format>
#include <variant>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/type.hpp"

namespace lyra::mir {

namespace {

auto TypeAfterProjection(
    const TypeArena& types, TypeId base_type, const Projection& proj)
    -> TypeId {
  const Type& type = types[base_type];

  return std::visit(
      Overloaded{
          [&](const IndexProjection& /*p*/) -> TypeId {
            if (type.Kind() == TypeKind::kUnpackedArray) {
              return type.AsUnpackedArray().element_type;
            }
            if (type.Kind() == TypeKind::kDynamicArray) {
              return type.AsDynamicArray().element_type;
            }
            if (type.Kind() == TypeKind::kQueue) {
              return type.AsQueue().element_type;
            }
            throw common::InternalError(
                "TypeAfterProjection",
                std::format(
                    "kIndex projection on non-array type: {}", ToString(type)));
          },
          [&](const FieldProjection& p) -> TypeId {
            if (type.Kind() != TypeKind::kUnpackedStruct) {
              throw common::InternalError(
                  "TypeAfterProjection",
                  std::format(
                      "kField projection on non-struct type: {}",
                      ToString(type)));
            }
            const auto& struct_info = type.AsUnpackedStruct();
            if (p.field_index < 0 || static_cast<size_t>(p.field_index) >=
                                         struct_info.fields.size()) {
              throw common::InternalError(
                  "TypeAfterProjection", "field index out of range");
            }
            return struct_info.fields[static_cast<size_t>(p.field_index)].type;
          },
          [&](const BitRangeProjection& p) -> TypeId {
            // BitRange yields the element type stored in the projection
            return p.element_type;
          },
          [&](const UnionMemberProjection& p) -> TypeId {
            if (type.Kind() != TypeKind::kUnpackedUnion) {
              throw common::InternalError(
                  "TypeAfterProjection",
                  std::format(
                      "kUnionMember projection on non-union type: {}",
                      ToString(type)));
            }
            const auto& union_info = type.AsUnpackedUnion();
            if (p.member_index >= union_info.members.size()) {
              throw common::InternalError(
                  "TypeAfterProjection", "union member index out of range");
            }
            return union_info.members[p.member_index].type;
          },
          [](const SliceProjection& /*p*/) -> TypeId {
            throw common::InternalError(
                "TypeAfterProjection", "slice projection not yet supported");
          },
          [](const DerefProjection& /*p*/) -> TypeId {
            throw common::InternalError(
                "TypeAfterProjection", "deref projection not yet supported");
          },
      },
      proj.info);
}

}  // namespace

auto TypeOfPlace(const TypeArena& types, const Place& place) -> TypeId {
  TypeId t = place.root.type;
  for (const auto& proj : place.projections) {
    t = TypeAfterProjection(types, t, proj);
  }
  return t;
}

}  // namespace lyra::mir
