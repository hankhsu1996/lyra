#include "lyra/mir/place_type.hpp"

#include <format>
#include <variant>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"

namespace lyra::mir {

namespace {

auto TypeAfterProjection(
    const TypeArena& types, TypeId base_type, const Projection& proj)
    -> TypeId {
  const Type& type = types[base_type];

  switch (proj.kind) {
    case Projection::Kind::kIndex: {
      if (type.Kind() == TypeKind::kUnpackedArray) {
        return type.AsUnpackedArray().element_type;
      }
      if (type.Kind() == TypeKind::kDynamicArray) {
        return type.AsDynamicArray().element_type;
      }
      throw common::InternalError(
          "TypeAfterProjection",
          std::format(
              "kIndex projection on non-array type: {}", ToString(type)));
    }
    case Projection::Kind::kField: {
      if (type.Kind() != TypeKind::kUnpackedStruct) {
        throw common::InternalError(
            "TypeAfterProjection",
            std::format(
                "kField projection on non-struct type: {}", ToString(type)));
      }
      const auto* field_idx = std::get_if<int>(&proj.operand);
      if (field_idx == nullptr) {
        throw common::InternalError(
            "TypeAfterProjection", "kField projection requires constant index");
      }
      const auto& struct_info = type.AsUnpackedStruct();
      if (*field_idx < 0 ||
          static_cast<size_t>(*field_idx) >= struct_info.fields.size()) {
        throw common::InternalError(
            "TypeAfterProjection", "field index out of range");
      }
      return struct_info.fields[static_cast<size_t>(*field_idx)].type;
    }
    case Projection::Kind::kSlice:
    case Projection::Kind::kDeref:
      throw common::InternalError(
          "TypeAfterProjection", "projection kind not yet supported");
  }
  throw common::InternalError("TypeAfterProjection", "unknown projection kind");
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
