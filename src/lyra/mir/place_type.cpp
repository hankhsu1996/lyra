#include "lyra/mir/place_type.hpp"

#include <format>

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
      if (type.Kind() != TypeKind::kUnpackedArray) {
        throw common::InternalError(
            "TypeAfterProjection",
            std::format(
                "kIndex projection on non-array type: {}", ToString(type)));
      }
      return type.AsUnpackedArray().element_type;
    }
    case Projection::Kind::kField:
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
