#include "lyra/semantic/signedness_cast.hpp"

#include <format>

namespace lyra::semantic {

namespace {

auto MakeSignednessVariant(TypeId type, TypeArena& arena, bool make_signed)
    -> std::expected<TypeId, std::string> {
  const Type& t = arena[type];

  if (!IsPacked(t)) {
    return std::unexpected(
        std::format(
            "${}() requires packed type, got {}",
            make_signed ? "signed" : "unsigned", ToString(t)));
  }

  switch (t.Kind()) {
    case TypeKind::kIntegral: {
      const auto& info = t.AsIntegral();
      if (info.is_signed == make_signed) {
        return type;  // Already correct signedness
      }
      return arena.Intern(
          TypeKind::kIntegral, IntegralInfo{
                                   .bit_width = info.bit_width,
                                   .is_signed = make_signed,
                                   .is_four_state = info.is_four_state});
    }

    case TypeKind::kPackedArray: {
      // For packed arrays, we need to flip the signedness of the element type
      const auto& info = t.AsPackedArray();
      auto new_elem =
          MakeSignednessVariant(info.element_type, arena, make_signed);
      if (!new_elem) {
        return new_elem;
      }
      if (*new_elem == info.element_type) {
        return type;  // Already correct signedness
      }
      return arena.Intern(
          TypeKind::kPackedArray,
          PackedArrayInfo{.element_type = *new_elem, .range = info.range});
    }

    case TypeKind::kPackedStruct: {
      const auto& info = t.AsPackedStruct();
      if (info.is_signed == make_signed) {
        return type;  // Already correct signedness
      }
      PackedStructInfo new_info = info;  // Copy
      new_info.is_signed = make_signed;
      return arena.Intern(TypeKind::kPackedStruct, std::move(new_info));
    }

    default:
      return std::unexpected(
          std::format(
              "${}() requires packed type, got {}",
              make_signed ? "signed" : "unsigned", ToString(t)));
  }
}

}  // namespace

auto MakeSignedVariant(TypeId type, TypeArena& arena)
    -> std::expected<TypeId, std::string> {
  return MakeSignednessVariant(type, arena, true);
}

auto MakeUnsignedVariant(TypeId type, TypeArena& arena)
    -> std::expected<TypeId, std::string> {
  return MakeSignednessVariant(type, arena, false);
}

auto GetBitVectorType(TypeArena& arena, uint32_t width) -> TypeId {
  return arena.Intern(
      TypeKind::kIntegral,
      IntegralInfo{
          .bit_width = width, .is_signed = false, .is_four_state = false});
}

}  // namespace lyra::semantic
