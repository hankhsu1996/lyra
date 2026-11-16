#include "lyra/lowering/ast_to_hir/typespec.hpp"

#include <cstdint>
#include <optional>

#include <slang/ast/types/AllTypes.h>
#include <slang/ast/types/Type.h>

#include "lyra/hir/typespec.hpp"
#include "lyra/support/data_object.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// What kind of type `declared` is (LRM 37.25), none for a type nothing
// describes.
auto TypespecKindOf(const slang::ast::Type& declared)
    -> std::optional<support::TypespecKind> {
  const slang::ast::Type& type = declared.getCanonicalType();
  if (type.isEvent()) {
    return support::TypespecKind::kEvent;
  }
  if (const auto* floating = type.as_if<slang::ast::FloatingType>()) {
    switch (floating->floatKind) {
      case slang::ast::FloatingType::Real:
        return support::TypespecKind::kReal;
      case slang::ast::FloatingType::ShortReal:
        return support::TypespecKind::kShortReal;
      case slang::ast::FloatingType::RealTime:
        return support::TypespecKind::kRealtime;
    }
    return std::nullopt;
  }
  if (type.as_if<slang::ast::EnumType>() != nullptr) {
    return support::TypespecKind::kEnum;
  }
  if (const auto* predefined =
          type.as_if<slang::ast::PredefinedIntegerType>()) {
    switch (predefined->integerKind) {
      case slang::ast::PredefinedIntegerType::Integer:
        return support::TypespecKind::kInteger;
      case slang::ast::PredefinedIntegerType::Int:
        return support::TypespecKind::kInt;
      case slang::ast::PredefinedIntegerType::Time:
        return support::TypespecKind::kTime;
      case slang::ast::PredefinedIntegerType::ShortInt:
        return support::TypespecKind::kShortInt;
      case slang::ast::PredefinedIntegerType::LongInt:
        return support::TypespecKind::kLongInt;
      case slang::ast::PredefinedIntegerType::Byte:
        return support::TypespecKind::kByte;
    }
    return std::nullopt;
  }
  if (const auto* scalar = type.as_if<slang::ast::ScalarType>()) {
    switch (scalar->scalarKind) {
      case slang::ast::ScalarType::Bit:
        return support::TypespecKind::kBit;
      case slang::ast::ScalarType::Logic:
      case slang::ast::ScalarType::Reg:
        return support::TypespecKind::kLogic;
    }
    return std::nullopt;
  }
  if (type.as_if<slang::ast::PackedStructType>() != nullptr) {
    return support::TypespecKind::kPackedStruct;
  }
  if (type.as_if<slang::ast::PackedUnionType>() != nullptr) {
    return support::TypespecKind::kPackedUnion;
  }
  if (const auto* packed = type.as_if<slang::ast::PackedArrayType>()) {
    const std::optional<support::TypespecKind> element =
        TypespecKindOf(packed->elementType);
    if (!element.has_value()) {
      return std::nullopt;
    }
    switch (*element) {
      case support::TypespecKind::kBit:
      case support::TypespecKind::kLogic:
        return element;
      case support::TypespecKind::kInt:
      case support::TypespecKind::kShortInt:
      case support::TypespecKind::kLongInt:
      case support::TypespecKind::kByte:
      case support::TypespecKind::kInteger:
      case support::TypespecKind::kTime:
      case support::TypespecKind::kReal:
      case support::TypespecKind::kShortReal:
      case support::TypespecKind::kRealtime:
      case support::TypespecKind::kEnum:
      case support::TypespecKind::kPackedStruct:
      case support::TypespecKind::kPackedUnion:
      case support::TypespecKind::kPackedArray:
      case support::TypespecKind::kEvent:
        return support::TypespecKind::kPackedArray;
    }
    return std::nullopt;
  }
  return std::nullopt;
}

// The outermost packed range `type` declares, none for a type that is no
// packed array.
auto PackedRangeOf(const slang::ast::Type& type)
    -> std::optional<support::Range> {
  const auto* packed = type.as_if<slang::ast::PackedArrayType>();
  if (packed == nullptr) {
    return std::nullopt;
  }
  return support::Range{
      .left = packed->range.left, .right = packed->range.right};
}

}  // namespace

auto TypespecOf(const slang::ast::Type& declared)
    -> std::optional<hir::Typespec> {
  const std::optional<support::TypespecKind> kind = TypespecKindOf(declared);
  if (!kind.has_value()) {
    return std::nullopt;
  }
  const slang::ast::Type& type = declared.getCanonicalType();
  hir::Typespec described{
      .kind = *kind,
      .width = static_cast<std::uint32_t>(type.getBitWidth()),
      .range = PackedRangeOf(type),
      .base = std::nullopt};
  if (const auto* enumeration = type.as_if<slang::ast::EnumType>()) {
    const std::optional<support::TypespecKind> base =
        TypespecKindOf(enumeration->baseType);
    if (!base.has_value()) {
      return std::nullopt;
    }
    described.base = hir::Typespec::Base{
        .kind = *base,
        .range = PackedRangeOf(enumeration->baseType.getCanonicalType())};
  }
  return described;
}

}  // namespace lyra::lowering::ast_to_hir
