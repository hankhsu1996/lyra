#include "lyra/lowering/ast_to_mir/type.hpp"

#include <cstdint>
#include <expected>
#include <string>
#include <vector>

#include <fmt/format.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/AllTypes.h>

#include "lyra/common/diagnostic.hpp"
#include "lyra/common/type.hpp"

namespace lyra::lowering::ast_to_mir {

using Type = common::Type;

auto LowerType(const slang::ast::Type& type, slang::SourceRange source_range)
    -> Result<Type> {
  // Handle type aliases (typedef): unwrap to canonical type but preserve name
  if (type.isAlias()) {
    const auto& canonical = type.getCanonicalType();
    auto result = LowerType(canonical, source_range);
    if (result) {
      const auto& alias = type.as<slang::ast::TypeAliasType>();
      result->alias_name = std::string(alias.name);
    }
    return result;
  }

  if (type.isString()) {
    return Type::String();
  }

  if (type.isVoid()) {
    return Type::Void();
  }

  if (type.isFloating()) {
    const auto& floating = type.as<slang::ast::FloatingType>();
    if (floating.floatKind == slang::ast::FloatingType::ShortReal) {
      return Type::ShortReal();
    }
    // Real and RealTime both map to real (64-bit)
    return Type::Real();
  }

  // Check for packed struct before isIntegral() (PackedStructType is an
  // IntegralType)
  if (type.kind == slang::ast::SymbolKind::PackedStructType) {
    const auto& struct_type = type.as<slang::ast::PackedStructType>();

    // Build field metadata
    std::vector<common::PackedStructField> fields;
    for (const auto& field :
         struct_type.membersOfType<slang::ast::FieldSymbol>()) {
      auto field_type_result = LowerType(field.getType(), source_range);
      if (!field_type_result) {
        return field_type_result;
      }

      fields.push_back(
          common::PackedStructField{
              .name = std::string(field.name),
              .bit_offset = field.bitOffset,
              .bit_width = field.getType().getBitWidth(),
              .field_type = std::make_shared<Type>(*field_type_result)});
    }

    return Type::PackedStruct(
        std::move(fields), struct_type.bitWidth, struct_type.isSigned,
        struct_type.isFourState);
  }

  // Check for multi-dimensional packed arrays before isIntegral()
  // e.g., bit [3:0][7:0] has isPackedArray()=true
  if (type.isPackedArray()) {
    const auto& packed = type.as<slang::ast::PackedArrayType>();
    auto element_result = LowerType(packed.elementType, source_range);
    if (!element_result) {
      return element_result;
    }

    bool is_signed = type.isSigned();
    bool is_four_state = type.isFourState();
    return Type::PackedArray(
        *element_result, packed.range.width(), packed.range.lower(), is_signed,
        is_four_state);
  }

  if (type.isIntegral()) {
    auto width = type.getBitWidth();
    bool is_signed = type.isSigned();
    bool is_four_state = type.isFourState();

    // Get range info for non-zero-based indexing (e.g., bit [63:32])
    auto range = type.getFixedRange();
    int32_t lower_bound = range.lower();

    return Type::Integral(width, is_signed, is_four_state, lower_bound);
  }

  if (type.isUnpackedArray()) {
    const auto& array_type = type.as<slang::ast::FixedSizeUnpackedArrayType>();
    auto element_result = LowerType(array_type.elementType, source_range);
    if (!element_result) {
      return element_result;
    }
    return Type::UnpackedArray(
        *element_result, array_type.range.width(), array_type.range.lower());
  }

  return std::unexpected(
      Diagnostic::Error(
          source_range, fmt::format("unsupported type '{}'", type.toString())));
}

}  // namespace lyra::lowering::ast_to_mir
