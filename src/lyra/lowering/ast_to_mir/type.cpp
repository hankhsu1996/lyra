#include "lyra/lowering/ast_to_mir/type.hpp"

#include <expected>

#include <fmt/format.h>
#include <slang/ast/types/AllTypes.h>

#include "lyra/common/diagnostic.hpp"
#include "lyra/common/type.hpp"

namespace lyra::lowering::ast_to_mir {

using Type = common::Type;

auto LowerType(const slang::ast::Type& type, slang::SourceRange source_range)
    -> Result<Type> {
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

  if (type.isIntegral()) {
    auto width = type.getBitWidth();
    bool is_signed = type.isSigned();

    if (width >= 1 && width <= 64) {
      return Type::TwoState(width, is_signed);
    }

    return std::unexpected(
        Diagnostic::Error(
            source_range,
            fmt::format(
                "unsupported integral type width {} (must be 1-64)", width)));
  }

  if (type.isUnpackedArray()) {
    const auto& array_type = type.as<slang::ast::FixedSizeUnpackedArrayType>();
    auto element_result = LowerType(array_type.elementType, source_range);
    if (!element_result) {
      return element_result;
    }
    return Type::Array(
        *element_result, array_type.range.width(), array_type.range.lower());
  }

  return std::unexpected(
      Diagnostic::Error(
          source_range, fmt::format("unsupported type '{}'", type.toString())));
}

}  // namespace lyra::lowering::ast_to_mir
