#include "lyra/lowering/ast_to_mir/type.hpp"

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

    if (width == 1) {
      return Type::TwoState(1, is_signed);
    }
    if (width == 32) {
      return Type::TwoState(32, is_signed);
    }
    if (width == 64) {
      return Type::TwoState(64, is_signed);
    }

    return std::unexpected(
        Diagnostic::Error(
            source_range,
            fmt::format("unsupported integral type width {}", width)));
  }

  return std::unexpected(
      Diagnostic::Error(
          source_range, fmt::format("unsupported type '{}'", type.toString())));
}

}  // namespace lyra::lowering::ast_to_mir
