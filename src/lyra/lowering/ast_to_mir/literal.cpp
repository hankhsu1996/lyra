#include "lyra/lowering/ast_to_mir/literal.hpp"

#include <cstddef>
#include <cstdint>
#include <expected>
#include <memory>
#include <span>
#include <string>
#include <utility>
#include <vector>

#include <slang/ast/expressions/LiteralExpressions.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/AllTypes.h>
#include <slang/numeric/ConstantValue.h>
#include <slang/numeric/SVInt.h>
#include <slang/text/SourceLocation.h>

#include "lyra/common/bit_utils.hpp"
#include "lyra/common/constant.hpp"
#include "lyra/common/diagnostic.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/common/wide_bit.hpp"
#include "lyra/lowering/ast_to_mir/type.hpp"
#include "lyra/mir/expression.hpp"
#include "lyra/mir/statement.hpp"

namespace lyra::lowering::ast_to_mir {

namespace {

// Helper to convert SVInt to Constant (shared by IntegerLiteral and
// UnbasedUnsizedIntegerLiteral)
auto SVIntToConstant(const slang::SVInt& sv_int) -> common::Constant {
  auto width = sv_int.getBitWidth();
  auto is_signed = sv_int.isSigned();

  if (sv_int.isSingleWord()) {
    // Standard case: fits in 64 bits
    std::span<const uint64_t> raw(sv_int.getRawPtr(), 1);
    auto raw_word = raw.front();

    if (is_signed) {
      int64_t extended = lyra::common::SignExtend(raw_word, width);
      return common::Constant::IntegralSigned(extended, width);
    }
    uint64_t masked = raw_word & lyra::common::MakeBitMask(width);
    return common::Constant::IntegralUnsigned(masked, width);
  }

  // Wide literal (>64 bits): use WideBit
  size_t num_words = sv_int.getNumWords();
  std::span<const uint64_t> raw(sv_int.getRawPtr(), num_words);
  std::vector<uint64_t> words(raw.begin(), raw.end());
  common::WideBit wide_value(std::move(words));
  wide_value.MaskToWidth(width);

  return common::Constant::IntegralWide(
      std::move(wide_value), width, is_signed);
}

}  // namespace

auto LowerLiteral(const slang::ast::IntegerLiteral& literal)
    -> Result<common::Constant> {
  const auto& sv_int = literal.getValue();

  // Reject four-state values with unknown bits
  if (sv_int.hasUnknown()) {
    return std::unexpected(
        Diagnostic::Error(
            literal.sourceRange, "unsupported literal with unknown bits"));
  }

  return SVIntToConstant(sv_int);
}

auto LowerLiteral(const slang::ast::StringLiteral& literal)
    -> common::Constant {
  // In bit concatenation contexts, string literals have integral type (bit[N]).
  // Use the integer interpretation for those cases.
  if (literal.type->isIntegral()) {
    const auto& int_val = literal.getIntValue();
    if (int_val.isInteger()) {
      auto result = SVIntToConstant(int_val.integer());
      result.is_string_literal = true;
      return result;
    }
  }

  // String type context - return as string literal (String() sets
  // is_string_literal = true)
  auto value = std::string(literal.getValue());
  return common::Constant::String(std::move(value));
}

auto LowerLiteral(const slang::ast::RealLiteral& literal) -> common::Constant {
  if (literal.type->isFloating()) {
    const auto& floating = literal.type->as<slang::ast::FloatingType>();
    if (floating.floatKind == slang::ast::FloatingType::ShortReal) {
      return common::Constant::ShortReal(
          static_cast<float>(literal.getValue()));
    }
  }
  return common::Constant::Real(literal.getValue());
}

auto LowerLiteral(const slang::ast::UnbasedUnsizedIntegerLiteral& literal)
    -> Result<common::Constant> {
  // Check for X/Z values which are not supported (four-state)
  auto bit_value = literal.getLiteralValue();
  if (bit_value.value == slang::logic_t::X_VALUE ||
      bit_value.value == slang::logic_t::Z_VALUE) {
    return std::unexpected(
        Diagnostic::Error(
            literal.sourceRange,
            "unsupported unbased unsized literal with X or Z value"));
  }

  // getValue() returns fully-expanded SVInt sized to context type
  return SVIntToConstant(literal.getValue());
}

auto ConstantValueToConstant(const slang::ConstantValue& cv)
    -> Result<common::Constant> {
  if (cv.isInteger()) {
    const auto& sv_int = cv.integer();
    if (sv_int.hasUnknown()) {
      return std::unexpected(
          Diagnostic::Error({}, "unsupported constant with unknown bits"));
    }
    return SVIntToConstant(sv_int);
  }

  if (cv.isReal()) {
    return common::Constant::Real(cv.real());
  }

  if (cv.isShortReal()) {
    return common::Constant::ShortReal(cv.shortReal());
  }

  if (cv.isString()) {
    return common::Constant::String(std::string(cv.str()));
  }

  return std::unexpected(
      Diagnostic::Error({}, "unsupported constant value type"));
}

auto ConstantValueToExpression(
    const slang::ConstantValue& cv, const slang::ast::Type& type,
    slang::SourceRange source_range, common::TypeArena& arena)
    -> Result<std::unique_ptr<mir::Expression>> {
  // Scalar types (including packed structs, which slang folds to integers):
  // delegate to ConstantValueToConstant
  if (cv.isInteger() || cv.isReal() || cv.isShortReal() || cv.isString()) {
    auto constant_result = ConstantValueToConstant(cv);
    if (!constant_result) {
      return std::unexpected(std::move(constant_result.error()));
    }
    return std::make_unique<mir::ConstantExpression>(
        std::move(*constant_result));
  }

  // Unpacked aggregate (struct or array)
  if (cv.isUnpacked()) {
    auto elements = cv.elements();
    const auto& canonical = type.getCanonicalType();

    // Unpacked struct
    if (canonical.kind == slang::ast::SymbolKind::UnpackedStructType) {
      const auto& struct_type = canonical.as<slang::ast::UnpackedStructType>();
      auto fields = struct_type.fields;
      std::vector<std::unique_ptr<mir::Expression>> field_exprs;
      field_exprs.reserve(elements.size());

      for (size_t i = 0; i < elements.size(); ++i) {
        auto field_result = ConstantValueToExpression(
            elements[i], fields[i]->getType(), source_range, arena);
        if (!field_result) {
          return std::unexpected(std::move(field_result.error()));
        }
        field_exprs.push_back(std::move(*field_result));
      }

      auto type_result = LowerType(type, source_range, arena);
      if (!type_result) {
        return std::unexpected(std::move(type_result.error()));
      }

      return std::make_unique<mir::UnpackedStructLiteralExpression>(
          *type_result, std::move(field_exprs));
    }

    // Unpacked array
    if (canonical.kind == slang::ast::SymbolKind::FixedSizeUnpackedArrayType) {
      const auto& array_type =
          canonical.as<slang::ast::FixedSizeUnpackedArrayType>();
      std::vector<std::unique_ptr<mir::Expression>> elem_exprs;
      elem_exprs.reserve(elements.size());

      for (const auto& elem : elements) {
        auto elem_result = ConstantValueToExpression(
            elem, array_type.elementType, source_range, arena);
        if (!elem_result) {
          return std::unexpected(std::move(elem_result.error()));
        }
        elem_exprs.push_back(std::move(*elem_result));
      }

      auto type_result = LowerType(type, source_range, arena);
      if (!type_result) {
        return std::unexpected(std::move(type_result.error()));
      }

      return std::make_unique<mir::ArrayLiteralExpression>(
          *type_result, std::move(elem_exprs));
    }

    return std::unexpected(
        Diagnostic::Error(
            source_range, "unsupported unpacked aggregate constant"));
  }

  return std::unexpected(
      Diagnostic::Error(source_range, "unsupported constant value type"));
}

auto ExtractMaskAndValue(
    const slang::SVInt& sv_int, mir::CaseCondition condition)
    -> std::pair<int64_t, int64_t> {
  auto width = sv_int.getBitWidth();

  // Build mask by checking each bit for wildcard status
  uint64_t mask = 0;
  uint64_t value = 0;

  auto width_u32 = static_cast<uint32_t>(width);
  for (uint32_t i = 0; i < width_u32; ++i) {
    auto bit = sv_int[static_cast<int32_t>(i)];
    bool is_wildcard = false;

    if (condition == mir::CaseCondition::kWildcardXZ) {
      // casex: both X and Z are wildcards
      is_wildcard =
          (bit.value == slang::logic_t::X_VALUE ||
           bit.value == slang::logic_t::Z_VALUE);
    } else if (condition == mir::CaseCondition::kWildcardZ) {
      // casez: only Z is wildcard
      is_wildcard = (bit.value == slang::logic_t::Z_VALUE);
    }

    if (!is_wildcard) {
      mask |= (1ULL << i);
      // For 0 or 1 bits, use the actual value; for X in casez, treat as 0
      if (bit.value == 1) {
        value |= (1ULL << i);
      }
    }
  }

  // Truncate to width
  uint64_t width_mask = lyra::common::MakeBitMask(width);
  mask &= width_mask;
  value &= width_mask;

  return {static_cast<int64_t>(value), static_cast<int64_t>(mask)};
}

}  // namespace lyra::lowering::ast_to_mir
