#include "lyra/lowering/ast_to_mir/literal.hpp"

#include <cstddef>
#include <cstdint>
#include <expected>
#include <memory>
#include <span>
#include <utility>
#include <vector>

#include <slang/ast/expressions/LiteralExpressions.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/AllTypes.h>
#include <slang/numeric/ConstantValue.h>
#include <slang/numeric/SVInt.h>
#include <slang/text/SourceLocation.h>

#include "lyra/common/bit_utils.hpp"
#include "lyra/common/diagnostic.hpp"
#include "lyra/common/literal.hpp"
#include "lyra/common/wide_bit.hpp"
#include "lyra/lowering/ast_to_mir/type.hpp"
#include "lyra/mir/expression.hpp"
#include "lyra/mir/statement.hpp"

namespace lyra::lowering::ast_to_mir {

namespace {

// Helper to convert SVInt to Literal (shared by IntegerLiteral and
// UnbasedUnsizedIntegerLiteral)
auto SVIntToLiteral(const slang::SVInt& sv_int) -> common::Literal {
  auto width = sv_int.getBitWidth();
  auto is_signed = sv_int.isSigned();

  if (sv_int.isSingleWord()) {
    // Standard case: fits in 64 bits
    std::span<const uint64_t> raw(sv_int.getRawPtr(), 1);
    auto raw_word = raw.front();

    if (is_signed) {
      int64_t extended = lyra::common::SignExtend(raw_word, width);
      return common::Literal::IntegralSigned(extended, width);
    }
    uint64_t masked = raw_word & lyra::common::MakeBitMask(width);
    return common::Literal::IntegralUnsigned(masked, width);
  }

  // Wide literal (>64 bits): use WideBit
  size_t num_words = sv_int.getNumWords();
  std::span<const uint64_t> raw(sv_int.getRawPtr(), num_words);
  std::vector<uint64_t> words(raw.begin(), raw.end());
  common::WideBit wide_value(std::move(words));
  wide_value.MaskToWidth(width);

  return common::Literal::IntegralWide(std::move(wide_value), width, is_signed);
}

}  // namespace

auto LowerLiteral(const slang::ast::IntegerLiteral& literal)
    -> Result<common::Literal> {
  const auto& sv_int = literal.getValue();

  // Reject four-state values with unknown bits
  if (sv_int.hasUnknown()) {
    return std::unexpected(
        Diagnostic::Error(
            literal.sourceRange, "unsupported literal with unknown bits"));
  }

  return SVIntToLiteral(sv_int);
}

auto LowerLiteral(const slang::ast::StringLiteral& literal) -> common::Literal {
  // In bit concatenation contexts, string literals have integral type (bit[N]).
  // Use the integer interpretation for those cases.
  if (literal.type->isIntegral()) {
    const auto& int_val = literal.getIntValue();
    if (int_val.isInteger()) {
      auto result = SVIntToLiteral(int_val.integer());
      result.is_string_literal = true;
      return result;
    }
  }

  // String type context - return as string literal (String() sets
  // is_string_literal = true)
  auto value = std::string(literal.getValue());
  return common::Literal::String(std::move(value));
}

auto LowerLiteral(const slang::ast::RealLiteral& literal) -> common::Literal {
  if (literal.type->isFloating()) {
    const auto& floating = literal.type->as<slang::ast::FloatingType>();
    if (floating.floatKind == slang::ast::FloatingType::ShortReal) {
      return common::Literal::ShortReal(static_cast<float>(literal.getValue()));
    }
  }
  return common::Literal::Real(literal.getValue());
}

auto LowerLiteral(const slang::ast::UnbasedUnsizedIntegerLiteral& literal)
    -> Result<common::Literal> {
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
  return SVIntToLiteral(literal.getValue());
}

auto ConstantValueToLiteral(const slang::ConstantValue& cv)
    -> Result<common::Literal> {
  if (cv.isInteger()) {
    const auto& sv_int = cv.integer();
    if (sv_int.hasUnknown()) {
      return std::unexpected(
          Diagnostic::Error({}, "unsupported constant with unknown bits"));
    }
    return SVIntToLiteral(sv_int);
  }

  if (cv.isReal()) {
    return common::Literal::Real(cv.real());
  }

  if (cv.isShortReal()) {
    return common::Literal::ShortReal(cv.shortReal());
  }

  if (cv.isString()) {
    return common::Literal::String(std::string(cv.str()));
  }

  return std::unexpected(
      Diagnostic::Error({}, "unsupported constant value type"));
}

auto ConstantValueToExpression(
    const slang::ConstantValue& cv, const slang::ast::Type& type,
    slang::SourceRange source_range)
    -> Result<std::unique_ptr<mir::Expression>> {
  // Scalar types (including packed structs, which slang folds to integers):
  // delegate to ConstantValueToLiteral
  if (cv.isInteger() || cv.isReal() || cv.isShortReal() || cv.isString()) {
    auto literal_result = ConstantValueToLiteral(cv);
    if (!literal_result) {
      return std::unexpected(std::move(literal_result.error()));
    }
    return std::make_unique<mir::LiteralExpression>(std::move(*literal_result));
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
            elements[i], fields[i]->getType(), source_range);
        if (!field_result) {
          return std::unexpected(std::move(field_result.error()));
        }
        field_exprs.push_back(std::move(*field_result));
      }

      auto type_result = LowerType(type, source_range);
      if (!type_result) {
        return std::unexpected(std::move(type_result.error()));
      }

      return std::make_unique<mir::UnpackedStructLiteralExpression>(
          *type_result, std::move(field_exprs));
    }

    // Unpacked array - not yet supported
    return std::unexpected(
        Diagnostic::Error(
            source_range, "unpacked array constants not yet supported"));
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
