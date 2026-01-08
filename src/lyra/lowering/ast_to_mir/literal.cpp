#include "lyra/lowering/ast_to_mir/literal.hpp"

#include <cstddef>
#include <cstdint>
#include <expected>
#include <utility>
#include <vector>

#include <slang/ast/expressions/LiteralExpressions.h>
#include <slang/ast/types/AllTypes.h>
#include <slang/numeric/SVInt.h>

#include "lyra/common/bit_utils.hpp"
#include "lyra/common/diagnostic.hpp"
#include "lyra/common/literal.hpp"
#include "lyra/common/wide_bit.hpp"
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
    auto raw = sv_int.getRawPtr()[0];

    if (is_signed) {
      int64_t extended = lyra::common::SignExtend(raw, width);
      return common::Literal::IntegralSigned(extended, width);
    }
    uint64_t masked = raw & lyra::common::MakeBitMask(width);
    return common::Literal::IntegralUnsigned(masked, width);
  }

  // Wide literal (>64 bits): use WideBit
  size_t num_words = sv_int.getNumWords();
  std::vector<uint64_t> words(
      sv_int.getRawPtr(), sv_int.getRawPtr() + num_words);
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

auto ExtractMaskAndValue(
    const slang::SVInt& sv_int, mir::CaseCondition condition)
    -> std::pair<int64_t, int64_t> {
  auto width = sv_int.getBitWidth();

  // Build mask by checking each bit for wildcard status
  uint64_t mask = 0;
  uint64_t value = 0;

  for (uint32_t i = 0; i < width; ++i) {
    auto bit = sv_int[i];
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
