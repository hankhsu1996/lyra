#include "lyra/lowering/ast_to_mir/literal.hpp"

#include <cstdint>
#include <expected>
#include <span>
#include <utility>

#include <slang/ast/expressions/LiteralExpressions.h>
#include <slang/ast/types/AllTypes.h>
#include <slang/numeric/SVInt.h>

#include "lyra/common/bit_utils.hpp"
#include "lyra/common/diagnostic.hpp"
#include "lyra/common/literal.hpp"
#include "lyra/mir/statement.hpp"

namespace lyra::lowering::ast_to_mir {

auto LowerLiteral(const slang::ast::IntegerLiteral& literal)
    -> Result<common::Literal> {
  const auto& sv_int = literal.getValue();

  if (!sv_int.isSingleWord() || sv_int.hasUnknown()) {
    return std::unexpected(
        Diagnostic::Error(
            literal.sourceRange, "unsupported wide or unknown literal"));
  }

  std::span<const uint64_t> data(sv_int.getRawPtr(), 1);
  auto raw = data[0];
  auto width = sv_int.getBitWidth();
  auto is_signed = sv_int.isSigned();

  if (is_signed) {
    int64_t extended = lyra::common::SignExtend(raw, width);
    return common::Literal::IntegralSigned(extended, width);
  }
  uint64_t masked = raw & lyra::common::MakeBitMask(width);
  return common::Literal::IntegralUnsigned(masked, width);
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
