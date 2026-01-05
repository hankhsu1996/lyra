#include "lyra/lowering/ast_to_mir/literal.hpp"

#include <cstdint>
#include <expected>
#include <span>
#include <utility>

#include <slang/ast/expressions/LiteralExpressions.h>

#include "lyra/common/bit_utils.hpp"
#include "lyra/common/diagnostic.hpp"
#include "lyra/common/literal.hpp"

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
    return common::Literal::TwoStateSigned(extended, width);
  }
  uint64_t masked = raw & lyra::common::MakeBitMask(width);
  return common::Literal::TwoStateUnsigned(masked, width);
}

auto LowerLiteral(const slang::ast::StringLiteral& literal) -> common::Literal {
  auto value = std::string(literal.getValue());
  return common::Literal::String(std::move(value));
}

auto LowerLiteral(const slang::ast::RealLiteral& literal) -> common::Literal {
  return common::Literal::Real(literal.getValue());
}

}  // namespace lyra::lowering::ast_to_mir
