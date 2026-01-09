#pragma once

#include <utility>

#include "lyra/common/diagnostic.hpp"
#include "lyra/common/literal.hpp"
#include "lyra/mir/statement.hpp"

namespace slang {
class SVInt;
class ConstantValue;
}  // namespace slang

namespace slang::ast {
class IntegerLiteral;
class RealLiteral;
class StringLiteral;
class UnbasedUnsizedIntegerLiteral;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_mir {

// Lowers a slang AST IntegerLiteral into a MIR Literal.
auto LowerLiteral(const slang::ast::IntegerLiteral& literal)
    -> Result<common::Literal>;

// Lowers a slang AST StringLiteral into a MIR Literal.
auto LowerLiteral(const slang::ast::StringLiteral& literal) -> common::Literal;

// Lowers a slang AST RealLiteral into a MIR Literal.
auto LowerLiteral(const slang::ast::RealLiteral& literal) -> common::Literal;

// Lowers a slang AST UnbasedUnsizedIntegerLiteral into a MIR Literal.
auto LowerLiteral(const slang::ast::UnbasedUnsizedIntegerLiteral& literal)
    -> Result<common::Literal>;

// Converts a slang ConstantValue to a MIR Literal.
// Used for elaboration-time constants like parameters.
auto ConstantValueToLiteral(const slang::ConstantValue& cv)
    -> Result<common::Literal>;

/// Extracts (value, mask) from an SVInt for casez/casex pattern matching.
/// For casex: both X and Z bits become wildcards (mask bit = 0)
/// For casez: only Z bits become wildcards (X bits must match exactly)
/// The returned value is already masked (value & mask).
auto ExtractMaskAndValue(
    const slang::SVInt& sv_int, mir::CaseCondition condition)
    -> std::pair<int64_t, int64_t>;

}  // namespace lyra::lowering::ast_to_mir
