#pragma once

#include <memory>
#include <utility>

#include "lyra/common/constant.hpp"
#include "lyra/common/diagnostic.hpp"
#include "lyra/mir/expression.hpp"
#include "lyra/mir/statement.hpp"

namespace slang {
class SVInt;
class ConstantValue;
class SourceRange;
}  // namespace slang

namespace slang::ast {
class IntegerLiteral;
class RealLiteral;
class StringLiteral;
class Type;
class UnbasedUnsizedIntegerLiteral;
}  // namespace slang::ast

namespace lyra::common {
class TypeArena;
}

namespace lyra::lowering::ast_to_mir {

// Lowers a slang AST IntegerLiteral into a MIR Constant.
auto LowerLiteral(const slang::ast::IntegerLiteral& literal)
    -> Result<common::Constant>;

// Lowers a slang AST StringLiteral into a MIR Constant.
auto LowerLiteral(const slang::ast::StringLiteral& literal) -> common::Constant;

// Lowers a slang AST RealLiteral into a MIR Constant.
auto LowerLiteral(const slang::ast::RealLiteral& literal) -> common::Constant;

// Lowers a slang AST UnbasedUnsizedIntegerLiteral into a MIR Constant.
auto LowerLiteral(const slang::ast::UnbasedUnsizedIntegerLiteral& literal)
    -> Result<common::Constant>;

// Converts a slang ConstantValue to a MIR Constant.
// Used for elaboration-time constants like parameters.
// Only handles scalar types (integer, real, string).
auto ConstantValueToConstant(const slang::ConstantValue& cv)
    -> Result<common::Constant>;

// Converts a slang ConstantValue to a MIR Expression.
// Handles both scalar types (via ConstantExpression) and aggregate types
// (unpacked structs via UnpackedStructLiteralExpression).
// The type parameter is needed to determine the structure of aggregate values.
auto ConstantValueToExpression(
    const slang::ConstantValue& cv, const slang::ast::Type& type,
    slang::SourceRange source_range, common::TypeArena& arena)
    -> Result<std::unique_ptr<mir::Expression>>;

/// Extracts (value, mask) from an SVInt for casez/casex pattern matching.
/// For casex: both X and Z bits become wildcards (mask bit = 0)
/// For casez: only Z bits become wildcards (X bits must match exactly)
/// The returned value is already masked (value & mask).
auto ExtractMaskAndValue(
    const slang::SVInt& sv_int, mir::CaseCondition condition)
    -> std::pair<int64_t, int64_t>;

}  // namespace lyra::lowering::ast_to_mir
