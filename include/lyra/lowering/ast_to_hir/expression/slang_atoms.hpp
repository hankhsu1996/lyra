#pragma once

#include <optional>
#include <string_view>

#include <slang/ast/Expression.h>
#include <slang/ast/SemanticFacts.h>
#include <slang/ast/expressions/Operator.h>
#include <slang/numeric/Time.h>
#include <slang/syntax/SyntaxNode.h>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/binary_op.hpp"
#include "lyra/hir/conversion.hpp"
#include "lyra/hir/inc_dec_op.hpp"
#include "lyra/hir/method.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/hir/unary_op.hpp"
#include "lyra/support/system_subroutine.hpp"

// Stateless slang -> HIR translators. Each function is a pure 1:1 mapping
// from a slang AST atom to its HIR counterpart, with no recursion and no
// lowering state. They live here (separate from the recursive lowering
// machinery in `expression/lower.cpp`) so the "encapsulate slang's quirks"
// concern stays distinct from the "walk the AST and produce HIR" concern.
namespace lyra::lowering::ast_to_hir {

auto LowerSlangLiteralBase(const slang::syntax::SyntaxNode* syntax)
    -> hir::IntegerLiteralBase;

auto LowerConversionKind(slang::ast::ConversionKind k) -> hir::ConversionKind;

auto LowerBinaryOp(slang::ast::BinaryOperator op) -> hir::BinaryOp;

auto LowerUnaryOp(slang::ast::UnaryOperator op, diag::SourceSpan span)
    -> diag::Result<hir::UnaryOp>;

// LRM 11.4.2: maps slang's inc/dec UnaryOperator values to hir::IncDecOp.
// Throws InternalError if `op` is not one of the four inc/dec variants
// (callers must dispatch on `slang::ast::OpInfo::isLValue(op)` first).
auto LowerSlangIncDecOp(slang::ast::UnaryOperator op) -> hir::IncDecOp;

auto LowerTimeUnit(slang::TimeUnit u) -> hir::TimeScale;

auto FromSlangSubroutineKind(slang::ast::SubroutineKind k)
    -> support::SystemSubroutineKind;

auto LowerEnumMethodName(std::string_view name)
    -> std::optional<hir::EnumMethodKind>;

auto LowerStringMethodName(std::string_view name)
    -> std::optional<hir::StringMethodKind>;

// Recover the original user-written rhs from slang's compound expansion:
// slang lowers `lhs op= e` to `right = Conv(lhs.type) { BinaryOp(op) {
// Conv(common, LValueRef), Conv(common, e) } }`. This helper peels the
// outer Conversion (if any), finds which BinaryOp operand wraps the
// LValueRef placeholder, and unwraps the other operand's promotion
// Conversion (if any) to expose the original `e`. Slang's invariant is at
// most one Conversion at each wrap site; an InternalError surfaces if
// that invariant is ever violated.
auto BareCompoundUserRhs(const slang::ast::Expression& slang_expanded_rhs)
    -> const slang::ast::Expression&;

}  // namespace lyra::lowering::ast_to_hir
