#pragma once

#include <optional>
#include <string_view>

#include <slang/ast/Expression.h>
#include <slang/ast/SemanticFacts.h>
#include <slang/ast/expressions/Operator.h>
#include <slang/numeric/Time.h>
#include <slang/parsing/KnownSystemName.h>

#include "lyra/hir/binary_op.hpp"
#include "lyra/hir/conversion.hpp"
#include "lyra/hir/inc_dec_op.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/hir/subroutine_kind.hpp"
#include "lyra/hir/unary_op.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/system_subroutine.hpp"

// Stateless slang -> HIR translators. Each function is a pure 1:1 mapping
// from a slang AST atom to its HIR counterpart, with no recursion and no
// lowering state. They live here (separate from the recursive lowering
// machinery in `expression/lower.cpp`) so the "encapsulate slang's quirks"
// concern stays distinct from the "walk the AST and produce HIR" concern.
namespace lyra::lowering::ast_to_hir {

auto LowerConversionKind(slang::ast::ConversionKind k) -> hir::ConversionKind;

auto LowerBinaryOp(slang::ast::BinaryOperator op) -> hir::BinaryOp;

auto LowerUnaryOp(slang::ast::UnaryOperator op) -> hir::UnaryOp;

// LRM 11.4.2: maps slang's inc/dec UnaryOperator values to hir::IncDecOp.
// Throws InternalError if `op` is not one of the four inc/dec variants
// (callers must dispatch on `slang::ast::OpInfo::isLValue(op)` first).
auto LowerSlangIncDecOp(slang::ast::UnaryOperator op) -> hir::IncDecOp;

auto LowerTimeUnit(slang::TimeUnit u) -> hir::TimeScale;

auto FromSlangSubroutineKind(slang::ast::SubroutineKind k)
    -> support::SystemSubroutineKind;

auto ToHirSubroutineKind(slang::ast::SubroutineKind k) -> hir::SubroutineKind;

auto LowerEnumMethodName(std::string_view name)
    -> std::optional<support::BuiltinFn>;

auto LowerStringMethodName(std::string_view name)
    -> std::optional<support::BuiltinFn>;

auto LowerArrayMethodName(std::string_view name)
    -> std::optional<support::BuiltinFn>;

// The two families whose `delete` has both an empty-the-container form and a
// drop-one-entry form (LRM 7.9.3 / 7.10.2.3) read `argument_count` -- how many
// arguments the source wrote after the receiver -- to say which one it named.
// The source distinguishes them by nothing else, and this is the layer holding
// the source, so every layer below names the operation by its identity rather
// than counting operands itself.
auto LowerQueueMethodName(std::string_view name, std::size_t argument_count)
    -> std::optional<support::BuiltinFn>;

auto LowerAssociativeMethodName(
    std::string_view name, std::size_t argument_count)
    -> std::optional<support::BuiltinFn>;

// LRM 20.8.2 Table 20-4. The standard cross-lists every row with a C standard
// math library function and defines the SV function's behavior to be that
// function's, so which entry a call names is the whole of what separates one
// row from another.
auto LowerRealMathName(slang::parsing::KnownSystemName name)
    -> std::optional<support::BuiltinFn>;

// LRM 20.5 conversions that read a real as an integral value or the reverse.
// `$itor` is not among them: it asks for the LRM 6.12.1 conversion an ordinary
// assignment already performs, so it needs no entry of its own.
auto LowerRealConversionName(slang::parsing::KnownSystemName name)
    -> std::optional<support::BuiltinFn>;

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
