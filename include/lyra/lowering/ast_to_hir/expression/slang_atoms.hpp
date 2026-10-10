#pragma once

#include <optional>
#include <string_view>

#include <slang/ast/Expression.h>
#include <slang/ast/SemanticFacts.h>
#include <slang/ast/expressions/Operator.h>
#include <slang/ast/symbols/ClassSymbols.h>
#include <slang/parsing/KnownSystemName.h>

#include "lyra/hir/binary_op.hpp"
#include "lyra/hir/conversion.hpp"
#include "lyra/hir/enum_method.hpp"
#include "lyra/hir/inc_dec_op.hpp"
#include "lyra/hir/subroutine_kind.hpp"
#include "lyra/hir/unary_op.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/imported_runtime_class.hpp"
#include "lyra/support/system_subroutine.hpp"

// Stateless slang -> HIR translators. Each function is a pure 1:1 mapping
// from a slang AST atom to its HIR counterpart, with no recursion and no
// lowering state. They stand apart from the recursive lowering so the
// "encapsulate slang's quirks" concern stays distinct from the "walk the AST
// and produce HIR" concern.
namespace lyra::lowering::ast_to_hir {

auto LowerConversionKind(slang::ast::ConversionKind k) -> hir::ConversionKind;

auto LowerBinaryOp(slang::ast::BinaryOperator op) -> hir::BinaryOp;

auto LowerUnaryOp(slang::ast::UnaryOperator op) -> hir::UnaryOp;

// LRM 11.4.2: maps slang's inc/dec UnaryOperator values to hir::IncDecOp.
// Throws InternalError if `op` is not one of the four inc/dec variants
// (callers must dispatch on `slang::ast::OpInfo::isLValue(op)` first).
auto LowerSlangIncDecOp(slang::ast::UnaryOperator op) -> hir::IncDecOp;

auto FromSlangSubroutineKind(slang::ast::SubroutineKind k)
    -> support::SystemSubroutineKind;

auto ToHirSubroutineKind(slang::ast::SubroutineKind k) -> hir::SubroutineKind;

auto LowerEnumMethodName(std::string_view name)
    -> std::optional<hir::EnumMethod>;

auto LowerStringMethodName(std::string_view name)
    -> std::optional<support::BuiltinFn>;

// A class the runtime library defines and every unit imports by reference is a
// direct member of the built-in `std` package (LRM 9.7 `process` is the first
// Lyra supports). Keying on the declaring package as well as the name -- rather
// than on a bare name match anywhere -- is what makes this the library
// declaration's identity, not a user class that happens to share the name.
auto ImportedRuntimeClassOf(const slang::ast::ClassType& cls)
    -> std::optional<support::ImportedRuntimeClass>;

// LRM 9.7's `process` methods. The runtime library carries each of them out,
// so a call names the entry and no per-unit method declaration exists.
auto LowerProcessMethodName(std::string_view name)
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

// What slang's expansion of `lhs op= e` into `Conv(lhs.type) { BinaryOp(op) {
// Conv(common, LValueRef), Conv(common, e) } }` states: the type the binary
// operator is applied at (LRM 11.6.1 Table 11-21, 11.8.1), and the operand
// beside the target as the operator takes it -- `e` brought to that type, or
// `e` itself where the operator sizes its right operand on its own. Slang's
// invariant is at most one Conversion at each wrap site and the target as the
// left operand; an InternalError surfaces if that invariant is ever violated.
struct CompoundExpansion {
  const slang::ast::Type* applied_at;
  const slang::ast::Expression* operand;
};
auto CompoundExpansionOf(const slang::ast::Expression& slang_expanded_rhs)
    -> CompoundExpansion;

}  // namespace lyra::lowering::ast_to_hir
