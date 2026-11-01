#pragma once

#include <expected>
#include <string_view>

#include "lyra/hir/type_definition.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// LRM Table 6-7 default constant for an integral type: all-`x` for 4-state,
// all-zero for 2-state.
[[nodiscard]] auto DefaultIntegralConstant(const mir::IntegralType& integral)
    -> mir::IntegralConstant;

// Builds a primitive MIR expression evaluating to the LRM Table 6-7 default
// value of `type`, returning the top node detached for the caller to intern.
// A composite default registers the child expressions it references into
// `block` before returning the outer node, so interning the result yields a
// self-contained subtree of arena entries.
//
// `int x;` and `int x = 0;` are different in SV source -- HIR preserves that
// distinction via `optional<initializer>`. By MIR every variable has an
// explicit initializer expression: the SV "no initializer means LRM default"
// sugar is decomposed into a primitive Expr at the HIR-to-MIR boundary so
// downstream layers see one shape (an Expr) instead of two.
[[nodiscard]] auto BuildDefaultValueExpr(
    const mir::CompilationUnit& unit, mir::Builder block, mir::TypeId type)
    -> mir::Expr;

// The same, or, for a type no declaration holds -- a reference, an index of a
// wildcard-indexed array, the machine vocabulary -- what it is instead. A
// product of such a type has none either, since its default is each
// component's (LRM Table 7-1).
[[nodiscard]] auto DefaultValueOf(
    const mir::CompilationUnit& unit, mir::Builder block, mir::TypeId type)
    -> std::expected<mir::Expr, std::string_view>;

// The same for a type named as the source layer names it.
[[nodiscard]] auto BuildDefaultValueFromHir(
    const UnitLowerer& unit_lowerer, mir::Builder block, hir::TypeId hir_type)
    -> mir::Expr;

// LRM Table 7-1's unpacked-struct row, as a value of `structure`: each member
// at the initial value its declaration assigns (LRM 7.2.2), and at the value
// the table gives its own type where it assigns none. This is the body of what
// a structure answers its declared default with, and nothing else states it.
[[nodiscard]] auto BuildDeclaredStructValue(
    const UnitLowerer& unit_lowerer, mir::Builder block,
    const hir::UnpackedStructDefinition& source, mir::TypeId structure)
    -> mir::Expr;

}  // namespace lyra::lowering::hir_to_mir
