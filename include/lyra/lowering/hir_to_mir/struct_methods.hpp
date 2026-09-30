#pragma once

#include <optional>
#include <span>
#include <vector>

#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/struct_decl.hpp"
#include "lyra/mir/type_declaration_ref.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/value_operation.hpp"

namespace lyra::lowering::hir_to_mir {

class UnitLowerer;

// A method for every operation on a whole value the struct `declaration`
// names has (LRM 7.2), over values of `structure` whose members are of
// `members`. Asked while the declaration is being made, so what the struct
// itself answers is read off its members; a member that is itself a struct
// is reached by the declaration naming it, which is settled before this one.
[[nodiscard]] auto StructMethodsOf(
    UnitLowerer& lowerer, const mir::TypeDeclarationRef& declaration,
    mir::TypeId structure, std::span<const mir::TypeId> members)
    -> std::vector<mir::StructMethod>;

// `entry`'s question asked of a value, with `operands` after the value it is
// asked of: the method of the struct's declaration where the type deciding it
// is a struct the source declared -- `receiver`'s, or for a question asked of a
// type rather than of a value, the result's -- and the library's entry for
// every other value. The operands are the same either way, since a struct
// answers the question with the parameters any value does.
[[nodiscard]] auto BuildValueOperation(
    const mir::CompilationUnit& unit, mir::Block& block,
    support::BuiltinFn entry, std::optional<mir::ExprId> receiver,
    std::vector<mir::ExprId> operands, mir::TypeId result) -> mir::ExprId;

// LRM 11.4.5 `==` or `!=` on two structs of one type, as the struct's method
// for it: one bit, which can be unknown exactly where a member can hold an
// unknown.
[[nodiscard]] auto BuildStructComparison(
    const mir::CompilationUnit& unit, mir::Block& block,
    support::ValueOperator comparison, mir::ExprId lhs, mir::ExprId rhs)
    -> mir::ExprId;

// LRM 11.4.5 `===`, a known bit.
[[nodiscard]] auto BuildCaseEquality(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId lhs,
    mir::ExprId rhs) -> mir::ExprId;

// LRM 20.9 `$isunknown`, a known bit.
[[nodiscard]] auto BuildUnknownTest(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId value)
    -> mir::ExprId;

// LRM 20.9 `$countbits` of `value` under the control set `control`, an `int`.
[[nodiscard]] auto BuildBitCount(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId value,
    mir::ExprId control) -> mir::ExprId;

// LRM 20.6.2 `$bits` of what `value` holds, an `int`.
[[nodiscard]] auto BuildBitWidth(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId value)
    -> mir::ExprId;

}  // namespace lyra::lowering::hir_to_mir
