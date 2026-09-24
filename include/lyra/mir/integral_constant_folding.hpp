#pragma once

#include <optional>

#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/mir/unary_op.hpp"

namespace lyra::mir {

// The constant an integral operation evaluates to when every operand is a
// constant the unit holds, and nothing otherwise. Such an operation's value is
// fixed before the program runs, so it is stated by the unit like any other
// constant rather than computed each time control reaches it -- an optimizer
// downstream is not in a position to recover it, because the value library's
// operations are calls it cannot see into.
//
// Each is asked by whatever builds that operation, with the operation it is
// about to state, so no consumer reads a built node back to learn what it
// holds. The answer is computed by the value library the run itself uses, so a
// folded operation and an unfolded one cannot disagree; and an operation the
// library would answer at a representation other than `result`, or would
// refuse for operands of unlike storage, is not folded, since that is a
// question for a run that reaches it.

[[nodiscard]] auto FoldBinary(
    const CompilationUnit& unit, const Block& block, BinaryOp op, ExprId lhs,
    ExprId rhs, TypeId result) -> std::optional<IntegralConstant>;

[[nodiscard]] auto FoldUnary(
    const CompilationUnit& unit, const Block& block, UnaryOp op, ExprId operand,
    TypeId result) -> std::optional<IntegralConstant>;

// A conversion of `operand` into the representation `result` names.
[[nodiscard]] auto FoldConversion(
    const CompilationUnit& unit, const Block& block, ExprId operand,
    TypeId result) -> std::optional<IntegralConstant>;

// The position an index names, in the type position arithmetic is done in.
[[nodiscard]] auto FoldPosition(
    const CompilationUnit& unit, const Block& block, ExprId index,
    TypeId result) -> std::optional<IntegralConstant>;

}  // namespace lyra::mir
