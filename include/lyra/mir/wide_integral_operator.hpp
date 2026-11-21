#pragma once

#include <optional>
#include <variant>
#include <vector>

#include "lyra/mir/code_source.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/integral_operation.hpp"

namespace lyra::mir {

// An integral operator a node states (LRM 11.4), and its operands, nodes of
// the node's block.
struct IntegralOperator {
  support::IntegralOp op;
  std::vector<ExprId> operands;
};

// The integral operator the node `expr` of `block` states, where it states
// one: an operator over integral operands, a call of a built-in function the
// library carries out as one, or an integral value read as the machine boolean
// a condition tests, which is its truth.
[[nodiscard]] auto IntegralOperatorOf(
    const CompilationUnit& unit, const Block& block, const Expr& expr)
    -> std::optional<IntegralOperator>;

// How a legalized unit states an operator over an integral value wider than a
// machine operates on at once, as a legalization expands an operation on a
// type the machine has no instruction for into ones it has: a conversion to a
// type no wider than a machine's integers is the operand's low bits (LRM
// 6.24.1), as LLVM legalizes a truncation of a wide integer to its low part
// (`ExpandIntRes_TRUNCATE`); every other operator is a call of the operation
// its type answers it with.
struct Truncated {};
struct CallsItsTypesOperation {
  TypeOperationSource source;
};
using WideOperatorExpansion = std::variant<Truncated, CallsItsTypesOperation>;

// How a legalized unit states `applied`, the operator the node `expr` of
// `block` states; nothing where it is over no wide value, or is one the
// library carries out for every width -- an operator handed a value made
// elsewhere, and one whose wide operand only says how far a narrow value is
// moved or raised.
[[nodiscard]] auto WideOperatorExpansionOf(
    const CompilationUnit& unit, const Block& block, const Expr& expr,
    const IntegralOperator& applied) -> std::optional<WideOperatorExpansion>;

}  // namespace lyra::mir
