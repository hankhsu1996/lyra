#pragma once

#include "lir/operand.hpp"

namespace lyra::mir {
class Expression;
class BinaryExpression;
}  // namespace lyra::mir

namespace lyra::lowering {
class LirBuilder;

// Lowers a MIR Expression into LIR instructions and returns a value
// that holds the result.
auto LowerExpression(const mir::Expression& expression, LirBuilder& builder)
    -> lir::Operand;

// Lower a binary expression into LIR instructions and returns a value
// that holds the result.
auto LowerBinaryExpression(
    const mir::BinaryExpression& expression, lir::Operand lhs, lir::Operand rhs,
    LirBuilder& builder) -> lir::Operand;

}  // namespace lyra::lowering
