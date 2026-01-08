#pragma once

#include "lyra/lir/context.hpp"

namespace lyra::mir {
class Expression;
class UnaryExpression;
class BinaryExpression;
class TernaryExpression;
}  // namespace lyra::mir

namespace lyra::lowering::mir_to_lir {
class LirBuilder;
class InstructionCapture;

// Lowers a MIR Expression into LIR instructions and returns a value
// that holds the result.
auto LowerExpression(const mir::Expression& expression, LirBuilder& builder)
    -> lir::TempRef;

// Lower a unary expression into LIR instructions and returns a value
// that holds the result.
auto LowerUnaryExpression(
    const mir::UnaryExpression& expression, lir::TempRef operand,
    LirBuilder& builder) -> lir::TempRef;

// Lower a binary expression into LIR instructions and returns a value
// that holds the result.
auto LowerBinaryExpression(
    const mir::BinaryExpression& expression, lir::TempRef lhs, lir::TempRef rhs,
    LirBuilder& builder) -> lir::TempRef;

// Lower a ternary expression into LIR instructions and returns a value
// that holds the result.
auto LowerTernaryExpression(
    const mir::TernaryExpression& expression, LirBuilder& builder)
    -> lir::TempRef;

// Helper function to handle increment/decrement operations
auto LowerIncrementDecrementExpression(
    const mir::UnaryExpression& expression, LirBuilder& builder)
    -> lir::TempRef;

// Lower an expression for $monitor, capturing instructions into a
// MonitorExpressionBlock for later re-evaluation. Returns the index of the
// created block in Module::monitor_expression_blocks.
auto LowerMonitorExpression(
    const mir::Expression& expression, LirBuilder& builder) -> size_t;

}  // namespace lyra::lowering::mir_to_lir
