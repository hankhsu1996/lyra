#pragma once

#include "lyra/common/type.hpp"
#include "lyra/mir/interp/runtime_value.hpp"

namespace lyra::mir::interp {

// Evaluate a MIR binary operation on two runtime values.
// The op parameter is cast from common::BinaryOp.
// Throws InternalError if operands are not integral.
auto EvalBinary(int op, const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue;

// Evaluate a MIR unary operation on a runtime value.
// The op parameter is cast from common::UnaryOp.
// Throws InternalError if operand is not integral.
auto EvalUnary(int op, const RuntimeValue& operand) -> RuntimeValue;

// Evaluate a cast operation (width/sign conversion).
// Both types must be integral and resolved before calling.
// Extension uses source type's signedness.
auto EvalCast(
    const RuntimeValue& operand, const Type& source_type,
    const Type& target_type) -> RuntimeValue;

}  // namespace lyra::mir::interp
