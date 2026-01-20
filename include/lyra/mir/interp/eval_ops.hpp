#pragma once

#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/mir/interp/runtime_value.hpp"
#include "lyra/mir/operator.hpp"

namespace lyra::mir::interp {

// Evaluate a MIR binary operation on two runtime values.
// Throws InternalError if operands are not integral.
auto EvalBinary(BinaryOp op, const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue;

// Evaluate a MIR unary operation on a runtime value.
// Throws InternalError if operand is not integral.
auto EvalUnary(UnaryOp op, const RuntimeValue& operand) -> RuntimeValue;

// Evaluate a cast operation (width/sign conversion).
// Both types must be packed (kIntegral or kPackedArray).
// Extension uses source type's signedness.
auto EvalCast(
    const RuntimeValue& operand, const Type& source_type,
    const Type& target_type, const TypeArena& arena) -> RuntimeValue;

}  // namespace lyra::mir::interp
