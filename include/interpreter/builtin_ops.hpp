#pragma once

#include "runtime/runtime_value.hpp"

namespace lyra::interpreter {

// Unary operations
auto UnaryPlus(const RuntimeValue& operand) -> RuntimeValue;
auto UnaryMinus(const RuntimeValue& operand) -> RuntimeValue;
auto UnaryLogicalNot(const RuntimeValue& operand) -> RuntimeValue;
auto UnaryBitwiseNot(const RuntimeValue& operand) -> RuntimeValue;

// Reduction operations
auto ReductionAnd(const RuntimeValue& operand) -> RuntimeValue;
auto ReductionNand(const RuntimeValue& operand) -> RuntimeValue;
auto ReductionOr(const RuntimeValue& operand) -> RuntimeValue;
auto ReductionNor(const RuntimeValue& operand) -> RuntimeValue;
auto ReductionXor(const RuntimeValue& operand) -> RuntimeValue;
auto ReductionXnor(const RuntimeValue& operand) -> RuntimeValue;

// Binary arithmetic operations
auto BinaryAdd(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue;
auto BinarySubtract(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue;
auto BinaryMultiply(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue;
auto BinaryDivide(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue;
auto BinaryModulo(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue;

// Binary comparison operations
auto BinaryEqual(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue;
auto BinaryNotEqual(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue;
auto BinaryLessThan(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue;
auto BinaryLessThanEqual(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue;
auto BinaryGreaterThan(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue;
auto BinaryGreaterThanEqual(const RuntimeValue& lhs, const RuntimeValue& rhs)
    -> RuntimeValue;

}  // namespace lyra::interpreter
