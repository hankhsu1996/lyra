#pragma once

#include "lyra/mir/interp/runtime_value.hpp"

namespace lyra::mir::interp {

// Arithmetic operations (return RuntimeReal)
auto RealAdd(const RuntimeReal& lhs, const RuntimeReal& rhs) -> RuntimeReal;
auto RealSub(const RuntimeReal& lhs, const RuntimeReal& rhs) -> RuntimeReal;
auto RealMul(const RuntimeReal& lhs, const RuntimeReal& rhs) -> RuntimeReal;
auto RealDiv(const RuntimeReal& lhs, const RuntimeReal& rhs) -> RuntimeReal;

// Unary arithmetic (return RuntimeReal)
auto RealNeg(const RuntimeReal& op) -> RuntimeReal;
auto RealPlus(const RuntimeReal& op) -> RuntimeReal;  // identity

// Comparison operations (return 2-state 1-bit RuntimeIntegral)
auto RealEq(const RuntimeReal& lhs, const RuntimeReal& rhs) -> RuntimeIntegral;
auto RealNe(const RuntimeReal& lhs, const RuntimeReal& rhs) -> RuntimeIntegral;
auto RealLt(const RuntimeReal& lhs, const RuntimeReal& rhs) -> RuntimeIntegral;
auto RealLe(const RuntimeReal& lhs, const RuntimeReal& rhs) -> RuntimeIntegral;
auto RealGt(const RuntimeReal& lhs, const RuntimeReal& rhs) -> RuntimeIntegral;
auto RealGe(const RuntimeReal& lhs, const RuntimeReal& rhs) -> RuntimeIntegral;

// Logical operations (return 2-state 1-bit RuntimeIntegral)
// Evaluated eagerly (no short-circuit), matching integral behavior.
auto RealLogicalAnd(const RuntimeReal& lhs, const RuntimeReal& rhs)
    -> RuntimeIntegral;
auto RealLogicalOr(const RuntimeReal& lhs, const RuntimeReal& rhs)
    -> RuntimeIntegral;
auto RealLogicalNot(const RuntimeReal& op) -> RuntimeIntegral;

// Power operation (uses std::pow, IEEE 754 semantics)
auto RealPower(const RuntimeReal& lhs, const RuntimeReal& rhs) -> RuntimeReal;

// Conversions
// Real → Integral: truncates toward zero (C++ static_cast semantics)
auto RealToIntegral(
    const RuntimeReal& src, uint32_t target_width, bool target_signed)
    -> RuntimeIntegral;

// Integral → Real: converts to double (may lose precision for large values)
auto IntegralToReal(const RuntimeIntegral& src, bool src_is_signed)
    -> RuntimeReal;

// Truthiness for branch conditions (value != 0.0)
auto RealIsTrue(const RuntimeReal& op) -> bool;

}  // namespace lyra::mir::interp
