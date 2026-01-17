#pragma once

#include <cstdint>

#include "lyra/mir/interp/runtime_value.hpp"

namespace lyra::mir::interp {

// Helper: Create an unknown (all-X) integral result.
// Use this for operations with X/Z operands or unimplemented multi-word cases.
auto MakeUnknownIntegral(uint32_t bit_width) -> RuntimeIntegral;

// Arithmetic operations
auto IntegralAdd(
    const RuntimeIntegral& lhs, const RuntimeIntegral& rhs, uint32_t width)
    -> RuntimeIntegral;
auto IntegralSub(
    const RuntimeIntegral& lhs, const RuntimeIntegral& rhs, uint32_t width)
    -> RuntimeIntegral;
auto IntegralMul(
    const RuntimeIntegral& lhs, const RuntimeIntegral& rhs, uint32_t width)
    -> RuntimeIntegral;
auto IntegralDiv(
    const RuntimeIntegral& lhs, const RuntimeIntegral& rhs, uint32_t width)
    -> RuntimeIntegral;
auto IntegralMod(
    const RuntimeIntegral& lhs, const RuntimeIntegral& rhs, uint32_t width)
    -> RuntimeIntegral;

// Bitwise operations
auto IntegralAnd(
    const RuntimeIntegral& lhs, const RuntimeIntegral& rhs, uint32_t width)
    -> RuntimeIntegral;
auto IntegralOr(
    const RuntimeIntegral& lhs, const RuntimeIntegral& rhs, uint32_t width)
    -> RuntimeIntegral;
auto IntegralXor(
    const RuntimeIntegral& lhs, const RuntimeIntegral& rhs, uint32_t width)
    -> RuntimeIntegral;

// Unary operations
auto IntegralNeg(const RuntimeIntegral& op, uint32_t width) -> RuntimeIntegral;
auto IntegralNot(const RuntimeIntegral& op, uint32_t width) -> RuntimeIntegral;

// Comparison operations (return 1-bit result)
auto IntegralEq(const RuntimeIntegral& lhs, const RuntimeIntegral& rhs)
    -> RuntimeIntegral;
auto IntegralNe(const RuntimeIntegral& lhs, const RuntimeIntegral& rhs)
    -> RuntimeIntegral;
auto IntegralLt(
    const RuntimeIntegral& lhs, const RuntimeIntegral& rhs, bool is_signed)
    -> RuntimeIntegral;
auto IntegralLe(
    const RuntimeIntegral& lhs, const RuntimeIntegral& rhs, bool is_signed)
    -> RuntimeIntegral;
auto IntegralGt(
    const RuntimeIntegral& lhs, const RuntimeIntegral& rhs, bool is_signed)
    -> RuntimeIntegral;
auto IntegralGe(
    const RuntimeIntegral& lhs, const RuntimeIntegral& rhs, bool is_signed)
    -> RuntimeIntegral;

// Logical operations (return 1-bit result)
auto IntegralLogicalAnd(const RuntimeIntegral& lhs, const RuntimeIntegral& rhs)
    -> RuntimeIntegral;
auto IntegralLogicalOr(const RuntimeIntegral& lhs, const RuntimeIntegral& rhs)
    -> RuntimeIntegral;
auto IntegralLogicalNot(const RuntimeIntegral& op) -> RuntimeIntegral;

// Shift operations
auto IntegralShl(
    const RuntimeIntegral& lhs, const RuntimeIntegral& rhs, uint32_t width)
    -> RuntimeIntegral;
auto IntegralShr(
    const RuntimeIntegral& lhs, const RuntimeIntegral& rhs, uint32_t width,
    bool is_signed) -> RuntimeIntegral;

// Resize operation for 2-state integrals (truncation or extension).
// Extension uses src_is_signed to determine sign-extend vs zero-extend.
auto IntegralResize2State(
    const RuntimeIntegral& src, bool src_is_signed, uint32_t target_width)
    -> RuntimeIntegral;

}  // namespace lyra::mir::interp
