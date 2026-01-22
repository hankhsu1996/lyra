#pragma once

#include <cstdint>
#include <string_view>

#include "lyra/mir/interp/runtime_value.hpp"

namespace lyra::mir::interp {

// Create a known zero-initialized integral result.
auto MakeKnownIntegral(uint32_t bit_width) -> RuntimeIntegral;

// Create an unknown (all-X) integral result.
// Use this for operations with X/Z operands.
auto MakeUnknownIntegral(uint32_t bit_width) -> RuntimeIntegral;

// Convert string bytes to integral (for string literal in packed context).
// First character is most significant byte, packed into MSB of result.
auto StringBytesToIntegral(std::string_view str, uint32_t bit_width)
    -> RuntimeIntegral;

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
    const RuntimeIntegral& lhs, const RuntimeIntegral& rhs, uint32_t width,
    bool is_signed) -> RuntimeIntegral;
auto IntegralMod(
    const RuntimeIntegral& lhs, const RuntimeIntegral& rhs, uint32_t width,
    bool is_signed) -> RuntimeIntegral;

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

// Case statement matching (return 1-bit result, always 0 or 1)
// casez: Z bits from both operands are wildcards (ignored); X bits must match
auto IntegralCaseZMatch(const RuntimeIntegral& lhs, const RuntimeIntegral& rhs)
    -> RuntimeIntegral;
// casex: X and Z bits from both operands are wildcards (ignored)
auto IntegralCaseXMatch(const RuntimeIntegral& lhs, const RuntimeIntegral& rhs)
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

// Extract a slice of bits from an integral value.
// Returns bits [bit_offset + width - 1 : bit_offset].
//
// Contract:
// - If bit_offset >= src.bit_width: returns zero (width bits of zero)
// - If bit_offset + width > src.bit_width: zero-extends missing high bits
// - If width == 0: returns zero-width integral (bit_width=0, empty value)
// - Result is always unsigned with bit_width = width
auto IntegralExtractSlice(
    const RuntimeIntegral& src, uint32_t bit_offset, uint32_t width)
    -> RuntimeIntegral;

// Insert a value into a slice of bits in an integral.
// Modifies bits [bit_offset + width - 1 : bit_offset], preserves others.
//
// Contract:
// - If bit_offset >= dst.bit_width: returns dst unchanged
// - If bit_offset + width > dst.bit_width: only modifies valid bits
// - If width == 0: returns dst unchanged
// - src is truncated/zero-extended to width bits before insertion
// - Result has same bit_width as dst
auto IntegralInsertSlice(
    const RuntimeIntegral& dst, const RuntimeIntegral& src, uint32_t bit_offset,
    uint32_t width) -> RuntimeIntegral;

// Insert a value into a slice of bits in an integral (4-state aware).
// Same as IntegralInsertSlice but handles 4-state sources by also
// inserting x_mask and z_mask bits. For 2-state sources (empty masks),
// the mask operations are a no-op.
auto IntegralInsertSlice4State(
    const RuntimeIntegral& dst, const RuntimeIntegral& src, uint32_t bit_offset,
    uint32_t width) -> RuntimeIntegral;

}  // namespace lyra::mir::interp
