#pragma once

// Shared DPI boundary contract verification.
// One canonical verifier called from both signature build time and LLVM
// lowering. Grows as D3/D4 add new contract rules.

#include <span>

#include "lyra/mir/call.hpp"

namespace lyra::mir {

// Canonical passing-mode derivation from direction and ABI type.
// Packed vector input is by-pointer (const svLogicVecVal*/svBitVecVal*).
// All other inputs are by-value. All output/inout are by-pointer.
// Used by both signature construction and contract verification.
[[nodiscard]] inline auto GetDpiPassingMode(
    ParameterDirection dir, DpiAbiTypeClass abi_type) -> DpiPassingMode {
  if (dir != ParameterDirection::kInput) {
    return DpiPassingMode::kByPointer;
  }
  if (IsPackedVecDpiType(abi_type)) {
    return DpiPassingMode::kByPointer;
  }
  return DpiPassingMode::kByValue;
}

// Validate the structural contract of a frozen DPI signature:
// - Return ABI type must be valid (not kInvalid)
// - Each param ABI type must be valid (not kInvalid, not kVoid)
// - Direction/passing consistency: input -> kByValue, output/inout ->
// kByPointer
void ValidateDpiSignatureContract(const DpiSignature& sig, const char* caller);

// Validate that call-site bindings match the signature contract:
// - Count must match
// - input: input_value present, writeback_dest absent
// - output: input_value absent, writeback_dest present
// - inout: both present
void ValidateDpiCallContract(
    const DpiSignature& sig, std::span<const DpiArgBinding> args,
    const char* caller);

}  // namespace lyra::mir
