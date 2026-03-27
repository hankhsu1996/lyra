#pragma once

// Shared DPI boundary contract verification.
// One canonical verifier called from both signature build time and LLVM
// lowering. Grows as D3/D4 add new contract rules.

#include <span>

#include "lyra/mir/call.hpp"

namespace lyra::mir {

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
