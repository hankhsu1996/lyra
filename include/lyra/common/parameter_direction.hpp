#pragma once

// SystemVerilog parameter direction, shared across HIR and MIR layers.

namespace lyra {

// Parameter direction for subroutine arguments.
// Maps directly to SystemVerilog argument directions (IEEE 1800-2023 13.5).
enum class ParameterDirection {
  kInput,   // Input only (pass by value semantics)
  kOutput,  // Output only (callee writes, caller receives)
  kInOut,  // Bidirectional (caller passes initialized value, callee may modify)
  kRef,    // Reference (alias to caller's variable) - not yet supported
};

}  // namespace lyra
