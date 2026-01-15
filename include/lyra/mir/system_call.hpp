#pragma once

#include <cstdint>

namespace lyra::mir {

// Semantic role of a system subroutine (fixed classification).
enum class SystemCallRole : uint8_t {
  kPure,    // No side effects ($clog2, $bits)
  kEffect,  // Immediate observable effect ($display, $write)
  kState,   // Mutation of simulation runtime ($monitor, $finish)
};

// Descriptor for a system call.
// Shape is in Rvalue (kCall); semantics are here.
struct SystemCallDesc {
  SystemCallRole role;
  int opcode;  // intrinsic id / operation kind
};

}  // namespace lyra::mir
