#pragma once

#include <cstdint>

#include "lyra/runtime/signal_coord.hpp"

namespace lyra::runtime {

struct RuntimeInstance;

// Pre-bound child context for expression connection processes.
// Stored as the last field in the expression connection process frame.
// Written by constructor at install time. Read by codegen at runtime.
// ProcessFrameHeader is unchanged -- this is frame payload only.
struct ExprConnectionChildBinding {
  RuntimeInstance* child_instance = nullptr;
  uint32_t child_slot_byte_offset = 0;
  LocalSignalId child_local_signal{0};
};

}  // namespace lyra::runtime
