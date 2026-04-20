#pragma once

#include <cstdint>
#include <type_traits>
#include <variant>

#include "lyra/runtime/activation_trace.hpp"
#include "lyra/runtime/signal_coord.hpp"
#include "lyra/runtime/small_byte_buffer.hpp"

namespace lyra::runtime {

struct RuntimeInstance;
struct RuntimeProcess;

// IEEE 1800 simulation regions (simplified).
// Active -> Inactive -> NBA is the core loop for RTL simulation.
enum class Region : uint8_t {
  kActive,    // Blocking assignments, $display
  kInactive,  // #0 delays (same time slot)
  kNBA,       // Nonblocking assignment updates
};

// Hot queue entry for process wakeup. Carries process identity directly as
// a RuntimeProcess pointer; no integer indirection on the dispatch path.
// Trace-only fields (cause, trigger_slot) are stored per-process on
// RuntimeProcess::wake_trace when activation tracing is enabled.
struct WakeupEntry {
  RuntimeProcess* process = nullptr;
  uint32_t resume_block = 0;
};

// Typed NBA payloads. Each mode has its own struct with exactly the fields
// it needs, so field semantics never depend on a mode tag.

// Full overwrite: direct compare/copy of byte_size bytes at write_ptr.
struct NbaFullOverwrite {
  uint32_t byte_size;
  SmallByteBuffer value;
};

// Masked merge: per-byte (old & ~mask) | (new & mask).
struct NbaMaskedMerge {
  uint32_t byte_size;
  SmallByteBuffer value;
  SmallByteBuffer mask;
};

// Canonical packed two-plane narrow overwrite for 4-state byte-addressable
// packed subview writes. Writes region_byte_size bytes at write_ptr (value
// plane) and at write_ptr + second_region_offset (unknown plane).
struct NbaCanonicalPackedTwoPlane {
  uint32_t region_byte_size;
  uint32_t second_region_offset;
  SmallByteBuffer value;
  SmallByteBuffer unk;
};

using NbaPayload =
    std::variant<NbaFullOverwrite, NbaMaskedMerge, NbaCanonicalPackedTwoPlane>;

// R5: NBA notification signal identity. Typed at enqueue time, not
// reverse-engineered at commit time.
struct NbaNotifyGlobal {
  GlobalSignalId signal;
};
struct NbaNotifyLocal {
  RuntimeInstance* instance = nullptr;
  LocalSignalId signal{};
};
using NbaNotifySignal = std::variant<NbaNotifyGlobal, NbaNotifyLocal>;

// Generic NBA queue entry: deferred write committed in the NBA region.
// Common header (write target + notification) plus typed payload.
//
// ARCHITECTURAL INVARIANT:
// The generic nba_queue_ serves only non-instance-owned targets:
//   - Global/package signals (NbaNotifyGlobal)
//   - Cross-instance local signals (NbaNotifyLocal where the writing
//     process belongs to a different instance than the target)
// Instance-owned local signals use per-instance deferred storage
// (RuntimeInstanceStorage::deferred_inline_base / deferred_appendix_base)
// committed by CommitDeferredLocalNbas, not this queue.
struct NbaEntry {
  void* write_ptr;
  const void* notify_base_ptr;
  NbaNotifySignal notify_signal;
  NbaPayload payload;
};

static_assert(
    std::is_nothrow_move_constructible_v<NbaEntry>,
    "NbaEntry must be nothrow-movable for efficient std::vector growth");

// Forward declaration for callback
class Engine;

}  // namespace lyra::runtime
