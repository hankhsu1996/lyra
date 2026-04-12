#pragma once

#include <cstdint>
#include <functional>
#include <limits>
#include <variant>

#include "lyra/runtime/signal_coord.hpp"

namespace lyra::runtime {

struct SuspendRecord;

// Simulation time in ticks (timescale-independent).
using SimTime = uint64_t;

// Constant representing unlimited simulation time.
// Use as max_time argument to Run() for "run until natural completion".
inline constexpr SimTime kNoTimeLimit = std::numeric_limits<SimTime>::max();

// Unique identifier for a process instance.
// Combines process definition ID with instance path for hierarchical designs.
struct ProcessHandle {
  uint32_t process_id = 0;
  InstanceId instance_id = InstanceId{0};  // For future hierarchy support

  auto operator==(const ProcessHandle&) const -> bool = default;
};

// Hash function for ProcessHandle (for use in unordered containers).
struct ProcessHandleHash {
  auto operator()(const ProcessHandle& h) const noexcept -> size_t {
    return std::hash<uint64_t>{}(
        (static_cast<uint64_t>(h.process_id) << 32) | h.instance_id.value);
  }
};

// Resume point within a process (block index + instruction index).
// Allows processes to suspend and resume at arbitrary points.
struct ResumePoint {
  uint32_t block_index = 0;
  uint32_t instruction_index = 0;
};

// Waitable signal identifier. This IS the design storage slot ID -
// NotifyChange, Subscribe, and NBA commit all use the same ID space.
using SignalId = uint32_t;

// Sentinel for "no design slot" in ABI parameters.
// Lowering passes this when a target is not a design slot (e.g., local/temp).
inline constexpr uint32_t kNoSlotId = UINT32_MAX;

// Dense trigger range: maps a slot to a contiguous range in a backing array.
// Empty = count == 0.
struct TriggerRange {
  uint32_t start = 0;
  uint32_t count = 0;
};

// R5: Typed connection destination for init-time decoding.
// Used as intermediates when decoding ConnectionDescriptors in
// InitConnectionBatch. Not stored on the hot path.
struct GlobalConnectionTarget {
  GlobalSignalId signal;
};

struct LocalConnectionTarget {
  InstanceId instance_id;
  LocalSignalId signal;
};

using ConnectionTarget =
    std::variant<GlobalConnectionTarget, LocalConnectionTarget>;

// Coarse per-process wait-state classification.
// Set by the process envelope after each activation. Observability may
// refine kSuspendedWait into edge/change/multi via subscription analysis.
enum class ProcessWaitKind : uint8_t {
  kRunning,
  kReady,
  kSuspendedDelay,
  kSuspendedWait,
  kSuspendedEdge,
  kSuspendedChange,
  kSuspendedMulti,
  kSuspendedRepeat,
  kSuspendedEvent,
  kSuspendedUnknown,
  kFinished,
};

// Pre-resolved connection destination for hot-path propagation.
// InstanceId is resolved to dense inst_idx at init time by
// InitConnectionBatch, eliminating per-dispatch GetInstanceIndex calls.
struct GlobalConnectionDst {
  GlobalSignalId signal{};
};

struct LocalConnectionDst {
  uint32_t inst_idx = UINT32_MAX;
  LocalSignalId signal{};
};

using BatchedConnectionDst =
    std::variant<GlobalConnectionDst, LocalConnectionDst>;

// First-class runtime process object.
// Consolidates per-process state that was previously spread across
// parallel engine-level arrays indexed by process_id.
struct RuntimeProcess {
  bool is_enqueued = false;
  SuspendRecord* suspend_record = nullptr;
  bool is_comb_kernel = false;
};

}  // namespace lyra::runtime
