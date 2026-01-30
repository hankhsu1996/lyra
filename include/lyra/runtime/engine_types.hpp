#pragma once

#include <cstdint>
#include <functional>
#include <limits>

namespace lyra::runtime {

// Simulation time in ticks (timescale-independent).
using SimTime = uint64_t;

// Constant representing unlimited simulation time.
// Use as max_time argument to Run() for "run until natural completion".
inline constexpr SimTime kNoTimeLimit = std::numeric_limits<SimTime>::max();

// Unique identifier for a process instance.
// Combines process definition ID with instance path for hierarchical designs.
struct ProcessHandle {
  uint32_t process_id = 0;
  uint32_t instance_id = 0;  // For future hierarchy support

  auto operator==(const ProcessHandle&) const -> bool = default;
};

// Hash function for ProcessHandle (for use in unordered containers).
struct ProcessHandleHash {
  auto operator()(const ProcessHandle& h) const noexcept -> size_t {
    return std::hash<uint64_t>{}(
        (static_cast<uint64_t>(h.process_id) << 32) | h.instance_id);
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

}  // namespace lyra::runtime
