#pragma once

#include <array>
#include <cstddef>
#include <cstdint>

namespace lyra::runtime {

// Trigger install kind: explicit per-trigger classification written by codegen.
// Runtime installs subscriptions directly from this kind without inferring it
// from EdgeKind or late-bound header presence.
enum class TriggerInstallKind : uint8_t {
  kEdge = 0,
  kChange = 1,
  kContainer = 2,
};

// Trigger flags: explicit per-trigger metadata written by codegen.
inline constexpr uint8_t kTriggerInitiallyActive = 0x01;
// R5: signal_id is a body-local LocalSignalId. Runtime resolves the
// owning instance from the process owner (same-instance).
inline constexpr uint8_t kTriggerLocalSignal = 0x02;
// R5: cross-instance local trigger. target_instance_id and
// target_local_signal_id carry the typed identity. Always combined
// with kTriggerLocalSignal.
inline constexpr uint8_t kTriggerCrossInstanceLocal = 0x04;

// R5: Per-dependency signal record for typed rebind dependencies.
// Each dependency carries its own domain identity (local or global),
// independent of the target trigger's domain.
struct DepSignalRecord {
  uint32_t signal_id = 0;
  uint8_t flags = 0;
  std::array<uint8_t, 3> padding = {};
  // R5: valid when kDepCrossInstanceLocal is set.
  uint32_t target_instance_id = 0;
  uint32_t target_local_signal_id = 0;
};
inline constexpr uint8_t kDepLocalSignal = 0x01;
inline constexpr uint8_t kDepCrossInstanceLocal = 0x02;
static_assert(sizeof(DepSignalRecord) == 16);
static_assert(alignof(DepSignalRecord) == 4);

struct WaitTriggerRecord {
  uint32_t signal_id = 0;
  uint8_t edge = 0;  // common::EdgeKind
  // kEdge/kChange: bit position within observed byte.
  // kContainer: must be 0 (unused).
  uint8_t bit_index = 0;
  uint8_t kind = 0;   // TriggerInstallKind
  uint8_t flags = 0;  // kTriggerInitiallyActive

  // Per-kind semantics:
  //   kEdge/kChange: byte offset within slot (observation start).
  //   kContainer: element index (sv_index). Runtime uses this directly
  //     as the logical container element index, not as a byte offset.
  uint32_t byte_offset = 0;

  // Per-kind semantics:
  //   kEdge/kChange: observation size in bytes; 0 = full slot.
  //   kContainer: unused (set to 0).
  uint32_t byte_size = 0;

  // For kContainer only. Element stride in bytes. 0 for non-container triggers.
  uint32_t container_elem_stride = 0;

  // R5: valid when kTriggerCrossInstanceLocal is set.
  uint32_t target_instance_id = 0;
  uint32_t target_local_signal_id = 0;
};

static_assert(
    sizeof(WaitTriggerRecord) == 28, "WaitTriggerRecord size mismatch");
static_assert(
    alignof(WaitTriggerRecord) == 4, "WaitTriggerRecord alignment mismatch");
static_assert(
    offsetof(WaitTriggerRecord, bit_index) == 5,
    "WaitTriggerRecord bit_index offset mismatch");
static_assert(
    offsetof(WaitTriggerRecord, kind) == 6,
    "WaitTriggerRecord kind offset mismatch");
static_assert(
    offsetof(WaitTriggerRecord, flags) == 7,
    "WaitTriggerRecord flags offset mismatch");
static_assert(
    offsetof(WaitTriggerRecord, byte_offset) == 8,
    "WaitTriggerRecord byte_offset offset mismatch");
static_assert(
    offsetof(WaitTriggerRecord, byte_size) == 12,
    "WaitTriggerRecord byte_size offset mismatch");
static_assert(
    offsetof(WaitTriggerRecord, container_elem_stride) == 16,
    "WaitTriggerRecord container_elem_stride offset mismatch");
static_assert(
    offsetof(WaitTriggerRecord, target_instance_id) == 20,
    "WaitTriggerRecord target_instance_id offset mismatch");
static_assert(
    offsetof(WaitTriggerRecord, target_local_signal_id) == 24,
    "WaitTriggerRecord target_local_signal_id offset mismatch");

// Late-bound header for dynamic-index edge triggers.
// References into plan_ops and dep_slots pools via start/count spans.
struct LateBoundHeader {
  uint32_t trigger_index = 0;
  uint16_t plan_ops_start = 0;
  uint16_t plan_ops_count = 0;
  uint16_t dep_slots_start = 0;
  uint16_t dep_slots_count = 0;
  int32_t index_base = 0;
  int32_t index_step = 1;
  uint32_t total_bits = 0;
  uint32_t container_elem_stride = 0;  // 0 = not container, >0 = container mode
};

static_assert(sizeof(LateBoundHeader) == 28, "LateBoundHeader size mismatch");
static_assert(
    alignof(LateBoundHeader) == 4, "LateBoundHeader alignment mismatch");
static_assert(
    offsetof(LateBoundHeader, plan_ops_start) == 4,
    "LateBoundHeader plan_ops_start offset mismatch");
static_assert(
    offsetof(LateBoundHeader, dep_slots_start) == 8,
    "LateBoundHeader dep_slots_start offset mismatch");
static_assert(
    offsetof(LateBoundHeader, index_base) == 12,
    "LateBoundHeader index_base offset mismatch");
static_assert(
    offsetof(LateBoundHeader, index_step) == 16,
    "LateBoundHeader index_step offset mismatch");
static_assert(
    offsetof(LateBoundHeader, total_bits) == 20,
    "LateBoundHeader total_bits offset mismatch");
static_assert(
    offsetof(LateBoundHeader, container_elem_stride) == 24,
    "LateBoundHeader container_elem_stride offset mismatch");

}  // namespace lyra::runtime
