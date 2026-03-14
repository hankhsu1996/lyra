#pragma once

#include <array>
#include <cstddef>
#include <cstdint>
#include <vector>

#include "lyra/common/bit_target_mapping.hpp"
#include "lyra/common/edge_kind.hpp"
#include "lyra/runtime/engine_types.hpp"
#include "lyra/runtime/index_plan.hpp"
#include "lyra/runtime/wait_site.hpp"

namespace lyra::runtime {

// Reference into a process's IndexPlanPool.
struct IndexPlanRef {
  uint32_t start = 0;
  uint16_t count = 0;
};

// Per-process plan storage for rebind index plans.
//
// Ownership model: plans are appended during SubscribeRebind and referenced
// by IndexPlanRef spans stored in the target's cold entry. The entire pool
// is cleared in ClearProcessSubscriptions when the process's installed
// subscriptions are removed. Individual plan entries are never removed --
// they are append-only and lifetime-coupled to the installed wait.
struct IndexPlanPool {
  std::vector<IndexPlanOp> ops;
};

// Subscription kind discriminator for SubRef typed dispatch.
enum class SubKind : uint8_t {
  kEdge,
  kChange,
  kRebindWatcher,
  kContainer,
};

// Per-process ownership record. Indexes into the typed vector for a given slot.
//
// Invariants maintained by swap-and-pop removal:
// - SubRef.index always points to a valid entry in the typed sub vector
//   for signal_subs_[SubRef.slot_id].
// - The pointed-to sub's process_sub_idx always points back to this SubRef's
//   position in process_state.sub_refs.
// - When a sub is swapped during removal, both the moved sub's
//   process_sub_idx and its SubRef.index are updated atomically.
// - If the moved sub is a rebind target (has edge_target_id),
//   edge_target_table_[edge_target_id].index is also updated.
struct SubRef {
  uint32_t slot_id;
  uint32_t index;
  SubKind kind;
};

// Stable indirection handle for rebind targets.
// Stored in edge_target_table_, updated on swap-and-pop.
struct EdgeTargetHandle {
  uint32_t slot_id;
  SubKind kind;  // kEdge or kContainer
  uint32_t index;
};

// Cold state for EdgeSub rebind targets. Lives in edge_cold_pool_, indexed
// by EdgeSub.cold_idx. Only allocated when the edge sub is a rebind target.
struct EdgeTargetCold {
  uint32_t edge_target_id = UINT32_MAX;
  IndexPlanRef plan_ref = {};
  BitTargetMapping rebind_mapping = {};
  uint32_t last_rebind_epoch = 0;
  // Byte snapshot at the observed byte_offset for same-byte rebind detection.
  // When rebinding moves bit_index within the same byte, this preserves the
  // pre-change value so the edge pass detects the transition correctly.
  uint8_t edge_last_byte = 0;
  bool has_edge_last_byte = false;
};

// Cold state for ChangeSub large snapshots. Lives in change_cold_pool_,
// indexed by ChangeSub.cold_idx. Only allocated when byte_size exceeds
// the inline snapshot capacity.
struct ChangeSnapshotCold {
  std::vector<uint8_t> snapshot;
};

// Cold state for RebindWatcherSub. Lives in watcher_cold_pool_, indexed
// by RebindWatcherSub.cold_idx. Always allocated (holds edge_target_id
// and dep slot snapshot).
struct WatcherCold {
  uint32_t edge_target_id = UINT32_MAX;
  std::vector<uint8_t> snapshot;
};

// Cold state for ContainerSub. Lives in container_cold_pool_, indexed
// by ContainerSub.cold_idx. Always allocated (holds container runtime state
// and optional rebind target metadata).
struct ContainerCold {
  uint32_t container_base_off = UINT32_MAX;
  uint32_t container_elem_stride = 0;
  int64_t container_sv_index = 0;
  uint64_t container_epoch = 0;

  // Rebind target fields (only used when this container sub is a rebind
  // target, i.e. @(posedge d[i]) where d is a dynamic array/queue).
  uint32_t edge_target_id = UINT32_MAX;
  IndexPlanRef plan_ref = {};
  BitTargetMapping rebind_mapping = {};
  uint32_t last_rebind_epoch = 0;
};

// Dense hot-path record for posedge/negedge signal wakeup (32B).
// This is the dominant pipeline shape -- FlushSignalUpdates iterates
// a dense std::vector<EdgeSub> and touches only this layout.
struct EdgeSub {
  // Wakeup identity (12B)
  uint32_t process_id;
  uint32_t instance_id;
  uint32_t resume_block;

  // Observation target (8B)
  uint32_t byte_offset;
  uint32_t byte_size;

  // Edge / state (4B)
  uint8_t bit_index;
  common::EdgeKind edge;
  uint8_t last_bit;
  uint8_t flags;  // kActive=0x01, kHasCold=0x02

  // Removal / ownership (8B)
  uint32_t process_sub_idx;  // index in owning process_state.sub_refs
  uint32_t cold_idx;         // UINT32_MAX = no cold state (edge_cold_pool_)
};
static_assert(sizeof(EdgeSub) == 32);

// Dense record for kAnyChange subscribers (48B).
// Has a different hot path from edge detection -- splitting from EdgeSub
// keeps the dominant clock-edge path smaller.
struct ChangeSub {
  uint32_t process_id;
  uint32_t instance_id;
  uint32_t resume_block;
  uint32_t byte_offset;
  uint32_t byte_size;
  uint32_t process_sub_idx;
  uint32_t cold_idx;  // UINT32_MAX if inline snapshot only (change_cold_pool_)

  // Inline snapshot for small observed ranges.
  static constexpr uint32_t kInlineSnapshotCap = 16;
  std::array<uint8_t, kInlineSnapshotCap> snapshot_inline{};

  uint8_t flags;  // kActive=0x01
  std::array<uint8_t, 3> padding_{};
};
static_assert(sizeof(ChangeSub) == 48);

// Dense record for rebind watchers (pass 1 only) (24B).
// Logically a different pass -- lives in its own dense array so
// pass 2 never branches over them.
struct RebindWatcherSub {
  uint32_t process_id;
  uint32_t byte_offset;
  uint32_t byte_size;
  uint32_t process_sub_idx;
  uint32_t cold_idx;  // always valid (watcher_cold_pool_)
  uint32_t flags;     // kActive=0x01
};
static_assert(sizeof(RebindWatcherSub) == 24);

// Dense record for container element subscriptions (24B).
// Container subscriptions are rare and structurally different.
//
// Observation model: container subs observe bit 0 of byte 0 of the selected
// element (byte_off = sv_index * elem_stride within the heap-allocated data).
// This is sufficient because container edge triggers (@(posedge d[i])) are
// defined over the LSB of the element, matching the scalar edge semantics.
// Multi-bit or multi-byte element observation is not supported.
struct ContainerSub {
  uint32_t process_id;
  uint32_t instance_id;
  uint32_t resume_block;
  uint32_t process_sub_idx;
  uint32_t cold_idx;      // always valid (container_cold_pool_)
  common::EdgeKind edge;  // trigger kind (posedge/negedge/anychange)
  uint8_t last_bit;       // last observed bit value for edge detection
  uint8_t flags;          // kActive=0x01
  uint8_t padding_ = 0;
};
static_assert(sizeof(ContainerSub) == 24);

// Flag constants for sub flags fields.
inline constexpr uint8_t kSubActive = 0x01;
inline constexpr uint8_t kSubHasCold = 0x02;

// Dirty-range filter mode for per-slot edge/change subscriber processing.
// Decoded once per dirty slot to avoid repeated per-sub Overlaps() calls.
enum class RangeFilterMode : uint8_t {
  kNone,     // No relevant ranges (edge/change subs skipped)
  kFull,     // Full-extent dirty (all subs match, skip Overlaps)
  kPartial,  // Partial ranges (per-sub Overlaps required)
};

// Per-slot subscription storage. Four dense typed arrays.
// The flush path becomes four dense scans with no mixed-node branching.
struct SlotSubscriptions {
  std::vector<EdgeSub> edge_subs;
  std::vector<ChangeSub> change_subs;
  std::vector<RebindWatcherSub> rebind_subs;
  std::vector<ContainerSub> container_subs;
};

// Per-process installed wait-site cache.
// Tracks which compiled wait site is physically installed and the refresh
// policy derived from the compiled shape at install time.
struct InstalledWaitState {
  WaitSiteId wait_site_id = kInvalidWaitSiteId;
  bool valid = false;
  // Installed subscriptions are all snapshot-bearing (kEdge/kChange) and can
  // be maintained by updating snapshot state alone, without rebuilding
  // structural subscription shape. Derived from compiled shape at install
  // time: true iff shape is kStatic.
  bool can_refresh_snapshot = false;
};

// Per-process state (keyed by ProcessHandle).
struct ProcessState {
  bool is_enqueued = false;  // De-dup flag for next-delta queue
  size_t subscription_count = 0;
  std::vector<SubRef> sub_refs;
  IndexPlanPool plan_pool;
  InstalledWaitState installed_wait;

  // Snapshot refresh watermark: epoch + dirty count at last refresh.
  // If both match the current UpdateSet state, no new dirty slots appeared
  // since the last refresh and the scan can be skipped entirely.
  uint32_t last_refresh_epoch = 0;
  uint32_t last_refresh_dirty_count = 0;
};

}  // namespace lyra::runtime
