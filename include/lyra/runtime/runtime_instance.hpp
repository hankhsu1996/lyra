#pragma once

#include <cstddef>
#include <cstdint>
#include <span>
#include <string>
#include <vector>

#include "lyra/common/ext_ref_binding.hpp"
#include "lyra/common/selection_step.hpp"
#include "lyra/runtime/instance_event_state.hpp"
#include "lyra/runtime/instance_observability.hpp"
#include "lyra/runtime/process_frame.hpp"
#include "lyra/runtime/signal_coord.hpp"

namespace lyra::runtime {

// Per-instance owned storage.
// Each RuntimeInstance owns its module-local state. Storage is always
// heap-allocated via AllocateOwnedStorage and freed by
// FreeRuntimeInstanceStorage.
// The inline region holds fixed-offset slot data; the appendix region holds
// backing data for owned containers.
//
// Binary contract: field order and types must match the LLVM struct type
// emitted by BuildRuntimeInstanceStorageType. Codegen accesses these fields
// via GEP, so the layout is a hard ABI. Raw pointers are intentional --
// unique_ptr would change the struct layout.
struct RuntimeInstanceStorage {
  uint8_t* inline_base = nullptr;
  uint64_t inline_size = 0;

  uint8_t* appendix_base = nullptr;
  uint64_t appendix_size = 0;

  // Deferred inline region: mirrors inline_base layout for NBA writes.
  // Local owned-inline <= writes go here; committed in the NBA phase.
  uint8_t* deferred_inline_base = nullptr;

  // Deferred appendix region: mirrors appendix_base layout for NBA writes.
  // Local owned-container <= writes go here; committed in the NBA phase.
  uint8_t* deferred_appendix_base = nullptr;

  // Span accessors for owned storage regions. Non-const overloads return
  // mutable spans for write access; const overloads return read-only spans.
  // The non-const versions do not modify the struct itself (shallow const),
  // but provide mutable access to the pointed-to storage -- same pattern as
  // std::vector::data().
  // NOLINTBEGIN(readability-make-member-function-const)
  [[nodiscard]] auto InlineRegion() -> std::span<uint8_t> {
    return {inline_base, inline_size};
  }
  [[nodiscard]] auto AppendixRegion() -> std::span<uint8_t> {
    return {appendix_base, appendix_size};
  }
  [[nodiscard]] auto DeferredInlineRegion() -> std::span<uint8_t> {
    return {deferred_inline_base, inline_size};
  }
  [[nodiscard]] auto DeferredAppendixRegion() -> std::span<uint8_t> {
    return {deferred_appendix_base, appendix_size};
  }
  // NOLINTEND(readability-make-member-function-const)
  [[nodiscard]] auto InlineRegion() const -> std::span<const uint8_t> {
    return {inline_base, inline_size};
  }
  [[nodiscard]] auto DeferredInlineRegion() const -> std::span<const uint8_t> {
    return {deferred_inline_base, inline_size};
  }
};

// Free any owned storage in an instance's storage record.
void FreeRuntimeInstanceStorage(RuntimeInstanceStorage& storage);

// Per-instance set of local signals with pending deferred (NBA) writes.
// Lightweight sparse-set: O(1) mark, O(pending_count) iterate and clear.
//
// All local owned NBA writes (inline and container-backed) go through
// instance-owned deferred storage. The pending set tracks which signals
// have uncommitted writes and whether the deferred slot has been
// initialized this delta (for copy-on-first-touch of partial writes).
struct NbaPendingSet {
  std::vector<uint8_t> seen;
  std::vector<LocalSignalId> list;
  // Per-signal: whether deferred storage contains a valid full-slot
  // snapshot for this signal in the current delta. Set on first write
  // (whole-slot sets implicitly; partial triggers copy-on-first-touch).
  // Reset in Clear().
  std::vector<uint8_t> slot_initialized;
  bool initialized = false;

  void Init(uint32_t local_signal_count) {
    seen.assign(local_signal_count, 0);
    slot_initialized.assign(local_signal_count, 0);
    list.reserve(local_signal_count);
    initialized = true;
  }

  void MarkPending(LocalSignalId lid) {
    if (seen[lid.value] == 0) {
      seen[lid.value] = 1;
      list.push_back(lid);
    }
  }

  void Clear() {
    for (auto lid : list) {
      seen[lid.value] = 0;
      slot_initialized[lid.value] = 0;
    }
    list.clear();
  }

  [[nodiscard]] auto IsInitialized() const -> bool {
    return initialized;
  }
};

// Per-instance scope time metadata (immutable after init).
// Populated from BodyRealizationDesc during InitInstanceTimeMetadata.
struct RuntimeScopeTimeMetadata {
  int8_t time_unit_power = 0;
  int8_t time_precision_power = 0;
};

// Transient per-instance scratch state for the fixpoint solver.
// Object-owned but engine-managed: zeroed at the start of each
// FlushAndPropagateConnections call, read/written during fixpoint
// iteration, carries no semantic state across calls.
struct RuntimeFixpointScratch {
  bool in_next = false;
  bool comb_touched_seen = false;
  uint32_t delta_pre = 0;
};

// Per-instance sparse-set dedup flags for engine dirty/pending indexes.
// Object-owned on RuntimeInstance, engine-managed: set by
// MarkInstanceDeltaDirty / MarkInstanceNbaPending, cleared during
// ClearLocalUpdatesDelta / ClearLocalUpdates / CommitDeferredLocalNbas.
struct RuntimeDedupState {
  bool in_delta_dirty = false;
  bool in_timeslot_dirty = false;
  bool in_nba_pending = false;
};

// Per-instance local-domain fixpoint workspace.
// Owns the pending/next worklists and dedup flags for the fixpoint
// solver's local signal propagation. Object-owned on RuntimeInstance,
// engine-managed: seeded at the start of FlushAndPropagateConnections,
// promoted each iteration, cleared on convergence.
struct RuntimeLocalFixpointWorkspace {
  std::vector<LocalSignalId> pending;
  std::vector<LocalSignalId> next;
  // Dedup flags sized to local_signal_count. 1 iff signal is in next.
  std::vector<uint8_t> seen;
};

// Runtime-owned representation of one module instance.
//
// This is the first-class object model introduced by R1. A RuntimeInstance
// owns its module-local state, carries stable identity for runtime services
// (%m, observers), and is the execution anchor for module-local process code.
//
// Replaces the prior de facto spread of instance identity across
// StagedProcess, ProcessFrameHeader, InstanceLedgerEntry, and arena offsets.
//
// Binary contract: field order and types must match the LLVM struct type
// emitted by BuildRuntimeInstanceType / BuildRuntimeInstanceStorageType
// in layout.cpp. The field enums below are the single canonical source of
// field ordering for codegen GEP access.
struct RuntimeInstance {
  ~RuntimeInstance() {
    FreeRuntimeInstanceStorage(storage);
  }

  RuntimeInstance() = default;
  RuntimeInstance(const RuntimeInstance&) = delete;
  auto operator=(const RuntimeInstance&) -> RuntimeInstance& = delete;
  RuntimeInstance(RuntimeInstance&&) = delete;
  auto operator=(RuntimeInstance&&) -> RuntimeInstance& = delete;

  InstanceId instance_id = InstanceId{0};
  SharedBodyFn body = nullptr;

  RuntimeInstanceStorage storage;

  const char* path_c_str = nullptr;
  uint32_t owner_ordinal = 0;

  // Process binding: all module processes for this instance share one object.
  uint32_t module_proc_base = 0;
  uint32_t num_module_processes = 0;

  // Per-instance resolved ext-ref binding records.
  // One entry per external-ref recipe in the body. Each record carries
  // storage slot (address), target instance, and target local signal
  // (behavioral identity). Null if the body has no external refs.
  // Part of the binary contract with codegen (accessed via GEP).
  const common::ResolvedExtRefBinding* ext_ref_bindings = nullptr;
  uint32_t ext_ref_binding_count = 0;

  // Primary structural hierarchy (not part of binary contract with codegen).
  // Established during constructor assembly: CreateChild sets parent and
  // appends a typed RuntimeChildEdge to parent->children in a single
  // creation path.
  RuntimeInstance* parent = nullptr;

  // Typed structural child edges. Each edge carries structural metadata
  // (generate-scope context + ordinal within that context) for procedural
  // walking by compile-time path recipes. These are edge metadata, not
  // object identity -- they enable navigation without key lookup.
  struct ChildEdge {
    common::RepertoireCoord coord;
    uint32_t child_ordinal_in_coord = 0;
    RuntimeInstance* child = nullptr;
  };
  std::vector<ChildEdge> children;

  // Instance-owned hierarchical path string. Derived from structural
  // position during assembly (parent path + child display label).
  // The display label is consumed during assembly and not stored.
  // Owns the storage that path_c_str points into.
  // Not part of the binary contract with codegen.
  std::string path_storage;

  // R5: Per-instance observability state.
  // Populated by Engine::InitModuleInstancesFromBundles. Not part of the
  // binary contract with codegen (not accessed via GEP, no LLVM struct type).
  RuntimeInstanceObservability observability;

  // L8a: Per-instance named event runtime state.
  // Populated by Engine::InitModuleInstancesFromBundles from body-local
  // event count. Not part of the binary contract with codegen.
  RuntimeInstanceEventState event_state;

  // Per-instance pending NBA set for instance-owned deferred writes.
  // Tracks which local signals have pending deferred values in
  // storage.deferred_inline_base. Not part of the binary contract.
  NbaPendingSet nba_pending;

  // Fixpoint solver scratch state (not part of the binary contract).
  RuntimeFixpointScratch fixpoint_scratch;

  // Per-instance sparse-set dedup flags (not part of the binary contract).
  RuntimeDedupState dedup_state;

  // Per-instance local-domain fixpoint workspace (not part of the
  // binary contract). Owns pending/next worklists and dedup flags.
  RuntimeLocalFixpointWorkspace local_fixpoint;

  // Per-instance scope time metadata (immutable after init).
  // Populated from BodyRealizationDesc during InitInstanceTimeMetadata.
  // Not part of the binary contract with codegen.
  RuntimeScopeTimeMetadata scope_time_metadata;
};

// Strongly typed field indices for RuntimeInstanceStorage.
// Must match the LLVM struct type emitted by BuildRuntimeInstanceStorageType.
enum class RuntimeInstanceStorageField : unsigned {
  kInlineBase = 0,
  kInlineSize = 1,
  kAppendixBase = 2,
  kAppendixSize = 3,
  kDeferredInlineBase = 4,
  kDeferredAppendixBase = 5,
  kFieldCount = 6,
};

// Strongly typed field indices for RuntimeInstance.
// Must match the LLVM struct type emitted by BuildRuntimeInstanceType.
enum class RuntimeInstanceField : unsigned {
  kInstanceId = 0,
  kBody = 1,
  kStorage = 2,
  kPathCStr = 3,
  kOwnerOrdinal = 4,
  kModuleProcBase = 5,
  kNumModuleProcesses = 6,
  kExtRefBindings = 7,
  kExtRefBindingCount = 8,
  kFieldCount = 9,
};

// Hard binary contract assertions for RuntimeInstanceStorage.
// If the struct layout changes, these fail at compile time.
static_assert(offsetof(RuntimeInstanceStorage, inline_base) == 0);
static_assert(
    offsetof(RuntimeInstanceStorage, inline_size) ==
    offsetof(RuntimeInstanceStorage, inline_base) + sizeof(uint8_t*));
static_assert(
    offsetof(RuntimeInstanceStorage, appendix_base) ==
    offsetof(RuntimeInstanceStorage, inline_size) + sizeof(uint64_t));
static_assert(
    offsetof(RuntimeInstanceStorage, appendix_size) ==
    offsetof(RuntimeInstanceStorage, appendix_base) + sizeof(uint8_t*));
static_assert(
    offsetof(RuntimeInstanceStorage, deferred_inline_base) ==
    offsetof(RuntimeInstanceStorage, appendix_size) + sizeof(uint64_t));
static_assert(
    offsetof(RuntimeInstanceStorage, deferred_appendix_base) ==
    offsetof(RuntimeInstanceStorage, deferred_inline_base) + sizeof(uint8_t*));

// Hard binary contract assertions for RuntimeInstance.
// Field order must match RuntimeInstanceField enum and LLVM struct type.
static_assert(offsetof(RuntimeInstance, instance_id) == 0);
static_assert(
    offsetof(RuntimeInstance, body) > offsetof(RuntimeInstance, instance_id));
static_assert(
    offsetof(RuntimeInstance, storage) ==
    offsetof(RuntimeInstance, body) + sizeof(SharedBodyFn));
static_assert(
    offsetof(RuntimeInstance, path_c_str) ==
    offsetof(RuntimeInstance, storage) + sizeof(RuntimeInstanceStorage));
static_assert(
    offsetof(RuntimeInstance, owner_ordinal) ==
    offsetof(RuntimeInstance, path_c_str) + sizeof(const char*));
static_assert(
    offsetof(RuntimeInstance, module_proc_base) ==
    offsetof(RuntimeInstance, owner_ordinal) + sizeof(uint32_t));
static_assert(
    offsetof(RuntimeInstance, num_module_processes) ==
    offsetof(RuntimeInstance, module_proc_base) + sizeof(uint32_t));
static_assert(
    offsetof(RuntimeInstance, ext_ref_bindings) >
    offsetof(RuntimeInstance, num_module_processes));

// Allocate zero-initialized owned storage for an instance's inline region.
auto AllocateOwnedInlineStorage(uint64_t size) -> uint8_t*;

// Allocate zero-initialized owned storage for an instance's appendix region.
auto AllocateOwnedAppendixStorage(uint64_t size) -> uint8_t*;

}  // namespace lyra::runtime
