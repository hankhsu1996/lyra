#pragma once

#include <cstddef>
#include <cstdint>

namespace lyra::runtime {
struct InstanceMetadataBundle;
struct RuntimeInstance;
}  // namespace lyra::runtime

// Versioned runtime descriptor passed from codegen to LyraRunSimulation.
// Consolidates metadata tables and flags into a single extensible struct,
// avoiding parameter explosion as new metadata is added.
//
// This is an internal ABI with no external consumers. Codegen and runtime
// are always built from the same source tree. Fields can be added, removed,
// or reordered across versions without backward-compatibility concern.
//
// Runtime ABI version history:
//   1-6: (historical, see git history)
//   7: Descriptor-driven process dispatch
//   8: Removed comb_funcs/num_comb_funcs (comb dispatch is now runtime-owned
//      via descriptor resolution). Shared body ABI narrowed to 2-arg
//      (frame, resume). Descriptor shared_body pointers now point to
//      2-arg shared body functions.
//   9: Added design_state. Runtime owns all persistent simulation-process
//      header binding initialization. Codegen no longer writes design_ptr
//      into simulation-process headers.
// v10: Removed unstable_offsets from ProcessFrameHeader and
//      ProcessDescriptorEntry. Owned-container slots use inline
//      OwnedStorageHandle + appendix backing instead.
// v11: Connection-only function array. Null-padded __lyra_module_funcs
//      removed. Process state construction moved to runtime constructor.
//      Standalone terminology replaced by connection throughout ABI.
// v12: Removed process_descriptors and num_process_descriptors. Process
//      binding is now constructor-owned via RuntimeConstructor. Only
//      num_connection_processes remains for dispatch partition.
// v13: Process metadata production moved to constructor-time realization.
//      process_meta_words/pool ABI fields now populated from
//      ConstructionResult instead of compile-time LLVM globals.
//      Constructor API extended: BeginBody takes metadata template,
//      AddInstance takes instance_path.
// v14: Trigger/comb metadata production moved to constructor-time
//      realization. process_trigger_words and comb_kernel_words ABI
//      fields now populated from ConstructionResult instead of
//      compile-time LLVM globals. Constructor API extended:
//      Create takes connection trigger template, BeginBody takes
//      trigger/comb templates.
// v15: Slot/trace metadata and instance-path ownership moved to
//      constructor-time realization. slot_meta_words, trace_signal_meta,
//      and instance paths now populated from ConstructionResult.
//      Constructor API extended: Create takes package observable
//      descriptor template, BeginBody takes body observable descriptor
//      template. Legacy static __lyra_slot_meta_table,
//      __lyra_trace_signal_meta_table, and __lyra_instance_paths globals
//      removed.
// v16: Design-state initialization moved to constructor-time realization.
//      4-state X-encoding, owned-container handle construction, owned
//      backing init, and parameter stores are now constructor-owned via
//      body/package init descriptors. Emitted design main only performs
//      arena zeroing. Arena layout restructured: each instance's appendix
//      is contiguous with its inline region for body-relative addressing.
//      Constructor API extended: Create takes package init descriptors,
//      BeginBody takes body init descriptors, AddInstance takes
//      per-instance param payload.
// v17: Body descriptor carries direct body-sized state facts
//      (inline_state_size_bytes, appendix_state_size_bytes,
//      total_state_size_bytes). Runtime constructor derives instance
//      byte placement from body-sized arithmetic instead of
//      design-global slot byte offset oracle.
// v19: R4 per-instance metadata bundles. Module-instance slot, trigger,
//      and comb metadata are derived from bundles by the engine. Flat
//      slot_meta/process_trigger/comb handoff fields now contain
//      connection/design-global data only.
// v20: A1b immediate cover site count for runtime hit-count array sizing.
// v21: D6d simulation-global precision power and per-body timescale metadata.
inline constexpr uint32_t kRuntimeAbiVersion = 21;

struct LyraRuntimeAbi {
  uint32_t version;  // = kRuntimeAbiVersion

  const uint32_t* slot_meta_words;
  uint32_t slot_meta_word_count;

  const uint32_t* process_meta_words;
  uint32_t process_meta_word_count;
  const char* process_meta_string_pool;
  uint32_t process_meta_string_pool_size;

  const uint32_t* back_edge_site_meta_words;
  uint32_t back_edge_site_meta_word_count;
  const char* back_edge_site_meta_string_pool;
  uint32_t back_edge_site_meta_string_pool_size;

  const void* conn_descs;
  uint32_t num_conn_descs;

  const uint32_t* comb_kernel_words;
  uint32_t num_comb_kernel_words;

  uint32_t feature_flags;

  const uint32_t* wait_site_words;
  uint32_t wait_site_word_count;

  const uint32_t* trace_signal_meta_words;
  uint32_t trace_signal_meta_word_count;
  const char* trace_signal_meta_string_pool;
  uint32_t trace_signal_meta_string_pool_size;

  // Text signal trace destination. nullptr = stdout, non-null = file path.
  const char* signal_trace_path;

  const uint32_t* process_trigger_words;
  uint32_t num_process_trigger_words;

  // Dispatch partition boundary: processes [0, num_connection) are connection
  // processes. Currently from compile-time layout; will migrate to
  // constructor result when remaining dispatch ownership moves.
  uint32_t num_connection_processes;

  // Design-global service root. No longer the owner of module-local state;
  // module-local owned state lives in RuntimeInstance heap storage.
  // Retained for: package/global slot access, forwarded-slot compatibility,
  // and coordination subsystem design-global paths.
  void* design_state;

  // Instance pointer array for slot storage resolution.
  // Indexed by instance_id. Populated from ConstructionResult::instance_ptrs.
  const lyra::runtime::RuntimeInstance* const* instance_ptrs;
  uint32_t num_instances;

  // R4: Per-instance metadata bundles for engine-derived registries.
  // Module-instance slot, trigger, and comb metadata are derived from
  // these bundles. Populated from ConstructionResult::instance_bundles.
  const lyra::runtime::InstanceMetadataBundle* instance_bundles;
  uint32_t num_instance_bundles;
  uint32_t pad_bundles;

  // A1b: Number of immediate cover sites for runtime hit-count array sizing.
  uint32_t num_immediate_cover_sites;
  uint32_t pad_cover;

  // D6d: Simulation-global precision power (e.g., -12 for 1ps).
  // Set from mir::Design::global_precision_power at emission time.
  int8_t global_precision_power;
};

// Hard size/offset contract.
static_assert(sizeof(LyraRuntimeAbi) == 248);
static_assert(offsetof(LyraRuntimeAbi, version) == 0);
static_assert(offsetof(LyraRuntimeAbi, num_connection_processes) == 188);
static_assert(offsetof(LyraRuntimeAbi, design_state) == 192);
static_assert(offsetof(LyraRuntimeAbi, instance_ptrs) == 200);
static_assert(offsetof(LyraRuntimeAbi, num_instances) == 208);
static_assert(offsetof(LyraRuntimeAbi, instance_bundles) == 216);
static_assert(offsetof(LyraRuntimeAbi, num_instance_bundles) == 224);
static_assert(offsetof(LyraRuntimeAbi, num_immediate_cover_sites) == 232);
static_assert(offsetof(LyraRuntimeAbi, global_precision_power) == 240);
