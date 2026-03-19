#pragma once

#include <cstdint>

// Versioned runtime descriptor passed from codegen to LyraRunSimulation.
// Consolidates metadata tables and flags into a single extensible struct,
// avoiding parameter explosion as new metadata is added.
//
// Layout stability: fields are append-only. New fields are added after
// existing ones. The version field gates which fields the runtime reads.
// Runtime ABI version history:
//   1: Initial version (slot meta, process meta, back-edge site meta,
//   connections,
//      comb kernels, feature flags)
//   2: Added wait-site metadata word table (wait_site_words,
//      wait_site_word_count)
//   3: Added comb kernel function pointers (comb_funcs, num_comb_funcs)
//   4: Added trace signal metadata (trace_signal_meta_words,
//      trace_signal_meta_word_count, trace_signal_meta_string_pool,
//      trace_signal_meta_string_pool_size)
//   5: Added trace output configuration (signal_trace_path)
//   6: Added process trigger metadata (process_trigger_words,
//      num_process_trigger_words)
//   7: Descriptor-driven process dispatch (process_descriptors,
//      num_process_descriptors, num_standalone_processes)
inline constexpr uint32_t kRuntimeAbiVersion = 7;

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

  // Connection descriptors for batched connection evaluation.
  const void* conn_descs;
  uint32_t num_conn_descs;

  // Comb kernel word table for batched always_comb evaluation.
  const uint32_t* comb_kernel_words;
  uint32_t num_comb_kernel_words;

  uint32_t feature_flags;

  // Wait-site metadata word table for persistent wait installation.
  const uint32_t* wait_site_words;
  uint32_t wait_site_word_count;

  // v3: Comb kernel function pointers (void-returning comb ABI, separate from
  // process return-code ABI). One per comb kernel, in word-table order.
  using LyraCombKernelFunc = void (*)(void*, uint32_t);
  LyraCombKernelFunc* comb_funcs;
  uint32_t num_comb_funcs;

  // v4: Trace signal metadata word table + string pool.
  const uint32_t* trace_signal_meta_words;
  uint32_t trace_signal_meta_word_count;
  const char* trace_signal_meta_string_pool;
  uint32_t trace_signal_meta_string_pool_size;

  // v5: Trace output configuration.
  // Text signal trace destination. Only meaningful when kEnableSignalTrace is
  // set in feature_flags. nullptr = write to stdout, non-null = file path.
  const char* signal_trace_path;

  // v6: Process trigger metadata word table for constructor-time
  // trigger-group formation (G13).
  const uint32_t* process_trigger_words;
  uint32_t num_process_trigger_words;

  // v7: Descriptor-driven module-process dispatch.
  // process_descriptors points to a constant codegen-emitted array of
  // fixed-size descriptor entries (one per module process).
  //
  // Authoritative entry layout (32 bytes, naturally aligned):
  //   offset  0: ptr    shared_body       (7-arg shared body function pointer)
  //   offset  8: u64    base_byte_offset  (instance base in design state)
  //   offset 16: u32    instance_id       (module instance index)
  //   offset 20: u32    base_slot_id      (signal ID offset)
  //   offset 24: ptr    unstable_offsets  (unstable offset table, or null)
  //
  // This layout is the source of truth. Codegen (GetDescriptorEntryType
  // in emit_design_main.cpp) and runtime (ProcessDescriptorEntry in
  // simulation.cpp) must both conform to it. The runtime side enforces
  // conformance via static_assert on sizeof and offsetof.
  //
  // num_process_descriptors is the module process count.
  // num_standalone_processes is the standalone (connection) process count.
  // Runtime contract: num_standalone + num_descriptors == num_processes.
  const void* process_descriptors;
  uint32_t num_process_descriptors;
  uint32_t num_standalone_processes;
};
