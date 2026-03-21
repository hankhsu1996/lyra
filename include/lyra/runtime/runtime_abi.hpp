#pragma once

#include <cstddef>
#include <cstdint>

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
inline constexpr uint32_t kRuntimeAbiVersion = 11;

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

  // Descriptor-driven module-process dispatch.
  // Canonical entry layout defined in process_descriptor.hpp
  // (ProcessDescriptorEntry). Codegen produces via GetDescriptorEntryType().
  //
  // num_process_descriptors is the module process count.
  // num_connection_processes is the connection process count.
  // Runtime contract: num_connection + num_descriptors == num_processes.
  const void* process_descriptors;
  uint32_t num_process_descriptors;
  uint32_t num_connection_processes;

  // Source of truth for design-state binding. Runtime uses this to populate
  // design_ptr in all persistent simulation-process headers. Per-header
  // design_ptr is runtime-populated cached binding derived from this field.
  void* design_state;
};

// Hard size/offset contract. If the struct layout changes (fields added,
// removed, or reordered), these assertions fail at compile time rather
// than manifesting as runtime SIGILL in AOT binaries.
static_assert(sizeof(LyraRuntimeAbi) == 216);
static_assert(offsetof(LyraRuntimeAbi, version) == 0);
static_assert(offsetof(LyraRuntimeAbi, process_descriptors) == 192);
static_assert(offsetof(LyraRuntimeAbi, num_connection_processes) == 204);
static_assert(offsetof(LyraRuntimeAbi, design_state) == 208);
