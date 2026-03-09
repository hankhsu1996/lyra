#pragma once

#include <cstdint>

// Versioned runtime descriptor passed from codegen to LyraRunSimulation.
// Consolidates metadata tables and flags into a single extensible struct,
// avoiding parameter explosion as new metadata is added.
//
// Layout stability: fields are append-only. New fields are added after
// existing ones. The version field gates which fields the runtime reads.
// Runtime ABI version history:
//   1: Initial version (slot meta, process meta, loop site meta, connections,
//      comb kernels, feature flags)
//   2: Added wait-site metadata word table (wait_site_words,
//   wait_site_word_count)
inline constexpr uint32_t kRuntimeAbiVersion = 2;

struct LyraRuntimeAbi {
  uint32_t version;  // = kRuntimeAbiVersion

  const uint32_t* slot_meta_words;
  uint32_t slot_meta_word_count;

  const uint32_t* process_meta_words;
  uint32_t process_meta_word_count;
  const char* process_meta_string_pool;
  uint32_t process_meta_string_pool_size;

  const uint32_t* loop_site_meta_words;
  uint32_t loop_site_meta_word_count;
  const char* loop_site_meta_string_pool;
  uint32_t loop_site_meta_string_pool_size;

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
};
