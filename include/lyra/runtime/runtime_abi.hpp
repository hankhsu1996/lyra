#pragma once

#include <cstdint>

// Versioned runtime descriptor passed from codegen to LyraRunSimulation.
// Consolidates metadata tables and flags into a single extensible struct,
// avoiding parameter explosion as new metadata is added.
//
// Layout stability: fields are append-only. New fields are added after
// existing ones. The version field gates which fields the runtime reads.
struct LyraRuntimeAbi {
  uint32_t version;  // = 1

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
};
