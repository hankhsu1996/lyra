#pragma once

#include <cstdint>

#include "lyra/runtime/dyn_array.hpp"

extern "C" {

// Push/insert: take handle_slot (auto-allocate if *handle_slot == nullptr)
void LyraQueuePushBack(
    void* handle_slot, const void* elem, int32_t elem_size, uint32_t max_bound,
    void (*clone_fn)(void*, const void*), void (*destroy_fn)(void*));

void LyraQueuePushFront(
    void* handle_slot, const void* elem, int32_t elem_size, uint32_t max_bound,
    void (*clone_fn)(void*, const void*), void (*destroy_fn)(void*));

void LyraQueueInsert(
    void* handle_slot, int64_t index, const void* elem, int32_t elem_size,
    uint32_t max_bound, void (*clone_fn)(void*, const void*),
    void (*destroy_fn)(void*));

// Pop/delete: take handle directly (null-safe, no-op if null/empty)
void LyraQueuePopBack(LyraDynArrayHandle q, void* out_elem);
void LyraQueuePopFront(LyraDynArrayHandle q, void* out_elem);
void LyraQueueDeleteAt(LyraDynArrayHandle q, int64_t index);
}
