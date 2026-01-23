#pragma once

#include <cstdint>

using LyraDynArrayHandle = void*;

extern "C" {

// Allocate a new dynamic array with `size` elements, zero-initialized.
auto LyraDynArrayNew(
    int64_t size, int32_t elem_size, void (*clone_fn)(void*, const void*),
    void (*destroy_fn)(void*)) -> LyraDynArrayHandle;

// Allocate a new dynamic array, copying min(size, src.size) elements from src.
// Remaining elements are zero-initialized. src may be null (treated as empty).
auto LyraDynArrayNewCopy(
    int64_t size, int32_t elem_size, void (*clone_fn)(void*, const void*),
    void (*destroy_fn)(void*), LyraDynArrayHandle src) -> LyraDynArrayHandle;

// Returns 0 if null, else the number of live elements.
auto LyraDynArraySize(LyraDynArrayHandle arr) -> int64_t;

// Returns pointer to element at index. UB if null or out-of-bounds.
auto LyraDynArrayElementPtr(LyraDynArrayHandle arr, int64_t index) -> void*;

// Deep-copy the array. Null-safe (returns null).
auto LyraDynArrayClone(LyraDynArrayHandle src) -> LyraDynArrayHandle;

// Delete: destroy elements, free buffer, set size=0. Handle remains valid.
void LyraDynArrayDelete(LyraDynArrayHandle arr);

// Release: destroy elements, free buffer, free struct. Null-safe.
void LyraDynArrayRelease(LyraDynArrayHandle arr);

// Store new handle to design slot + notify engine unconditionally.
// Does NOT release old handle (caller's responsibility).
void LyraStoreDynArray(
    void* engine, void* slot, void* new_handle, uint32_t signal_id);

// Element-level wrappers for nested dynamic arrays.
void LyraDynArrayCloneElem(void* dst, const void* src);
void LyraDynArrayDestroyElem(void* elem);
}
