#include "lyra/runtime/queue.hpp"

#include <cstdint>
#include <cstdlib>
#include <cstring>

#include "lyra/runtime/dyn_array.hpp"
#include "lyra/runtime/dyn_array_data.hpp"

using LyraDynArrayData = lyra::runtime::DynArrayData;

namespace {

auto EnsureAllocated(
    void* handle_slot, int32_t elem_size, void (*clone_fn)(void*, const void*),
    void (*destroy_fn)(void*)) -> LyraDynArrayData* {
  auto** slot = static_cast<LyraDynArrayHandle*>(handle_slot);
  if (*slot == nullptr) {
    *slot = LyraDynArrayNew(0, elem_size, clone_fn, destroy_fn);
  }
  auto* arr = static_cast<LyraDynArrayData*>(*slot);
  if (arr->elem_size != elem_size || arr->clone_fn != clone_fn ||
      arr->destroy_fn != destroy_fn) {
    std::abort();
  }
  return arr;
}

void GrowBuffer(LyraDynArrayData* arr, int64_t new_size) {
  if (arr->elem_size <= 0) {
    std::abort();
  }
  auto alloc_size = static_cast<size_t>(new_size) * arr->elem_size;
  // NOLINTNEXTLINE(cppcoreguidelines-owning-memory,cppcoreguidelines-no-malloc)
  void* new_data = std::realloc(arr->data, alloc_size);
  if (new_data == nullptr) {
    std::abort();
  }
  arr->data = new_data;
}

void CloneElement(LyraDynArrayData* arr, void* dst, const void* src) {
  if (arr->clone_fn != nullptr) {
    arr->clone_fn(dst, src);
  } else {
    std::memcpy(dst, src, static_cast<size_t>(arr->elem_size));
  }
}

void TruncateFromBack(LyraDynArrayData* arr, uint32_t max_bound) {
  if (max_bound == 0) {
    return;
  }
  auto max_size = static_cast<int64_t>(max_bound) + 1;
  auto* base = static_cast<char*>(arr->data);
  while (arr->size > max_size) {
    arr->size--;
    if (arr->destroy_fn != nullptr) {
      // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic)
      arr->destroy_fn(base + (arr->size * arr->elem_size));
    }
  }
}

}  // namespace

extern "C" void LyraQueuePushBack(
    void* handle_slot, const void* elem, int32_t elem_size, uint32_t max_bound,
    void (*clone_fn)(void*, const void*), void (*destroy_fn)(void*)) {
  auto* arr = EnsureAllocated(handle_slot, elem_size, clone_fn, destroy_fn);

  GrowBuffer(arr, arr->size + 1);
  // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic)
  auto* dst = static_cast<char*>(arr->data) + (arr->size * arr->elem_size);
  CloneElement(arr, dst, elem);
  arr->size++;

  TruncateFromBack(arr, max_bound);
}

extern "C" void LyraQueuePushFront(
    void* handle_slot, const void* elem, int32_t elem_size, uint32_t max_bound,
    void (*clone_fn)(void*, const void*), void (*destroy_fn)(void*)) {
  auto* arr = EnsureAllocated(handle_slot, elem_size, clone_fn, destroy_fn);

  GrowBuffer(arr, arr->size + 1);
  // Shift existing elements right
  if (arr->size > 0) {
    std::memmove(
        // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic)
        static_cast<char*>(arr->data) + arr->elem_size, arr->data,
        static_cast<size_t>(arr->size) * arr->elem_size);
  }
  CloneElement(arr, arr->data, elem);
  arr->size++;

  TruncateFromBack(arr, max_bound);
}

extern "C" void LyraQueuePopBack(LyraDynArrayHandle q, void* out_elem) {
  if (q == nullptr) {
    return;
  }
  auto* arr = static_cast<LyraDynArrayData*>(q);
  if (arr->size == 0) {
    return;
  }
  arr->size--;
  // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic)
  auto* src = static_cast<char*>(arr->data) + (arr->size * arr->elem_size);
  std::memcpy(out_elem, src, static_cast<size_t>(arr->elem_size));
}

extern "C" void LyraQueuePopFront(LyraDynArrayHandle q, void* out_elem) {
  if (q == nullptr) {
    return;
  }
  auto* arr = static_cast<LyraDynArrayData*>(q);
  if (arr->size == 0) {
    return;
  }
  // Move first element out
  std::memcpy(out_elem, arr->data, static_cast<size_t>(arr->elem_size));
  arr->size--;
  // Shift remaining left
  if (arr->size > 0) {
    std::memmove(
        arr->data,
        // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic)
        static_cast<char*>(arr->data) + arr->elem_size,
        static_cast<size_t>(arr->size) * arr->elem_size);
  }
}

extern "C" void LyraQueueInsert(
    void* handle_slot, int64_t index, const void* elem, int32_t elem_size,
    uint32_t max_bound, void (*clone_fn)(void*, const void*),
    void (*destroy_fn)(void*)) {
  auto* arr = EnsureAllocated(handle_slot, elem_size, clone_fn, destroy_fn);

  if (index < 0 || index > arr->size) {
    return;
  }

  GrowBuffer(arr, arr->size + 1);
  // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic)
  auto* insert_ptr = static_cast<char*>(arr->data) + (index * arr->elem_size);
  // Shift elements after index right
  int64_t tail_count = arr->size - index;
  if (tail_count > 0) {
    std::memmove(
        // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic)
        insert_ptr + arr->elem_size, insert_ptr,
        static_cast<size_t>(tail_count) * arr->elem_size);
  }
  CloneElement(arr, insert_ptr, elem);
  arr->size++;

  TruncateFromBack(arr, max_bound);
}

extern "C" void LyraQueueDeleteAt(LyraDynArrayHandle q, int64_t index) {
  if (q == nullptr) {
    return;
  }
  auto* arr = static_cast<LyraDynArrayData*>(q);
  if (index < 0 || index >= arr->size) {
    return;
  }
  // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic)
  auto* elem_ptr = static_cast<char*>(arr->data) + (index * arr->elem_size);
  if (arr->destroy_fn != nullptr) {
    arr->destroy_fn(elem_ptr);
  }
  int64_t tail_count = arr->size - index - 1;
  if (tail_count > 0) {
    std::memmove(
        // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic)
        elem_ptr, elem_ptr + arr->elem_size,
        static_cast<size_t>(tail_count) * arr->elem_size);
  }
  arr->size--;
}
