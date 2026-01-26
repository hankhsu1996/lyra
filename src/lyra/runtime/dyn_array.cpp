#include "lyra/runtime/dyn_array.hpp"

#include <algorithm>
#include <cstdlib>
#include <cstring>

#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/dyn_array_data.hpp"
#include "lyra/runtime/engine.hpp"

using LyraDynArrayData = lyra::runtime::DynArrayData;

namespace {

void DestroyElements(LyraDynArrayData* arr) {
  if (arr->data == nullptr && arr->size != 0) {
    throw lyra::common::InternalError(
        "DestroyElements", "corrupt state: null data with non-zero size");
  }
  if (arr->destroy_fn == nullptr || arr->data == nullptr) {
    return;
  }
  auto* base = static_cast<char*>(arr->data);
  for (int64_t i = 0; i < arr->size; ++i) {
    // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic)
    arr->destroy_fn(base + (i * arr->elem_size));
  }
}

void CloneElements(
    void* dst_buf, const void* src_buf, int64_t count, int32_t elem_size,
    void (*clone_fn)(void*, const void*)) {
  auto* dst = static_cast<char*>(dst_buf);
  const auto* src = static_cast<const char*>(src_buf);
  if (clone_fn == nullptr) {
    std::memcpy(dst, src, static_cast<size_t>(count) * elem_size);
  } else {
    for (int64_t i = 0; i < count; ++i) {
      // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic)
      clone_fn(dst + (i * elem_size), src + (i * elem_size));
    }
  }
}

}  // namespace

extern "C" auto LyraDynArrayNew(
    int64_t size, int32_t elem_size, void (*clone_fn)(void*, const void*),
    void (*destroy_fn)(void*)) -> LyraDynArrayHandle {
  if (size < 0) {
    throw lyra::common::InternalError(
        "LyraDynArrayNew", "array size must be non-negative");
  }
  if (elem_size <= 0) {
    throw lyra::common::InternalError(
        "LyraDynArrayNew", "element size must be positive");
  }

  // NOLINTNEXTLINE(cppcoreguidelines-owning-memory)
  auto* arr = new LyraDynArrayData();
  arr->size = size;
  arr->elem_size = elem_size;
  arr->clone_fn = clone_fn;
  arr->destroy_fn = destroy_fn;

  if (size > 0) {
    if (static_cast<size_t>(size) > SIZE_MAX / elem_size) {
      throw lyra::common::InternalError(
          "LyraDynArrayNew", "allocation size overflow");
    }
    auto alloc_size = static_cast<size_t>(size) * elem_size;
    // NOLINTNEXTLINE(cppcoreguidelines-owning-memory,cppcoreguidelines-no-malloc)
    arr->data = std::malloc(alloc_size);
    if (arr->data == nullptr) {
      std::abort();
    }
    std::memset(arr->data, 0, alloc_size);
  } else {
    arr->data = nullptr;
  }

  return arr;
}

extern "C" auto LyraDynArrayNewCopy(
    int64_t size, int32_t elem_size, void (*clone_fn)(void*, const void*),
    void (*destroy_fn)(void*), LyraDynArrayHandle src) -> LyraDynArrayHandle {
  if (size < 0) {
    throw lyra::common::InternalError(
        "LyraDynArrayNewCopy", "array size must be non-negative");
  }
  if (elem_size <= 0) {
    throw lyra::common::InternalError(
        "LyraDynArrayNewCopy", "element size must be positive");
  }

  // NOLINTNEXTLINE(cppcoreguidelines-owning-memory)
  auto* arr = new LyraDynArrayData();
  arr->size = size;
  arr->elem_size = elem_size;
  arr->clone_fn = clone_fn;
  arr->destroy_fn = destroy_fn;

  if (size > 0) {
    if (static_cast<size_t>(size) > SIZE_MAX / elem_size) {
      throw lyra::common::InternalError(
          "LyraDynArrayNewCopy", "allocation size overflow");
    }
    auto alloc_size = static_cast<size_t>(size) * elem_size;
    // NOLINTNEXTLINE(cppcoreguidelines-owning-memory,cppcoreguidelines-no-malloc)
    arr->data = std::malloc(alloc_size);
    if (arr->data == nullptr) {
      std::abort();
    }
    std::memset(arr->data, 0, alloc_size);

    if (src != nullptr) {
      auto* src_arr = static_cast<LyraDynArrayData*>(src);
      if (src_arr->elem_size != elem_size) {
        throw lyra::common::InternalError(
            "LyraDynArrayNewCopy",
            "source and destination element sizes must match");
      }
      int64_t copy_count = std::min(size, src_arr->size);
      if (copy_count > 0) {
        CloneElements(
            arr->data, src_arr->data, copy_count, src_arr->elem_size,
            src_arr->clone_fn);
      }
    }
  } else {
    arr->data = nullptr;
  }

  return arr;
}

extern "C" auto LyraDynArraySize(LyraDynArrayHandle arr) -> int64_t {
  if (arr == nullptr) {
    return 0;
  }
  return static_cast<LyraDynArrayData*>(arr)->size;
}

extern "C" auto LyraDynArrayElementPtr(LyraDynArrayHandle arr, int64_t index)
    -> void* {
  if (arr == nullptr) {
    throw lyra::common::InternalError(
        "LyraDynArrayElementPtr", "ElementPtr called on null handle");
  }
  auto* a = static_cast<LyraDynArrayData*>(arr);
  if (a->data == nullptr) {
    throw lyra::common::InternalError(
        "LyraDynArrayElementPtr", "ElementPtr called on empty array");
  }
  if (index < 0 || index >= a->size) {
    throw lyra::common::InternalError(
        "LyraDynArrayElementPtr", "index out of bounds");
  }
  // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic)
  return static_cast<char*>(a->data) + (index * a->elem_size);
}

extern "C" auto LyraDynArrayClone(LyraDynArrayHandle src)
    -> LyraDynArrayHandle {
  if (src == nullptr) {
    return nullptr;
  }
  auto* s = static_cast<LyraDynArrayData*>(src);

  // NOLINTNEXTLINE(cppcoreguidelines-owning-memory)
  auto* arr = new LyraDynArrayData();
  arr->size = s->size;
  arr->elem_size = s->elem_size;
  arr->clone_fn = s->clone_fn;
  arr->destroy_fn = s->destroy_fn;

  if (s->size > 0) {
    if (static_cast<size_t>(s->size) > SIZE_MAX / s->elem_size) {
      throw lyra::common::InternalError(
          "LyraDynArrayClone", "allocation size overflow");
    }
    auto alloc_size = static_cast<size_t>(s->size) * s->elem_size;
    // NOLINTNEXTLINE(cppcoreguidelines-owning-memory,cppcoreguidelines-no-malloc)
    arr->data = std::malloc(alloc_size);
    if (arr->data == nullptr) {
      std::abort();
    }
    CloneElements(arr->data, s->data, s->size, s->elem_size, s->clone_fn);
  } else {
    arr->data = nullptr;
  }

  return arr;
}

extern "C" void LyraDynArrayDelete(LyraDynArrayHandle arr) {
  if (arr == nullptr) {
    return;
  }
  auto* a = static_cast<LyraDynArrayData*>(arr);
  if (a->data == nullptr && a->size == 0) {
    return;
  }
  DestroyElements(a);
  // NOLINTNEXTLINE(cppcoreguidelines-owning-memory,cppcoreguidelines-no-malloc)
  std::free(a->data);
  a->data = nullptr;
  a->size = 0;
}

extern "C" void LyraDynArrayRelease(LyraDynArrayHandle arr) {
  if (arr == nullptr) {
    return;
  }
  auto* a = static_cast<LyraDynArrayData*>(arr);
  DestroyElements(a);
  // NOLINTNEXTLINE(cppcoreguidelines-owning-memory,cppcoreguidelines-no-malloc)
  std::free(a->data);
  // NOLINTNEXTLINE(cppcoreguidelines-owning-memory)
  delete a;
}

extern "C" void LyraStoreDynArray(
    void* engine_ptr, void* slot_ptr, void* new_handle, uint32_t signal_id) {
  auto** handle_slot = static_cast<void**>(slot_ptr);
  *handle_slot = new_handle;

  if (engine_ptr != nullptr) {
    auto* engine = static_cast<lyra::runtime::Engine*>(engine_ptr);
    engine->NotifyChange(signal_id, false, new_handle != nullptr, true);
  }
}

extern "C" void LyraDynArrayCloneElem(void* dst, const void* src) {
  auto* const* src_handle = static_cast<void* const*>(src);
  auto** dst_handle = static_cast<void**>(dst);
  *dst_handle = LyraDynArrayClone(*src_handle);
}

extern "C" void LyraDynArrayDestroyElem(void* elem) {
  auto** handle = static_cast<void**>(elem);
  LyraDynArrayRelease(*handle);
  *handle = nullptr;
}
