#include "lyra/runtime/string.hpp"

#include <algorithm>
#include <cassert>
#include <cstdlib>
#include <cstring>
#include <print>
#include <string_view>

namespace {

// Internal string representation with reference counting
struct LyraStringData {
  char* data;
  uint64_t len;
  uint64_t refcount;
};

}  // namespace

extern "C" auto LyraStringFromLiteral(const char* data, int64_t len)
    -> LyraStringHandle {
  assert(len >= 0 && "string length must be non-negative");

  // Allocate the struct
  // NOLINTNEXTLINE(cppcoreguidelines-owning-memory)
  auto* str = new LyraStringData();

  // Allocate and copy the data (memcpy preserves embedded NULs)
  // NOLINTNEXTLINE(cppcoreguidelines-owning-memory)
  str->data = new char[static_cast<size_t>(len)];
  std::memcpy(str->data, data, static_cast<size_t>(len));

  str->len = static_cast<uint64_t>(len);
  str->refcount = 1;

  return str;
}

extern "C" auto LyraStringCmp(LyraStringHandle a, LyraStringHandle b)
    -> int32_t {
  auto* str_a = static_cast<LyraStringData*>(a);
  auto* str_b = static_cast<LyraStringData*>(b);

  // Compare up to the minimum length
  uint64_t min_len = std::min(str_a->len, str_b->len);
  int cmp = std::memcmp(str_a->data, str_b->data, min_len);

  if (cmp != 0) {
    return cmp;
  }

  // All compared bytes equal - use length as tiebreaker
  if (str_a->len < str_b->len) {
    return -1;
  }
  if (str_a->len > str_b->len) {
    return 1;
  }
  return 0;
}

extern "C" auto LyraStringRetain(LyraStringHandle handle) -> LyraStringHandle {
  if (handle == nullptr) {
    return nullptr;
  }
  auto* str = static_cast<LyraStringData*>(handle);
  ++str->refcount;
  return handle;
}

extern "C" auto LyraStringConcat(const LyraStringHandle* elems, int64_t count)
    -> LyraStringHandle {
  assert(count >= 0 && "concat count must be non-negative");

  // Sum total length
  uint64_t total_len = 0;
  for (int64_t i = 0; i < count; ++i) {
    auto* elem = static_cast<LyraStringData*>(elems[i]);
    if (elem != nullptr) {
      total_len += elem->len;
    }
  }

  // Allocate result
  // NOLINTNEXTLINE(cppcoreguidelines-owning-memory)
  auto* result = new LyraStringData();
  // NOLINTNEXTLINE(cppcoreguidelines-owning-memory)
  result->data = new char[total_len];
  result->len = total_len;
  result->refcount = 1;

  // Copy each element's data sequentially
  uint64_t offset = 0;
  for (int64_t i = 0; i < count; ++i) {
    auto* elem = static_cast<LyraStringData*>(elems[i]);
    if (elem != nullptr && elem->len > 0) {
      std::memcpy(result->data + offset, elem->data, elem->len);
      offset += elem->len;
    }
  }

  return result;
}

extern "C" void LyraStringRelease(LyraStringHandle handle) {
  if (handle == nullptr) {
    return;
  }
  auto* str = static_cast<LyraStringData*>(handle);
  --str->refcount;
  if (str->refcount == 0) {
    // NOLINTNEXTLINE(cppcoreguidelines-owning-memory)
    delete[] str->data;
    // NOLINTNEXTLINE(cppcoreguidelines-owning-memory)
    delete str;
  }
}

extern "C" void LyraPrintString(LyraStringHandle handle) {
  if (handle == nullptr) {
    return;
  }
  auto* str = static_cast<LyraStringData*>(handle);
  std::print("{}", std::string_view(str->data, str->len));
}
