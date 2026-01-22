#include "lyra/runtime/string.hpp"

#include <algorithm>
#include <cassert>
#include <cstdlib>
#include <cstring>

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
