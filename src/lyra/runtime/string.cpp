#include "lyra/runtime/string.hpp"

#include <algorithm>
#include <cassert>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <print>
#include <span>
#include <string>
#include <string_view>

#include "lyra/common/format.hpp"
#include "lyra/runtime/marshal.hpp"

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

  std::span<const LyraStringHandle> handles(elems, static_cast<size_t>(count));

  // Build concatenated string
  std::string buffer;
  for (LyraStringHandle handle : handles) {
    auto* elem = static_cast<LyraStringData*>(handle);
    if (elem != nullptr && elem->len > 0) {
      buffer.append(elem->data, elem->len);
    }
  }

  // Allocate result and copy from buffer
  // NOLINTNEXTLINE(cppcoreguidelines-owning-memory)
  auto* result = new LyraStringData();
  // NOLINTNEXTLINE(cppcoreguidelines-owning-memory)
  result->data = new char[buffer.size()];
  std::memcpy(result->data, buffer.data(), buffer.size());
  result->len = buffer.size();
  result->refcount = 1;

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

// Internal buffer for string formatting
struct LyraStringFormatBuffer {
  std::string data;
};

extern "C" auto LyraStringFormatStart() -> LyraStringFormatBuffer* {
  // NOLINTNEXTLINE(cppcoreguidelines-owning-memory)
  auto* buf = new LyraStringFormatBuffer();
  buf->data.reserve(64);  // Reduce realloc churn for typical format strings
  return buf;
}

extern "C" void LyraStringFormatLiteral(
    LyraStringFormatBuffer* buf, const char* str, int64_t len) {
  buf->data.append(str, static_cast<size_t>(len));
}

extern "C" void LyraStringFormatValue(
    LyraStringFormatBuffer* buf, int32_t format, const void* data,
    int32_t width, bool is_signed, int32_t output_width, int32_t precision,
    bool zero_pad, bool left_align, const void* /*x_mask*/,
    const void* /*z_mask*/) {
  buf->data += lyra::runtime::FormatRuntimeValue(
      static_cast<lyra::FormatKind>(format), data, width, is_signed,
      output_width, precision, zero_pad, left_align);
}

extern "C" void LyraStringFormatString(
    LyraStringFormatBuffer* buf, LyraStringHandle handle) {
  if (handle == nullptr) {
    return;
  }
  auto* str = static_cast<LyraStringData*>(handle);
  buf->data.append(str->data, str->len);
}

extern "C" auto LyraStringFormatFinish(LyraStringFormatBuffer* buf)
    -> LyraStringHandle {
  LyraStringHandle result = LyraStringFromLiteral(
      buf->data.data(), static_cast<int64_t>(buf->data.size()));
  // NOLINTNEXTLINE(cppcoreguidelines-owning-memory)
  delete buf;  // Consume buffer
  return result;
}
