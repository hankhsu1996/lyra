#include "lyra/runtime/string.hpp"

#include <algorithm>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <print>
#include <span>
#include <string>
#include <string_view>
#include <vector>

#include "lyra/common/format.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/marshal.hpp"
#include "lyra/semantic/format.hpp"
#include "lyra/semantic/value.hpp"

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
  if (len < 0) {
    throw lyra::common::InternalError(
        "LyraStringFromLiteral", "string length must be non-negative");
  }

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
  // Treat null as empty string for comparison purposes
  auto* str_a = static_cast<LyraStringData*>(a);
  auto* str_b = static_cast<LyraStringData*>(b);

  uint64_t len_a = (str_a != nullptr) ? str_a->len : 0;
  uint64_t len_b = (str_b != nullptr) ? str_b->len : 0;

  // Compare up to the minimum length
  uint64_t min_len = std::min(len_a, len_b);
  if (min_len > 0) {
    int cmp = std::memcmp(str_a->data, str_b->data, min_len);
    if (cmp != 0) {
      return cmp;
    }
  }

  // All compared bytes equal - use length as tiebreaker
  if (len_a < len_b) {
    return -1;
  }
  if (len_a > len_b) {
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
  if (count < 0) {
    throw lyra::common::InternalError(
        "LyraStringConcat", "concat count must be non-negative");
  }

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
  if (str->refcount == 0) {
    throw lyra::common::InternalError(
        "LyraStringRelease", "string refcount underflow (double-free)");
  }
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

// Value kind for runtime format operands
enum class RuntimeFormatValueKind : int32_t {
  kIntegral = 0,
  kReal = 1,
  kString = 2,
};

extern "C" auto LyraStringFormatRuntime(
    LyraStringHandle format_handle, void* const* data_ptrs,
    const int32_t* widths, const int8_t* signeds, const int32_t* kinds,
    int64_t count) -> LyraStringHandle {
  // Extract format string
  auto* fmt_str = static_cast<LyraStringData*>(format_handle);
  std::string_view fmt(fmt_str->data, fmt_str->len);

  // Wrap raw pointers in spans for safe access
  auto n = static_cast<size_t>(count);
  std::span<void* const> data_span(data_ptrs, n);
  std::span<const int32_t> width_span(widths, n);
  std::span<const int8_t> signed_span(signeds, n);
  std::span<const int32_t> kind_span(kinds, n);

  // Marshal operands to FormatArg
  std::vector<lyra::semantic::FormatArg> args;
  args.reserve(n);

  for (size_t i = 0; i < n; ++i) {
    auto kind = static_cast<RuntimeFormatValueKind>(kind_span[i]);
    bool is_signed = signed_span[i] != 0;

    lyra::semantic::RuntimeValue value;
    switch (kind) {
      case RuntimeFormatValueKind::kIntegral: {
        int32_t width = width_span[i];
        uint64_t raw_value = 0;
        if (width <= 8) {
          raw_value = *static_cast<const uint8_t*>(data_span[i]);
        } else if (width <= 16) {
          raw_value = *static_cast<const uint16_t*>(data_span[i]);
        } else if (width <= 32) {
          raw_value = *static_cast<const uint32_t*>(data_span[i]);
        } else {
          raw_value = *static_cast<const uint64_t*>(data_span[i]);
        }
        // Mask to actual width
        if (width < 64) {
          raw_value &= (1ULL << width) - 1;
        }
        value = lyra::semantic::MakeIntegral(
            raw_value, static_cast<uint32_t>(width));
        break;
      }
      case RuntimeFormatValueKind::kReal: {
        int32_t width = width_span[i];
        if (width == 32) {
          value = lyra::semantic::MakeShortReal(
              *static_cast<const float*>(data_span[i]));
        } else {
          value = lyra::semantic::MakeReal(
              *static_cast<const double*>(data_span[i]));
        }
        break;
      }
      case RuntimeFormatValueKind::kString: {
        // data_span[i] is a LyraStringHandle (void*)
        auto* str_data = static_cast<LyraStringData*>(data_span[i]);
        if (str_data != nullptr) {
          value = lyra::semantic::MakeString(
              std::string(str_data->data, str_data->len));
        } else {
          value = lyra::semantic::MakeString("");
        }
        break;
      }
    }

    args.push_back({.value = std::move(value), .is_signed = is_signed});
  }

  // Call semantic FormatMessage
  auto result = lyra::semantic::FormatMessage(fmt, args);

  std::string output;
  if (result.has_value()) {
    output = std::move(result->output);
  }
  // On error, return empty string (errors should be caught at compile time)

  return LyraStringFromLiteral(
      output.data(), static_cast<int64_t>(output.size()));
}
