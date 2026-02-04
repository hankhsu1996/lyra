#include "lyra/runtime/io.hpp"

#include <algorithm>
#include <cstdint>
#include <cstring>
#include <filesystem>
#include <format>
#include <fstream>
#include <iterator>
#include <limits>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <vector>

#include <fmt/core.h>

#include "lyra/common/format.hpp"
#include "lyra/common/memfile.hpp"
#include "lyra/runtime/engine.hpp"
#include "lyra/runtime/file_manager.hpp"
#include "lyra/runtime/marshal.hpp"
#include "lyra/runtime/output_sink.hpp"
#include "lyra/runtime/simulation.hpp"
#include "lyra/runtime/string.hpp"
#include "lyra/semantic/format.hpp"
#include "lyra/semantic/value.hpp"

namespace {

enum class VarKind : int32_t {
  kIntegral = 0,
  kReal = 1,
};

struct VarEntry {
  std::string name;
  void* addr;
  VarKind kind;
  int32_t width;
  bool is_signed;
  bool is_four_state;
};

// NOLINTBEGIN(cppcoreguidelines-avoid-non-const-global-variables)
std::vector<VarEntry> g_registered_vars;
// NOLINTEND(cppcoreguidelines-avoid-non-const-global-variables)

// Compute storage size for an integral type per ABI contract.
// Narrow types use power-of-2 rounding; wide types use 64-bit words.
auto GetIntegralStorageBytes(uint32_t width) -> size_t {
  if (width <= 8) {
    return 1;
  }
  if (width <= 16) {
    return 2;
  }
  if (width <= 32) {
    return 4;
  }
  if (width <= 64) {
    return 8;
  }
  // Wide types: 64-bit words, 8-byte aligned
  return ((width + 63) / 64) * 8;
}

// Read a packed integral from a slot. All ABI layout knowledge lives here.
auto ReadPackedIntegralFromSlot(
    const void* addr, uint32_t width, bool is_four_state)
    -> lyra::semantic::RuntimeValue {
  size_t num_words = (static_cast<size_t>(width) + 63) / 64;
  std::vector<uint64_t> value_words(num_words, 0);
  std::vector<uint64_t> unknown_words(num_words, 0);

  size_t storage_bytes = GetIntegralStorageBytes(width);

  // Use span for bounds-safe access (avoids pointer arithmetic)
  size_t total_bytes = is_four_state ? storage_bytes * 2 : storage_bytes;
  std::span<const uint8_t> data_span(
      static_cast<const uint8_t*>(addr), total_bytes);

  // memcpy avoids alignment/aliasing UB
  // For narrow types, only copy storage_bytes (not num_words * 8)
  std::memcpy(value_words.data(), data_span.data(), storage_bytes);

  if (is_four_state) {
    // Layout: {value_storage, unknown_storage} - struct with two elements
    std::memcpy(
        unknown_words.data(), data_span.subspan(storage_bytes).data(),
        storage_bytes);
  }

  // Mask top words to semantic width (high bits may be garbage)
  const uint32_t rem = width % 64;
  if (rem != 0) {
    uint64_t mask = (uint64_t{1} << rem) - 1;
    value_words.back() &= mask;
    unknown_words.back() &= mask;
  }

  if (is_four_state) {
    return lyra::semantic::MakeIntegralWide(
        value_words.data(), unknown_words.data(), num_words, width);
  }
  return lyra::semantic::MakeIntegralWide(value_words.data(), num_words, width);
}

void SnapshotIntegralSemantic(const VarEntry& var) {
  // ABI layer: build RuntimeValue from memory
  auto value = ReadPackedIntegralFromSlot(
      var.addr, static_cast<uint32_t>(var.width), var.is_four_state);

  // Format layer: get SV literal (FormatAsSvLiteral owns all N'...
  // construction)
  std::string literal = lyra::semantic::FormatAsSvLiteral(value);

  // New protocol: v:i:name=literal
  lyra::runtime::WriteOutput(
      std::format("__LYRA_VAR:v:i:{}={}\n", var.name, literal));
}

void SnapshotReal(const VarEntry& var) {
  double value = 0.0;
  std::memcpy(&value, var.addr, sizeof(double));
  // New protocol: v:r:name=value (plain decimal, not SV literal)
  // Use max_digits10 (17 for double) to ensure round-trip precision
  lyra::runtime::WriteOutput(
      std::format(
          "__LYRA_VAR:v:r:{}={:.{}g}\n", var.name, value,
          std::numeric_limits<double>::max_digits10));
}

}  // namespace

extern "C" void LyraPrintLiteral(const char* str) {
  lyra::runtime::WriteOutput(str);
}

extern "C" void LyraPrintValue(
    void* engine_ptr, int32_t format, int32_t value_kind, const void* data,
    int32_t width, bool is_signed, int32_t output_width, int32_t precision,
    bool zero_pad, bool left_align, const void* unknown_data,
    const void* /*z_mask*/, int8_t module_timeunit_power) {
  std::string formatted = lyra::runtime::FormatRuntimeValue(
      static_cast<lyra::FormatKind>(format),
      static_cast<lyra::runtime::RuntimeValueKind>(value_kind), data, width,
      is_signed, output_width, precision, zero_pad, left_align, engine_ptr,
      module_timeunit_power, unknown_data);
  lyra::runtime::WriteOutput(formatted);
}

extern "C" void LyraPrintEnd(int32_t kind) {
  if (kind == static_cast<int32_t>(lyra::PrintKind::kDisplay)) {
    lyra::runtime::WriteOutput("\n");
  }
  // No flush here - flush only at simulation end
}

extern "C" void LyraRegisterVar(
    const char* name, void* addr, int32_t kind, int32_t width, bool is_signed,
    bool is_four_state) {
  g_registered_vars.push_back(
      {name, addr, static_cast<VarKind>(kind), width, is_signed,
       is_four_state});
}

extern "C" void LyraSnapshotVars() {
  for (const auto& var : g_registered_vars) {
    switch (var.kind) {
      case VarKind::kIntegral:
        SnapshotIntegralSemantic(var);
        break;
      case VarKind::kReal:
        SnapshotReal(var);
        break;
    }
  }
  g_registered_vars.clear();
}

extern "C" auto LyraFopenFd(
    void* engine_ptr, LyraStringHandle filename_handle,
    LyraStringHandle mode_handle) -> int32_t {
  auto* engine = static_cast<lyra::runtime::Engine*>(engine_ptr);
  std::string filename{LyraStringAsView(filename_handle)};
  std::string mode{LyraStringAsView(mode_handle)};
  return engine->GetFileManager().FopenFd(filename, mode);
}

extern "C" auto LyraFopenMcd(void* engine_ptr, LyraStringHandle filename_handle)
    -> int32_t {
  auto* engine = static_cast<lyra::runtime::Engine*>(engine_ptr);
  std::string filename{LyraStringAsView(filename_handle)};
  return engine->GetFileManager().FopenMcd(filename);
}

extern "C" void LyraFclose(void* engine_ptr, int32_t descriptor) {
  auto* engine = static_cast<lyra::runtime::Engine*>(engine_ptr);
  engine->GetFileManager().Fclose(descriptor);
}

extern "C" auto LyraFgetc(void* engine_ptr, int32_t descriptor) -> int32_t {
  auto* engine = static_cast<lyra::runtime::Engine*>(engine_ptr);
  return engine->GetFileManager().Fgetc(descriptor);
}

extern "C" auto LyraUngetc(
    void* engine_ptr, int32_t character, int32_t descriptor) -> int32_t {
  auto* engine = static_cast<lyra::runtime::Engine*>(engine_ptr);
  return engine->GetFileManager().Ungetc(character, descriptor);
}

extern "C" auto LyraFgets(
    void* engine_ptr, int32_t descriptor, LyraStringHandle* str_out)
    -> int32_t {
  auto* engine = static_cast<lyra::runtime::Engine*>(engine_ptr);
  std::string result;
  int32_t count = engine->GetFileManager().Fgets(descriptor, result);
  *str_out =
      LyraStringFromLiteral(result.data(), static_cast<int64_t>(result.size()));
  return count;
}

extern "C" void LyraFflush(
    void* engine_ptr, bool has_desc, int32_t descriptor) {
  auto* engine = static_cast<lyra::runtime::Engine*>(engine_ptr);
  if (has_desc) {
    engine->GetFileManager().Fflush(descriptor);
  } else {
    engine->GetFileManager().Fflush(std::nullopt);
  }
}

extern "C" void LyraFWrite(
    void* engine_ptr, uint32_t descriptor, LyraStringHandle message,
    bool add_newline) {
  if (descriptor == 0) return;

  auto* engine = static_cast<lyra::runtime::Engine*>(engine_ptr);
  lyra::runtime::StreamTargets targets =
      engine->GetFileManager().CollectStreams(descriptor);

  if (!targets.include_stdout && targets.file_stream_count == 0) return;

  const char* ptr = nullptr;
  uint64_t len = 0;
  LyraStringGetView(message, &ptr, &len);
  std::string_view msg{ptr, len};

  if (targets.include_stdout) {
    lyra::runtime::WriteOutput(msg);
    if (add_newline) lyra::runtime::WriteOutput("\n");
  }
  for (int i = 0; i < targets.file_stream_count; ++i) {
    *targets.file_streams.at(i) << msg;
    if (add_newline) *targets.file_streams.at(i) << '\n';
  }
}

extern "C" void LyraReadmem(
    LyraStringHandle filename_handle, void* target, int32_t element_width,
    int32_t stride_bytes, int32_t value_size_bytes, int32_t element_count,
    int64_t min_addr, int64_t current_addr, int64_t final_addr, int64_t step,
    bool is_hex, int32_t element_kind) {
  // Sanity checks
  if (element_width <= 0 || stride_bytes <= 0 || value_size_bytes <= 0 ||
      element_count <= 0) {
    fmt::print(stderr, "$readmem: invalid element parameters\n");
    return;
  }

  auto elem_kind = static_cast<MemElementKind>(element_kind);

  // Assert layout consistency
  int32_t expected_stride = (elem_kind == MemElementKind::kFourState)
                                ? 2 * value_size_bytes
                                : value_size_bytes;
  if (stride_bytes != expected_stride) {
    fmt::print(
        stderr, "$readmem: stride mismatch (got {}, expected {})\n",
        stride_bytes, expected_stride);
    return;
  }

  std::string filename{LyraStringAsView(filename_handle)};

  // Resolve path relative to fs_base_dir (same as $fopen)
  std::filesystem::path path{filename};
  if (path.is_relative()) {
    path = lyra::runtime::GetFsBaseDir() / path;
  }

  std::ifstream file(path);
  if (!file) {
    fmt::print(stderr, "$readmem: cannot open file '{}'\n", filename);
    return;
  }

  std::string content(
      (std::istreambuf_iterator<char>(file)), std::istreambuf_iterator<char>());

  int64_t max_addr = min_addr + element_count - 1;
  auto bit_width = static_cast<size_t>(element_width);
  auto stride = static_cast<size_t>(stride_bytes);
  auto value_size = static_cast<size_t>(value_size_bytes);

  std::string_view task_name = is_hex ? "$readmemh" : "$readmemb";

  // Create a span over the target storage for bounds-safe access
  auto total_bytes = static_cast<size_t>(element_count) * stride;
  std::span<uint8_t> target_span(static_cast<uint8_t*>(target), total_bytes);

  // Store callback - maps SV address to storage index with bounds check
  auto store = [&](std::string_view token, int64_t addr) {
    // Bounds check before computing storage index
    if (addr < min_addr || addr > max_addr) {
      return;  // Ignore out-of-bounds addresses
    }

    auto words_result =
        lyra::common::ParseMemTokenToWords(token, bit_width, is_hex);
    if (!words_result) {
      fmt::print(stderr, "{}: {}\n", task_name, words_result.error());
      return;
    }
    const auto& words = *words_result;

    // Compute storage offset: (addr - min_addr) * stride
    auto storage_index = static_cast<size_t>(addr - min_addr);
    size_t offset = storage_index * stride;

    // Copy value (same for both 2-state and 4-state)
    size_t bytes_to_copy = std::min(value_size, words.size() * 8);
    std::memcpy(
        target_span.subspan(offset).data(), words.data(), bytes_to_copy);

    if (elem_kind == MemElementKind::kFourState) {
      // 4-state: zero the x_mask field (all bits known, no X/Z)
      std::memset(
          target_span.subspan(offset + value_size).data(), 0, value_size);
    }
  };

  // Forward to canonical parser (step handles direction)
  auto result = lyra::common::ParseMemFile(
      content, is_hex, min_addr, max_addr, current_addr, final_addr, step,
      task_name, store);
  if (!result.success) {
    fmt::print(stderr, "{}: {}\n", task_name, result.error);
  }
}

extern "C" void LyraPrintModulePath(void* engine_ptr, uint32_t instance_id) {
  auto* engine = static_cast<lyra::runtime::Engine*>(engine_ptr);
  lyra::runtime::WriteOutput(engine->GetInstancePath(instance_id));
}

extern "C" void LyraWritemem(
    LyraStringHandle filename_handle, const void* source, int32_t element_width,
    int32_t stride_bytes, int32_t value_size_bytes, int32_t element_count,
    int64_t min_addr, int64_t current_addr, int64_t final_addr, int64_t step,
    bool is_hex, int32_t element_kind) {
  // Sanity checks
  if (element_width <= 0 || stride_bytes <= 0 || value_size_bytes <= 0 ||
      element_count <= 0) {
    fmt::print(stderr, "$writemem: invalid element parameters\n");
    return;
  }

  auto elem_kind = static_cast<MemElementKind>(element_kind);

  // Assert layout consistency
  int32_t expected_stride = (elem_kind == MemElementKind::kFourState)
                                ? 2 * value_size_bytes
                                : value_size_bytes;
  if (stride_bytes != expected_stride) {
    fmt::print(
        stderr, "$writemem: stride mismatch (got {}, expected {})\n",
        stride_bytes, expected_stride);
    return;
  }

  std::string filename{LyraStringAsView(filename_handle)};

  // Resolve path relative to fs_base_dir
  std::filesystem::path path{filename};
  if (path.is_relative()) {
    path = lyra::runtime::GetFsBaseDir() / path;
  }

  std::ofstream file(path);
  if (!file) {
    fmt::print(
        stderr, "$writemem: cannot open file '{}' for writing\n", filename);
    return;
  }

  int64_t max_addr = min_addr + element_count - 1;
  auto bit_width = static_cast<size_t>(element_width);
  auto stride = static_cast<size_t>(stride_bytes);
  auto value_size = static_cast<size_t>(value_size_bytes);
  size_t word_count = (bit_width + 63) / 64;

  // Create a span over the source storage for bounds-safe access
  auto total_bytes = static_cast<size_t>(element_count) * stride;
  std::span<const uint8_t> source_span(
      static_cast<const uint8_t*>(source), total_bytes);

  // Write each element with direction-aware iteration
  int64_t addr = current_addr;
  while (step > 0 ? addr <= final_addr : addr >= final_addr) {
    // Bounds check
    if (addr >= min_addr && addr <= max_addr) {
      auto storage_index = static_cast<size_t>(addr - min_addr);
      size_t offset = storage_index * stride;

      // Read words from value plane only (for 4-state, x_mask is ignored)
      std::vector<uint64_t> words(word_count, 0);
      std::memcpy(
          words.data(), source_span.subspan(offset).data(),
          std::min(value_size, word_count * 8));

      // Format and write
      std::string formatted =
          lyra::common::FormatMemWords(words, bit_width, is_hex);
      file << formatted << '\n';
    }
    addr += step;
  }
}

extern "C" auto LyraFread(
    void* engine_ptr, int32_t descriptor, void* target, int32_t element_width,
    int32_t stride_bytes, int32_t is_memory, int64_t start_index,
    int64_t max_count, int64_t element_count) -> int32_t {
  auto* engine = static_cast<lyra::runtime::Engine*>(engine_ptr);

  // Calculate bytes per element (ceil(element_width / 8))
  auto bytes_per_elem = static_cast<size_t>((element_width + 7) / 8);
  auto stride = static_cast<size_t>(stride_bytes);

  // Buffer for reading bytes
  std::vector<uint8_t> buffer(bytes_per_elem);
  int32_t total_bytes_read = 0;

  // Helper to pack buffer into target location (big-endian)
  auto pack_big_endian = [&](std::span<uint8_t> dest) {
    // Big-endian: buffer[0] is MSB, so it goes to highest byte position
    // Target is little-endian (x86), so we reverse the byte order
    // Zero the destination first (in case bytes_per_elem < stride)
    std::fill(dest.begin(), dest.end(), 0);
    // Copy bytes in reverse order (big-endian to little-endian)
    for (size_t i = 0; i < bytes_per_elem; ++i) {
      dest[bytes_per_elem - 1 - i] = buffer[i];
    }
  };

  if (is_memory == 0) {
    // Integral variant: read bytes_per_elem bytes into target
    int32_t bytes_read = engine->GetFileManager().FreadBytes(
        descriptor, buffer.data(), bytes_per_elem);
    if (bytes_read > 0) {
      // Zero remaining bytes if partial read
      if (static_cast<size_t>(bytes_read) < bytes_per_elem) {
        std::fill(buffer.begin() + bytes_read, buffer.end(), 0);
      }
      pack_big_endian(std::span(static_cast<uint8_t*>(target), stride));
      total_bytes_read = bytes_read;
    }
  } else {
    // Memory variant: read into array elements
    // Determine start and count
    size_t start_idx = (start_index < 0) ? 0 : static_cast<size_t>(start_index);
    size_t count = (max_count < 0) ? static_cast<size_t>(element_count)
                                   : static_cast<size_t>(max_count);

    // Validate start
    if (start_idx >= static_cast<size_t>(element_count)) {
      return 0;  // OOB start
    }

    // Limit count to available elements
    size_t end_idx =
        std::min(start_idx + count, static_cast<size_t>(element_count));

    for (size_t idx = start_idx; idx < end_idx; ++idx) {
      int32_t bytes_read = engine->GetFileManager().FreadBytes(
          descriptor, buffer.data(), bytes_per_elem);

      if (static_cast<size_t>(bytes_read) < bytes_per_elem) {
        // Partial element - stop at element boundary
        break;
      }

      // Pack into element at idx
      pack_big_endian(
          std::span(static_cast<uint8_t*>(target), stride * element_count)
              .subspan(idx * stride, stride));
      total_bytes_read += bytes_read;
    }
  }

  return total_bytes_read;
}

extern "C" auto LyraFscanf(
    void* engine_ptr, int32_t descriptor, LyraStringHandle format,
    int32_t output_count, void** output_ptrs) -> int32_t {
  auto* engine = static_cast<lyra::runtime::Engine*>(engine_ptr);
  auto& file_manager = engine->GetFileManager();

  // Get format string
  std::string_view format_str = LyraStringAsView(format);

  // Track output index
  int32_t output_idx = 0;

  // Use FileManager::Fscanf for the actual scanning
  return file_manager.Fscanf(
      descriptor, format_str, [&](const lyra::runtime::ScanResult& result) {
        if (output_idx >= output_count) return;  // No more outputs

        void* output_ptr = output_ptrs[output_idx];

        if (result.IsInt()) {
          // Store as int32_t (matches MIR interpreter behavior)
          *static_cast<int32_t*>(output_ptr) =
              static_cast<int32_t>(result.AsInt());
        } else {
          // Store as string handle
          const std::string& str = result.AsString();
          *static_cast<LyraStringHandle*>(output_ptr) = LyraStringFromLiteral(
              str.data(), static_cast<int64_t>(str.size()));
        }

        ++output_idx;
      });
}
