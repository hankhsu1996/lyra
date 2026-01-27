#include "lyra/runtime/io.hpp"

#include <cstdint>
#include <cstring>
#include <filesystem>
#include <fstream>
#include <limits>
#include <print>
#include <span>
#include <string>
#include <string_view>
#include <vector>

#include "lyra/common/format.hpp"
#include "lyra/common/memfile.hpp"
#include "lyra/runtime/engine.hpp"
#include "lyra/runtime/marshal.hpp"
#include "lyra/runtime/simulation.hpp"
#include "lyra/runtime/string.hpp"

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
};

// NOLINTBEGIN(cppcoreguidelines-avoid-non-const-global-variables)
std::vector<VarEntry> g_registered_vars;
// NOLINTEND(cppcoreguidelines-avoid-non-const-global-variables)

void SnapshotWideIntegral(const VarEntry& var) {
  const size_t num_words = (static_cast<size_t>(var.width) + 63) / 64;
  std::vector<uint64_t> words(num_words);

  // memcpy avoids alignment/aliasing UB
  std::memcpy(words.data(), var.addr, num_words * sizeof(uint64_t));

  // Mask MSW to semantic width (high bits may be garbage)
  const uint32_t rem = static_cast<uint32_t>(var.width) % 64;
  if (rem != 0) {
    words.back() &= (uint64_t{1} << rem) - 1;
  }

  // Format as hex (MSB first, matching MIR's IntegralToHex)
  std::string hex;
  bool leading = true;
  for (size_t i = num_words; i-- > 0;) {
    if (leading && words[i] == 0 && i > 0) {
      continue;
    }
    if (leading) {
      hex += std::format("{:x}", words[i]);
      leading = false;
    } else {
      hex += std::format("{:016x}", words[i]);
    }
  }
  if (hex.empty()) {
    hex = "0";
  }
  std::print("__LYRA_VAR:h:{}={}\n", var.name, hex);
}

void SnapshotIntegral(const VarEntry& var) {
  if (var.width > 64) {
    SnapshotWideIntegral(var);
    return;
  }

  uint64_t raw = 0;
  std::memcpy(&raw, var.addr, static_cast<size_t>((var.width + 7) / 8));

  // Mask to actual width
  if (var.width < 64) {
    raw &= (1ULL << var.width) - 1;
  }

  if (var.is_signed) {
    // Sign-extend if MSB is set
    auto value = static_cast<int64_t>(raw);
    if (var.width < 64) {
      uint64_t sign_bit = 1ULL << (var.width - 1);
      if ((raw & sign_bit) != 0) {
        value = static_cast<int64_t>(raw | ~((1ULL << var.width) - 1));
      }
    }
    std::print("__LYRA_VAR:i:{}={}\n", var.name, value);
  } else {
    std::print("__LYRA_VAR:i:{}={}\n", var.name, raw);
  }
}

void SnapshotReal(const VarEntry& var) {
  double value = 0.0;
  std::memcpy(&value, var.addr, sizeof(double));
  // Use max_digits10 (17 for double) to ensure round-trip precision
  std::print(
      "__LYRA_VAR:r:{}={:.{}g}\n", var.name, value,
      std::numeric_limits<double>::max_digits10);
}

}  // namespace

extern "C" void LyraPrintLiteral(const char* str) {
  std::print("{}", str);
}

extern "C" void LyraPrintValue(
    void* engine_ptr, int32_t format, int32_t value_kind, const void* data,
    int32_t width, bool is_signed, int32_t output_width, int32_t precision,
    bool zero_pad, bool left_align, const void* /*x_mask*/,
    const void* /*z_mask*/, int8_t module_timeunit_power) {
  std::string formatted = lyra::runtime::FormatRuntimeValue(
      static_cast<lyra::FormatKind>(format),
      static_cast<lyra::runtime::RuntimeValueKind>(value_kind), data, width,
      is_signed, output_width, precision, zero_pad, left_align, engine_ptr,
      module_timeunit_power);
  std::print("{}", formatted);
}

extern "C" void LyraPrintEnd(int32_t kind) {
  if (kind == static_cast<int32_t>(lyra::PrintKind::kDisplay)) {
    std::print("\n");
  }
  // No flush here - flush only at simulation end
}

extern "C" void LyraRegisterVar(
    const char* name, void* addr, int32_t kind, int32_t width, bool is_signed) {
  g_registered_vars.push_back(
      {name, addr, static_cast<VarKind>(kind), width, is_signed});
}

extern "C" void LyraSnapshotVars() {
  for (const auto& var : g_registered_vars) {
    switch (var.kind) {
      case VarKind::kIntegral:
        SnapshotIntegral(var);
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
    std::print("{}", msg);
    if (add_newline) std::print("\n");
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
    std::print(stderr, "$readmem: invalid element parameters\n");
    return;
  }

  auto elem_kind = static_cast<MemElementKind>(element_kind);

  // Assert layout consistency
  int32_t expected_stride = (elem_kind == MemElementKind::kFourState)
                                ? 2 * value_size_bytes
                                : value_size_bytes;
  if (stride_bytes != expected_stride) {
    std::print(
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
    std::print(stderr, "$readmem: cannot open file '{}'\n", filename);
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
      std::print(stderr, "{}: {}\n", task_name, words_result.error());
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
    std::print(stderr, "{}: {}\n", task_name, result.error);
  }
}

extern "C" void LyraWritemem(
    LyraStringHandle filename_handle, const void* source, int32_t element_width,
    int32_t stride_bytes, int32_t value_size_bytes, int32_t element_count,
    int64_t min_addr, int64_t current_addr, int64_t final_addr, int64_t step,
    bool is_hex, int32_t element_kind) {
  // Sanity checks
  if (element_width <= 0 || stride_bytes <= 0 || value_size_bytes <= 0 ||
      element_count <= 0) {
    std::print(stderr, "$writemem: invalid element parameters\n");
    return;
  }

  auto elem_kind = static_cast<MemElementKind>(element_kind);

  // Assert layout consistency
  int32_t expected_stride = (elem_kind == MemElementKind::kFourState)
                                ? 2 * value_size_bytes
                                : value_size_bytes;
  if (stride_bytes != expected_stride) {
    std::print(
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
    std::print(
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
