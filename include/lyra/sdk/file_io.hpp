#pragma once

#include <cstdint>
#include <filesystem>
#include <fstream>
#include <map>
#include <memory>
#include <string>
#include <string_view>

namespace lyra::sdk {

/// File mode for file descriptors (FD model).
/// Maps to LRM Table 21-6 file open types.
enum class FileMode : uint8_t {
  kRead,        // "r"  - read
  kWrite,       // "w"  - write (truncate)
  kAppend,      // "a"  - append
  kReadWrite,   // "r+" - read/write
  kWriteRead,   // "w+" - write/read (truncate)
  kAppendRead,  // "a+" - append/read
};

/// Parse file mode string to enum.
/// Returns kWrite as default for unrecognized modes.
inline auto ParseFileMode(std::string_view mode_str) -> FileMode {
  // Strip "b" suffix (binary mode - ignored on most systems)
  std::string mode{mode_str};
  std::erase(mode, 'b');

  if (mode == "r") {
    return FileMode::kRead;
  }
  if (mode == "w") {
    return FileMode::kWrite;
  }
  if (mode == "a") {
    return FileMode::kAppend;
  }
  if (mode == "r+" || mode == "+r") {
    return FileMode::kReadWrite;
  }
  if (mode == "w+" || mode == "+w") {
    return FileMode::kWriteRead;
  }
  if (mode == "a+" || mode == "+a") {
    return FileMode::kAppendRead;
  }
  return FileMode::kWrite;  // Default
}

/// Convert FileMode to fstream open mode flags.
inline auto ToOpenMode(FileMode mode) -> std::ios_base::openmode {
  switch (mode) {
    case FileMode::kRead:
      return std::ios_base::in;
    case FileMode::kWrite:
      return std::ios_base::out | std::ios_base::trunc;
    case FileMode::kAppend:
      return std::ios_base::out | std::ios_base::app;
    case FileMode::kReadWrite:
      return std::ios_base::in | std::ios_base::out;
    case FileMode::kWriteRead:
      return std::ios_base::in | std::ios_base::out | std::ios_base::trunc;
    case FileMode::kAppendRead:
      return std::ios_base::in | std::ios_base::out | std::ios_base::app;
  }
  return std::ios_base::out;
}

/// Manages file I/O state for $fopen/$fclose system tasks.
///
/// Implements two descriptor models per IEEE 1800:
/// - MCD (multichannel descriptor): bit-per-file, OR-able for multi-output.
///   Each file gets a unique bit in [1:30]. Bit 0 is reserved for stdout.
///   MCD descriptors can be OR'd together to write to multiple files at once.
/// - FD (file descriptor): index with bit 31 set, not OR-able.
///   Returns 0x80000000 | index, where indices 0-2 are stdin/stdout/stderr.
///
/// Thread safety: This class is NOT thread-safe. Each simulation should have
/// its own FileManager instance. In single-threaded simulation (current model),
/// this is guaranteed by ownership in RuntimeConfig/SimulationContext.
class FileManager {
 public:
  FileManager() = default;
  ~FileManager() = default;

  // Non-copyable (owns file handles)
  FileManager(const FileManager&) = delete;
  auto operator=(const FileManager&) -> FileManager& = delete;

  // Movable
  FileManager(FileManager&&) = default;
  auto operator=(FileManager&&) -> FileManager& = default;

  /// $fopen(filename) - MCD mode (write-only).
  /// Returns MCD descriptor (single bit set in [1:30]), or 0 on failure.
  auto FopenMcd(std::string_view filename) -> int32_t {
    // Find free channel (bits 1-30, bit 0 is stdout)
    for (int channel = 1; channel <= 30; ++channel) {
      int32_t mask = 1 << channel;
      if (!mcd_channels_.contains(mask)) {
        auto path = ResolvePath(filename);
        auto stream = std::make_unique<std::fstream>(
            path, std::ios_base::out | std::ios_base::trunc);
        if (!stream->is_open()) {
          return 0;  // Open failed
        }
        mcd_channels_[mask] = std::move(stream);
        return mask;
      }
    }
    return 0;  // No free channels
  }

  /// $fopen(filename, mode) - FD mode with explicit mode.
  /// Returns FD descriptor (0x80000000 | index), or 0 on failure.
  auto FopenFd(std::string_view filename, std::string_view mode_str)
      -> int32_t {
    auto mode = ParseFileMode(mode_str);
    auto path = ResolvePath(filename);
    auto stream = std::make_unique<std::fstream>(path, ToOpenMode(mode));
    if (!stream->is_open()) {
      return 0;  // Open failed
    }

    int32_t index = next_fd_index_++;
    int32_t descriptor = kFdBit | index;
    fd_table_[index] = std::move(stream);
    return descriptor;
  }

  /// $fclose(descriptor) - close file and release handle.
  void Fclose(int32_t descriptor) {
    // STDOUT/STDERR: no-op
    if (descriptor == kStdout || descriptor == kStderr) {
      return;
    }

    // FD: close single descriptor
    if ((descriptor & kFdBit) != 0) {
      int32_t index = descriptor & ~kFdBit;
      fd_table_.erase(index);
      return;
    }

    // MCD: must be power-of-2 (single channel)
    if (descriptor != 0 && (descriptor & (descriptor - 1)) == 0) {
      mcd_channels_.erase(descriptor);
    }
  }

  /// Write content to file(s) specified by descriptor.
  /// Handles both MCD (multichannel, OR-able) and FD (single file) modes.
  /// Invalid or closed descriptors are silently ignored per LRM.
  ///
  /// @param descriptor MCD (bit mask, MSB clear) or FD (MSB set)
  /// @param content The text to write
  /// @param transcript_sink Output stream for transcript (stdout/MCD bit 0)
  void WriteToDescriptor(
      uint32_t descriptor, std::string_view content,
      std::ostream& transcript_sink) {
    if (descriptor == 0) {
      return;  // Invalid/closed
    }

    if ((descriptor & 0x8000'0000U) != 0) {
      // FD mode - single file
      uint32_t index = descriptor & 0x7FFF'FFFFU;
      if (index == 1 || index == 2) {
        // stdout (1) or stderr (2) -> transcript
        transcript_sink << content;
      } else if (auto it = fd_table_.find(static_cast<int32_t>(index));
                 it != fd_table_.end()) {
        *it->second << content;
      }
      // Invalid index: silently ignored
    } else {
      // MCD mode - check each bit
      if ((descriptor & 1U) != 0) {
        transcript_sink << content;  // bit 0 = transcript
      }
      for (uint32_t bit = 1; bit <= 30; ++bit) {
        if ((descriptor & (1U << bit)) != 0) {
          auto mask = static_cast<int32_t>(1U << bit);
          if (auto it = mcd_channels_.find(mask); it != mcd_channels_.end()) {
            *it->second << content;
          }
          // Bit set but no channel: silently ignored (closed or never opened)
        }
      }
    }
  }

 private:
  // Standard streams in FD model (bit 31 set | stream index).
  // Index 0 = stdin, 1 = stdout, 2 = stderr.
  static constexpr int32_t kStdout = static_cast<int32_t>(0x80000001);
  static constexpr int32_t kStderr = static_cast<int32_t>(0x80000002);

  // Bit 31 distinguishes FD (set) from MCD (clear).
  static constexpr int32_t kFdBit = static_cast<int32_t>(0x80000000);

  // MCD channels: bit-mask -> file stream
  std::map<int32_t, std::unique_ptr<std::fstream>> mcd_channels_;

  // FD table: index -> file stream
  std::map<int32_t, std::unique_ptr<std::fstream>> fd_table_;

  // Next FD index to allocate. Starts at 3 (after stdin=0, stdout=1, stderr=2).
  int32_t next_fd_index_ = 3;

  static auto ResolvePath(std::string_view filename) -> std::filesystem::path {
    std::filesystem::path path{std::string(filename)};
    if (path.is_relative()) {
      path = std::filesystem::current_path() / path;
    }
    return path;
  }
};

}  // namespace lyra::sdk
