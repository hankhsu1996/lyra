#pragma once

#include <array>
#include <cstdint>
#include <fstream>
#include <map>
#include <memory>
#include <optional>
#include <ostream>
#include <string>

namespace lyra::runtime {

// Result of CollectStreams: identifies which output streams a descriptor
// targets.
struct StreamTargets {
  bool include_stdout = false;
  std::array<std::ostream*, 30> file_streams{};
  int file_stream_count = 0;
};

// Manages open file handles for $fopen/$fclose system tasks.
// Supports both MCD (multi-channel descriptor) and FD (file descriptor) modes.
//
// Descriptor encoding (IEEE 1800-2017 21.3):
// - MCD: bit 31 = 0, bits [30:1] = channel flags, bit 0 = stdout
//   Returned by $fopen(filename) as (1 << channel), channel in [1,30]
// - FD:  bit 31 = 1, bits [30:0] = file index (>= 3, 0-2 reserved)
//   Returned by $fopen(filename, mode) as (kFdBit | index)
// - Descriptor 0 is invalid by definition (both modes return 0 on failure)
class FileManager {
 public:
  static constexpr uint32_t kFdBit = 0x80000000;

  // Descriptor decode helpers
  static constexpr auto IsFdDescriptor(uint32_t desc) -> bool {
    return (desc & kFdBit) != 0;
  }
  static constexpr auto DecodeFdIndex(uint32_t desc) -> int32_t {
    return static_cast<int32_t>(desc & ~kFdBit);
  }

  FileManager() = default;
  ~FileManager();
  FileManager(const FileManager&) = delete;
  FileManager(FileManager&&) = delete;
  auto operator=(const FileManager&) -> FileManager& = delete;
  auto operator=(FileManager&&) -> FileManager& = delete;

  // MCD mode: returns (1 << bit_index), bit_index in [1,30]. Returns 0 on
  // failure.
  auto FopenMcd(const std::string& filename) -> int32_t;

  // FD mode: returns (kFdBit | index), index in [3,...]. Returns 0 on failure.
  auto FopenFd(const std::string& filename, const std::string& mode) -> int32_t;

  // Close descriptor. MCD: iterates set bits. FD: closes single file. Invalid:
  // no-op.
  void Fclose(int32_t descriptor);

  // Flush descriptor(s). nullopt = flush all open files + stdout.
  // With descriptor: flush specific file(s) per MCD/FD encoding.
  void Fflush(std::optional<int32_t> descriptor);

  // Read single byte from FD. Returns 0-255 on success, -1 on error/EOF.
  // MCD descriptors always return -1 (MCD is write-only).
  auto Fgetc(int32_t descriptor) -> int32_t;

  // Push character back onto input stream. Returns (c & 0xFF) on success, -1 on
  // failure. If c == -1 (EOF), returns -1. Only one character of pushback
  // guaranteed. MCD descriptors always return -1.
  auto Ungetc(int32_t character, int32_t descriptor) -> int32_t;

  // Decode descriptor and collect target output streams.
  // MCD bit 0 = stdout; bits 1-30 = MCD channels; bit 31 = FD mode.
  auto CollectStreams(uint32_t descriptor) -> StreamTargets;

 private:
  static constexpr int kMaxMcdBit = 30;

  // FD table entry with stream and single-character pushback buffer.
  struct FdEntry {
    std::unique_ptr<std::fstream> stream;
    std::optional<int> pushback;
  };

  std::map<int, std::unique_ptr<std::fstream>> mcd_channels_;
  std::map<int32_t, FdEntry> fd_table_;
  int32_t next_fd_index_ = 3;  // 0=stdin, 1=stdout, 2=stderr reserved

  static auto ParseMode(const std::string& mode)
      -> std::optional<std::ios_base::openmode>;

  // Resolve filename against the runtime's fs_base_dir.
  // Absolute paths returned as-is (normalized); relative paths joined with
  // base.
  static auto ResolvePath(const std::string& filename) -> std::string;
};

}  // namespace lyra::runtime
