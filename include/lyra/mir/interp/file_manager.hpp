#pragma once

#include <array>
#include <cstdint>
#include <fstream>
#include <map>
#include <memory>
#include <optional>
#include <ostream>
#include <string>

namespace lyra::mir::interp {

// Result of CollectStreams: identifies which output streams a descriptor
// targets.
struct StreamTargets {
  bool include_stdout = false;
  std::array<std::ostream*, 30> file_streams{};
  int file_stream_count = 0;
};

// Manages open file handles for $fopen/$fclose system tasks.
// Supports both MCD (multi-channel descriptor) and FD (file descriptor) modes.
class FileManager {
 public:
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

  // Decode descriptor and collect target output streams.
  // MCD bit 0 = stdout; bits 1-30 = MCD channels; bit 31 = FD mode.
  auto CollectStreams(uint32_t descriptor) -> StreamTargets;

 private:
  static constexpr uint32_t kFdBit = 0x80000000;
  static constexpr int kMaxMcdBit = 30;

  std::map<int, std::unique_ptr<std::fstream>> mcd_channels_;
  std::map<int32_t, std::unique_ptr<std::fstream>> fd_table_;
  int32_t next_fd_index_ = 3;  // 0=stdin, 1=stdout, 2=stderr reserved

  static auto ParseMode(const std::string& mode)
      -> std::optional<std::ios_base::openmode>;
};

}  // namespace lyra::mir::interp
