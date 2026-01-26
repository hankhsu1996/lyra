#include "lyra/mir/interp/file_manager.hpp"

#include <cstdint>
#include <fstream>
#include <ios>
#include <memory>
#include <optional>
#include <string>
#include <utility>

namespace lyra::mir::interp {

FileManager::~FileManager() {
  mcd_channels_.clear();
  fd_table_.clear();
}

auto FileManager::FopenMcd(const std::string& filename) -> int32_t {
  // Find first unused MCD bit in [1, 30]
  for (int bit = 1; bit <= kMaxMcdBit; ++bit) {
    if (!mcd_channels_.contains(bit)) {
      auto stream = std::make_unique<std::fstream>(
          filename, std::ios::out | std::ios::trunc);
      if (!stream->is_open()) {
        return 0;
      }
      mcd_channels_[bit] = std::move(stream);
      return static_cast<int32_t>(1U << bit);
    }
  }
  return 0;  // No channels available
}

auto FileManager::FopenFd(const std::string& filename, const std::string& mode)
    -> int32_t {
  auto flags = ParseMode(mode);
  if (!flags) {
    return 0;
  }

  auto stream = std::make_unique<std::fstream>(filename, *flags);
  if (!stream->is_open()) {
    return 0;
  }

  int32_t index = next_fd_index_++;
  fd_table_[index] = std::move(stream);
  return static_cast<int32_t>(kFdBit | static_cast<uint32_t>(index));
}

void FileManager::Fclose(int32_t descriptor) {
  auto udesc = static_cast<uint32_t>(descriptor);

  if ((udesc & kFdBit) != 0) {
    // FD mode: close single file
    auto index = static_cast<int32_t>(udesc & ~kFdBit);
    fd_table_.erase(index);
  } else {
    // MCD mode: iterate set bits, close each channel
    for (int bit = 1; bit <= kMaxMcdBit; ++bit) {
      if ((udesc & (1U << bit)) != 0) {
        mcd_channels_.erase(bit);
      }
    }
  }
}

auto FileManager::CollectStreams(uint32_t descriptor) -> StreamTargets {
  StreamTargets targets;

  if (descriptor == 0) {
    return targets;  // No-op
  }

  if ((descriptor & kFdBit) != 0) {
    // FD mode: mask off bit 31, look up fd index
    auto index = static_cast<int32_t>(descriptor & ~kFdBit);
    auto it = fd_table_.find(index);
    if (it != fd_table_.end() && it->second && it->second->is_open()) {
      targets.file_streams.at(targets.file_stream_count++) = it->second.get();
    }
  } else {
    // MCD mode: bit 0 = stdout, bits 1-30 = file channels
    if ((descriptor & 1U) != 0) {
      targets.include_stdout = true;
    }
    for (int bit = 1; bit <= kMaxMcdBit; ++bit) {
      if ((descriptor & (1U << bit)) != 0) {
        auto it = mcd_channels_.find(bit);
        if (it != mcd_channels_.end() && it->second && it->second->is_open()) {
          targets.file_streams.at(targets.file_stream_count++) =
              it->second.get();
        }
      }
    }
  }

  return targets;
}

auto FileManager::ParseMode(const std::string& mode)
    -> std::optional<std::ios_base::openmode> {
  std::string base = mode;
  bool binary = false;

  // Check for trailing 'b'
  if (!base.empty() && base.back() == 'b') {
    binary = true;
    base.pop_back();
  }

  std::ios_base::openmode flags{};
  if (base == "r") {
    flags = std::ios::in;
  } else if (base == "w") {
    flags = std::ios::out | std::ios::trunc;
  } else if (base == "a") {
    flags = std::ios::out | std::ios::app;
  } else if (base == "r+" || base == "+r") {
    flags = std::ios::in | std::ios::out;
  } else if (base == "w+" || base == "+w") {
    flags = std::ios::in | std::ios::out | std::ios::trunc;
  } else if (base == "a+" || base == "+a") {
    flags = std::ios::in | std::ios::out | std::ios::app;
  } else {
    return std::nullopt;
  }

  if (binary) {
    flags |= std::ios::binary;
  }

  return flags;
}

}  // namespace lyra::mir::interp
