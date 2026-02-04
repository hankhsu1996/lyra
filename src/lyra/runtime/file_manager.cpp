#include "lyra/runtime/file_manager.hpp"

#include <cstdint>
#include <filesystem>
#include <fstream>
#include <ios>
#include <iostream>
#include <memory>
#include <optional>
#include <string>
#include <utility>

#include "lyra/runtime/simulation.hpp"

namespace lyra::runtime {

auto FileManager::ResolvePath(const std::string& filename) -> std::string {
  std::filesystem::path p(filename);
  if (p.is_absolute()) {
    return p.lexically_normal().string();
  }
  const auto& base = GetFsBaseDir();
  return (base / p).lexically_normal().string();
}

FileManager::~FileManager() {
  mcd_channels_.clear();
  fd_table_.clear();
}

auto FileManager::FopenMcd(const std::string& filename) -> int32_t {
  auto resolved = ResolvePath(filename);
  // Find first unused MCD bit in [1, 30]
  for (int bit = 1; bit <= kMaxMcdBit; ++bit) {
    if (!mcd_channels_.contains(bit)) {
      auto stream = std::make_unique<std::fstream>(
          resolved, std::ios::out | std::ios::trunc);
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

  auto resolved = ResolvePath(filename);
  auto stream = std::make_unique<std::fstream>(resolved, *flags);
  if (!stream->is_open()) {
    return 0;
  }

  int32_t index = next_fd_index_++;
  fd_table_[index] = FdEntry{.stream = std::move(stream), .pushback = {}};
  return static_cast<int32_t>(kFdBit | static_cast<uint32_t>(index));
}

void FileManager::Fclose(int32_t descriptor) {
  auto udesc = static_cast<uint32_t>(descriptor);

  if (IsFdDescriptor(udesc)) {
    // FD mode: close single file
    int32_t index = DecodeFdIndex(udesc);
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

void FileManager::Fflush(std::optional<int32_t> descriptor) {
  if (!descriptor) {
    // Flush all open files + stdout
    std::cout.flush();
    for (auto& [bit, stream] : mcd_channels_) {
      if (stream && stream->is_open()) {
        stream->flush();
      }
    }
    for (auto& [index, entry] : fd_table_) {
      if (entry.stream && entry.stream->is_open()) {
        entry.stream->flush();
      }
    }
    return;
  }

  auto udesc = static_cast<uint32_t>(*descriptor);

  if (IsFdDescriptor(udesc)) {
    // FD mode: flush single file
    int32_t index = DecodeFdIndex(udesc);
    auto it = fd_table_.find(index);
    if (it != fd_table_.end() && it->second.stream &&
        it->second.stream->is_open()) {
      it->second.stream->flush();
    }
  } else {
    // MCD mode: bit 0 = stdout, bits 1-30 = file channels
    if ((udesc & 1U) != 0) {
      std::cout.flush();
    }
    for (int bit = 1; bit <= kMaxMcdBit; ++bit) {
      if ((udesc & (1U << bit)) != 0) {
        auto it = mcd_channels_.find(bit);
        if (it != mcd_channels_.end() && it->second && it->second->is_open()) {
          it->second->flush();
        }
      }
    }
  }
}

auto FileManager::Fgetc(int32_t descriptor) -> int32_t {
  auto udesc = static_cast<uint32_t>(descriptor);

  // MCD is write-only, return EOF
  if (!IsFdDescriptor(udesc)) {
    return -1;
  }

  int32_t index = DecodeFdIndex(udesc);
  auto it = fd_table_.find(index);
  if (it == fd_table_.end() || !it->second.stream ||
      !it->second.stream->is_open()) {
    return -1;
  }

  FdEntry& entry = it->second;

  // Check pushback first
  if (entry.pushback.has_value()) {
    int ch = *entry.pushback;
    entry.pushback.reset();
    return ch;
  }

  // Read from stream
  int ch = entry.stream->get();
  if (ch == std::char_traits<char>::eof()) {
    return -1;
  }
  return ch;
}

auto FileManager::Ungetc(int32_t character, int32_t descriptor) -> int32_t {
  // If character is EOF, return failure
  if (character == -1) {
    return -1;
  }

  auto udesc = static_cast<uint32_t>(descriptor);

  // MCD is write-only, return EOF
  if (!IsFdDescriptor(udesc)) {
    return -1;
  }

  int32_t index = DecodeFdIndex(udesc);
  auto it = fd_table_.find(index);
  if (it == fd_table_.end() || !it->second.stream ||
      !it->second.stream->is_open()) {
    return -1;
  }

  FdEntry& entry = it->second;

  // Only one character of pushback guaranteed
  if (entry.pushback.has_value()) {
    return -1;
  }

  int stored = character & 0xFF;
  entry.pushback = stored;
  return stored;
}

auto FileManager::Fgets(int32_t descriptor, std::string& out) -> int32_t {
  out.clear();
  auto udesc = static_cast<uint32_t>(descriptor);

  // MCD is write-only, return 0 (error)
  if (!IsFdDescriptor(udesc)) {
    return 0;
  }

  int32_t index = DecodeFdIndex(udesc);
  auto it = fd_table_.find(index);
  if (it == fd_table_.end() || !it->second.stream ||
      !it->second.stream->is_open()) {
    return 0;
  }

  FdEntry& entry = it->second;

  // Read characters until newline or EOF
  int32_t count = 0;
  while (true) {
    int ch;

    // Check pushback first
    if (entry.pushback.has_value()) {
      ch = *entry.pushback;
      entry.pushback.reset();
    } else {
      ch = entry.stream->get();
      if (ch == std::char_traits<char>::eof()) {
        break;  // EOF reached
      }
    }

    out += static_cast<char>(ch);
    ++count;

    // Stop if we hit a newline
    if (ch == '\n') {
      break;
    }
  }

  return count;
}

auto FileManager::FreadBytes(int32_t descriptor, uint8_t* out, size_t max_bytes)
    -> int32_t {
  auto udesc = static_cast<uint32_t>(descriptor);

  // MCD is write-only, return 0 (error)
  if (!IsFdDescriptor(udesc)) {
    return 0;
  }

  int32_t index = DecodeFdIndex(udesc);
  auto it = fd_table_.find(index);
  if (it == fd_table_.end() || !it->second.stream ||
      !it->second.stream->is_open()) {
    return 0;
  }

  FdEntry& entry = it->second;
  int32_t bytes_read = 0;

  while (static_cast<size_t>(bytes_read) < max_bytes) {
    int ch;

    // Check pushback first
    if (entry.pushback.has_value()) {
      ch = *entry.pushback;
      entry.pushback.reset();
    } else {
      ch = entry.stream->get();
      if (ch == std::char_traits<char>::eof()) {
        break;  // EOF reached
      }
    }

    out[bytes_read++] = static_cast<uint8_t>(ch);
  }

  return bytes_read;
}

auto FileManager::CollectStreams(uint32_t descriptor) -> StreamTargets {
  StreamTargets targets;

  if (descriptor == 0) {
    return targets;  // No-op
  }

  if (IsFdDescriptor(descriptor)) {
    // FD mode: look up fd index
    int32_t index = DecodeFdIndex(descriptor);
    auto it = fd_table_.find(index);
    if (it != fd_table_.end() && it->second.stream &&
        it->second.stream->is_open()) {
      targets.file_streams.at(targets.file_stream_count++) =
          it->second.stream.get();
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

}  // namespace lyra::runtime
