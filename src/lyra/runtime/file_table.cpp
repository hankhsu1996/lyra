#include "lyra/runtime/file_table.hpp"

#include <cstdint>
#include <fstream>
#include <ios>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>

namespace lyra::runtime {

namespace {

// LRM 21.3.1 Table 21-6 mode strings -> std::ios_base::openmode flags.
// Unknown modes return nullopt so $fopen yields 0 per LRM "if a file cannot
// be opened ... a zero is returned".
auto ParseMode(std::string_view mode)
    -> std::optional<std::ios_base::openmode> {
  using std::ios_base;
  static const std::unordered_map<std::string_view, ios_base::openmode> kMap{
      {"r", ios_base::in},
      {"rb", ios_base::in | ios_base::binary},
      {"w", ios_base::out | ios_base::trunc},
      {"wb", ios_base::out | ios_base::trunc | ios_base::binary},
      {"a", ios_base::out | ios_base::app},
      {"ab", ios_base::out | ios_base::app | ios_base::binary},
      {"r+", ios_base::in | ios_base::out},
      {"rb+", ios_base::in | ios_base::out | ios_base::binary},
      {"r+b", ios_base::in | ios_base::out | ios_base::binary},
      {"w+", ios_base::in | ios_base::out | ios_base::trunc},
      {"wb+",
       ios_base::in | ios_base::out | ios_base::trunc | ios_base::binary},
      {"w+b",
       ios_base::in | ios_base::out | ios_base::trunc | ios_base::binary},
      {"a+", ios_base::in | ios_base::out | ios_base::app},
      {"ab+", ios_base::in | ios_base::out | ios_base::app | ios_base::binary},
      {"a+b", ios_base::in | ios_base::out | ios_base::app | ios_base::binary},
  };
  const auto it = kMap.find(mode);
  if (it == kMap.end()) return std::nullopt;
  return it->second;
}

}  // namespace

auto FileTable::Open(
    std::string_view name, std::optional<std::string_view> mode)
    -> std::int32_t {
  const std::string name_z{name};
  if (mode.has_value()) {
    const auto flags = ParseMode(*mode);
    if (!flags.has_value()) return 0;
    auto stream = std::make_unique<std::fstream>(name_z, *flags);
    if (!stream->is_open()) return 0;
    // Reuse the first free slot above the reserved stdio indexes; grow on
    // demand. fstream destructor closes/flushes on slot reset.
    for (std::size_t i = kFdReservedSlots; i < fd_pool_.size(); ++i) {
      if (fd_pool_.at(i) == nullptr) {
        fd_pool_.at(i) = std::move(stream);
        fd_errors_.at(i) = ErrorRecord{};
        return kFdHighBit | static_cast<std::int32_t>(i);
      }
    }
    fd_pool_.push_back(std::move(stream));
    fd_errors_.emplace_back();
    return kFdHighBit | static_cast<std::int32_t>(fd_pool_.size() - 1);
  }
  // MCD path: open write-truncate (LRM 21.3.1 omits the type for MCD form).
  auto stream = std::make_unique<std::fstream>(
      name_z, std::ios_base::out | std::ios_base::trunc);
  if (!stream->is_open()) return 0;
  for (std::size_t slot = 1; slot < kMcdSlotCount; ++slot) {
    if (mcd_slots_.at(slot) == nullptr) {
      mcd_slots_.at(slot) = std::move(stream);
      return static_cast<std::int32_t>(1U << slot);
    }
  }
  // All slots in use: stream's destructor closes the just-opened handle so
  // nothing leaks. Return 0 per LRM.
  return 0;
}

void FileTable::Close(std::int32_t descriptor) {
  if (descriptor == 0) return;
  const auto raw = static_cast<std::uint32_t>(descriptor);
  if ((raw & (1U << 31U)) != 0U) {
    const std::size_t idx = raw & 0x7FFF'FFFFU;
    if (idx < kFdReservedSlots || idx >= fd_pool_.size()) return;
    fd_pool_.at(idx).reset();
    fd_errors_.at(idx) = ErrorRecord{};
    return;
  }
  // MCD: iterate each set bit in 1..30 and close that slot. Bit 0 is the
  // stdout sentinel and is never closed.
  for (std::size_t slot = 1; slot < kMcdSlotCount; ++slot) {
    if ((raw & (1U << slot)) == 0U) continue;
    mcd_slots_.at(slot).reset();
  }
}

auto FileTable::Resolve(std::int32_t descriptor) -> std::fstream* {
  if (descriptor == 0) return nullptr;
  const auto raw = static_cast<std::uint32_t>(descriptor);
  if ((raw & (1U << 31U)) != 0U) {
    const std::size_t idx = raw & 0x7FFF'FFFFU;
    // Stdio sentinels (0/1/2) and out-of-range indexes are not owned here.
    if (idx < kFdReservedSlots || idx >= fd_pool_.size()) return nullptr;
    return fd_pool_.at(idx).get();
  }
  // MCD single bit. Bit 0 alone is the stdout sentinel -- not owned here.
  for (std::size_t slot = 1; slot < kMcdSlotCount; ++slot) {
    if (raw == (1U << slot)) return mcd_slots_.at(slot).get();
  }
  return nullptr;
}

namespace {

// Decode an FD-shaped descriptor into a pool index, or nullopt for any value
// that's not an owned FD (MCD, stdio sentinel, out of range, or zero).
auto FdPoolIndex(std::int32_t descriptor, std::size_t pool_size)
    -> std::optional<std::size_t> {
  if (descriptor == 0) return std::nullopt;
  const auto raw = static_cast<std::uint32_t>(descriptor);
  if ((raw & (1U << 31U)) == 0U) return std::nullopt;
  const std::size_t idx = raw & 0x7FFF'FFFFU;
  if (idx < 3U || idx >= pool_size) return std::nullopt;
  return idx;
}

}  // namespace

void FileTable::SetError(
    std::int32_t fd, int errno_value, std::string message) {
  const auto idx = FdPoolIndex(fd, fd_pool_.size());
  if (!idx.has_value()) return;
  fd_errors_.at(*idx) =
      ErrorRecord{.errno_value = errno_value, .message = std::move(message)};
}

auto FileTable::LastError(std::int32_t fd) const -> int {
  const auto idx = FdPoolIndex(fd, fd_pool_.size());
  if (!idx.has_value()) return 0;
  return fd_errors_.at(*idx).errno_value;
}

auto FileTable::LastErrorMessage(std::int32_t fd) const -> std::string_view {
  const auto idx = FdPoolIndex(fd, fd_pool_.size());
  if (!idx.has_value()) return {};
  return fd_errors_.at(*idx).message;
}

void FileTable::ClearError(std::int32_t fd) {
  const auto idx = FdPoolIndex(fd, fd_pool_.size());
  if (!idx.has_value()) return;
  fd_errors_.at(*idx) = ErrorRecord{};
}

}  // namespace lyra::runtime
