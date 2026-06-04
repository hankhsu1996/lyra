#include "lyra/runtime/file_table.hpp"

#include <algorithm>
#include <cstdint>
#include <fstream>
#include <ios>
#include <memory>
#include <optional>
#include <stop_token>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>
#include <vector>

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

ChannelCancellation::ChannelCancellation(std::vector<std::stop_token> tokens)
    : tokens_(std::move(tokens)) {
}

auto ChannelCancellation::IsCancelled() const noexcept -> bool {
  return std::ranges::any_of(
      tokens_, [](const std::stop_token& t) { return t.stop_requested(); });
}

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
      if (fd_pool_.at(i).file == nullptr) {
        fd_pool_.at(i).file = std::move(stream);
        fd_pool_.at(i).error = ErrorRecord{};
        return kFdHighBit | static_cast<std::int32_t>(i);
      }
    }
    fd_pool_.emplace_back(
        FdSlot{
            .file = std::move(stream),
            .error = {},
            .cancel_source = {},
            .putback = std::nullopt});
    return kFdHighBit | static_cast<std::int32_t>(fd_pool_.size() - 1);
  }
  // MCD path: open write-truncate (LRM 21.3.1 omits the type for MCD form).
  auto stream = std::make_unique<std::fstream>(
      name_z, std::ios_base::out | std::ios_base::trunc);
  if (!stream->is_open()) return 0;
  for (std::size_t slot = 1; slot < kMcdSlotCount; ++slot) {
    if (mcd_slots_.at(slot).file == nullptr) {
      mcd_slots_.at(slot).file = std::move(stream);
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
    auto& slot = fd_pool_.at(idx);
    slot.file.reset();
    slot.error = ErrorRecord{};
    // LRM 21.3.2: fire cancel on any pending observers; replace the source
    // so the next $fopen on this slot starts with a fresh signal.
    slot.cancel_source.request_stop();
    slot.cancel_source = std::stop_source{};
    // Any pending $ungetc dies with the slot.
    slot.putback.reset();
    return;
  }
  // MCD: iterate each set bit in 1..30 and close that slot. Bit 0 is the
  // stdout sentinel and is never closed.
  for (std::size_t slot_idx = 1; slot_idx < kMcdSlotCount; ++slot_idx) {
    if ((raw & (1U << slot_idx)) == 0U) continue;
    auto& slot = mcd_slots_.at(slot_idx);
    slot.file.reset();
    slot.cancel_source.request_stop();
    slot.cancel_source = std::stop_source{};
  }
}

auto FileTable::Resolve(std::int32_t descriptor) -> std::fstream* {
  if (descriptor == 0) return nullptr;
  const auto raw = static_cast<std::uint32_t>(descriptor);
  if ((raw & (1U << 31U)) != 0U) {
    const std::size_t idx = raw & 0x7FFF'FFFFU;
    // Stdio sentinels (0/1/2) and out-of-range indexes are not owned here.
    if (idx < kFdReservedSlots || idx >= fd_pool_.size()) return nullptr;
    return fd_pool_.at(idx).file.get();
  }
  // MCD single bit. Bit 0 alone is the stdout sentinel -- not owned here.
  for (std::size_t slot = 1; slot < kMcdSlotCount; ++slot) {
    if (raw == (1U << slot)) return mcd_slots_.at(slot).file.get();
  }
  return nullptr;
}

auto FileTable::ResolveSlot(std::int32_t descriptor) -> FdSlot* {
  if (descriptor == 0) return nullptr;
  const auto raw = static_cast<std::uint32_t>(descriptor);
  // Slot access is only meaningful for FD-shape descriptors. MCDs are
  // write-only sinks with no putback / mode state; stdio sentinels are
  // not owned at this layer.
  if ((raw & (1U << 31U)) == 0U) return nullptr;
  const std::size_t idx = raw & 0x7FFF'FFFFU;
  if (idx < kFdReservedSlots || idx >= fd_pool_.size()) return nullptr;
  auto& slot = fd_pool_.at(idx);
  if (slot.file == nullptr) return nullptr;
  return &slot;
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
  fd_pool_.at(*idx).error =
      ErrorRecord{.errno_value = errno_value, .message = std::move(message)};
}

auto FileTable::LastError(std::int32_t fd) const -> int {
  const auto idx = FdPoolIndex(fd, fd_pool_.size());
  if (!idx.has_value()) return 0;
  return fd_pool_.at(*idx).error.errno_value;
}

auto FileTable::LastErrorMessage(std::int32_t fd) const -> std::string_view {
  const auto idx = FdPoolIndex(fd, fd_pool_.size());
  if (!idx.has_value()) return {};
  return fd_pool_.at(*idx).error.message;
}

void FileTable::ClearError(std::int32_t fd) {
  const auto idx = FdPoolIndex(fd, fd_pool_.size());
  if (!idx.has_value()) return;
  fd_pool_.at(*idx).error = ErrorRecord{};
}

auto FileTable::CancellationFor(std::int32_t descriptor)
    -> ChannelCancellation {
  std::vector<std::stop_token> tokens;
  if (descriptor == 0) return ChannelCancellation{std::move(tokens)};
  const auto raw = static_cast<std::uint32_t>(descriptor);
  if ((raw & (1U << 31U)) != 0U) {
    const std::size_t idx = raw & 0x7FFF'FFFFU;
    // Stdio sentinels (0/1/2) and out-of-range indexes have no observable
    // close event -- contribute no token.
    if (idx >= kFdReservedSlots && idx < fd_pool_.size()) {
      tokens.push_back(fd_pool_.at(idx).cancel_source.get_token());
    }
    return ChannelCancellation{std::move(tokens)};
  }
  // MCD: collect one token per set bit in 1..30. Bit 0 is stdout sentinel
  // (cannot be closed) so it contributes nothing.
  for (std::size_t slot = 1; slot < kMcdSlotCount; ++slot) {
    if ((raw & (1U << slot)) == 0U) continue;
    tokens.push_back(mcd_slots_.at(slot).cancel_source.get_token());
  }
  return ChannelCancellation{std::move(tokens)};
}

}  // namespace lyra::runtime
