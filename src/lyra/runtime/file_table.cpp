#include "lyra/runtime/file_table.hpp"

#include <algorithm>
#include <cerrno>
#include <cstddef>
#include <cstdint>
#include <fstream>
#include <ios>
#include <memory>
#include <optional>
#include <span>
#include <stop_token>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>
#include <vector>

#include "lyra/support/file_descriptor.hpp"

namespace lyra::runtime {

namespace {

// LRM 21.3.1 Table 21-6 mode strings -> openmode flags + per-direction
// permission bits. Unknown modes return nullopt so $fopen yields 0 per
// LRM "if a file cannot be opened ... a zero is returned". The
// permission bits drive LRM 21.3.4's "fd can be read from only if
// opened with r or r+" check in the read entries.
struct ParsedMode {
  std::ios_base::openmode flags;
  bool permits_read;
  bool permits_write;
};

auto ParseMode(std::string_view mode) -> std::optional<ParsedMode> {
  using std::ios_base;
  struct ModeRow {
    ios_base::openmode flags;
    bool read;
    bool write;
  };
  constexpr auto kRead = ios_base::in;
  constexpr auto kWrite = ios_base::out;
  constexpr auto kTrunc = ios_base::trunc;
  constexpr auto kApp = ios_base::app;
  constexpr auto kBin = ios_base::binary;
  static const std::unordered_map<std::string_view, ModeRow> kMap{
      {"r", {.flags = kRead, .read = true, .write = false}},
      {"rb", {.flags = kRead | kBin, .read = true, .write = false}},
      {"w", {.flags = kWrite | kTrunc, .read = false, .write = true}},
      {"wb", {.flags = kWrite | kTrunc | kBin, .read = false, .write = true}},
      {"a", {.flags = kWrite | kApp, .read = false, .write = true}},
      {"ab", {.flags = kWrite | kApp | kBin, .read = false, .write = true}},
      {"r+", {.flags = kRead | kWrite, .read = true, .write = true}},
      {"rb+", {.flags = kRead | kWrite | kBin, .read = true, .write = true}},
      {"r+b", {.flags = kRead | kWrite | kBin, .read = true, .write = true}},
      {"w+", {.flags = kRead | kWrite | kTrunc, .read = true, .write = true}},
      {"wb+",
       {.flags = kRead | kWrite | kTrunc | kBin, .read = true, .write = true}},
      {"w+b",
       {.flags = kRead | kWrite | kTrunc | kBin, .read = true, .write = true}},
      {"a+", {.flags = kRead | kWrite | kApp, .read = true, .write = true}},
      {"ab+",
       {.flags = kRead | kWrite | kApp | kBin, .read = true, .write = true}},
      {"a+b",
       {.flags = kRead | kWrite | kApp | kBin, .read = true, .write = true}},
  };
  const auto it = kMap.find(mode);
  if (it == kMap.end()) return std::nullopt;
  return ParsedMode{
      .flags = it->second.flags,
      .permits_read = it->second.read,
      .permits_write = it->second.write};
}

}  // namespace

ChannelCancellation::ChannelCancellation(std::vector<std::stop_token> tokens)
    : tokens_(std::move(tokens)) {
}

auto ChannelCancellation::IsCancelled() const noexcept
    -> lyra::value::PackedArray {
  const bool cancelled = std::ranges::any_of(
      tokens_, [](const std::stop_token& t) { return t.stop_requested(); });
  return lyra::value::PackedArray::Bit(cancelled);
}

auto FileTable::Open(
    std::string_view name, std::optional<std::string_view> mode)
    -> std::int32_t {
  const std::string name_z{name};
  if (mode.has_value()) {
    const auto parsed = ParseMode(*mode);
    if (!parsed.has_value()) return 0;
    auto stream = std::make_unique<std::fstream>(name_z, parsed->flags);
    if (!stream->is_open()) return 0;
    // Reuse the first free slot above the reserved stdio indexes; grow on
    // demand. fstream destructor closes/flushes on slot reset.
    for (std::size_t i = kFdReservedSlots; i < fd_pool_.size(); ++i) {
      if (fd_pool_.at(i).file == nullptr) {
        fd_pool_.at(i).file = std::move(stream);
        fd_pool_.at(i).error = ErrorRecord{};
        fd_pool_.at(i).permits_read = parsed->permits_read;
        fd_pool_.at(i).permits_write = parsed->permits_write;
        return support::kFdHighBit | static_cast<std::int32_t>(i);
      }
    }
    fd_pool_.emplace_back(
        FdSlot{
            .file = std::move(stream),
            .error = {},
            .cancel_source = {},
            .putback = std::nullopt,
            .permits_read = parsed->permits_read,
            .permits_write = parsed->permits_write});
    return support::kFdHighBit | static_cast<std::int32_t>(fd_pool_.size() - 1);
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

auto FileTable::PeekBuffered(const value::PackedArray& fd_pa) -> value::String {
  const auto fd = static_cast<std::int32_t>(fd_pa.ToInt64());
  auto* slot = ResolveSlot(fd);
  if (slot == nullptr) {
    SetError(fd, EBADF, "$fscanf: not an open file descriptor");
    return value::String{};
  }
  if (!slot->permits_read) {
    SetError(fd, EBADF, "$fscanf: file not open for reading");
    return value::String{};
  }
  std::string out;
  // LRM 21.3.4.1: any byte left by `$ungetc` (or a prior scan's offending
  // character) is observable before the underlying stream.
  if (slot->putback.has_value()) {
    out.push_back(*slot->putback);
  }
  // Drain the rest of the file from the current stream position. Read the
  // remainder in one shot via tellg/seekg + a sized buffer rather than
  // byte-by-byte to keep the hot path linear in file length.
  std::fstream* file = slot->file.get();
  const auto here = file->tellg();
  file->seekg(0, std::ios::end);
  const auto end = file->tellg();
  file->seekg(here);
  std::size_t file_bytes = 0;
  if (here != std::fstream::pos_type{-1} && end != std::fstream::pos_type{-1} &&
      end > here) {
    file_bytes = static_cast<std::size_t>(end - here);
  }
  if (file_bytes > 0) {
    const std::size_t prefix = out.size();
    out.resize(prefix + file_bytes);
    const std::span<char> tail = std::span{out}.subspan(prefix);
    file->read(tail.data(), static_cast<std::streamsize>(tail.size()));
    const auto got = static_cast<std::size_t>(file->gcount());
    out.resize(prefix + got);
    file_bytes = got;
  }
  slot->peek_len = file_bytes;
  // The `PeekBuffered` snapshot virtualises the putback byte into the
  // returned string; only `AdvanceFd` decides whether to drain it. Clearing
  // it here would lose the byte if the parser ultimately did not consume
  // it. Leave putback intact; `AdvanceFd` consumes it explicitly.
  return value::String(std::move(out));
}

void FileTable::AdvanceFd(
    const value::PackedArray& fd_pa, const value::PackedArray& n_pa) {
  const auto fd = static_cast<std::int32_t>(fd_pa.ToInt64());
  auto* slot = ResolveSlot(fd);
  if (slot == nullptr) return;
  if (!slot->permits_read) return;
  const auto n = static_cast<std::int32_t>(n_pa.ToInt64());
  auto remaining = static_cast<std::size_t>(n < 0 ? 0 : n);
  const std::size_t peek_len = slot->peek_len;
  slot->peek_len = 0;
  // Putback byte (if any) was the first byte the parser saw. Consume it
  // first so the count we apply to the file stream is purely file-relative.
  const bool had_putback = slot->putback.has_value();
  if (had_putback) {
    if (remaining > 0) {
      slot->putback.reset();
      --remaining;
    }
  }
  // `remaining` is now the number of file bytes the parser consumed out of
  // `peek_len`. Anything past that has to go back to the stream so the next
  // read sees it again.
  const std::size_t file_consumed = std::min(remaining, peek_len);
  const std::size_t file_unconsumed = peek_len - file_consumed;
  if (file_unconsumed == 0) return;
  std::fstream* file = slot->file.get();
  if (file_unconsumed == 1U && !slot->putback.has_value()) {
    // LRM 21.3.4.1: a single leftover byte is the "offending input
    // character" -- park it in the slot's putback so $fgetc / $fgets /
    // $fread / the next scan-source see it without a stream seek.
    file->seekg(-1, std::ios::cur);
    const int ch = file->get();
    if (ch != std::char_traits<char>::eof()) {
      slot->putback = static_cast<char>(ch);
    }
    return;
  }
  // Multi-byte tail: rewind the stream by the unconsumed count.
  file->seekg(-static_cast<std::streamoff>(file_unconsumed), std::ios::cur);
}

auto FileTable::CancellationFor(const lyra::value::PackedArray& descriptor)
    -> ChannelCancellation {
  std::vector<std::stop_token> tokens;
  const auto raw_signed = static_cast<std::int32_t>(descriptor.ToInt64());
  if (raw_signed == 0) return ChannelCancellation{std::move(tokens)};
  const auto raw = static_cast<std::uint32_t>(raw_signed);
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
