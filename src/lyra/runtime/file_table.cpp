#include "lyra/runtime/file_table.hpp"

#include <algorithm>
#include <cerrno>
#include <cstddef>
#include <cstdint>
#include <fstream>
#include <ios>
#include <iostream>
#include <memory>
#include <optional>
#include <ostream>
#include <span>
#include <stop_token>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>
#include <vector>

#include "lyra/runtime/stream_dispatcher.hpp"
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

namespace {

void WriteToFile(
    std::ostream& sink, std::string_view body, bool append_newline) {
  sink.write(body.data(), static_cast<std::streamsize>(body.size()));
  if (append_newline) sink.put('\n');
}

void WriteToStream(
    StreamDispatcher& stream, std::string_view body, bool append_newline) {
  stream.Append(body);
  stream.FinishRecord(append_newline);
}

void DispatchWrite(
    FileTable& files, StreamDispatcher& stream,
    const value::PackedArray& descriptor_pa, std::string_view body,
    bool append_newline) {
  const auto descriptor = static_cast<std::int32_t>(descriptor_pa.ToInt64());
  if (descriptor == 0) return;
  const auto raw = static_cast<std::uint32_t>(descriptor);

  // FD path: bit 31 set => single descriptor. Stdio sentinels route through
  // the stream dispatcher / std::cerr so test-harness ordering with stdout
  // prints is preserved; other FDs go to their owned fstream.
  if ((raw & (1U << 31U)) != 0U) {
    if (descriptor == support::kStdoutFd) {
      WriteToStream(stream, body, append_newline);
      return;
    }
    if (descriptor == support::kStderrFd) {
      WriteToFile(std::cerr, body, append_newline);
      return;
    }
    std::fstream* sink = files.Resolve(descriptor);
    if (sink == nullptr) return;
    WriteToFile(*sink, body, append_newline);
    return;
  }

  // MCD path: iterate bits 0..30 in ascending order. Bit 0 -> stream sink.
  // Bits 1..30 -> FileTable slots. Unset / unmapped bits silently no-op.
  for (std::uint32_t bit = 0; bit <= 30U; ++bit) {
    if ((raw & (1U << bit)) == 0U) continue;
    if (bit == 0U) {
      WriteToStream(stream, body, append_newline);
      continue;
    }
    const auto channel = static_cast<std::int32_t>(1U << bit);
    std::fstream* sink = files.Resolve(channel);
    if (sink == nullptr) continue;
    WriteToFile(*sink, body, append_newline);
  }
}

}  // namespace

void FileTable::Write(
    const value::PackedArray& descriptor, const value::String& text) {
  DispatchWrite(*this, *stream_, descriptor, text.View(), false);
}

void FileTable::Writeln(
    const value::PackedArray& descriptor, const value::String& text) {
  DispatchWrite(*this, *stream_, descriptor, text.View(), true);
}

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

namespace {

// Narrow a 32-bit signed int wrapped in a PackedArray. Identity for
// well-formed int-shaped inputs; descriptor / offset / operation operands
// all enter as `int`.
auto AsInt32(const value::PackedArray& pa) -> std::int32_t {
  return static_cast<std::int32_t>(pa.ToInt64());
}

auto MakeInt(std::int32_t v) -> value::PackedArray {
  return value::PackedArray::Int(v);
}

}  // namespace

auto FileTable::Open(const value::String& name) -> value::PackedArray {
  return MakeInt(Open(name.View(), std::nullopt));
}

auto FileTable::Open(const value::String& name, const value::String& mode)
    -> value::PackedArray {
  return MakeInt(Open(name.View(), mode.View()));
}

void FileTable::Close(const value::PackedArray& descriptor) {
  Close(AsInt32(descriptor));
}

auto FileTable::Getc(const value::PackedArray& fd_pa) -> value::PackedArray {
  const std::int32_t fd = AsInt32(fd_pa);
  auto* slot = ResolveSlot(fd);
  if (slot == nullptr) {
    SetError(fd, EBADF, "$fgetc: not an open file descriptor");
    return MakeInt(-1);
  }
  // LRM 21.3.4: read entries require an FD opened with r or r+ type.
  if (!slot->permits_read) {
    SetError(fd, EBADF, "$fgetc: file not open for reading");
    return MakeInt(-1);
  }
  // LRM 21.3.4.1: drain any byte left by $ungetc (or by the scanner's
  // "offending character" pushback) before consulting the stream.
  if (slot->putback.has_value()) {
    const auto byte = static_cast<unsigned char>(*slot->putback);
    slot->putback.reset();
    return MakeInt(byte);
  }
  const int c = slot->file->get();
  if (c == std::char_traits<char>::eof()) {
    // LRM 21.3.4.1: EOF is signalled as -1 (wider than 8 bits so callers can
    // distinguish from 0xFF). fstream's eof bit is now set; $feof picks it up.
    SetError(fd, 0, "$fgetc: EOF");
    return MakeInt(-1);
  }
  return MakeInt(c & 0xFF);
}

auto FileTable::Ungetc(
    const value::PackedArray& c_pa, const value::PackedArray& fd_pa)
    -> value::PackedArray {
  const std::int32_t fd = AsInt32(fd_pa);
  auto* slot = ResolveSlot(fd);
  if (slot == nullptr) {
    SetError(fd, EBADF, "$ungetc: not an open file descriptor");
    return MakeInt(-1);
  }
  // LRM 21.3.4.1 NOTE: implementations may limit pushback depth. We keep a
  // single-byte buffer per FD; a second $ungetc before any read returns -1
  // per "if an error occurs pushing a character ... code is set to EOF".
  // Using a Lyra-side slot instead of std::fstream's putback area lifts
  // libstdc++'s "must read first" restriction so $ungetc on a freshly-opened
  // stream works as the LRM specifies.
  if (slot->putback.has_value()) {
    SetError(fd, EAGAIN, "$ungetc: putback buffer full");
    return MakeInt(-1);
  }
  slot->putback = static_cast<char>(AsInt32(c_pa) & 0xFF);
  return MakeInt(0);
}

auto FileTable::Gets(value::String& dest, const value::PackedArray& fd_pa)
    -> value::PackedArray {
  const std::int32_t fd = AsInt32(fd_pa);
  auto* slot = ResolveSlot(fd);
  if (slot == nullptr) {
    SetError(fd, EBADF, "$fgets: not an open file descriptor");
    dest = value::String{};
    return MakeInt(0);
  }
  if (!slot->permits_read) {
    SetError(fd, EBADF, "$fgets: file not open for reading");
    dest = value::String{};
    return MakeInt(0);
  }
  // LRM 21.3.4.2 + 21.3.4.1: byte-by-byte loop so any pending $ungetc byte
  // is the first byte of the line. std::getline would bypass the slot-side
  // putback.
  std::string line;
  while (true) {
    int c = 0;
    if (slot->putback.has_value()) {
      c = static_cast<unsigned char>(*slot->putback);
      slot->putback.reset();
    } else {
      c = slot->file->get();
    }
    if (c == std::char_traits<char>::eof()) break;
    line.push_back(static_cast<char>(c));
    if (c == '\n') break;
  }
  if (line.empty()) {
    SetError(fd, 0, "$fgets: EOF");
    dest = value::String{};
    return MakeInt(0);
  }
  dest = value::String{line};
  return MakeInt(static_cast<std::int32_t>(line.size()));
}

auto FileTable::Read(value::PackedArray& dest, const value::PackedArray& fd_pa)
    -> value::PackedArray {
  const std::int32_t fd = AsInt32(fd_pa);
  auto* slot = ResolveSlot(fd);
  if (slot == nullptr) {
    SetError(fd, EBADF, "$fread: not an open file descriptor");
    return MakeInt(0);
  }
  if (!slot->permits_read) {
    SetError(fd, EBADF, "$fread: file not open for reading");
    return MakeInt(0);
  }
  const std::uint64_t width = dest.BitWidth();
  if (width == 0U) {
    SetError(fd, EINVAL, "$fread: destination has zero bit width");
    return MakeInt(0);
  }
  const auto byte_count = static_cast<std::size_t>((width + 7U) / 8U);
  std::vector<char> buf(byte_count, '\0');
  std::size_t pos = 0;
  // LRM 21.3.4.1: any pending $ungetc byte is the next byte read from the
  // FD; place it at the head of the buffer before consulting the stream.
  if (slot->putback.has_value()) {
    buf[pos++] = *slot->putback;
    slot->putback.reset();
  }
  const auto rest = std::span<char>(buf).subspan(pos);
  slot->file->read(rest.data(), static_cast<std::streamsize>(rest.size()));
  const auto got = pos + static_cast<std::size_t>(slot->file->gcount());
  if (got == 0U) {
    SetError(fd, 0, "$fread: EOF");
    return MakeInt(0);
  }
  // LRM 21.3.4.4: 2-value, big-endian (first byte fills the MSBs). On a
  // short read, the trailing buffer is already zero from the vector
  // constructor and lands in the destination's LSBs ("as much as
  // available"). PackedArray::FromBytes supports any width; the
  // destination's declared shape (sign / 4-state) is preserved.
  dest = value::PackedArray::FromBytes(
      buf, width, dest.IsSigned(), dest.IsFourState());
  return MakeInt(static_cast<std::int32_t>(got));
}

namespace {

auto ReadUnpackedImpl(
    FileTable& files, value::UnpackedArray<value::PackedArray>& dest,
    const value::PackedArray& fd_pa, std::int64_t declared_left,
    std::int64_t declared_right, std::int64_t start_sv,
    std::optional<std::int64_t> count) -> value::PackedArray {
  const std::int32_t fd = AsInt32(fd_pa);
  auto* slot = files.ResolveSlot(fd);
  if (slot == nullptr) {
    files.SetError(fd, EBADF, "$fread: not an open file descriptor");
    return MakeInt(0);
  }
  if (!slot->permits_read) {
    files.SetError(fd, EBADF, "$fread: file not open for reading");
    return MakeInt(0);
  }
  if (dest.RawSize() == 0U) return MakeInt(0);

  const bool ascending = declared_left <= declared_right;
  const auto lowest_sv = std::min(declared_left, declared_right);
  const auto highest_sv = std::max(declared_left, declared_right);
  // LRM 21.3.4.4: out-of-range start has no defined behaviour. Stamp EBADF
  // and return 0 so the user sees the failure rather than a silent no-op.
  if (start_sv < lowest_sv || start_sv > highest_sv) {
    files.SetError(fd, EINVAL, "$fread: start index out of array range");
    return MakeInt(0);
  }
  const auto available_elements =
      static_cast<std::size_t>(highest_sv - start_sv + 1);
  const auto target_count =
      count.has_value()
          ? std::min(static_cast<std::size_t>(*count), available_elements)
          : available_elements;

  const auto element_width = dest.RawAt(0).BitWidth();
  const bool elem_signed = dest.RawAt(0).IsSigned();
  const bool elem_four_state = dest.RawAt(0).IsFourState();
  const auto bytes_per_elem =
      static_cast<std::size_t>((element_width + 7U) / 8U);
  std::size_t total_bytes = 0;

  for (std::size_t k = 0; k < target_count; ++k) {
    std::vector<char> buf(bytes_per_elem, '\0');
    std::size_t pos = 0;
    // LRM 21.3.4.1: drain any $ungetc-deposited byte only on the very first
    // element's first byte; subsequent bytes come straight from the stream.
    if (k == 0 && slot->putback.has_value()) {
      buf[pos++] = *slot->putback;
      slot->putback.reset();
    }
    const auto rest = std::span<char>(buf).subspan(pos);
    slot->file->read(rest.data(), static_cast<std::streamsize>(rest.size()));
    const auto got_this = pos + static_cast<std::size_t>(slot->file->gcount());
    if (got_this == 0U) break;
    // Partial element gets LSB zero-padding via PackedArray::FromBytes'
    // "shorter input -> trailing zeros" path -- consistent with the packed
    // form's "as much as available" behaviour.
    auto effective = std::span<const char>(buf).first(got_this);
    auto elem_value = value::PackedArray::FromBytes(
        effective, element_width, elem_signed, elem_four_state);
    const std::int64_t sv_index = start_sv + static_cast<std::int64_t>(k);
    const std::int64_t storage_idx =
        ascending ? (sv_index - declared_left) : (declared_left - sv_index);
    dest.ElementRef(
        value::PackedArray::Int(static_cast<std::int32_t>(storage_idx))) =
        std::move(elem_value);
    total_bytes += got_this;
    if (got_this < bytes_per_elem) break;
  }
  return MakeInt(static_cast<std::int32_t>(total_bytes));
}

}  // namespace

auto FileTable::Read(
    value::UnpackedArray<value::PackedArray>& dest,
    const value::PackedArray& fd, const value::PackedArray& declared_left,
    const value::PackedArray& declared_right,
    const value::PackedArray& sv_start) -> value::PackedArray {
  return ReadUnpackedImpl(
      *this, dest, fd, declared_left.ToInt64(), declared_right.ToInt64(),
      sv_start.ToInt64(), std::nullopt);
}

auto FileTable::Read(
    value::UnpackedArray<value::PackedArray>& dest,
    const value::PackedArray& fd, const value::PackedArray& declared_left,
    const value::PackedArray& declared_right,
    const value::PackedArray& sv_start, const value::PackedArray& count)
    -> value::PackedArray {
  return ReadUnpackedImpl(
      *this, dest, fd, declared_left.ToInt64(), declared_right.ToInt64(),
      sv_start.ToInt64(), count.ToInt64());
}

auto FileTable::Seek(
    const value::PackedArray& fd_pa, const value::PackedArray& offset,
    const value::PackedArray& operation) -> value::PackedArray {
  const std::int32_t fd = AsInt32(fd_pa);
  auto* slot = ResolveSlot(fd);
  if (slot == nullptr) {
    SetError(fd, EBADF, "$fseek: not an open file descriptor");
    return MakeInt(-1);
  }
  const auto offset_v = static_cast<std::streamoff>(AsInt32(offset));
  std::ios_base::seekdir dir{};
  switch (AsInt32(operation)) {
    case 0:
      dir = std::ios_base::beg;
      break;
    case 1:
      dir = std::ios_base::cur;
      break;
    case 2:
      dir = std::ios_base::end;
      break;
    default:
      SetError(fd, EINVAL, "$fseek: operation must be 0, 1, or 2");
      return MakeInt(-1);
  }
  auto& stream = *slot->file;
  stream.clear();
  stream.seekg(offset_v, dir);
  stream.seekp(offset_v, dir);
  if (stream.fail()) {
    SetError(fd, errno, "$fseek: seek failed");
    return MakeInt(-1);
  }
  // LRM 21.3.5: "Repositioning the current file position with $fseek or
  // $rewind shall cancel any $ungetc operations."
  slot->putback.reset();
  return MakeInt(0);
}

auto FileTable::Rewind(const value::PackedArray& fd_pa) -> value::PackedArray {
  return Seek(fd_pa, MakeInt(0), MakeInt(0));
}

auto FileTable::Tell(const value::PackedArray& fd_pa) -> value::PackedArray {
  const std::int32_t fd = AsInt32(fd_pa);
  std::fstream* stream = Resolve(fd);
  if (stream == nullptr) {
    SetError(fd, EBADF, "$ftell: not an open file descriptor");
    return MakeInt(-1);
  }
  const auto pos = stream->tellg();
  if (pos == std::fstream::pos_type{-1}) {
    SetError(fd, errno, "$ftell: tell failed");
    return MakeInt(-1);
  }
  return MakeInt(static_cast<std::int32_t>(pos));
}

auto FileTable::Eof(const value::PackedArray& fd_pa) -> value::PackedArray {
  const std::int32_t fd = AsInt32(fd_pa);
  std::fstream* stream = Resolve(fd);
  if (stream == nullptr) return MakeInt(0);
  return MakeInt(stream->eof() ? 1 : 0);
}

auto FileTable::Error(const value::PackedArray& fd_pa, value::String& dest)
    -> value::PackedArray {
  const std::int32_t fd = AsInt32(fd_pa);
  const int errno_value = LastError(fd);
  if (errno_value == 0) {
    dest = value::String{};
    return MakeInt(0);
  }
  dest = value::String{LastErrorMessage(fd)};
  ClearError(fd);
  return MakeInt(errno_value);
}

void FileTable::Flush() {
  // Flush-all path: iterate every set bit in the entire 1..30 MCD range and
  // every reserved+pool FD index. Cheap enough vs disk cost.
  for (std::uint32_t bit = 1; bit <= 30U; ++bit) {
    std::fstream* s = Resolve(static_cast<std::int32_t>(1U << bit));
    if (s != nullptr) s->flush();
  }
}

void FileTable::Flush(const value::PackedArray& descriptor_pa) {
  const std::int32_t descriptor = AsInt32(descriptor_pa);
  if (descriptor == 0) return;
  const auto raw = static_cast<std::uint32_t>(descriptor);
  if ((raw & (1U << 31U)) != 0U) {
    // FD form: stdio sentinels need no flush from us (the sink-write path
    // routes them through the stream dispatcher / std::cerr).
    std::fstream* s = Resolve(descriptor);
    if (s != nullptr) s->flush();
    return;
  }
  // MCD form: walk set bits and flush each owned channel.
  for (std::uint32_t bit = 1; bit <= 30U; ++bit) {
    if ((raw & (1U << bit)) == 0U) continue;
    std::fstream* s = Resolve(static_cast<std::int32_t>(1U << bit));
    if (s != nullptr) s->flush();
  }
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
