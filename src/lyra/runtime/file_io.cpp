#include "lyra/runtime/file_io.hpp"

#include <cerrno>
#include <cstdint>
#include <fstream>
#include <functional>
#include <ios>
#include <iostream>
#include <optional>
#include <ostream>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/overloaded.hpp"
#include "lyra/runtime/byte_codec.hpp"
#include "lyra/runtime/file_table.hpp"
#include "lyra/runtime/runtime_services.hpp"
#include "lyra/runtime/stream_dispatcher.hpp"
#include "lyra/value/format.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/string.hpp"

namespace lyra::runtime {

namespace {

auto FormatItemsToString(
    RuntimeServices& services, std::span<const value::PrintItem> items)
    -> std::string {
  const value::FormatContext ctx{.time_format = &services.TimeFormat()};
  std::string out;
  for (const value::PrintItem& item : items) {
    std::visit(
        Overloaded{
            [&](const value::PrintLiteralItem& lit) {
              out.append(std::string_view{lit.data, lit.size});
            },
            [&](const value::PrintValueItem& v) {
              out.append(value::Format(v.spec, v.arg, ctx));
            },
        },
        item);
  }
  return out;
}

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

}  // namespace

auto LyraFOpen(
    RuntimeServices& services, const value::String& name,
    std::optional<value::String> mode) -> value::PackedArray {
  std::optional<std::string_view> mode_view = std::nullopt;
  if (mode.has_value()) mode_view = mode->View();
  return value::PackedArray::Int(services.Files().Open(name.View(), mode_view));
}

void LyraFClose(
    RuntimeServices& services, const value::PackedArray& descriptor) {
  services.Files().Close(static_cast<std::int32_t>(descriptor.ToInt64()));
}

void LyraSubmitFStrobe(
    RuntimeServices& services, const value::PackedArray& descriptor,
    std::function<void()> print_action) {
  auto cancel = services.Files().CancellationFor(
      static_cast<std::int32_t>(descriptor.ToInt64()));
  services.SubmitPostponed(
      [cancel = std::move(cancel), action = std::move(print_action)]() mutable {
        if (cancel.IsCancelled()) return;
        action();
      });
}

void LyraFPrint(
    RuntimeServices& services, value::PrintKind kind,
    const value::PackedArray& descriptor_pa,
    std::span<const value::PrintItem> items) {
  const auto descriptor = static_cast<std::int32_t>(descriptor_pa.ToInt64());
  if (descriptor == 0) return;
  const bool append_newline =
      kind == value::PrintKind::kDisplay || kind == value::PrintKind::kFDisplay;
  const std::string body = FormatItemsToString(services, items);
  const auto raw = static_cast<std::uint32_t>(descriptor);

  // FD path: bit 31 set => single descriptor. Stdio sentinels route through
  // services.Stream() / std::cerr so test-harness ordering with $display is
  // preserved; other FDs go to their owned fstream.
  if ((raw & (1U << 31U)) != 0U) {
    if (descriptor == FileTable::kStdoutFd) {
      WriteToStream(services.Stream(), body, append_newline);
      return;
    }
    if (descriptor == FileTable::kStderrFd) {
      WriteToFile(std::cerr, body, append_newline);
      return;
    }
    std::fstream* sink = services.Files().Resolve(descriptor);
    if (sink == nullptr) return;
    WriteToFile(*sink, body, append_newline);
    return;
  }

  // MCD path: iterate bits 0..30 in ascending order. Bit 0 -> stream sink.
  // Bits 1..30 -> FileTable. Unset / unmapped bits silently no-op.
  for (std::uint32_t bit = 0; bit <= 30U; ++bit) {
    if ((raw & (1U << bit)) == 0U) continue;
    if (bit == 0U) {
      WriteToStream(services.Stream(), body, append_newline);
      continue;
    }
    const auto channel = static_cast<std::int32_t>(1U << bit);
    std::fstream* sink = services.Files().Resolve(channel);
    if (sink == nullptr) continue;
    WriteToFile(*sink, body, append_newline);
  }
}

namespace {

// Narrow a 32-bit signed int wrapped in a PackedArray. Identity for
// well-formed int-shaped inputs; callers depend on this for descriptor /
// offset / operation operands which all enter as `int`.
auto AsInt32(const value::PackedArray& pa) -> std::int32_t {
  return static_cast<std::int32_t>(pa.ToInt64());
}

auto MakeInt(std::int32_t v) -> value::PackedArray {
  return value::PackedArray::Int(v);
}

// LRM 21.3.7: $ferror reports the most recent file-I/O error for an FD.
// Stamp the descriptive message verbatim (naming the system task) so a
// caller reading $ferror after a failure gets a useful string; the errno
// is reported separately as the return value of $ferror.
void Stamp(
    RuntimeServices& services, std::int32_t fd, int err,
    std::string_view message) {
  services.Files().SetError(fd, err, std::string{message});
}

}  // namespace

auto LyraFGetc(RuntimeServices& services, const value::PackedArray& fd_pa)
    -> value::PackedArray {
  const std::int32_t fd = AsInt32(fd_pa);
  auto* slot = services.Files().ResolveSlot(fd);
  if (slot == nullptr) {
    Stamp(services, fd, EBADF, "$fgetc: not an open file descriptor");
    return MakeInt(-1);
  }
  // LRM 21.3.4: read entries require an FD opened with r or r+ type.
  if (!slot->permits_read) {
    Stamp(services, fd, EBADF, "$fgetc: file not open for reading");
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
    Stamp(services, fd, 0, "$fgetc: EOF");
    return MakeInt(-1);
  }
  return MakeInt(c & 0xFF);
}

auto LyraFUngetc(
    RuntimeServices& services, const value::PackedArray& c_pa,
    const value::PackedArray& fd_pa) -> value::PackedArray {
  const std::int32_t fd = AsInt32(fd_pa);
  auto* slot = services.Files().ResolveSlot(fd);
  if (slot == nullptr) {
    Stamp(services, fd, EBADF, "$ungetc: not an open file descriptor");
    return MakeInt(-1);
  }
  // LRM 21.3.4.1 NOTE: implementations may limit pushback depth. We keep a
  // single-byte buffer per FD; a second $ungetc before any read returns
  // EOF per "if an error occurs pushing a character ... code is set to
  // EOF". Using a Lyra-side slot instead of std::fstream's putback area
  // lifts libstdc++'s "must read first" restriction so $ungetc on a
  // freshly-opened stream works as the LRM specifies.
  if (slot->putback.has_value()) {
    Stamp(services, fd, EAGAIN, "$ungetc: putback buffer full");
    return MakeInt(-1);
  }
  slot->putback = static_cast<char>(AsInt32(c_pa) & 0xFF);
  return MakeInt(0);
}

auto LyraFGets(
    RuntimeServices& services, value::String& dest,
    const value::PackedArray& fd_pa) -> value::PackedArray {
  const std::int32_t fd = AsInt32(fd_pa);
  auto* slot = services.Files().ResolveSlot(fd);
  if (slot == nullptr) {
    Stamp(services, fd, EBADF, "$fgets: not an open file descriptor");
    dest = value::String{};
    return MakeInt(0);
  }
  if (!slot->permits_read) {
    Stamp(services, fd, EBADF, "$fgets: file not open for reading");
    dest = value::String{};
    return MakeInt(0);
  }
  // LRM 21.3.4.2 + 21.3.4.1: byte-by-byte loop so any pending $ungetc
  // byte is the first byte of the line. std::getline would bypass the
  // slot-side putback.
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
    Stamp(services, fd, 0, "$fgets: EOF");
    dest = value::String{};
    return MakeInt(0);
  }
  dest = value::String{line};
  return MakeInt(static_cast<std::int32_t>(line.size()));
}

auto LyraFRead(
    RuntimeServices& services, value::PackedArray& dest,
    const value::PackedArray& fd_pa) -> value::PackedArray {
  const std::int32_t fd = AsInt32(fd_pa);
  auto* slot = services.Files().ResolveSlot(fd);
  if (slot == nullptr) {
    Stamp(services, fd, EBADF, "$fread: not an open file descriptor");
    return MakeInt(0);
  }
  if (!slot->permits_read) {
    Stamp(services, fd, EBADF, "$fread: file not open for reading");
    return MakeInt(0);
  }
  const std::uint64_t width = dest.BitWidth();
  if (width == 0U) {
    Stamp(services, fd, EINVAL, "$fread: destination has zero bit width");
    return MakeInt(0);
  }
  const auto byte_count = static_cast<std::size_t>((width + 7U) / 8U);
  std::vector<char> buf(byte_count, '\0');
  std::size_t pos = 0;
  // LRM 21.3.4.1: any pending $ungetc byte is the next byte read from
  // the FD; place it at the head of the buffer before consulting the
  // stream.
  if (slot->putback.has_value()) {
    buf[pos++] = *slot->putback;
    slot->putback.reset();
  }
  const auto rest = std::span<char>(buf).subspan(pos);
  slot->file->read(rest.data(), static_cast<std::streamsize>(rest.size()));
  const auto got = pos + static_cast<std::size_t>(slot->file->gcount());
  if (got == 0U) {
    Stamp(services, fd, 0, "$fread: EOF");
    return MakeInt(0);
  }
  // LRM 21.3.4.4: 2-value, big-endian (first byte fills the MSBs). On a
  // short read, the trailing buffer is already zero from the vector
  // constructor and lands in the destination's LSBs ("as much as
  // available"). BytesToPackedArray supports any width; the destination's
  // declared shape (sign / 4-state) is preserved.
  dest = BytesToPackedArray(buf, width, dest.IsSigned(), dest.IsFourState());
  return MakeInt(static_cast<std::int32_t>(got));
}

auto LyraFRead(
    RuntimeServices& services, value::UnpackedArray<value::PackedArray>& dest,
    const value::PackedArray& fd_pa, std::optional<std::int64_t> sv_start,
    std::optional<std::int64_t> count, std::int64_t declared_left,
    std::int64_t declared_right) -> value::PackedArray {
  const std::int32_t fd = AsInt32(fd_pa);
  auto* slot = services.Files().ResolveSlot(fd);
  if (slot == nullptr) {
    Stamp(services, fd, EBADF, "$fread: not an open file descriptor");
    return MakeInt(0);
  }
  if (!slot->permits_read) {
    Stamp(services, fd, EBADF, "$fread: file not open for reading");
    return MakeInt(0);
  }
  if (dest.Size() == 0U) return MakeInt(0);

  const bool ascending = declared_left <= declared_right;
  const auto lowest_sv = std::min(declared_left, declared_right);
  const auto highest_sv = std::max(declared_left, declared_right);
  const auto start_sv = sv_start.value_or(lowest_sv);
  // LRM 21.3.4.4: out-of-range start has no defined behaviour. Stamp EBADF
  // and return 0 so the user sees the failure rather than a silent no-op.
  if (start_sv < lowest_sv || start_sv > highest_sv) {
    Stamp(services, fd, EINVAL, "$fread: start index out of array range");
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
    // Partial element gets LSB zero-padding via BytesToPackedArray's
    // "shorter input -> trailing zeros" path -- consistent with the
    // packed form's "as much as available" behaviour.
    auto effective = std::span<const char>(buf).first(got_this);
    auto elem_value = BytesToPackedArray(
        effective, element_width, elem_signed, elem_four_state);
    const std::int64_t sv_index = start_sv + static_cast<std::int64_t>(k);
    const std::int64_t storage_idx =
        ascending ? (sv_index - declared_left) : (declared_left - sv_index);
    dest.ElementAt(
        value::PackedArray::Int(static_cast<std::int32_t>(storage_idx))) =
        std::move(elem_value);
    total_bytes += got_this;
    if (got_this < bytes_per_elem) break;
  }
  return MakeInt(static_cast<std::int32_t>(total_bytes));
}

auto LyraFSeek(
    RuntimeServices& services, const value::PackedArray& fd_pa,
    const value::PackedArray& offset, const value::PackedArray& operation)
    -> value::PackedArray {
  const std::int32_t fd = AsInt32(fd_pa);
  auto* slot = services.Files().ResolveSlot(fd);
  if (slot == nullptr) {
    Stamp(services, fd, EBADF, "$fseek: not an open file descriptor");
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
      Stamp(services, fd, EINVAL, "$fseek: operation must be 0, 1, or 2");
      return MakeInt(-1);
  }
  auto& stream = *slot->file;
  stream.clear();
  stream.seekg(offset_v, dir);
  stream.seekp(offset_v, dir);
  if (stream.fail()) {
    Stamp(services, fd, errno, "$fseek: seek failed");
    return MakeInt(-1);
  }
  // LRM 21.3.5: "Repositioning the current file position with $fseek or
  // $rewind shall cancel any $ungetc operations."
  slot->putback.reset();
  return MakeInt(0);
}

auto LyraFRewind(RuntimeServices& services, const value::PackedArray& fd_pa)
    -> value::PackedArray {
  return LyraFSeek(services, fd_pa, MakeInt(0), MakeInt(0));
}

auto LyraFTell(RuntimeServices& services, const value::PackedArray& fd_pa)
    -> value::PackedArray {
  const std::int32_t fd = AsInt32(fd_pa);
  std::fstream* stream = services.Files().Resolve(fd);
  if (stream == nullptr) {
    Stamp(services, fd, EBADF, "$ftell: not an open file descriptor");
    return MakeInt(-1);
  }
  const auto pos = stream->tellg();
  if (pos == std::fstream::pos_type{-1}) {
    Stamp(services, fd, errno, "$ftell: tell failed");
    return MakeInt(-1);
  }
  return MakeInt(static_cast<std::int32_t>(pos));
}

auto LyraFEof(RuntimeServices& services, const value::PackedArray& fd_pa)
    -> value::PackedArray {
  const std::int32_t fd = AsInt32(fd_pa);
  std::fstream* stream = services.Files().Resolve(fd);
  if (stream == nullptr) return MakeInt(0);
  return MakeInt(stream->eof() ? 1 : 0);
}

auto LyraFError(
    RuntimeServices& services, const value::PackedArray& fd_pa,
    value::String& dest) -> value::PackedArray {
  const std::int32_t fd = AsInt32(fd_pa);
  const int errno_value = services.Files().LastError(fd);
  if (errno_value == 0) {
    dest = value::String{};
    return MakeInt(0);
  }
  dest = value::String{services.Files().LastErrorMessage(fd)};
  services.Files().ClearError(fd);
  return MakeInt(errno_value);
}

void LyraFFlush(
    RuntimeServices& services,
    std::optional<value::PackedArray> descriptor_pa) {
  // Flush-all path: iterate every set bit in the entire 1..30 MCD range and
  // every reserved+pool FD index. Cheap enough vs disk cost.
  if (!descriptor_pa.has_value()) {
    for (std::uint32_t bit = 1; bit <= 30U; ++bit) {
      std::fstream* s =
          services.Files().Resolve(static_cast<std::int32_t>(1U << bit));
      if (s != nullptr) s->flush();
    }
    return;
  }
  const std::int32_t descriptor = AsInt32(*descriptor_pa);
  if (descriptor == 0) return;
  const auto raw = static_cast<std::uint32_t>(descriptor);
  if ((raw & (1U << 31U)) != 0U) {
    // FD form: stdio sentinels need no flush from us (LyraFPrint already
    // routes them through services.Stream() / std::cerr).
    std::fstream* s = services.Files().Resolve(descriptor);
    if (s != nullptr) s->flush();
    return;
  }
  // MCD form: walk set bits and flush each owned channel.
  for (std::uint32_t bit = 1; bit <= 30U; ++bit) {
    if ((raw & (1U << bit)) == 0U) continue;
    std::fstream* s =
        services.Files().Resolve(static_cast<std::int32_t>(1U << bit));
    if (s != nullptr) s->flush();
  }
}

}  // namespace lyra::runtime
