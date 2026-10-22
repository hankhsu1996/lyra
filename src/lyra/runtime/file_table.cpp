#include "lyra/runtime/file_table.hpp"

#include <algorithm>
#include <cerrno>
#include <cstddef>
#include <cstdint>
#include <format>
#include <fstream>
#include <ios>
#include <iostream>
#include <memory>
#include <optional>
#include <span>
#include <stop_token>
#include <streambuf>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/support/file_descriptor.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/string.hpp"
#include "lyra/value/unpacked_range.hpp"

namespace lyra::runtime {

namespace {

// LRM 21.3.1 Table 21-6 mode strings -> openmode flags + per-direction
// permissions. Unknown modes return nullopt so $fopen yields 0 per LRM "if a
// file cannot be opened ... a zero is returned". The permissions decide whether
// an opened file is read, written or both, which is what LRM 21.3.4's "fd can
// be read from only if opened with r or r+" comes down to.
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

// The `int` the source wrote for a descriptor, offset, operation or character
// (LRM 21.3), which reaches here widened to a machine integer.
auto AsInt32(std::int64_t value) -> std::int32_t {
  return static_cast<std::int32_t>(value);
}

auto MakeInt(std::int32_t v) -> value::Int {
  return value::Int::FromInt(v);
}

auto IsFileDescriptor(std::int32_t descriptor) -> bool {
  return (static_cast<std::uint32_t>(descriptor) & (1U << 31U)) != 0U;
}

// The index the low 31 bits of a file descriptor carry (LRM 21.3.1).
auto IndexOf(std::int32_t descriptor) -> std::size_t {
  return static_cast<std::uint32_t>(descriptor) & 0x7FFF'FFFFU;
}

// Decode a file descriptor into an index of a table `size` long, or nullopt for
// any value that is not one (zero, an MCD, out of range).
auto IndexInTable(std::int32_t descriptor, std::size_t size)
    -> std::optional<std::size_t> {
  if (!IsFileDescriptor(descriptor)) return std::nullopt;
  const std::size_t idx = IndexOf(descriptor);
  if (idx >= size) return std::nullopt;
  return idx;
}

auto IsOpen(const FileTable::File& file) -> bool {
  return file.reads != nullptr || file.writes != nullptr;
}

// LRM 21.3.4.1: a byte `$ungetc` pushed back, or one a scan left unread, is the
// next byte any read takes. Answers the stream's end-of-file value at its end.
auto TakeByte(FileTable::File& file) -> int {
  if (file.putback.has_value()) {
    const auto byte = static_cast<unsigned char>(*file.putback);
    file.putback.reset();
    return byte;
  }
  return file.reads->get();
}

// The one position LRM 21.3.5 gives a descriptor, used by whichever input or
// output operation comes next, is the stream buffer's: a file's input and
// output sides share it.
auto BufferOf(const FileTable::File& file) -> std::streambuf* {
  return file.reads != nullptr ? file.reads->rdbuf() : file.writes->rdbuf();
}

constexpr auto kEitherSide = std::ios_base::in | std::ios_base::out;

}  // namespace

// LRM 21.3.1 pre-opens STDIN, STDOUT and STDERR, and makes channel 0 of a
// multichannel descriptor standard output as well.
FileTable::FileTable() : fd_files_(IndexOf(support::kStderrFd) + 1) {
  fd_files_.at(IndexOf(support::kStdinFd)).reads = &std::cin;
  fd_files_.at(IndexOf(support::kStdoutFd)).writes = &std::cout;
  fd_files_.at(IndexOf(support::kStderrFd)).writes = &std::cerr;
  channel_files_.at(0).writes = &std::cout;
}

template <typename Visit>
void FileTable::ForEachOpenFile(std::int64_t descriptor_value, Visit visit) {
  const std::int32_t descriptor = AsInt32(descriptor_value);
  if (IsFileDescriptor(descriptor)) {
    if (File* file = FileIfOpen(descriptor); file != nullptr) visit(*file);
    return;
  }
  // LRM 21.3.1: an MCD names every channel whose bit it sets, in ascending
  // order; bit 31 is clear, so bits 0..30 are all there are.
  const auto raw = static_cast<std::uint32_t>(descriptor);
  for (std::size_t bit = 0; bit < kChannelCount; ++bit) {
    if ((raw & (1U << bit)) == 0U) continue;
    File& file = channel_files_.at(bit);
    if (IsOpen(file)) visit(file);
  }
}

auto FileTable::FileIfOpen(std::int32_t fd) -> File* {
  const auto idx = IndexInTable(fd, fd_files_.size());
  if (!idx.has_value()) return nullptr;
  File& file = fd_files_.at(*idx);
  return IsOpen(file) ? &file : nullptr;
}

auto FileTable::FileToRead(std::int64_t fd_value, std::string_view task)
    -> File* {
  const std::int32_t fd = AsInt32(fd_value);
  File* file = FileIfOpen(fd);
  if (file == nullptr) {
    SetError(fd, EBADF, std::format("{}: not an open file descriptor", task));
    return nullptr;
  }
  if (file->reads == nullptr) {
    SetError(fd, EBADF, std::format("{}: file not open for reading", task));
    return nullptr;
  }
  return file;
}

void FileTable::WriteTo(
    std::int64_t descriptor, std::string_view text, bool newline) {
  ForEachOpenFile(descriptor, [&](File& file) {
    if (file.writes == nullptr) {
      file.error = ErrorRecord{
          .errno_value = EBADF, .message = "file not open for writing"};
      return;
    }
    file.writes->write(text.data(), static_cast<std::streamsize>(text.size()));
    if (newline) file.writes->put('\n');
  });
}

void FileTable::Write(std::int64_t descriptor, const value::String& text) {
  WriteTo(descriptor, text.View(), false);
}

void FileTable::Writeln(std::int64_t descriptor, const value::String& text) {
  WriteTo(descriptor, text.View(), true);
}

ChannelCancellation::ChannelCancellation(std::vector<std::stop_token> tokens)
    : tokens_(std::move(tokens)) {
}

auto ChannelCancellation::IsCancelled() const noexcept -> value::Bit {
  return value::Bit::FromBool(
      std::ranges::any_of(tokens_, [](const std::stop_token& t) {
        return t.stop_requested();
      }));
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
    // Take the place of the first closed file; grow on demand. The pre-opened
    // files are always open, so their places are never taken.
    const auto free = std::ranges::find_if(
        fd_files_, [](const File& file) { return !IsOpen(file); });
    const auto idx = static_cast<std::size_t>(free - fd_files_.begin());
    File& file = free == fd_files_.end() ? fd_files_.emplace_back() : *free;
    file.reads = parsed->permits_read ? stream.get() : nullptr;
    file.writes = parsed->permits_write ? stream.get() : nullptr;
    file.owned = std::move(stream);
    file.error = ErrorRecord{};
    return support::kFdHighBit | static_cast<std::int32_t>(idx);
  }
  // MCD path: open write-truncate (LRM 21.3.1 omits the type for MCD form).
  auto stream = std::make_unique<std::fstream>(
      name_z, std::ios_base::out | std::ios_base::trunc);
  if (!stream->is_open()) return 0;
  for (std::size_t bit = 0; bit < kChannelCount; ++bit) {
    File& file = channel_files_.at(bit);
    if (IsOpen(file)) continue;
    file.writes = stream.get();
    file.owned = std::move(stream);
    return static_cast<std::int32_t>(1U << bit);
  }
  // Every channel in use: stream's destructor closes the just-opened handle so
  // nothing leaks. Return 0 per LRM.
  return 0;
}

void FileTable::Close(std::int64_t descriptor) {
  ForEachOpenFile(descriptor, [](File& file) {
    if (file.owned == nullptr) return;
    file.owned.reset();
    file.reads = nullptr;
    file.writes = nullptr;
    file.error = ErrorRecord{};
    // LRM 21.3.2: fire cancel on any pending observers; replace the source
    // so the next $fopen to take this place starts with a fresh signal.
    file.cancel_source.request_stop();
    file.cancel_source = std::stop_source{};
    // Any pending $ungetc dies with the file.
    file.putback.reset();
  });
}

void FileTable::SetError(
    std::int32_t fd, int errno_value, std::string message) {
  const auto idx = IndexInTable(fd, fd_files_.size());
  if (!idx.has_value()) return;
  fd_files_.at(*idx).error =
      ErrorRecord{.errno_value = errno_value, .message = std::move(message)};
}

auto FileTable::LastError(std::int32_t fd) const -> int {
  const auto idx = IndexInTable(fd, fd_files_.size());
  if (!idx.has_value()) return 0;
  return fd_files_.at(*idx).error.errno_value;
}

auto FileTable::LastErrorMessage(std::int32_t fd) const -> std::string_view {
  const auto idx = IndexInTable(fd, fd_files_.size());
  if (!idx.has_value()) return {};
  return fd_files_.at(*idx).error.message;
}

void FileTable::ClearError(std::int32_t fd) {
  const auto idx = IndexInTable(fd, fd_files_.size());
  if (!idx.has_value()) return;
  fd_files_.at(*idx).error = ErrorRecord{};
}

auto FileTable::ScanInputOf(std::int64_t fd) -> FileScanInput {
  return FileScanInput(FileToRead(fd, "$fscanf"));
}

auto FileScanInput::Peek() -> int {
  if (file_ == nullptr) return kNoByte;
  if (file_->putback.has_value()) {
    return static_cast<unsigned char>(*file_->putback);
  }
  const int byte = file_->reads->peek();
  return byte == std::char_traits<char>::eof() ? kNoByte : byte;
}

auto FileScanInput::Consume() -> int {
  if (file_ == nullptr) return kNoByte;
  const int byte = TakeByte(*file_);
  return byte == std::char_traits<char>::eof() ? kNoByte : byte;
}

void FileScanInput::Unget(int byte) {
  if (file_ == nullptr || file_->putback.has_value()) {
    throw InternalError(
        "a scan gave back a byte its input has no room for -- please report "
        "this as a bug");
  }
  file_->putback = static_cast<char>(byte);
}

auto FileTable::Open(const value::String& name) -> value::Int {
  return MakeInt(Open(name.View(), std::nullopt));
}

auto FileTable::OpenWithMode(
    const value::String& name, const value::String& mode) -> value::Int {
  return MakeInt(Open(name.View(), mode.View()));
}

auto FileTable::Getc(std::int64_t fd_value) -> value::Int {
  File* file = FileToRead(fd_value, "$fgetc");
  if (file == nullptr) return MakeInt(-1);
  const int c = TakeByte(*file);
  if (c == std::char_traits<char>::eof()) {
    // LRM 21.3.4.1: EOF is signalled as -1 (wider than 8 bits so callers can
    // distinguish from 0xFF). The stream's eof bit is now set; $feof picks it
    // up.
    SetError(AsInt32(fd_value), 0, "$fgetc: EOF");
    return MakeInt(-1);
  }
  return MakeInt(c & 0xFF);
}

auto FileTable::Ungetc(std::int64_t c, std::int64_t fd_value) -> value::Int {
  File* file = FileToRead(fd_value, "$ungetc");
  if (file == nullptr) return MakeInt(-1);
  // LRM 21.3.4.1 NOTE: implementations may limit pushback depth. We keep a
  // single-byte buffer per FD; a second $ungetc before any read returns -1
  // per "if an error occurs pushing a character ... code is set to EOF".
  if (file->putback.has_value()) {
    SetError(AsInt32(fd_value), EAGAIN, "$ungetc: putback buffer full");
    return MakeInt(-1);
  }
  file->putback = static_cast<char>(AsInt32(c) & 0xFF);
  // The pushed byte is the next one read, so the descriptor is no longer at
  // its end (LRM 21.3.8), as C's `ungetc` clears end-of-file.
  file->reads->clear(file->reads->rdstate() & std::ios_base::badbit);
  return MakeInt(0);
}

auto FileTable::Gets(std::int64_t fd_value) -> TextRead {
  const auto read_nothing = [] {
    return TextRead{MakeInt(0), value::String{}};
  };
  File* file = FileToRead(fd_value, "$fgets");
  if (file == nullptr) return read_nothing();
  // LRM 21.3.4.2 + 21.3.4.1: byte by byte, so a pushed-back byte is the first
  // of the line.
  std::string line;
  while (true) {
    const int c = TakeByte(*file);
    if (c == std::char_traits<char>::eof()) break;
    line.push_back(static_cast<char>(c));
    if (c == '\n') break;
  }
  if (line.empty()) {
    SetError(AsInt32(fd_value), 0, "$fgets: EOF");
    return read_nothing();
  }
  return TextRead{
      MakeInt(static_cast<std::int32_t>(line.size())), value::String{line}};
}

auto FileTable::ReadInto(const value::IntegralView& dest, std::int64_t fd_value)
    -> value::Int {
  File* file = FileToRead(fd_value, "$fread");
  if (file == nullptr) return MakeInt(0);
  const auto byte_count = static_cast<std::size_t>((dest.width + 7U) / 8U);
  std::vector<char> buf(byte_count, '\0');
  std::size_t pos = 0;
  // LRM 21.3.4.1: any pending $ungetc byte is the next byte read from the
  // FD; place it at the head of the buffer before consulting the stream.
  if (file->putback.has_value()) {
    buf[pos++] = *file->putback;
    file->putback.reset();
  }
  const auto rest = std::span<char>(buf).subspan(pos);
  file->reads->read(rest.data(), static_cast<std::streamsize>(rest.size()));
  const auto got = pos + static_cast<std::size_t>(file->reads->gcount());
  if (got == 0U) {
    SetError(AsInt32(fd_value), 0, "$fread: EOF");
    return MakeInt(0);
  }
  // LRM 21.3.4.4: 2-value, big-endian. On a short read, the trailing buffer is
  // already zero from the vector constructor and lands in the destination's
  // LSBs ("as much as available").
  ReadBigEndian(dest, buf);
  return MakeInt(static_cast<std::int32_t>(got));
}

void ReadBigEndian(
    const value::IntegralView& dest, std::span<const char> bytes) {
  const std::uint64_t spanned = std::uint64_t{bytes.size()} * 8U;
  std::vector<std::uint64_t> read(value::WordCountForBits(spanned));
  const value::IntegralView read_bits{
      .value = read, .unknown = {}, .width = spanned};
  value::FromBytes(read_bits, bytes);
  value::Extract(
      dest, read_bits.AsConst(),
      static_cast<std::int64_t>(spanned) -
          static_cast<std::int64_t>(dest.width));
}

auto ReadMemoryWords(
    FileTable& files, std::int64_t fd_value, std::uint64_t element_width,
    const value::UnpackedRange& declared, std::int64_t start_sv,
    std::int64_t count,
    const std::function<void(std::int64_t, std::span<const char>)>& write_word)
    -> std::int32_t {
  FileTable::File* file = files.FileToRead(fd_value, "$fread");
  if (file == nullptr) return 0;

  // LRM 21.3.4.4: out-of-range start has no defined behaviour. Stamp EINVAL
  // and return 0 so the user sees the failure rather than a silent no-op.
  if (start_sv < declared.Low() || start_sv > declared.High()) {
    files.SetError(
        AsInt32(fd_value), EINVAL, "$fread: start index out of array range");
    return 0;
  }
  const auto available_elements =
      static_cast<std::size_t>(declared.High() - start_sv + 1);
  const auto target_count =
      std::min(static_cast<std::size_t>(count), available_elements);

  const auto bytes_per_elem =
      static_cast<std::size_t>((element_width + 7U) / 8U);
  std::size_t total_bytes = 0;

  for (std::size_t k = 0; k < target_count; ++k) {
    std::vector<char> buf(bytes_per_elem, '\0');
    std::size_t pos = 0;
    // LRM 21.3.4.1: drain any $ungetc-deposited byte only on the very first
    // element's first byte; subsequent bytes come straight from the stream.
    if (k == 0 && file->putback.has_value()) {
      buf[pos++] = *file->putback;
      file->putback.reset();
    }
    const auto rest = std::span<char>(buf).subspan(pos);
    file->reads->read(rest.data(), static_cast<std::streamsize>(rest.size()));
    const auto got_this = pos + static_cast<std::size_t>(file->reads->gcount());
    if (got_this == 0U) break;
    // A partial element keeps the zeros the buffer started with in the bytes
    // the file did not reach, which are its least significant -- the packed
    // form's "as much as available" behaviour. Written by source-declared
    // index: the declared range is the receiver's static-type coordinate
    // system, and the holder resolves it.
    write_word(start_sv + static_cast<std::int64_t>(k), buf);
    total_bytes += got_this;
    if (got_this < bytes_per_elem) break;
  }
  return static_cast<std::int32_t>(total_bytes);
}

auto FileTable::Seek(
    std::int64_t fd_value, std::int64_t offset, std::int64_t operation)
    -> value::Int {
  const std::int32_t fd = AsInt32(fd_value);
  File* file = FileIfOpen(fd);
  if (file == nullptr) {
    SetError(fd, EBADF, "$fseek: not an open file descriptor");
    return MakeInt(-1);
  }
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
  const auto reached = BufferOf(*file)->pubseekoff(
      static_cast<std::streamoff>(AsInt32(offset)), dir, kEitherSide);
  if (reached == std::streampos{-1}) {
    SetError(fd, errno, "$fseek: seek failed");
    return MakeInt(-1);
  }
  // A seek that lands starts the stream afresh, end-of-file included, as C's
  // does.
  if (file->reads != nullptr) file->reads->clear();
  if (file->writes != nullptr) file->writes->clear();
  // LRM 21.3.5: "Repositioning the current file position with $fseek or
  // $rewind shall cancel any $ungetc operations."
  file->putback.reset();
  return MakeInt(0);
}

auto FileTable::Rewind(std::int64_t fd) -> value::Int {
  return Seek(fd, 0, 0);
}

auto FileTable::Tell(std::int64_t fd_value) -> value::Int {
  const std::int32_t fd = AsInt32(fd_value);
  File* file = FileIfOpen(fd);
  if (file == nullptr) {
    SetError(fd, EBADF, "$ftell: not an open file descriptor");
    return MakeInt(-1);
  }
  const auto pos =
      BufferOf(*file)->pubseekoff(0, std::ios_base::cur, kEitherSide);
  if (pos == std::streampos{-1}) {
    SetError(fd, errno, "$ftell: tell failed");
    return MakeInt(-1);
  }
  return MakeInt(static_cast<std::int32_t>(pos));
}

auto FileTable::Eof(std::int64_t fd_value) -> value::Int {
  const File* file = FileIfOpen(AsInt32(fd_value));
  const bool at_end =
      file != nullptr && file->reads != nullptr && file->reads->eof();
  return MakeInt(at_end ? 1 : 0);
}

auto FileTable::Error(std::int64_t fd_value) -> TextRead {
  const std::int32_t fd = AsInt32(fd_value);
  const int errno_value = LastError(fd);
  if (errno_value == 0) {
    return TextRead{MakeInt(0), value::String{}};
  }
  TextRead completion{
      MakeInt(errno_value), value::String{LastErrorMessage(fd)}};
  ClearError(fd);
  return completion;
}

void FileTable::FlushAll() {
  for (File& file : channel_files_) {
    if (file.writes != nullptr) file.writes->flush();
  }
  for (File& file : fd_files_) {
    if (file.writes != nullptr) file.writes->flush();
  }
}

void FileTable::Flush(std::int64_t descriptor) {
  ForEachOpenFile(descriptor, [](File& file) {
    if (file.writes != nullptr) file.writes->flush();
  });
}

auto FileTable::CancellationFor(std::int64_t descriptor)
    -> ChannelCancellation {
  std::vector<std::stop_token> tokens;
  ForEachOpenFile(descriptor, [&tokens](File& file) {
    tokens.push_back(file.cancel_source.get_token());
  });
  return ChannelCancellation{std::move(tokens)};
}

}  // namespace lyra::runtime
