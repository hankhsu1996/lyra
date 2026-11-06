#include "lyra/runtime/file_table.hpp"

#include <algorithm>
#include <atomic>
#include <cerrno>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <expected>
#include <format>
#include <functional>
#include <memory>
#include <optional>
#include <span>
#include <stop_token>
#include <string>
#include <string_view>
#include <system_error>
#include <unistd.h>
#include <unordered_map>
#include <utility>
#include <vector>

#include "lyra/base/errno_as_error_code.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/write_all.hpp"
#include "lyra/runtime/file.hpp"
#include "lyra/support/file_descriptor.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/string.hpp"
#include "lyra/value/unpacked_range.hpp"

namespace lyra::runtime {

namespace {

// How much a file holds before handing it to its descriptor.
constexpr std::size_t kBufferBytes = std::size_t{64} << 10;

// LRM 21.3.1 Table 21-6 mode strings are C's own, so the C library opens the
// file by them; what is read off the string here is which directions it
// permits, which is what LRM 21.3.4's "fd can be read from only if opened with
// r or r+" comes down to. An unknown mode is nothing, so `$fopen` yields 0 per
// LRM "if a file cannot be opened ... a zero is returned".
struct Permissions {
  bool read;
  bool write;
};

auto PermissionsOf(std::string_view mode) -> std::optional<Permissions> {
  constexpr Permissions kRead{.read = true, .write = false};
  constexpr Permissions kWrite{.read = false, .write = true};
  constexpr Permissions kBoth{.read = true, .write = true};
  static const std::unordered_map<std::string_view, Permissions> kMap{
      {"r", kRead},   {"rb", kRead},  {"w", kWrite},  {"wb", kWrite},
      {"a", kWrite},  {"ab", kWrite}, {"r+", kBoth},  {"rb+", kBoth},
      {"r+b", kBoth}, {"w+", kBoth},  {"wb+", kBoth}, {"w+b", kBoth},
      {"a+", kBoth},  {"ab+", kBoth}, {"a+b", kBoth},
  };
  const auto it = kMap.find(mode);
  if (it == kMap.end()) return std::nullopt;
  return it->second;
}

// The `int` the source wrote for a descriptor, offset, operation or character
// (LRM 21.3), which reaches here widened to a machine integer.
auto AsInt32(std::int64_t value) -> std::int32_t {
  return static_cast<std::int32_t>(value);
}

auto MakeInt(std::int32_t v) -> value::Int {
  return value::Int::FromInt(v);
}

// An error the table itself finds, spelled as the system's error of that
// number.
auto ErrorCodeOf(int code) -> std::error_code {
  return {code, std::system_category()};
}

auto IsFileDescriptor(std::int32_t descriptor) -> bool {
  return (static_cast<std::uint32_t>(descriptor) & (1U << 31U)) != 0U;
}

// The index the low 31 bits of a file descriptor carry (LRM 21.3.1).
auto IndexOf(std::int32_t descriptor) -> std::size_t {
  return static_cast<std::uint32_t>(descriptor) & 0x7FFF'FFFFU;
}

// The file descriptor whose low 31 bits carry `index` (LRM 21.3.1).
auto FdOf(std::size_t index) -> std::int32_t {
  return support::kFdHighBit | static_cast<std::int32_t>(index);
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

// The table whose files an ending delivers. A handler is handed nothing to
// find the run by, so the table of the run is found here.
auto DeliveringTable() noexcept -> std::atomic<FileTable*>& {
  static constinit std::atomic<FileTable*> table = nullptr;
  return table;
}

}  // namespace

// LRM 21.3.1 pre-opens STDIN, STDOUT and STDERR, and makes channel 0 of a
// multichannel descriptor standard output as well. The error stream holds
// nothing, as the C library's does not, so a report reaches its reader as it
// is made.
FileTable::FileTable() {
  fd_slots_.reserve(kPreOpenedCount);
  const auto pre_open = [this](std::unique_ptr<File> file, const char* name) {
    Slot& slot = fd_slots_.emplace_back();
    slot.file = std::move(file);
    slot.destination = name;
  };
  pre_open(
      std::make_unique<File>(STDIN_FILENO, true, false, kBufferBytes, nullptr),
      "standard input");
  pre_open(
      std::make_unique<File>(STDOUT_FILENO, false, true, kBufferBytes, stdout),
      "standard output");
  pre_open(
      std::make_unique<File>(STDERR_FILENO, false, true, 0, nullptr),
      "standard error");
  DeliveringTable().store(this);
}

FileTable::~FileTable() {
  // A second run constructed in the same process before this one ended has
  // taken the endings' attention; it keeps it.
  FileTable* expected = this;
  DeliveringTable().compare_exchange_strong(expected, nullptr);
  // Held so that a stop arriving now ends the process only once every file
  // has handed over, since this table is no longer the one an ending
  // delivers; each file hands over before the stream it was opened as closes.
  const StopSignalsHeld held;
  for (Slot& slot : fd_slots_) {
    slot.file.reset();
    slot.opened_as.reset();
  }
  for (Slot& slot : channel_slots_) {
    slot.file.reset();
    slot.opened_as.reset();
  }
}

template <typename Visit>
void FileTable::ForEachOpenSlot(std::int64_t descriptor_value, Visit visit) {
  const std::int32_t descriptor = AsInt32(descriptor_value);
  if (IsFileDescriptor(descriptor)) {
    if (Slot* slot = SlotOf(descriptor);
        slot != nullptr && slot->file != nullptr) {
      visit(*slot);
    }
    return;
  }
  // LRM 21.3.1: an MCD names every channel whose bit it sets, in ascending
  // order; bit 31 is clear, so bits 0..30 are all there are. Channel 0 is
  // standard output.
  const auto raw = static_cast<std::uint32_t>(descriptor);
  for (std::size_t bit = 0; bit < kChannelCount; ++bit) {
    if ((raw & (1U << bit)) == 0U) continue;
    Slot& slot = bit == 0 ? fd_slots_.at(IndexOf(support::kStdoutFd))
                          : channel_slots_.at(bit);
    if (slot.file != nullptr) visit(slot);
  }
}

auto FileTable::SlotOf(std::int32_t fd) -> Slot* {
  const auto idx = IndexInTable(fd, fd_slots_.size());
  return idx.has_value() ? &fd_slots_.at(*idx) : nullptr;
}

auto FileTable::FileIfOpen(std::int32_t fd) -> File* {
  Slot* slot = SlotOf(fd);
  return slot == nullptr ? nullptr : slot->file.get();
}

auto FileTable::IsPreOpened(const Slot& slot) const -> bool {
  const std::span<const Slot> pre_opened =
      std::span(fd_slots_).first(kPreOpenedCount);
  return std::ranges::any_of(pre_opened, [&slot](const Slot& candidate) {
    return &candidate == &slot;
  });
}

FileTable::Sentry::Sentry(FileTable& table, std::int64_t descriptor)
    : table_(&table), descriptor_(AsInt32(descriptor)) {
}

FileTable::Sentry::Sentry(Sentry&& other) noexcept
    : table_(std::exchange(other.table_, nullptr)),
      descriptor_(other.descriptor_),
      met_(other.met_) {
}

FileTable::Sentry::~Sentry() {
  if (table_ != nullptr) table_->EndOperation(descriptor_, met_);
}

void FileTable::Sentry::Meet(std::error_code met) {
  Meet(File::Refusal{.code = met, .lost_output = false});
}

void FileTable::Sentry::Meet(File::Refusal met) {
  if (!met_.code) met_ = met;
}

void FileTable::EndOperation(std::int32_t descriptor, File::Refusal met) {
  ForEachOpenSlot(descriptor, [&met](Slot& slot) {
    const File::Refusal refused = slot.file->TakeRefusal();
    if (!met.code) met = refused;
  });
  if (met.code) {
    met_by_descriptor_.insert_or_assign(descriptor, met);
  } else {
    met_by_descriptor_.erase(descriptor);
  }
}

auto FileTable::OpenedFile(Sentry& sentry) -> File* {
  File* file = FileIfOpen(sentry.descriptor_);
  if (file == nullptr) sentry.Meet(ErrorCodeOf(EBADF));
  return file;
}

auto FileTable::ReadableFile(Sentry& sentry) -> File* {
  File* file = OpenedFile(sentry);
  if (file == nullptr || file->PermitsRead()) return file;
  sentry.Meet(ErrorCodeOf(EBADF));
  return nullptr;
}

template <typename Visit>
void FileTable::ForEachNamed(Sentry& sentry, Visit visit) {
  bool named = false;
  ForEachOpenSlot(sentry.descriptor_, [&](Slot& slot) {
    named = true;
    visit(slot);
  });
  if (!named) sentry.Meet(ErrorCodeOf(EBADF));
}

void FileTable::WriteTo(
    std::int64_t descriptor, std::string_view text, bool newline) {
  Sentry sentry(*this, descriptor);
  ForEachNamed(sentry, [&](Slot& slot) {
    if (!slot.file->PermitsWrite()) {
      sentry.Meet(ErrorCodeOf(EBADF));
      return;
    }
    slot.file->Write(text);
    if (newline) slot.file->Write("\n");
  });
}

void FileTable::Write(std::int64_t descriptor, const value::String& text) {
  WriteTo(descriptor, text.View(), false);
}

void FileTable::Writeln(std::int64_t descriptor, const value::String& text) {
  WriteTo(descriptor, text.View(), true);
}

// A report is no output of the design's, so it is written past the design's
// error stream, which holds nothing, and what the stream refuses of it has
// nobody to tell.
void FileTable::WriteToStandardError(std::string_view text) {
  fd_slots_.at(IndexOf(support::kStdoutFd)).file->HandOver();
  base::WriteAsMuchAsTaken(STDERR_FILENO, text);
}

void FileTable::BeforeForeignCodeRuns() {
  fd_slots_.at(IndexOf(support::kStdoutFd)).file->BeforeForeignCodeRuns();
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
  const std::expected<std::int32_t, std::error_code> opened =
      OpenFile(name, mode);
  if (!opened.has_value()) {
    EndOperation(0, {.code = opened.error(), .lost_output = false});
    return 0;
  }
  // The descriptor now names this file, so `$ferror` given it no longer tells
  // the design of what a file closed under it lost.
  for (ClosedLoss& closed : closed_losses_) {
    if (closed.asked_by == *opened) closed.asked_by.reset();
  }
  EndOperation(*opened, {});
  return *opened;
}

auto FileTable::OpenFile(
    std::string_view name, std::optional<std::string_view> mode)
    -> std::expected<std::int32_t, std::error_code> {
  const std::string name_z{name};
  const std::string destination = std::format("'{}'", name);
  // The table changes shape while a file is added, and an ending that arrives
  // meanwhile reads it whole.
  const StopSignalsHeld held;
  if (mode.has_value()) {
    const auto permissions = PermissionsOf(*mode);
    if (!permissions.has_value()) {
      return std::unexpected(ErrorCodeOf(EINVAL));
    }
    const std::string mode_z{*mode};
    OpenedStream opened(
        std::fopen(name_z.c_str(), mode_z.c_str()), &std::fclose);
    if (opened == nullptr) return std::unexpected(base::ErrnoAsErrorCode());
    // Take the place of the first closed file; grow on demand. The pre-opened
    // files are always open, so their places are never taken.
    const auto free = std::ranges::find_if(
        fd_slots_, [](const Slot& slot) { return slot.file == nullptr; });
    const auto idx = static_cast<std::size_t>(free - fd_slots_.begin());
    Slot& slot = free == fd_slots_.end() ? fd_slots_.emplace_back() : *free;
    slot.file = std::make_unique<File>(
        ::fileno(opened.get()), permissions->read, permissions->write,
        kBufferBytes, nullptr);
    slot.opened_as = std::move(opened);
    slot.destination = destination;
    return FdOf(idx);
  }
  // MCD path: open write-truncate (LRM 21.3.1 omits the type for MCD form).
  OpenedStream opened(std::fopen(name_z.c_str(), "w"), &std::fclose);
  if (opened == nullptr) return std::unexpected(base::ErrnoAsErrorCode());
  for (std::size_t bit = 1; bit < kChannelCount; ++bit) {
    Slot& slot = channel_slots_.at(bit);
    if (slot.file != nullptr) continue;
    slot.file = std::make_unique<File>(
        ::fileno(opened.get()), false, true, kBufferBytes, nullptr);
    slot.opened_as = std::move(opened);
    slot.destination = destination;
    return static_cast<std::int32_t>(1U << bit);
  }
  // Every channel in use: the file just opened closes with this scope so
  // nothing leaks, and the opening answers 0 (LRM 21.3.1).
  return std::unexpected(ErrorCodeOf(EMFILE));
}

void FileTable::Close(std::int64_t descriptor) {
  Sentry sentry(*this, descriptor);
  const StopSignalsHeld held;
  ForEachNamed(sentry, [&](Slot& slot) {
    if (IsPreOpened(slot)) return;
    slot.file->HandOver();
    sentry.Meet(slot.file->TakeRefusal());
    std::error_code lost = slot.file->TakeUntoldLoss();
    slot.file.reset();
    // A descriptor the system refuses to close may not have kept what was
    // handed to it.
    if (std::fclose(slot.opened_as.release()) != 0) {
      const std::error_code refused = base::ErrnoAsErrorCode();
      sentry.Meet({.code = refused, .lost_output = true});
      if (!lost) lost = refused;
    }
    if (lost) {
      closed_losses_.push_back(
          ClosedLoss{
              .asked_by = AsInt32(descriptor),
              .lost = {.destination = slot.destination, .refused = lost}});
    }
    // LRM 21.3.2: fire cancel on any pending observers; replace the source
    // so the next $fopen to take this place starts with a fresh signal.
    slot.cancel_source.request_stop();
    slot.cancel_source = std::stop_source{};
  });
}

auto FileTable::ScanInputOf(std::int64_t fd) -> FileScanInput {
  Sentry sentry(*this, fd);
  File* file = ReadableFile(sentry);
  return {std::move(sentry), file};
}

auto FileScanInput::Peek() -> int {
  if (file_ == nullptr) return kNoByte;
  const int byte = file_->Peek();
  return byte == File::kEndOfFile ? kNoByte : byte;
}

auto FileScanInput::Consume() -> int {
  if (file_ == nullptr) return kNoByte;
  const int byte = file_->Take();
  return byte == File::kEndOfFile ? kNoByte : byte;
}

void FileScanInput::Unget(int byte) {
  if (file_ == nullptr || !file_->Unget(static_cast<char>(byte))) {
    throw InternalError(
        "a scan gave back a byte its input has no room for -- please report "
        "this as a bug");
  }
}

auto FileTable::Open(const value::String& name) -> value::Int {
  return MakeInt(Open(name.View(), std::nullopt));
}

auto FileTable::OpenWithMode(
    const value::String& name, const value::String& mode) -> value::Int {
  return MakeInt(Open(name.View(), mode.View()));
}

auto FileTable::Getc(std::int64_t fd) -> value::Int {
  Sentry sentry(*this, fd);
  File* file = ReadableFile(sentry);
  // LRM 21.3.4.1: the end of the file and an error are both -1, wider than 8
  // bits so callers can tell it from 0xFF.
  return MakeInt(file == nullptr ? -1 : file->Take());
}

auto FileTable::Ungetc(std::int64_t c, std::int64_t fd) -> value::Int {
  Sentry sentry(*this, fd);
  File* file = ReadableFile(sentry);
  if (file == nullptr) return MakeInt(-1);
  // LRM 21.3.4.1 NOTE: implementations may limit pushback depth. We keep a
  // single-byte buffer per FD; a second $ungetc before any read returns -1
  // per "if an error occurs pushing a character ... code is set to EOF".
  if (!file->Unget(static_cast<char>(AsInt32(c) & 0xFF))) {
    sentry.Meet(ErrorCodeOf(EAGAIN));
    return MakeInt(-1);
  }
  return MakeInt(0);
}

auto FileTable::Gets(std::int64_t fd) -> TextRead {
  const auto read_nothing = [] {
    return TextRead{MakeInt(0), value::String{}};
  };
  Sentry sentry(*this, fd);
  File* file = ReadableFile(sentry);
  if (file == nullptr) return read_nothing();
  // LRM 21.3.4.2 + 21.3.4.1: byte by byte, so a pushed-back byte is the first
  // of the line.
  std::string line;
  while (true) {
    const int c = file->Take();
    if (c == File::kEndOfFile) break;
    line.push_back(static_cast<char>(c));
    if (c == '\n') break;
  }
  if (line.empty()) return read_nothing();
  return TextRead{
      MakeInt(static_cast<std::int32_t>(line.size())), value::String{line}};
}

namespace {

// Reads up to `into.size()` bytes of `file` into `into`, and answers how many
// it read; a pushed-back byte is the first (LRM 21.3.4.1).
auto ReadBytes(File& file, std::span<char> into) -> std::size_t {
  std::size_t got = 0;
  for (char& byte : into) {
    const int c = file.Take();
    if (c == File::kEndOfFile) break;
    byte = static_cast<char>(c);
    ++got;
  }
  return got;
}

}  // namespace

auto FileTable::ReadInto(const value::IntegralView& dest, std::int64_t fd)
    -> value::Int {
  Sentry sentry(*this, fd);
  File* file = ReadableFile(sentry);
  if (file == nullptr) return MakeInt(0);
  const auto byte_count = static_cast<std::size_t>((dest.width + 7U) / 8U);
  std::vector<char> buf(byte_count, '\0');
  const std::size_t got = ReadBytes(*file, buf);
  if (got == 0U) return MakeInt(0);
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

auto FileTable::ReadMemoryWords(
    std::int64_t fd, std::uint64_t element_width,
    const value::UnpackedRange& declared, std::int64_t start_sv,
    std::int64_t count,
    const std::function<void(std::int64_t, std::span<const char>)>& write_word)
    -> std::int32_t {
  Sentry sentry(*this, fd);
  File* file = ReadableFile(sentry);
  if (file == nullptr) return 0;

  // LRM 21.3.4.4: out-of-range start has no defined behaviour. The operation
  // meets EINVAL and reads nothing, so the user sees the failure rather than a
  // silent no-op.
  if (start_sv < declared.Low() || start_sv > declared.High()) {
    sentry.Meet(ErrorCodeOf(EINVAL));
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
    const std::size_t got_this = ReadBytes(*file, buf);
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
    std::int64_t fd, std::int64_t offset, std::int64_t operation)
    -> value::Int {
  Sentry sentry(*this, fd);
  File* file = OpenedFile(sentry);
  if (file == nullptr) return MakeInt(-1);
  int whence = 0;
  switch (AsInt32(operation)) {
    case 0:
      whence = SEEK_SET;
      break;
    case 1:
      whence = SEEK_CUR;
      break;
    case 2:
      whence = SEEK_END;
      break;
    default:
      sentry.Meet(ErrorCodeOf(EINVAL));
      return MakeInt(-1);
  }
  return MakeInt(file->Seek(AsInt32(offset), whence) ? 0 : -1);
}

auto FileTable::Rewind(std::int64_t fd) -> value::Int {
  return Seek(fd, 0, 0);
}

auto FileTable::Tell(std::int64_t fd) -> value::Int {
  Sentry sentry(*this, fd);
  File* file = OpenedFile(sentry);
  if (file == nullptr) return MakeInt(-1);
  const std::optional<std::int64_t> pos = file->Tell();
  return MakeInt(pos.has_value() ? static_cast<std::int32_t>(*pos) : -1);
}

auto FileTable::Eof(std::int64_t fd_value) -> value::Int {
  const File* file = FileIfOpen(AsInt32(fd_value));
  const bool at_end = file != nullptr && file->AtEnd();
  return MakeInt(at_end ? 1 : 0);
}

auto FileTable::Error(std::int64_t fd) -> TextRead {
  const auto it = met_by_descriptor_.find(AsInt32(fd));
  if (it == met_by_descriptor_.end()) {
    return TextRead{MakeInt(0), value::String{}};
  }
  const File::Refusal& met = it->second;
  // Answered a refusal that lost output, the design is told the files `fd`
  // names lost some.
  if (met.lost_output) {
    ForEachOpenSlot(fd, [](Slot& slot) { slot.file->ClearUntoldLoss(); });
    std::erase_if(closed_losses_, [&](const ClosedLoss& closed) {
      return closed.asked_by == AsInt32(fd);
    });
  }
  return TextRead{MakeInt(met.code.value()), value::String{met.code.message()}};
}

auto FileTable::Finish() -> std::vector<LostOutput> {
  std::vector<LostOutput> lost;
  const auto finish = [&lost](Slot& slot) {
    if (slot.file == nullptr) return;
    slot.file->HandOver();
    if (const std::error_code refused = slot.file->TakeUntoldLoss()) {
      lost.push_back({.destination = slot.destination, .refused = refused});
    }
  };
  std::ranges::for_each(fd_slots_, finish);
  std::ranges::for_each(channel_slots_, finish);
  for (ClosedLoss& closed : closed_losses_) {
    lost.push_back(std::move(closed.lost));
  }
  closed_losses_.clear();
  return lost;
}

// Each open file is flushed by an operation of its own, under the descriptor
// that names it alone.
void FileTable::FlushAll() {
  for (std::size_t index = 0; index < fd_slots_.size(); ++index) {
    if (fd_slots_.at(index).file == nullptr) continue;
    Flush(FdOf(index));
  }
  for (std::size_t bit = 1; bit < kChannelCount; ++bit) {
    if (channel_slots_.at(bit).file == nullptr) continue;
    Flush(static_cast<std::int32_t>(1U << bit));
  }
  std::fflush(nullptr);
}

void FileTable::Flush(std::int64_t descriptor) {
  Sentry sentry(*this, descriptor);
  ForEachNamed(sentry, [](Slot& slot) { slot.file->HandOver(); });
}

auto FileTable::CancellationFor(std::int64_t descriptor)
    -> ChannelCancellation {
  std::vector<std::stop_token> tokens;
  ForEachOpenSlot(descriptor, [&tokens](Slot& slot) {
    tokens.push_back(slot.cancel_source.get_token());
  });
  return ChannelCancellation{std::move(tokens)};
}

void FileTable::DeliverFinished() noexcept {
  for (Slot& slot : fd_slots_) {
    if (slot.file != nullptr) slot.file->DeliverFinished();
  }
  for (Slot& slot : channel_slots_) {
    if (slot.file != nullptr) slot.file->DeliverFinished();
  }
}

void DeliverFinishedFiles() noexcept {
  if (FileTable* table = DeliveringTable().load(); table != nullptr) {
    table->DeliverFinished();
  }
}

}  // namespace lyra::runtime
