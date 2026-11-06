#pragma once

#include <array>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <expected>
#include <functional>
#include <memory>
#include <optional>
#include <span>
#include <stop_token>
#include <string>
#include <string_view>
#include <system_error>
#include <unordered_map>
#include <utility>
#include <vector>

#include "lyra/base/source_location.hpp"
#include "lyra/runtime/file.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/scan.hpp"
#include "lyra/value/string.hpp"
#include "lyra/value/tuple.hpp"
#include "lyra/value/unpacked_array.hpp"
#include "lyra/value/unpacked_range.hpp"

namespace lyra::runtime {

class FileScanInput;

// LRM 21.3.2 cancellation observable. One or more output channels' joint
// cancel state; `IsCancelled()` returns true once any participating channel
// has been closed since this view was acquired. Captured by value into a
// postponed closure so the closure short-circuits when the descriptor it
// targets is invalidated mid-file.
//
// "Whole-operation cancellation" -- any participating channel closing kills
// the entire submit, matching the literal LRM wording "operations on a ...
// multichannel descriptor are implicitly cancelled".
class ChannelCancellation {
 public:
  // A view over no channels, which nothing can cancel. It is what storage
  // holding one starts as, before the view it will carry is copied in.
  ChannelCancellation() = default;

  [[nodiscard]] auto IsCancelled() const noexcept -> value::Bit;

 private:
  friend class FileTable;
  explicit ChannelCancellation(std::vector<std::stop_token> tokens);
  std::vector<std::stop_token> tokens_;
};

// What a read that delivers text completes with: how many bytes it read, and
// the text those bytes make (LRM 21.3.4.2, 21.3.7).
using TextRead = value::Tuple<value::Int, value::String>;

// What a read of binary data completes with: how many bytes it read, and the
// destination those bytes filled (LRM 21.3.4.4). The destination crosses in
// as well, because its own shape decides how much is read -- a nine-bit word
// takes two bytes where an eight-bit one takes one -- and what the file does
// not reach keeps what it held.
template <class Destination>
using BinaryRead = value::Tuple<value::Int, Destination>;

// Output written to a destination and lost, which the design was never told
// of: the destination as a message names it, and what the system answered.
struct LostOutput {
  std::string destination;
  std::error_code refused;
};

// The files a run's descriptors name (LRM 21.3.1). Two descriptor shapes share
// one int32 namespace:
//
// - A multichannel descriptor (`$fopen` given no mode): bit 31 clear, each of
//   bits 0..30 a channel, so several OR-ed together send one write to each.
// - A file descriptor (`$fopen` given a mode): bit 31 set, the rest an index.
//
// Either names files of this table, and every operation acts on the files it
// names. Standard input, output and error are pre-opened (LRM 21.3.1): file
// descriptors 0, 1 and 2 hold the process's own standard streams, channel 0 of
// a multichannel descriptor is standard output as well, and nothing closes
// them. The table holds what every file has been written and not yet handed
// to its descriptor, and hands it over before anything else writes the same
// destination -- a report on the error stream, foreign code (LRM 35), a
// command the design runs (LRM 20.17.1) -- and on every ending of the run the
// process can still act on.
//
// Each file also owns a `std::stop_source` -- the LRM 21.3.2 cancel signal for
// the postponed operations tied to the channel, such as `$fstrobe`'s. Closing
// request_stops it and replaces it with a fresh one, so a channel reused after
// a close starts with a clean signal while observers of the old source see a
// permanent stop.
//
// What `$ferror` answers is what the most recent operation given a descriptor
// met (LRM 21.3.7): its own error, else the first refusal of a file the
// descriptor names, else nothing. An opening that fails answers descriptor 0,
// so its error is answered there (LRM 21.3.1). Asking is no operation. Being
// answered a refusal that lost output tells the design those files lost some;
// what it was never told of is the run's end to report.
class FileTable {
 public:
  // One operation on a descriptor (LRM 21.3), bracketed as a C++ stream's
  // sentry brackets one; as it ends it records what the operation met.
  class Sentry {
   public:
    Sentry(Sentry&& other) noexcept;
    ~Sentry();
    Sentry(const Sentry&) = delete;
    auto operator=(const Sentry&) -> Sentry& = delete;
    auto operator=(Sentry&&) -> Sentry& = delete;

   private:
    friend class FileTable;
    Sentry(FileTable& table, std::int64_t descriptor);
    // Keeps the first error the operation meets: one it finds itself, or a
    // refusal of the descriptor's.
    void Meet(std::error_code met);
    void Meet(File::Refusal met);

    FileTable* table_;
    std::int32_t descriptor_;
    File::Refusal met_;
  };

  FileTable();
  ~FileTable();
  FileTable(const FileTable&) = delete;
  auto operator=(const FileTable&) -> FileTable& = delete;
  FileTable(FileTable&&) = delete;
  auto operator=(FileTable&&) -> FileTable& = delete;

  // Returns descriptor value per LRM 21.3.1, or 0 on failure (file cannot
  // be opened, every channel in use, or unknown mode string). `mode`
  // absent -> MCD (bit 31 clear, single bit set in 1..30, file opened in
  // write-truncate mode). `mode` present -> FD (bit 31 set), opened as the
  // string says (LRM 21.3.1 Table 21-6).
  auto Open(std::string_view name, std::optional<std::string_view> mode)
      -> std::int32_t;

  // Closes the files `descriptor` names: one file for an FD, each set bit's
  // for an MCD, firing each closed file's cancel signal (LRM 21.3.2). What
  // the last hand-over and the closing were refused is what the operation
  // met. A pre-opened file is never closed.
  void Close(std::int64_t descriptor);

  // Returns a `ChannelCancellation` covering every file `descriptor` names
  // (LRM 21.3.2), which the consumer holds by value.
  [[nodiscard]] auto CancellationFor(std::int64_t descriptor)
      -> ChannelCancellation;

  // LRM 21.2.1 / 21.3.2 write to every file `descriptor` names; one not opened
  // for writing meets EBADF instead. `Writeln` appends a trailing newline.
  void Write(std::int64_t descriptor, const lyra::value::String& text);
  void Writeln(std::int64_t descriptor, const lyra::value::String& text);

  // Writes `text` to the error stream, after what the design wrote to
  // standard output has reached it, so that where the two streams are one
  // destination a report follows what was printed before it.
  void WriteToStandardError(std::string_view text);

  // Foreign code is about to run (LRM 35): what the design wrote to standard
  // output reaches it first, and what the foreign side then writes to the C
  // library's stream reaches it before the design writes standard output
  // again.
  void BeforeForeignCodeRuns();

  // LRM 21.3.1 $fopen. Opening without a mode yields a multichannel
  // descriptor (MCD form) and opening with one yields a single file
  // descriptor (FD form), so each has a name of its own. On failure both
  // return 0 (file cannot be opened, every channel in use, or unknown mode
  // string).
  auto Open(const value::String& name) -> value::Int;
  auto OpenWithMode(const value::String& name, const value::String& mode)
      -> value::Int;

  // LRM 21.3.4.1 $fgetc. Returns the next byte as an SV `int`, or -1 on EOF /
  // error. A pending $ungetc putback (if any) is the first byte returned.
  auto Getc(std::int64_t fd) -> value::Int;

  // LRM 21.3.4.1 $ungetc. Pushes the low byte of `c` back onto the FD's
  // input buffer (single-byte file-side putback; a second $ungetc before
  // any read returns -1). Returns 0 on success or -1 on error.
  auto Ungetc(std::int64_t c, std::int64_t fd) -> value::Int;

  // LRM 21.3.4.2 $fgets. Reads bytes up to and including the next newline, or
  // until EOF, and completes with how many it read -- zero on error -- and the
  // line they make.
  auto Gets(std::int64_t fd) -> TextRead;

  // What `$fscanf` of `fd` reads; the operation ends with the input.
  auto ScanInputOf(std::int64_t fd) -> FileScanInput;

  // LRM 21.3.4.3 $fscanf. Parses from `fd`'s stream under `format`, taking
  // only the bytes the parse accepts, and completes with the matched-conversion
  // count and one value per conversion; each of `prototypes` is what its
  // conversion answers with where the scan stopped before reaching it.
  template <typename... Targets>
  auto Scan(
      std::int64_t fd, const value::String& format,
      value::Tuple<Targets...> prototypes, base::SourceLocation at)
      -> value::Tuple<value::Integer, Targets...>;

  // LRM 21.3.4.4 $fread into a packed destination. Reads (width+7)/8 bytes
  // big-endian (first byte fills MSBs) into the destination at its own type,
  // and answers how many it read; a read that reaches no byte leaves the
  // destination as it was.
  template <value::IntegralValue T>
  auto Read(const T& dest, std::int64_t fd) -> BinaryRead<T>;
  auto ReadInto(const value::IntegralView& dest, std::int64_t fd) -> value::Int;

  // LRM 21.3.4.4 $fread into a memory. Iterates `dest` from SV index
  // `sv_start` toward the highest declared SV index, reading until EOF or
  // `count` elements. `bounds` states the destination's declared range. The
  // caller always supplies `sv_start` and `count`, materializing the lowest
  // declared index and the whole remaining range where the SV call leaves them
  // out, so one entry serves every form the source may write.
  template <value::IntegralValue T, std::size_t N>
  auto ReadMemory(
      const value::UnpackedArray<T, N>& dest, std::int64_t fd,
      std::span<const std::int64_t> bounds, std::int64_t sv_start,
      std::int64_t count) -> BinaryRead<value::UnpackedArray<T, N>>;

  // LRM 21.3.5 $fseek. `operation` is 0/1/2 for SEEK_SET / SEEK_CUR /
  // SEEK_END. Returns 0 on success or -1 on error. Per LRM, any pending
  // $ungetc operation is cancelled.
  auto Seek(std::int64_t fd, std::int64_t offset, std::int64_t operation)
      -> value::Int;

  // LRM 21.3.5 $rewind. Equivalent to $fseek(fd, 0, 0).
  auto Rewind(std::int64_t fd) -> value::Int;

  // LRM 21.3.5 $ftell. Returns the current position or -1 on error.
  auto Tell(std::int64_t fd) -> value::Int;

  // LRM 21.3.8 $feof. Returns a nonzero value once an EOF has been
  // observed on `fd`, zero otherwise.
  auto Eof(std::int64_t fd) -> value::Int;

  // LRM 21.3.7 $ferror. Completes with the code of the error the most recent
  // operation given `fd` met and its description, or zero and no text.
  auto Error(std::int64_t fd) -> TextRead;

  // LRM 21.3.6 $fflush. Flushing every open file and flushing the files a
  // descriptor names -- a single FD, or every set-bit MCD channel -- are two
  // requests, so each has a name of its own. Flushing every file also flushes
  // what foreign code wrote to the C library's streams.
  void FlushAll();
  void Flush(std::int64_t descriptor);

  // The run's end: what every open file holds is handed over, and what any
  // file, open or closed, lost of the output written to it while the design
  // was never told is answered, once a file.
  auto Finish() -> std::vector<LostOutput>;

  // LRM 21.3.4.4 memory load, over whatever holds the words. Reads from
  // `start_sv` toward the highest declared index, stopping at end of file or
  // after `count` words, and hands each word to `write_word` by its
  // source-declared index -- the coordinate system the declared range states,
  // which the holder resolves -- as the bytes it takes, most significant
  // first. `element_width` is how wide a word is, which is what decides how
  // many bytes one costs; a word the file ends inside has zeros for the bytes
  // it lacks. Answers with the byte count, zero where nothing was read.
  //
  // The holder is a parameter because a memory reached through a
  // monomorphized container and one reached through an erased handle are the
  // same load; a second copy of the addressing and the partial-word rule would
  // be two readings of one clause.
  auto ReadMemoryWords(
      std::int64_t fd, std::uint64_t element_width,
      const value::UnpackedRange& declared, std::int64_t start_sv,
      std::int64_t count,
      const std::function<void(std::int64_t, std::span<const char>)>&
          write_word) -> std::int32_t;

 private:
  // LRM 21.3.1: at most 31 MCD channels, bits 0..30.
  static constexpr std::size_t kChannelCount = 31;
  // The pre-opened files: standard input, output and error (LRM 21.3.1), the
  // first three file descriptors.
  static constexpr std::size_t kPreOpenedCount = 3;

  // What a descriptor names: the file while it is open, the C stream the
  // descriptor was opened as (none for a pre-opened file, which is never
  // closed), the destination as a message names it, and the cancel signal of
  // the operations tied to it.
  struct Slot {
    std::unique_ptr<File> file;
    OpenedStream opened_as{nullptr, &std::fclose};
    std::string destination;
    std::stop_source cancel_source;
  };

  // What a file already closed lost while the design was never told, and the
  // descriptor `$ferror` can still tell it by, until another file opened
  // takes that descriptor.
  struct ClosedLoss {
    std::optional<std::int32_t> asked_by;
    LostOutput lost;
  };

  // The descriptor an opening answers, or what it met.
  auto OpenFile(std::string_view name, std::optional<std::string_view> mode)
      -> std::expected<std::int32_t, std::error_code>;

  // The open file the operation's descriptor names, or null where it names
  // none, which the operation meets as EBADF; `ReadableFile` also where it is
  // not open for reading (LRM 21.3.4).
  auto OpenedFile(Sentry& sentry) -> File*;
  auto ReadableFile(Sentry& sentry) -> File*;

  // Visits every open file the operation's descriptor names, which the
  // operation meets as EBADF where it names none.
  template <typename Visit>
  void ForEachNamed(Sentry& sentry, Visit visit);

  // An operation given `descriptor` ends having met `met`; what the files the
  // descriptor names were refused meanwhile is taken, the first counting where
  // it met nothing itself.
  void EndOperation(std::int32_t descriptor, File::Refusal met);

  // The slot a file descriptor names, open or not, or null for a value that
  // is no file descriptor of this table.
  auto SlotOf(std::int32_t fd) -> Slot*;
  // The open file a file descriptor names, or null.
  auto FileIfOpen(std::int32_t fd) -> File*;
  [[nodiscard]] auto IsPreOpened(const Slot& slot) const -> bool;

  void WriteTo(std::int64_t descriptor, std::string_view text, bool newline);

  template <typename Visit>
  void ForEachOpenSlot(std::int64_t descriptor, Visit visit);

  // What finished output calls wrote to every open file reaches it, with
  // `write` alone.
  void DeliverFinished() noexcept;
  friend void DeliverFinishedFiles() noexcept;

  // Indexed by the low bits of a file descriptor; the first three are the
  // pre-opened files.
  std::vector<Slot> fd_slots_;
  // Indexed by channel; channel 0 is standard output and its slot unused.
  std::array<Slot, kChannelCount> channel_slots_;
  // What the most recent operation given each descriptor met, for each whose
  // most recent operation met an error.
  std::unordered_map<std::int32_t, File::Refusal> met_by_descriptor_;
  // What files already closed lost while the design was never told.
  std::vector<ClosedLoss> closed_losses_;
};

// What finished output calls wrote to every open file of the run reaches it,
// with nothing but `write`, from a signal handler or wherever else nothing may
// be called.
void DeliverFinishedFiles() noexcept;

// What `$fscanf` reads (LRM 21.3.4.3): a file from where it stands, a byte
// pushed back by `$ungetc` first, with a null character ordinary input (LRM
// 21.3.4.3(a)). A byte the scan ungets becomes the file's pushed-back byte, so
// the next read of any kind takes it. A file that cannot be read is an input
// with no bytes. The scan is one operation on its descriptor, which ends with
// the input.
class FileScanInput final : public value::ScanInput {
 public:
  FileScanInput(FileTable::Sentry sentry, File* file)
      : ScanInput(value::NullByte::kOrdinary),
        sentry_(std::move(sentry)),
        file_(file) {
  }

  auto Peek() -> int override;
  auto Consume() -> int override;
  void Unget(int byte) override;

 private:
  FileTable::Sentry sentry_;
  File* file_;
};

// LRM 21.3.4.4: `bytes` as the value `dest` holds, the first byte filling its
// most significant location and what they span below its width truncated.
void ReadBigEndian(
    const value::IntegralView& dest, std::span<const char> bytes);

template <typename... Targets>
auto FileTable::Scan(
    std::int64_t fd, const value::String& format,
    value::Tuple<Targets...> prototypes, base::SourceLocation at)
    -> value::Tuple<value::Integer, Targets...> {
  FileScanInput input = ScanInputOf(fd);
  return value::Scan(input, format, std::move(prototypes), at);
}

template <value::IntegralValue T>
auto FileTable::Read(const T& dest, std::int64_t fd) -> BinaryRead<T> {
  // The destination is read into where the answer keeps it.
  BinaryRead<T> answer(value::Int{}, dest);
  answer.template Component<0>() = ReadInto(
      answer.template Component<1>().AsMutableWords().MutableView(), fd);
  return answer;
}

template <value::IntegralValue T, std::size_t N>
auto FileTable::ReadMemory(
    const value::UnpackedArray<T, N>& dest, std::int64_t fd,
    std::span<const std::int64_t> bounds, std::int64_t sv_start,
    std::int64_t count) -> BinaryRead<value::UnpackedArray<T, N>> {
  // The memory is read into where the answer keeps it.
  BinaryRead<value::UnpackedArray<T, N>> answer(value::Int{}, dest);
  value::UnpackedArray<T, N>& memory = answer.template Component<1>();
  const value::UnpackedRange declared = value::UnpackedRangesOf(bounds).front();
  answer.template Component<0>() = value::Int::FromInt(ReadMemoryWords(
      fd, T::kWidth, declared, sv_start, count,
      [&memory, &declared](std::int64_t sv_index, std::span<const char> bytes) {
        ReadBigEndian(
            memory.RawAt(static_cast<std::size_t>(declared.ToOrdinal(sv_index)))
                .AsMutableWords()
                .MutableView(),
            bytes);
      }));
  return answer;
}

}  // namespace lyra::runtime
