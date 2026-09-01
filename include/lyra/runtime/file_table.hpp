#pragma once

#include <array>
#include <cstddef>
#include <cstdint>
#include <fstream>
#include <functional>
#include <memory>
#include <optional>
#include <stop_token>
#include <string>
#include <string_view>
#include <vector>

#include "lyra/value/packed_array.hpp"
#include "lyra/value/string.hpp"
#include "lyra/value/tuple.hpp"
#include "lyra/value/unpacked_array.hpp"

namespace lyra::runtime {

class StreamDispatcher;

// LRM 21.3.2 cancellation observable. One or more output channels' joint
// cancel state; `IsCancelled()` returns true once any participating channel
// has been closed since this view was acquired. Captured by value into a
// postponed closure so the closure short-circuits when the descriptor it
// targets is invalidated mid-slot.
//
// "Whole-operation cancellation" -- any participating channel closing kills
// the entire submit, matching the literal LRM wording "operations on a ...
// multichannel descriptor are implicitly cancelled".
class ChannelCancellation {
 public:
  // A view over no channels, which nothing can cancel. It is what storage
  // holding one starts as, before the view it will carry is copied in.
  ChannelCancellation() = default;

  [[nodiscard]] auto IsCancelled() const noexcept -> lyra::value::PackedArray;

 private:
  friend class FileTable;
  explicit ChannelCancellation(std::vector<std::stop_token> tokens);
  std::vector<std::stop_token> tokens_;
};

// What a read that delivers text completes with: how many bytes it read, and
// the text those bytes make (LRM 21.3.4.2, 21.3.7).
using TextRead =
    lyra::value::Tuple<lyra::value::PackedArray, lyra::value::String>;

// What a read of binary data completes with: how many bytes it read, and the
// destination those bytes filled (LRM 21.3.4.4). The destination crosses in
// as well, because its own shape decides how much is read -- a nine-bit word
// takes two bytes where an eight-bit one takes one -- and what the file does
// not reach keeps what it held.
using PackedRead =
    lyra::value::Tuple<lyra::value::PackedArray, lyra::value::PackedArray>;
using MemoryRead = lyra::value::Tuple<
    lyra::value::PackedArray,
    lyra::value::UnpackedArray<lyra::value::PackedArray>>;

// Owns file handles opened by `$fopen` (LRM 21.3.1). Two descriptor shapes
// share the same int32 namespace:
//
// - MCD (multichannel descriptor, no mode arg): bit 31 clear, bits 1..30 are
//   per-channel single-bit slots, bit 0 always refers to stdout. The caller
//   can bitwise-OR several MCDs and pass the union to one `$fdisplay` call
//   to fan output across channels.
//
// - FD (file descriptor, mode arg present): bit 31 set, bits 0..30 form an
//   index into the FD pool. The first three indexes are reserved for the
//   pre-bound STDIN / STDOUT / STDERR descriptors (32'h8000_0000..0002 per
//   LRM 21.3.1). Those slots are not backed by FileTable storage -- callers
//   route them directly through `RuntimeEffects::Stream()` / `std::cerr`
//   to preserve test-harness stdout ordering and avoid wrapping stdio in
//   an owning fstream (you can't fstream stdout).
//
// Owned slots hold `std::unique_ptr<std::fstream>`; fstream's destructor
// flushes and closes when the slot is reset, so no raw fopen / fclose
// machinery lives at this layer.
//
// Each slot also owns a `std::stop_source` -- the LRM 21.3.2 cancel signal
// for postponed operations tied to the channel ($fstrobe, and any future
// $fmonitor). Close() request_stops the slot's source and replaces it with
// a fresh one, so channel-reuse after close starts with a clean signal
// while observers of the old source see a permanent stop.
class FileTable {
 public:
  struct ErrorRecord {
    int errno_value = 0;
    std::string message;
  };

  // A live FD slot. Public so consumers needing more than just the
  // `std::fstream*` (slot-side putback for $ungetc / $fgetc / $fseek /
  // $rewind / the scanner; mode bits for read-side rejection) can reach
  // the fields directly via `ResolveSlot`. The narrow `Resolve` stays
  // for consumers that only need the stream ($fprint family, $fflush,
  // $feof, $ftell).
  struct FdSlot {
    std::unique_ptr<std::fstream> file;
    ErrorRecord error;
    std::stop_source cancel_source;
    // LRM 21.3.4.1 + 21.3.5 single-byte putback. Held on the slot rather
    // than in `std::fstream`'s putback area because the standard rejects
    // pushback on a freshly-opened stream and leaves seek-cancel behaviour
    // implementation-defined -- neither matches the LRM contract.
    std::optional<char> putback;
    // LRM 21.3.4 read permission; false -> EBADF on every read entry.
    bool permits_read = false;
    bool permits_write = false;
    // File bytes the last peek read, used to rewind any tail the next
    // advance reports as unconsumed.
    std::size_t peek_len = 0;
  };

  explicit FileTable(StreamDispatcher& stream) : stream_(&stream) {
  }
  ~FileTable() = default;
  FileTable(const FileTable&) = delete;
  auto operator=(const FileTable&) -> FileTable& = delete;
  FileTable(FileTable&&) = delete;
  auto operator=(FileTable&&) -> FileTable& = delete;

  // Returns descriptor value per LRM 21.3.1, or 0 on failure (file cannot
  // be opened, all MCD slots in use, or unknown mode string). `mode`
  // absent -> MCD (bit 31 clear, single bit set in 1..30, file opened in
  // write-truncate mode). `mode` present -> FD (bit 31 set), opened with
  // the `std::ios_base::openmode` derived from the string (LRM 21.3.1
  // Table 21-6).
  auto Open(std::string_view name, std::optional<std::string_view> mode)
      -> std::int32_t;

  // Closes the file(s) addressed by `descriptor`. For an MCD, iterates set
  // bits in 1..30 and closes each. For an FD, closes that single owned
  // slot. Bit 0 (stdout) and the pre-bound STDIN / STDOUT / STDERR FDs are
  // never closed. Also fires the cancel signal on every affected slot
  // (LRM 21.3.2) and replaces each slot's stop_source so the next open on
  // a reused slot starts with a fresh signal.
  void Close(std::int32_t descriptor);

  // Returns the owned `std::fstream*` for `descriptor`, or nullptr if the
  // descriptor does not address an owned stream. Returns nullptr for
  // descriptor == 0, for the pre-bound stdio FDs (STDOUT_FD / STDERR_FD),
  // and for descriptors whose slot is unallocated; callers route those
  // cases by direct value check before calling `Resolve`.
  //
  // For MCD inputs, callers are expected to have masked down to a single
  // bit before calling. Bit 0 alone -> nullptr (stdout sentinel).
  auto Resolve(std::int32_t descriptor) -> std::fstream*;

  // Returns the owned `FdSlot*` for an FD descriptor, or nullptr if the
  // descriptor does not address an owned FD slot (zero, MCD, stdio
  // sentinel, or unmapped index). Used by consumers that need slot-side
  // state (the putback buffer for $ungetc / $fgetc / $fseek / $rewind /
  // the scan-source).
  auto ResolveSlot(std::int32_t descriptor) -> FdSlot*;

  // LRM 21.3.4.3: snapshot the bytes available at the fd's logical read
  // position so a pure-value parser can run over them; pair with
  // `AdvanceFd` to commit the byte count the parser actually used.
  // Invalid / closed / non-readable descriptors return empty and stamp
  // EBADF.
  auto PeekBuffered(const value::PackedArray& fd) -> value::String;

  // LRM 21.3.4.3 commit half of the peek/advance pair: drop `n` bytes
  // from the head of the most recent peek (putback first, then file
  // bytes); any unconsumed tail goes back to the stream so the next read
  // sees it again.
  void AdvanceFd(const value::PackedArray& fd, const value::PackedArray& n);

  // LRM 21.3.7 $ferror state. The runtime entry points stamp the most recent
  // error for an FD via `SetError`; `$ferror(fd, str)` returns the saved
  // errno and copies the message into `str`, then clears the slot. MCDs are
  // write-only and have no per-channel error reporting on this surface.
  void SetError(std::int32_t fd, int errno_value, std::string message);
  [[nodiscard]] auto LastError(std::int32_t fd) const -> int;
  [[nodiscard]] auto LastErrorMessage(std::int32_t fd) const
      -> std::string_view;
  void ClearError(std::int32_t fd);

  // Returns a `ChannelCancellation` covering every owned channel
  // `descriptor` names (LRM 21.3.2). For an FD the view holds one token
  // from the FD slot's source; for an MCD the view holds a token per set
  // bit (1..30), with bit 0 / stdio sentinels / 0 silently skipped because
  // they cannot be closed. Held-by-value by the consumer; once a slot's
  // source is request_stopped, the consumer's stop_tokens see the stop
  // even if the slot is later reused (the new open installs a fresh
  // source -- the old token still observes the old, permanently-stopped
  // state through its own refcount).
  [[nodiscard]] auto CancellationFor(const lyra::value::PackedArray& descriptor)
      -> ChannelCancellation;

  // LRM 21.2.1 / 21.3.2 sink write. Dispatches by descriptor: stdout
  // sentinel routes through the stream dispatcher, stderr sentinel through
  // std::cerr, owned FDs / MCDs through this table's fstreams. `Writeln`
  // appends a trailing newline.
  void Write(
      const lyra::value::PackedArray& descriptor,
      const lyra::value::String& text);
  void Writeln(
      const lyra::value::PackedArray& descriptor,
      const lyra::value::String& text);

  // LRM 21.3.1 $fopen. The no-mode overload returns a multichannel
  // descriptor (MCD form); the mode overload returns a single file
  // descriptor (FD form). On failure both return 0 (file cannot be
  // opened, all MCD slots in use, or unknown mode string).
  auto Open(const lyra::value::String& name) -> lyra::value::PackedArray;
  auto Open(const lyra::value::String& name, const lyra::value::String& mode)
      -> lyra::value::PackedArray;

  // LRM 21.3.1 $fclose. No-op for 0 / pre-bound stdio FDs; for an MCD
  // closes every set-bit channel and fires the per-channel cancel signal.
  void Close(const lyra::value::PackedArray& descriptor);

  // LRM 21.3.4.1 $fgetc. Returns the next byte as an int32 PackedArray,
  // or -1 on EOF / error. A pending $ungetc putback (if any) is the
  // first byte returned.
  auto Getc(const lyra::value::PackedArray& fd) -> lyra::value::PackedArray;

  // LRM 21.3.4.1 $ungetc. Pushes the low byte of `c` back onto the FD's
  // input buffer (single-byte slot-side putback; a second $ungetc before
  // any read returns -1). Returns 0 on success or -1 on error.
  auto Ungetc(
      const lyra::value::PackedArray& c, const lyra::value::PackedArray& fd)
      -> lyra::value::PackedArray;

  // LRM 21.3.4.2 $fgets. Reads bytes up to and including the next newline, or
  // until EOF, and completes with how many it read -- zero on error -- and the
  // line they make.
  auto Gets(const lyra::value::PackedArray& fd) -> TextRead;

  // LRM 21.3.4.4 $fread into a packed destination. Reads (BitWidth+7)/8
  // bytes big-endian (first byte fills MSBs); the destination's shape drives
  // the result's width / sign / 4-state.
  auto Read(lyra::value::PackedArray dest, const lyra::value::PackedArray& fd)
      -> PackedRead;

  // LRM 21.3.4.4 $fread into a memory. Iterates `dest` from SV index
  // `sv_start` toward the highest declared SV index, reading until EOF or
  // `count` elements. `declared_left` / `declared_right` are the
  // destination's declared bounds. The caller always supplies `sv_start` and
  // `count`, materializing the lowest declared index and the whole remaining
  // range where the SV call leaves them out, so one entry serves every form
  // the source may write.
  auto Read(
      lyra::value::UnpackedArray<lyra::value::PackedArray> dest,
      const lyra::value::PackedArray& fd,
      const lyra::value::PackedArray& declared_left,
      const lyra::value::PackedArray& declared_right,
      const lyra::value::PackedArray& sv_start,
      const lyra::value::PackedArray& count) -> MemoryRead;

  // LRM 21.3.5 $fseek. `operation` is 0/1/2 for SEEK_SET / SEEK_CUR /
  // SEEK_END. Returns 0 on success or -1 on error. Per LRM, any pending
  // $ungetc operation is cancelled.
  auto Seek(
      const lyra::value::PackedArray& fd,
      const lyra::value::PackedArray& offset,
      const lyra::value::PackedArray& operation) -> lyra::value::PackedArray;

  // LRM 21.3.5 $rewind. Equivalent to $fseek(fd, 0, 0).
  auto Rewind(const lyra::value::PackedArray& fd) -> lyra::value::PackedArray;

  // LRM 21.3.5 $ftell. Returns the current position or -1 on error.
  auto Tell(const lyra::value::PackedArray& fd) -> lyra::value::PackedArray;

  // LRM 21.3.8 $feof. Returns a nonzero value once an EOF has been
  // observed on `fd`, zero otherwise.
  auto Eof(const lyra::value::PackedArray& fd) -> lyra::value::PackedArray;

  // LRM 21.3.7 $ferror. Completes with the most recent errno stamped on `fd`
  // and its textual message. The slot's error state is cleared after the read.
  auto Error(const lyra::value::PackedArray& fd) -> TextRead;

  // LRM 21.3.6 $fflush. No-arg form flushes every open file; the
  // addressed form flushes a single FD or every set-bit MCD channel.
  void Flush();
  void Flush(const lyra::value::PackedArray& descriptor);

 private:
  // LRM 21.3.1: at most 31 MCD slots (bits 1..30); bit 0 is stdout-sentinel.
  static constexpr std::size_t kMcdSlotCount = 31;
  // FD pool indexes 0/1/2 are stdio sentinels and stay empty (Resolve
  // returns nullptr for them; the dispatch site special-cases them).
  static constexpr std::size_t kFdReservedSlots = 3;

  struct McdSlot {
    std::unique_ptr<std::fstream> file;
    std::stop_source cancel_source;
  };

  StreamDispatcher* stream_ = nullptr;
  std::array<McdSlot, kMcdSlotCount> mcd_slots_{};
  std::vector<FdSlot> fd_pool_{kFdReservedSlots};
};

// LRM 21.3.4.4 memory load, over whatever holds the words. Reads from
// `start_sv` toward the highest declared index, stopping at end of file or
// after `count` words, and hands each word to `write_word` by its
// source-declared index -- the coordinate system the declared bounds state,
// which the holder resolves. `element_prototype` states the shape a word
// takes, which is what decides how many bytes one costs. Answers with the byte
// count, zero where nothing was read.
//
// The holder is a parameter because a memory reached through a monomorphized
// container and one reached through an erased handle are the same load; a
// second copy of the addressing and the partial-word rule would be two
// readings of one clause.
auto ReadMemoryWords(
    FileTable& files, const lyra::value::PackedArray& fd,
    const lyra::value::PackedArray& element_prototype,
    std::int64_t declared_left, std::int64_t declared_right,
    std::int64_t start_sv, std::int64_t count,
    const std::function<void(std::int64_t, lyra::value::PackedArray)>&
        write_word) -> std::int32_t;

}  // namespace lyra::runtime
