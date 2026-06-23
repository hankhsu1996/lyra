#pragma once

#include <array>
#include <cstdint>
#include <fstream>
#include <memory>
#include <optional>
#include <stop_token>
#include <string>
#include <string_view>
#include <vector>

#include "lyra/value/packed_array.hpp"
#include "lyra/value/string.hpp"

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
  [[nodiscard]] auto IsCancelled() const noexcept -> lyra::value::PackedArray;

 private:
  friend class FileTable;
  explicit ChannelCancellation(std::vector<std::stop_token> tokens);
  std::vector<std::stop_token> tokens_;
};

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
//   route them directly through `RuntimeServices::Stream()` / `std::cerr`
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
    // LRM 21.3.4.1 + 21.3.5: $ungetc parks a byte here; $fgetc / $fgets /
    // $fread / the scan-source drain it before touching the underlying
    // stream; $fseek / $rewind clear it ("Repositioning the current file
    // position with $fseek or $rewind shall cancel any $ungetc
    // operations"). We do not use std::fstream's own putback area: it
    // rejects pushback on a freshly-opened stream and its seek-cancel
    // behaviour is implementation-defined, neither of which matches the
    // LRM contract.
    std::optional<char> putback;
    // LRM 21.3.4: "Files opened using file descriptors (fd) can be read
    // from only if they were opened with either the r or r+ type
    // values." Captured from the mode string at $fopen time; the read
    // entries reject the FD with EBADF when this is false.
    bool permits_read = false;
    bool permits_write = false;
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

}  // namespace lyra::runtime
