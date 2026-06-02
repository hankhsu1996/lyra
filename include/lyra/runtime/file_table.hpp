#pragma once

#include <array>
#include <cstdint>
#include <fstream>
#include <memory>
#include <optional>
#include <string_view>
#include <vector>

namespace lyra::runtime {

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
class FileTable {
 public:
  FileTable() = default;
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
  // never closed.
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

  // FD encoding constants exposed for dispatch-site value comparisons.
  static constexpr std::int32_t kFdHighBit =
      static_cast<std::int32_t>(static_cast<std::uint32_t>(1) << 31U);
  static constexpr std::int32_t kStdinFd = kFdHighBit | 0;
  static constexpr std::int32_t kStdoutFd = kFdHighBit | 1;
  static constexpr std::int32_t kStderrFd = kFdHighBit | 2;

 private:
  // LRM 21.3.1: at most 31 MCD slots (bits 1..30); bit 0 is stdout-sentinel.
  static constexpr std::size_t kMcdSlotCount = 31;
  // FD pool indexes 0/1/2 are stdio sentinels and stay empty (Resolve
  // returns nullptr for them; the dispatch site special-cases them).
  static constexpr std::size_t kFdReservedSlots = 3;

  std::array<std::unique_ptr<std::fstream>, kMcdSlotCount> mcd_slots_{};
  std::vector<std::unique_ptr<std::fstream>> fd_pool_{kFdReservedSlots};
};

}  // namespace lyra::runtime
