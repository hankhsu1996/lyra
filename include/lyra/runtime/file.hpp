#pragma once

#include <array>
#include <atomic>
#include <csignal>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <memory>
#include <optional>
#include <string_view>
#include <vector>

namespace lyra::runtime {

// The signals that stop a run from outside: the keyboard's interrupt and a
// request to terminate.
inline constexpr std::array kStopSignals{SIGINT, SIGTERM};

// The stop signals are held off the calling thread while this stands, and
// delivered as it ends. What a handler for one of them reads has to be whole
// -- the bytes a file is handing over, the table of files there are -- and a
// thread that only serves the simulation holds them for its whole life, so
// that they are handled by the thread running the design.
class StopSignalsHeld {
 public:
  StopSignalsHeld();
  ~StopSignalsHeld();
  StopSignalsHeld(const StopSignalsHeld&) = delete;
  auto operator=(const StopSignalsHeld&) -> StopSignalsHeld& = delete;
  StopSignalsHeld(StopSignalsHeld&&) = delete;
  auto operator=(StopSignalsHeld&&) -> StopSignalsHeld& = delete;

 private:
  sigset_t before_{};
};

// From here on a stop from outside ends the process by its own action, on the
// calling thread whatever held it and whichever handler stood: what an ending
// does before it waits on something that may never come. Nothing here is
// beyond what a signal handler may call.
void StopSignalsEndTheProcess() noexcept;

// The C stream a file was opened as, closed with it, which closes its
// descriptor.
using OpenedStream = std::unique_ptr<std::FILE, decltype(&std::fclose)>;

// A file the design reads and writes through one descriptor (LRM 21.3): one
// position for both (LRM 21.3.5), and one byte of pushback (LRM 21.3.4.1).
//
// What the design writes is held here until it is handed to the descriptor:
// when the buffer is full, when the design asks for it (LRM 21.3.6), when
// the file is read, positioned or closed, when the run ends, and whenever
// something else is about to write the same destination. How much of it an
// output call has finished writing is published as it is written, so that an
// ending the process can still act on -- a signal from outside, a fault, a
// checker's report, a refused request for memory -- hands that part over with
// `write` alone, which is what a signal handler may call where it may call no
// library. An ending nothing can act on loses at most what the buffer holds.
//
// Foreign code (LRM 35) writes the C library's stream on the same descriptor,
// where there is one. Each side hands what it holds over before the other
// writes: this file's bytes before foreign code runs, and the stream's before
// the design next writes, so what the two print comes out in the order it was
// printed.
class File {
 public:
  // What a read answers at the end of the file.
  static constexpr int kEndOfFile = -1;

  // A file on `fd`, holding what the design writes in `capacity` bytes before
  // handing it over; a capacity of zero hands every write over at once.
  // `c_stream` is the C library's stream foreign code may write on the same
  // descriptor, or null where none does.
  File(
      int fd, bool permits_read, bool permits_write, std::size_t capacity,
      std::FILE* c_stream);
  ~File();
  File(const File&) = delete;
  auto operator=(const File&) -> File& = delete;
  File(File&&) = delete;
  auto operator=(File&&) -> File& = delete;

  [[nodiscard]] auto PermitsRead() const -> bool;
  [[nodiscard]] auto PermitsWrite() const -> bool;

  void Write(std::string_view text);

  // The next byte, taken or only looked at, or the end of the file.
  auto Take() -> int;
  auto Peek() -> int;

  // Makes `byte` the next one read, and the file no longer at its end
  // (LRM 21.3.8); refused where one is already pushed back.
  auto Unget(char byte) -> bool;

  // Moves the position, `whence` being SEEK_SET, SEEK_CUR or SEEK_END, which
  // cancels any pushback (LRM 21.3.5). False where the descriptor refused.
  auto Seek(std::int64_t offset, int whence) -> bool;

  // The position the next read or write acts at, or nothing where the
  // descriptor has none.
  auto Tell() -> std::optional<std::int64_t>;

  // Whether a read has found nothing left (LRM 21.3.8).
  [[nodiscard]] auto AtEnd() const -> bool;

  // What the design wrote reaches the descriptor.
  void HandOver();

  // Foreign code is about to run, and may write this descriptor's C stream.
  void BeforeForeignCodeRuns();

  // What finished output calls wrote reaches the descriptor, with `write`
  // alone.
  void DeliverFinished() noexcept;

 private:
  // What the C stream holds reaches the descriptor, where foreign code may
  // have written it.
  void DrainCStream();
  // Positions the descriptor at the next byte the design reads and drops what
  // was read ahead, so a write lands there. The buffer holds the design's
  // writes or what was read ahead, never both: a write first drops the read
  // ahead, and a read first hands the writes over.
  void StopReading();
  // Makes the next byte readable, or answers false at the end of the file.
  auto ReadAhead() -> bool;
  // How many bytes the design has not read of what was taken from the
  // descriptor, pushback included.
  [[nodiscard]] auto Unread() const -> std::int64_t;

  int fd_;
  bool permits_read_;
  bool permits_write_;
  std::FILE* c_stream_;
  bool c_stream_may_hold_ = false;
  std::vector<char> buffer_;
  // The bytes of `buffer_` read ahead and not yet taken, while reading.
  std::size_t read_begin_ = 0;
  std::size_t read_end_ = 0;
  // The bytes of `buffer_` output calls have finished writing, while writing.
  std::atomic<std::size_t> finished_ = 0;
  std::optional<char> putback_;
  bool at_end_ = false;
};

}  // namespace lyra::runtime
