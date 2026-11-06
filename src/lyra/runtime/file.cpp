#include "lyra/runtime/file.hpp"

#include <algorithm>
#include <atomic>
#include <cerrno>
#include <csignal>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <optional>
#include <pthread.h>
#include <span>
#include <string_view>
#include <sys/types.h>
#include <system_error>
#include <unistd.h>
#include <utility>

#include "lyra/base/errno_as_error_code.hpp"
#include "lyra/base/write_all.hpp"

namespace lyra::runtime {

namespace {

auto StopSignalSet() noexcept -> sigset_t {
  sigset_t stops{};
  ::sigemptyset(&stops);
  for (const int signal : kStopSignals) {
    ::sigaddset(&stops, signal);
  }
  return stops;
}

}  // namespace

StopSignalsHeld::StopSignalsHeld() {
  const sigset_t held = StopSignalSet();
  ::pthread_sigmask(SIG_BLOCK, &held, &before_);
}

StopSignalsHeld::~StopSignalsHeld() {
  ::pthread_sigmask(SIG_SETMASK, &before_, nullptr);
}

void StopSignalsEndTheProcess() noexcept {
  struct sigaction own_action{};
  own_action.sa_handler = SIG_DFL;
  ::sigemptyset(&own_action.sa_mask);
  for (const int signal : kStopSignals) {
    ::sigaction(signal, &own_action, nullptr);
  }
  const sigset_t stops = StopSignalSet();
  ::pthread_sigmask(SIG_UNBLOCK, &stops, nullptr);
}

File::File(
    int fd, bool permits_read, bool permits_write, std::size_t capacity,
    std::FILE* c_stream)
    : fd_(fd),
      permits_read_(permits_read),
      permits_write_(permits_write),
      c_stream_(c_stream),
      buffer_(capacity) {
}

File::~File() {
  HandOver();
}

auto File::PermitsRead() const -> bool {
  return permits_read_;
}

auto File::PermitsWrite() const -> bool {
  return permits_write_;
}

void File::Write(std::string_view text) {
  DrainCStream();
  StopReading();
  const std::size_t finished = finished_.load(std::memory_order_relaxed);
  if (finished + text.size() > buffer_.size()) HandOver();
  if (text.size() > buffer_.size()) {
    KeepLoss(base::WriteAll(fd_, text));
    return;
  }
  const std::size_t at = finished_.load(std::memory_order_relaxed);
  std::ranges::copy(text, std::span<char>(buffer_).subspan(at).begin());
  finished_.store(at + text.size(), std::memory_order_release);
}

auto File::Take() -> int {
  if (putback_.has_value()) {
    const auto byte = static_cast<unsigned char>(*putback_);
    putback_.reset();
    return byte;
  }
  if (!ReadAhead()) return kEndOfFile;
  return static_cast<unsigned char>(buffer_[read_begin_++]);
}

auto File::Peek() -> int {
  if (putback_.has_value()) {
    return static_cast<unsigned char>(*putback_);
  }
  if (!ReadAhead()) return kEndOfFile;
  return static_cast<unsigned char>(buffer_[read_begin_]);
}

auto File::Unget(char byte) -> bool {
  if (putback_.has_value()) return false;
  putback_ = byte;
  at_end_ = false;
  return true;
}

auto File::Seek(std::int64_t offset, int whence) -> bool {
  HandOver();
  // The descriptor stands past what was read ahead; the design's position is
  // where it has read to.
  if (whence == SEEK_CUR) offset -= Unread();
  read_begin_ = read_end_ = 0;
  putback_.reset();
  if (::lseek(fd_, offset, whence) < 0) {
    KeepRefusal({.code = base::ErrnoAsErrorCode(), .lost_output = false});
    return false;
  }
  at_end_ = false;
  return true;
}

auto File::Tell() -> std::optional<std::int64_t> {
  HandOver();
  const off_t at = ::lseek(fd_, 0, SEEK_CUR);
  if (at < 0) {
    KeepRefusal({.code = base::ErrnoAsErrorCode(), .lost_output = false});
    return std::nullopt;
  }
  return static_cast<std::int64_t>(at) - Unread();
}

auto File::AtEnd() const -> bool {
  return at_end_;
}

void File::HandOver() {
  DrainCStream();
  // Held before the count is read, so an ending that delivers between the
  // read and the write cannot have the same bytes written twice.
  const StopSignalsHeld held;
  const std::size_t finished = finished_.load(std::memory_order_relaxed);
  if (finished == 0) return;
  KeepLoss(base::WriteAll(fd_, std::span<const char>(buffer_).first(finished)));
  finished_.store(0, std::memory_order_release);
}

void File::BeforeForeignCodeRuns() {
  HandOver();
  c_stream_may_hold_ = c_stream_ != nullptr;
}

void File::DeliverFinished() noexcept {
  const std::size_t finished = finished_.load(std::memory_order_acquire);
  base::WriteAsMuchAsTaken(fd_, std::span<const char>(buffer_).first(finished));
  finished_.store(0, std::memory_order_release);
}

auto File::TakeRefusal() -> Refusal {
  return std::exchange(refused_, {});
}

void File::ClearUntoldLoss() {
  untold_loss_ = {};
}

auto File::TakeUntoldLoss() -> std::error_code {
  return std::exchange(untold_loss_, {});
}

void File::KeepRefusal(Refusal refused) {
  if (!refused_.code) refused_ = refused;
}

void File::KeepLoss(std::error_code refused) {
  if (!refused) return;
  KeepRefusal({.code = refused, .lost_output = true});
  if (!untold_loss_) untold_loss_ = refused;
}

void File::DrainCStream() {
  if (!c_stream_may_hold_) return;
  if (std::fflush(c_stream_) != 0) KeepLoss(base::ErrnoAsErrorCode());
  c_stream_may_hold_ = false;
}

void File::StopReading() {
  const std::int64_t unread = Unread();
  // An unseekable descriptor cannot be moved back over what was read ahead,
  // which C's stdio leaves as it is too, so a write follows it.
  if (unread > 0 && ::lseek(fd_, -unread, SEEK_CUR) < 0 && errno != ESPIPE) {
    KeepRefusal({.code = base::ErrnoAsErrorCode(), .lost_output = false});
  }
  read_begin_ = read_end_ = 0;
  putback_.reset();
}

auto File::ReadAhead() -> bool {
  if (read_begin_ < read_end_) return true;
  HandOver();
  ssize_t got = ::read(fd_, buffer_.data(), buffer_.size());
  while (got < 0 && errno == EINTR) {
    got = ::read(fd_, buffer_.data(), buffer_.size());
  }
  if (got < 0) {
    KeepRefusal({.code = base::ErrnoAsErrorCode(), .lost_output = false});
    return false;
  }
  if (got == 0) {
    at_end_ = true;
    return false;
  }
  read_begin_ = 0;
  read_end_ = static_cast<std::size_t>(got);
  return true;
}

auto File::Unread() const -> std::int64_t {
  return static_cast<std::int64_t>(read_end_ - read_begin_) +
         (putback_.has_value() ? 1 : 0);
}

}  // namespace lyra::runtime
