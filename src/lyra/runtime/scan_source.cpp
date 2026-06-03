#include "lyra/runtime/scan_source.hpp"

#include "lyra/base/internal_error.hpp"

namespace lyra::runtime {

StringScanSource::StringScanSource(std::string_view buf) : buf_(buf) {
}

auto StringScanSource::Peek() -> int {
  if (pushback_.has_value()) {
    return *pushback_;
  }
  if (cursor_ >= buf_.size()) {
    return -1;
  }
  return static_cast<unsigned char>(buf_[cursor_]);
}

auto StringScanSource::Consume() -> int {
  if (pushback_.has_value()) {
    const int byte = *pushback_;
    pushback_.reset();
    return byte;
  }
  if (cursor_ >= buf_.size()) {
    return -1;
  }
  const int byte = static_cast<unsigned char>(buf_[cursor_]);
  ++cursor_;
  return byte;
}

void StringScanSource::Unget(int byte) {
  if (pushback_.has_value()) {
    throw InternalError(
        "StringScanSource::Unget: pushback slot already holds a byte; "
        "scanner attempted to Unget twice without an intervening Consume");
  }
  if (byte == -1) {
    return;
  }
  pushback_ = byte;
}

FileScanSource::FileScanSource(std::fstream& stream) : stream_(&stream) {
}

auto FileScanSource::Peek() -> int {
  if (peeked_.has_value()) {
    return *peeked_;
  }
  const int byte = stream_->get();
  if (byte == std::char_traits<char>::eof()) {
    return -1;
  }
  peeked_ = byte;
  return byte;
}

auto FileScanSource::Consume() -> int {
  if (peeked_.has_value()) {
    const int byte = *peeked_;
    peeked_.reset();
    return byte;
  }
  const int byte = stream_->get();
  return byte == std::char_traits<char>::eof() ? -1 : byte;
}

void FileScanSource::Unget(int byte) {
  if (byte == -1) {
    return;
  }
  if (peeked_.has_value()) {
    throw InternalError(
        "FileScanSource::Unget: pushback slot already holds a byte; "
        "scanner attempted to Unget twice without an intervening Consume");
  }
  peeked_ = byte;
}

void FileScanSource::FlushPushback() {
  if (!peeked_.has_value()) {
    return;
  }
  // Clear eofbit / failbit so putback succeeds even when Peek hit EOF
  // earlier in the scan. LRM 21.3.4.3 "the offending input character is
  // left unread in the input stream" -- pushing back here makes the
  // unconsumed peeked byte visible to the next $fgetc on this FD.
  stream_->clear();
  stream_->putback(static_cast<char>(*peeked_));
  peeked_.reset();
}

}  // namespace lyra::runtime
