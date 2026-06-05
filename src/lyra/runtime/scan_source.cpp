#include "lyra/runtime/scan_source.hpp"

#include "lyra/base/internal_error.hpp"

namespace lyra::runtime {

auto ScanSource::IsWhitespace(int ch) -> bool {
  return ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r' || ch == '\f' ||
         ch == '\v';
}

StringScanSource::StringScanSource(std::string_view buf) : buf_(buf) {
}

auto StringScanSource::IsWhitespace(int ch) -> bool {
  // LRM 21.3.4.3(a): for $sscanf, null characters shall also be considered
  // white space.
  return ch == '\0' || ScanSource::IsWhitespace(ch);
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

FileScanSource::FileScanSource(FileTable::FdSlot& slot) : slot_(&slot) {
}

auto FileScanSource::Peek() -> int {
  if (peeked_.has_value()) {
    return *peeked_;
  }
  // LRM 21.3.4.1: drain any byte left by an earlier $ungetc (or a prior
  // scan's "offending character") before reading from the stream.
  if (slot_->putback.has_value()) {
    peeked_ = static_cast<unsigned char>(*slot_->putback);
    slot_->putback.reset();
    return *peeked_;
  }
  const int byte = slot_->file->get();
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
  if (slot_->putback.has_value()) {
    const int byte = static_cast<unsigned char>(*slot_->putback);
    slot_->putback.reset();
    return byte;
  }
  const int byte = slot_->file->get();
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
  // LRM 21.3.4.3 "the offending input character is left unread in the
  // input stream" -- park the unconsumed peek in the slot's putback so
  // the next read on this FD sees it. Invariant: the slot's putback is
  // empty here. Any pre-existing pushback would have been drained into
  // peeked_ by the very first Peek of this scan, so anything still in
  // peeked_ now must have come from the stream (or a scanner-side
  // Unget), never sat in slot.putback yet.
  if (slot_->putback.has_value()) {
    throw InternalError(
        "FileScanSource::FlushPushback: slot putback unexpectedly non-empty");
  }
  slot_->putback = static_cast<char>(*peeked_);
  peeked_.reset();
}

}  // namespace lyra::runtime
