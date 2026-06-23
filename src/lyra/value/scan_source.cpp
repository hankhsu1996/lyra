#include "lyra/value/scan_source.hpp"

#include "lyra/base/internal_error.hpp"

namespace lyra::value::detail {

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

}  // namespace lyra::value::detail
