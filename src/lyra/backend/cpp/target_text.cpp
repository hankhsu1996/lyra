#include "lyra/backend/cpp/target_text.hpp"

#include <array>
#include <charconv>
#include <cstddef>
#include <cstdint>
#include <string_view>
#include <system_error>

#include "lyra/base/internal_error.hpp"

namespace lyra::backend::cpp {

namespace {

// Indentation is copied from a fixed run of spaces rather than built a space at
// a time.
constexpr std::string_view kSpaces = "                                ";

constexpr std::size_t kSpacesPerLevel = 2;

// Wide enough for any 64-bit value in any base this target writes, sign
// included.
constexpr std::size_t kNumberRoom = 72;

template <typename Integer>
void AppendNumber(TargetText& out, Integer value, int base) {
  std::array<char, kNumberRoom> room{};
  const std::to_chars_result result =
      std::to_chars(room.data(), room.data() + room.size(), value, base);
  if (result.ec != std::errc{}) {
    throw InternalError(
        "backend::cpp: a whole number did not fit the room kept for writing "
        "one -- please report this as a bug");
  }
  out += std::string_view{
      room.data(), static_cast<std::size_t>(result.ptr - room.data())};
}

}  // namespace

auto TargetText::operator+=(std::string_view text) -> TargetText& {
  // An empty write is not a section's first byte, so it does not write the
  // section's blank line.
  if (text.empty()) {
    return *this;
  }
  PayWhatIsOwed();
  text_ += text;
  return *this;
}

void TargetText::OpenLine() {
  std::size_t remaining = depth_ * kSpacesPerLevel;
  if (remaining == 0) {
    return;
  }
  PayWhatIsOwed();
  while (remaining > kSpaces.size()) {
    text_ += kSpaces;
    remaining -= kSpaces.size();
  }
  text_ += kSpaces.substr(0, remaining);
}

void TargetText::Indent() {
  ++depth_;
}

void TargetText::Outdent() {
  --depth_;
}

void TargetText::PayWhatIsOwed() {
  text_.append(owed_blank_lines_, '\n');
  owed_blank_lines_ = 0;
}

void WriteNumber(TargetText& out, std::uint64_t value, int base) {
  AppendNumber(out, value, base);
}

void WriteNumber(TargetText& out, std::int64_t value, int base) {
  AppendNumber(out, value, base);
}

}  // namespace lyra::backend::cpp
