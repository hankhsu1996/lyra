#include "lyra/runtime/readmem.hpp"

#include <algorithm>
#include <cstdint>
#include <format>
#include <fstream>
#include <optional>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

#include "lyra/runtime/diagnostic.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/string.hpp"
#include "lyra/value/unpacked_array.hpp"

namespace lyra::runtime {

namespace {

auto TaskName(unsigned base) -> value::String {
  return value::String(base == 2U ? "$readmemb" : "$readmemh");
}

void Warn(RuntimeEffects& runtime, unsigned base, const std::string& text) {
  runtime.Diagnostic().EmitWarning(TaskName(base), value::String(text));
}

void Error(RuntimeEffects& runtime, unsigned base, const std::string& text) {
  runtime.Diagnostic().EmitError(TaskName(base), value::String(text));
}

// Splits the file text into tokens, dropping `//`-to-end-of-line and `/* */`
// comments and any whitespace between tokens. A token is a maximal run of
// non-whitespace, non-comment characters -- either an `@address` directive or a
// data word.
auto Tokenize(std::string_view text) -> std::vector<std::string> {
  std::vector<std::string> tokens;
  std::string current;
  const auto flush = [&] {
    if (!current.empty()) {
      tokens.push_back(current);
      current.clear();
    }
  };
  std::size_t i = 0;
  while (i < text.size()) {
    const char c = text[i];
    if (c == '/' && i + 1 < text.size() && text[i + 1] == '/') {
      flush();
      i += 2;
      while (i < text.size() && text[i] != '\n') ++i;
      continue;
    }
    if (c == '/' && i + 1 < text.size() && text[i + 1] == '*') {
      flush();
      i += 2;
      while (i + 1 < text.size()) {
        if (text[i] == '*' && text[i + 1] == '/') break;
        ++i;
      }
      i += 2;
      continue;
    }
    if (c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f' ||
        c == '\v') {
      flush();
      ++i;
      continue;
    }
    current.push_back(c);
    ++i;
  }
  flush();
  return tokens;
}

void ReadMemImpl(
    RuntimeEffects& runtime, value::UnpackedArray<value::PackedArray>& dest,
    const value::String& filename, std::int64_t declared_left,
    std::int64_t declared_right, unsigned base,
    std::optional<std::int64_t> start, std::optional<std::int64_t> finish) {
  if (dest.RawSize() == 0U) return;

  std::ifstream in{std::string{filename.View()}};
  if (!in.is_open()) {
    Error(
        runtime, base,
        std::format("cannot open file '{}'", std::string{filename.View()}));
    return;
  }
  std::ostringstream contents;
  contents << in.rdbuf();
  const std::string text = contents.str();

  const std::int64_t lowest = std::min(declared_left, declared_right);
  const std::int64_t highest = std::max(declared_left, declared_right);

  // The active window and fill direction follow LRM 21.4 from which optional
  // addresses the call supplied.
  std::int64_t active_lo = lowest;
  std::int64_t active_hi = highest;
  std::int64_t cursor = lowest;
  std::int64_t step = 1;
  if (start.has_value() && finish.has_value()) {
    cursor = *start;
    step = (*start <= *finish) ? 1 : -1;
    active_lo = std::min(*start, *finish);
    active_hi = std::max(*start, *finish);
  } else if (start.has_value()) {
    cursor = *start;
    active_lo = *start;
  }

  const auto& shape = dest.RawAt(0);
  const std::uint64_t width = shape.BitWidth();
  const bool is_signed = shape.IsSigned();
  const bool is_four_state = shape.IsFourState();

  bool saw_address = false;
  std::int64_t words_written = 0;
  const std::vector<std::string> tokens = Tokenize(text);
  for (const std::string& token : tokens) {
    if (token.front() == '@') {
      const auto addr = value::PackedArray::FromDigits(
          std::string_view{token}.substr(1), 16U, 64U, true, false);
      if (!addr) {
        Error(runtime, base, std::format("malformed address '{}'", token));
        return;
      }
      const std::int64_t a = addr->ToInt64();
      if (a < active_lo || a > active_hi) {
        Error(
            runtime, base,
            std::format("address {} is outside the load range", a));
        return;
      }
      cursor = a;
      saw_address = true;
      continue;
    }

    if (cursor < active_lo || cursor > active_hi) break;

    const auto elem = value::PackedArray::FromDigits(
        token, base, width, is_signed, is_four_state);
    if (!elem) {
      Error(runtime, base, std::format("malformed data word '{}'", token));
      return;
    }
    dest.ElementRef(
        value::PackedArray::Int(static_cast<std::int32_t>(cursor)),
        value::PackedArray::Int(static_cast<std::int32_t>(declared_left)),
        value::PackedArray::Int(static_cast<std::int32_t>(declared_right))) =
        *elem;
    ++words_written;
    cursor += step;
  }

  // LRM 21.4: a start/finish range with no in-file addresses must be filled
  // exactly; a word-count mismatch is a warning, not an error.
  if (start.has_value() && finish.has_value() && !saw_address) {
    const std::int64_t expected = active_hi - active_lo + 1;
    if (words_written != expected) {
      Warn(
          runtime, base,
          std::format(
              "file holds {} words but the address range spans {}",
              words_written, expected));
    }
  }
}

}  // namespace

void ReadMem(
    RuntimeEffects& runtime, value::UnpackedArray<value::PackedArray>& dest,
    const value::String& filename, const value::PackedArray& declared_left,
    const value::PackedArray& declared_right, const value::PackedArray& base) {
  ReadMemImpl(
      runtime, dest, filename, declared_left.ToInt64(),
      declared_right.ToInt64(), static_cast<unsigned>(base.ToInt64()),
      std::nullopt, std::nullopt);
}

void ReadMem(
    RuntimeEffects& runtime, value::UnpackedArray<value::PackedArray>& dest,
    const value::String& filename, const value::PackedArray& declared_left,
    const value::PackedArray& declared_right, const value::PackedArray& base,
    const value::PackedArray& start) {
  ReadMemImpl(
      runtime, dest, filename, declared_left.ToInt64(),
      declared_right.ToInt64(), static_cast<unsigned>(base.ToInt64()),
      start.ToInt64(), std::nullopt);
}

void ReadMem(
    RuntimeEffects& runtime, value::UnpackedArray<value::PackedArray>& dest,
    const value::String& filename, const value::PackedArray& declared_left,
    const value::PackedArray& declared_right, const value::PackedArray& base,
    const value::PackedArray& start, const value::PackedArray& finish) {
  ReadMemImpl(
      runtime, dest, filename, declared_left.ToInt64(),
      declared_right.ToInt64(), static_cast<unsigned>(base.ToInt64()),
      start.ToInt64(), finish.ToInt64());
}

}  // namespace lyra::runtime
