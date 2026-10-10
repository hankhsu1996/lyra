#include "lyra/runtime/mem_file.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <format>
#include <fstream>
#include <functional>
#include <optional>
#include <span>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

#include "lyra/runtime/diagnostic.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/value/format.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/string.hpp"

namespace lyra::runtime {

namespace {

enum class Direction : std::uint8_t { kLoad, kStore };

auto TaskName(unsigned base, Direction dir) -> value::String {
  const bool store = dir == Direction::kStore;
  if (base == 2U) {
    return value::String(store ? "$writememb" : "$readmemb");
  }
  return value::String(store ? "$writememh" : "$readmemh");
}

void Warn(
    RuntimeEffects& runtime, unsigned base, Direction dir,
    const std::string& text) {
  runtime.Diagnostic().EmitWarning(TaskName(base, dir), value::String(text));
}

void Error(
    RuntimeEffects& runtime, unsigned base, Direction dir,
    const std::string& text) {
  runtime.Diagnostic().EmitError(TaskName(base, dir), value::String(text));
}

// Reads the whole named file, or emits a "cannot open" error and returns
// nullopt. A missing input file is not fatal (LRM 21.4).
auto SlurpFile(
    RuntimeEffects& runtime, const value::String& filename, unsigned base)
    -> std::optional<std::string> {
  std::ifstream in{std::string{filename.View()}};
  if (!in.is_open()) {
    Error(
        runtime, base, Direction::kLoad,
        std::format("cannot open file '{}'", std::string{filename.View()}));
    return std::nullopt;
  }
  std::ostringstream contents;
  contents << in.rdbuf();
  return contents.str();
}

// Opens the named file for writing (truncating any existing content, LRM 21.5),
// or emits a "cannot open" error and returns nullopt.
auto CreateFile(
    RuntimeEffects& runtime, const value::String& filename, unsigned base)
    -> std::optional<std::ofstream> {
  std::ofstream out{std::string{filename.View()}};
  if (!out.is_open()) {
    Error(
        runtime, base, Direction::kStore,
        std::format("cannot open file '{}'", std::string{filename.View()}));
    return std::nullopt;
  }
  return out;
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

// Parses an `@hexaddr` directive (the leading `@` assumed) to its numeric
// value, or emits a malformed-address error and returns nullopt.
auto ParseAtAddress(
    RuntimeEffects& runtime, unsigned base, const std::string& token)
    -> std::optional<std::int64_t> {
  const std::optional<value::LongInt> address =
      ParsedMemoryWord<value::LongInt>(std::string_view{token}.substr(1), 16U);
  if (!address) {
    Error(
        runtime, base, Direction::kLoad,
        std::format("malformed address '{}'", token));
    return std::nullopt;
  }
  return address->ToInt64();
}

void MalformedWord(
    RuntimeEffects& runtime, unsigned base, const std::string& token) {
  Error(
      runtime, base, Direction::kLoad,
      std::format("malformed data word '{}'", token));
}

}  // namespace

auto RenderedMemoryWord(const value::ConstIntegralView& word, unsigned base)
    -> std::string {
  value::FormatSpec spec;
  spec.kind = base == 2U ? value::FormatKind::kBinary : value::FormatKind::kHex;
  return value::FormatIntegralOperand(spec, word, {});
}

void ReadMemKeyedCore(
    RuntimeEffects& runtime, const value::String& filename, unsigned base,
    std::optional<std::int64_t> start, std::optional<std::int64_t> finish,
    const std::function<bool(std::int64_t key, std::string_view token)>&
        store) {
  const auto text = SlurpFile(runtime, filename, base);
  if (!text) return;

  const bool bounded = start.has_value() && finish.has_value();
  const std::int64_t active_lo = bounded ? std::min(*start, *finish) : 0;
  const std::int64_t active_hi = bounded ? std::max(*start, *finish) : 0;
  std::int64_t cursor = start.value_or(0);
  const std::int64_t step = (bounded && *start > *finish) ? -1 : 1;

  for (const std::string& token : Tokenize(*text)) {
    if (token.front() == '@') {
      const auto a = ParseAtAddress(runtime, base, token);
      if (!a) return;
      if (bounded && (*a < active_lo || *a > active_hi)) {
        Error(
            runtime, base, Direction::kLoad,
            std::format("address {} is outside the load range", *a));
        return;
      }
      cursor = *a;
      continue;
    }

    if (bounded && (cursor < active_lo || cursor > active_hi)) break;

    if (!store(cursor, token)) {
      MalformedWord(runtime, base, token);
      return;
    }
    cursor += step;
  }
}

void WriteMemKeyedCore(
    RuntimeEffects& runtime, const value::String& filename, unsigned base,
    std::optional<std::int64_t> start, std::optional<std::int64_t> finish,
    std::span<const RenderedMemoryEntry> entries) {
  auto out = CreateFile(runtime, filename, base);
  if (!out) return;

  const bool bounded = start.has_value() && finish.has_value();
  const std::int64_t lo = bounded ? std::min(*start, *finish) : 0;
  const std::int64_t hi = bounded ? std::max(*start, *finish) : 0;
  for (const RenderedMemoryEntry& entry : entries) {
    if (bounded && (entry.key < lo || entry.key > hi)) continue;
    *out << '@' << entry.key_text << '\n' << entry.word_text << '\n';
  }
}

void ReadMemGridCore(
    RuntimeEffects& runtime, const value::String& filename, unsigned base,
    std::int64_t top_lo, std::int64_t top_hi, std::size_t inner_count,
    std::optional<std::int64_t> start, std::optional<std::int64_t> finish,
    const StoreMemoryWord& store) {
  const auto text = SlurpFile(runtime, filename, base);
  if (!text) return;

  // The active window and fill direction follow LRM 21.4 from which optional
  // addresses the call supplied; the addresses index the highest dimension.
  std::int64_t active_lo = top_lo;
  std::int64_t active_hi = top_hi;
  std::int64_t cursor = top_lo;
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

  bool saw_address = false;
  std::int64_t words = 0;
  std::size_t inner = 0;
  for (const std::string& token : Tokenize(*text)) {
    if (token.front() == '@') {
      const auto a = ParseAtAddress(runtime, base, token);
      if (!a) return;
      if (*a < active_lo || *a > active_hi) {
        Error(
            runtime, base, Direction::kLoad,
            std::format("address {} is outside the load range", *a));
        return;
      }
      cursor = *a;
      inner = 0;
      saw_address = true;
      continue;
    }

    if (cursor < active_lo || cursor > active_hi) break;

    if (!store(cursor, inner, token)) {
      MalformedWord(runtime, base, token);
      return;
    }
    ++words;
    if (++inner == inner_count) {
      cursor += step;
      inner = 0;
    }
  }

  // LRM 21.4: a start/finish range with no in-file addresses must be filled
  // exactly; a word-count mismatch is a warning, not an error. The range spans
  // its highest-dimension words times the leaves each expands to.
  if (start.has_value() && finish.has_value() && !saw_address) {
    const std::int64_t expected =
        (active_hi - active_lo + 1) * static_cast<std::int64_t>(inner_count);
    if (words != expected) {
      Warn(
          runtime, base, Direction::kLoad,
          std::format(
              "file holds {} words but the address range spans {}", words,
              expected));
    }
  }
}

void WriteMemGridCore(
    RuntimeEffects& runtime, const value::String& filename, unsigned base,
    std::int64_t top_lo, std::int64_t top_hi, std::size_t inner_count,
    std::optional<std::int64_t> start, std::optional<std::int64_t> finish,
    const RenderMemoryWord& rendered) {
  auto out = CreateFile(runtime, filename, base);
  if (!out) return;

  std::int64_t cursor = top_lo;
  std::int64_t last = top_hi;
  std::int64_t step = 1;
  if (start.has_value() && finish.has_value()) {
    cursor = *start;
    last = *finish;
    step = (*start <= *finish) ? 1 : -1;
  } else if (start.has_value()) {
    cursor = *start;
  }

  for (std::int64_t top = cursor;; top += step) {
    if (top < top_lo || top > top_hi) break;
    for (std::size_t i = 0; i < inner_count; ++i) {
      *out << rendered(top, i) << '\n';
    }
    if (top == last) break;
  }
}

}  // namespace lyra::runtime
