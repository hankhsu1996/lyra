#include "lyra/runtime/mem_file.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <fstream>
#include <functional>
#include <optional>
#include <span>
#include <sstream>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "lyra/base/diag_message.hpp"
#include "lyra/runtime/diagnostic.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/value/format.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/string.hpp"

namespace lyra::runtime {

namespace {

enum class Direction : std::uint8_t { kLoad, kStore };

auto TaskName(unsigned base, Direction dir) -> std::string_view {
  const bool store = dir == Direction::kStore;
  if (base == 2U) {
    return store ? "$writememb" : "$readmemb";
  }
  return store ? "$writememh" : "$readmemh";
}

// A file the task could not read or write is the design's to answer for, and
// the run goes on (LRM 21.4), so the report is an error or a warning made where
// the calling procedure is written, never a fatal one.
template <typename... Args>
void Warn(
    RuntimeEffects& runtime, base::DiagFormatFor<Args...> format,
    Args&&... args) {
  runtime.Diagnostic().Report(
      Severity::kWarning,
      base::DiagMessage(format, std::forward<Args>(args)...));
}

template <typename... Args>
void Error(
    RuntimeEffects& runtime, base::DiagFormatFor<Args...> format,
    Args&&... args) {
  runtime.Diagnostic().Report(
      Severity::kError, base::DiagMessage(format, std::forward<Args>(args)...));
}

// Reads the whole named file, or emits a "cannot open" error and returns
// nullopt. A missing input file is not fatal (LRM 21.4).
auto SlurpFile(
    RuntimeEffects& runtime, const value::String& filename, unsigned base)
    -> std::optional<std::string> {
  std::ifstream in{std::string{filename.View()}};
  if (!in.is_open()) {
    Error(
        runtime, "'{}' cannot open the file '{}'",
        TaskName(base, Direction::kLoad), filename.View());
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
        runtime, "'{}' cannot open the file '{}'",
        TaskName(base, Direction::kStore), filename.View());
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
  value::LongInt address;
  if (!value::FromDigits(
          address, value::DigitRadix::kHex,
          std::string_view{token}.substr(1))) {
    Error(
        runtime, "'{}' read a malformed address '{}'",
        TaskName(base, Direction::kLoad), token);
    return std::nullopt;
  }
  return address.ToInt64();
}

void MalformedWord(
    RuntimeEffects& runtime, unsigned base, const std::string& token) {
  Error(
      runtime, "'{}' read a malformed data word '{}'",
      TaskName(base, Direction::kLoad), token);
}

// Whether a token of a memory file is an `@address` rather than a data word
// (LRM 21.4).
auto IsAddress(std::string_view token) -> bool {
  return token.front() == '@';
}

// The addresses a call names, from the first toward the last (LRM 21.4,
// 21.5): the memory's own from its lowest where it names none, and from a
// start alone upward toward the memory's highest.
struct AddressRun {
  std::int64_t first;
  std::int64_t last;
};

auto AddressRunOf(
    std::int64_t top_lo, std::int64_t top_hi, std::optional<std::int64_t> start,
    std::optional<std::int64_t> finish) -> AddressRun {
  if (start.has_value() && finish.has_value()) {
    return {.first = *start, .last = *finish};
  }
  if (start.has_value()) {
    return {.first = *start, .last = std::max(*start, top_hi)};
  }
  return {.first = top_lo, .last = top_hi};
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
    if (IsAddress(token)) {
      const auto a = ParseAtAddress(runtime, base, token);
      if (!a) return;
      if (bounded && (*a < active_lo || *a > active_hi)) {
        Error(
            runtime, "'{}' read address {}, which is outside the load range",
            TaskName(base, Direction::kLoad), *a);
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

  // The addresses index the highest dimension.
  const AddressRun run = AddressRunOf(top_lo, top_hi, start, finish);
  const std::int64_t active_lo = std::min(run.first, run.last);
  const std::int64_t active_hi = std::max(run.first, run.last);
  const std::int64_t step = run.first <= run.last ? 1 : -1;
  std::int64_t cursor = run.first;

  const std::vector<std::string> tokens = Tokenize(*text);
  std::size_t inner = 0;
  for (const std::string& token : tokens) {
    if (IsAddress(token)) {
      const auto a = ParseAtAddress(runtime, base, token);
      if (!a) return;
      if (*a < active_lo || *a > active_hi) {
        Error(
            runtime, "'{}' read address {}, which is outside the load range",
            TaskName(base, Direction::kLoad), *a);
        return;
      }
      cursor = *a;
      inner = 0;
      continue;
    }

    if (cursor < active_lo || cursor > active_hi) break;

    // An address is an index into the memory (LRM 21.4), and a write at one
    // the memory does not hold performs no operation (LRM 7.4.5), so a range
    // reaching past the memory loads what lies inside it and still consumes
    // the words the file gives the rest.
    if (cursor >= top_lo && cursor <= top_hi && !store(cursor, inner, token)) {
      MalformedWord(runtime, base, token);
      return;
    }
    if (++inner == inner_count) {
      cursor += step;
      inner = 0;
    }
  }

  // LRM 21.4: a start/finish range with no in-file addresses must be filled
  // exactly; a word-count mismatch is a warning, not an error. The comparison
  // is between the file and the range, so every data word the file holds
  // counts, loaded or not. The range spans its highest-dimension words times
  // the leaves each expands to.
  if (start.has_value() && finish.has_value() &&
      std::ranges::none_of(tokens, IsAddress)) {
    const auto words = static_cast<std::int64_t>(tokens.size());
    const std::int64_t expected =
        (active_hi - active_lo + 1) * static_cast<std::int64_t>(inner_count);
    if (words != expected) {
      Warn(
          runtime,
          "'{}' read a file holding {} words, while the address range "
          "spans {}",
          TaskName(base, Direction::kLoad), words, expected);
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

  // Only the addresses the memory holds are written, since an address is an
  // index into it and a read at one it does not hold names no element (LRM
  // 7.4.5).
  const AddressRun run = AddressRunOf(top_lo, top_hi, start, finish);
  const std::int64_t lowest = std::max(std::min(run.first, run.last), top_lo);
  const std::int64_t highest = std::min(std::max(run.first, run.last), top_hi);
  const std::int64_t step = run.first <= run.last ? 1 : -1;
  const std::int64_t from = step > 0 ? lowest : highest;
  for (std::int64_t i = 0; i <= highest - lowest; ++i) {
    const std::int64_t top = from + (step * i);
    for (std::size_t ordinal = 0; ordinal < inner_count; ++ordinal) {
      *out << rendered(top, ordinal) << '\n';
    }
  }
}

}  // namespace lyra::runtime
