#include "lyra/runtime/mem_file.hpp"

#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdint>
#include <format>
#include <fstream>
#include <functional>
#include <optional>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

#include "lyra/runtime/diagnostic.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/value/associative_array.hpp"
#include "lyra/value/format.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/packed_type.hpp"
#include "lyra/value/string.hpp"
#include "lyra/value/unpacked_array.hpp"

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
  const auto addr = value::PackedArray::FromDigits(
      std::string_view{token}.substr(1), 16U, 64U, true, false);
  if (!addr) {
    Error(
        runtime, base, Direction::kLoad,
        std::format("malformed address '{}'", token));
    return std::nullopt;
  }
  return addr->ToInt64();
}

// Parses one data word at `base` into a value of `shape`'s width / signedness /
// state, or emits a malformed-word error and returns nullopt.
auto ParseWord(
    RuntimeEffects& runtime, unsigned base, const std::string& token,
    const value::PackedArray& shape) -> std::optional<value::PackedArray> {
  auto elem = value::PackedArray::FromDigits(
      token, base, shape.BitWidth(), shape.IsSigned(), shape.IsFourState());
  if (!elem) {
    Error(
        runtime, base, Direction::kLoad,
        std::format("malformed data word '{}'", token));
    return std::nullopt;
  }
  return elem;
}

// Renders one element as a full-width radix-`base` word: the %h / %b display of
// the packed value, so a 4-state x / z survives per the display rules that
// $readmem{h,b} reads back (LRM 21.5.1, 21.4.1).
auto RenderWord(const value::PackedArray& elem, unsigned base) -> std::string {
  value::FormatSpec spec;
  spec.kind = base == 2U ? value::FormatKind::kBinary : value::FormatKind::kHex;
  return value::Formatter<value::PackedArray>::Format(spec, elem, {});
}

// A dynamic array or queue is a 0-based memory whose address range is
// `[0, size-1]` (LRM 21.4.1: the current size is fixed, not resized by the
// load). Both containers expose the same ordinal element API, so one template
// serves both.
template <typename Container>
void ReadMemZeroBased(
    RuntimeEffects& runtime, Container& dest, const value::String& filename,
    unsigned base, std::optional<std::int64_t> start,
    std::optional<std::int64_t> finish) {
  ReadMemGridCore(
      runtime, filename, base, 0, static_cast<std::int64_t>(dest.RawSize()) - 1,
      1, start, finish,
      [&](std::int64_t a, std::size_t) -> value::PackedArray& {
        return dest.ElementRef(
            value::PackedArray::Int(static_cast<std::int32_t>(a)));
      });
}

template <typename Container>
void WriteMemZeroBased(
    RuntimeEffects& runtime, const Container& src,
    const value::String& filename, unsigned base,
    std::optional<std::int64_t> start, std::optional<std::int64_t> finish) {
  WriteMemGridCore(
      runtime, filename, base, 0, static_cast<std::int64_t>(src.RawSize()) - 1,
      1, start, finish,
      [&](std::int64_t a, std::size_t) -> const value::PackedArray& {
        return src.RawAt(static_cast<std::size_t>(a));
      });
}

using AssocMem =
    value::AssociativeArray<value::PackedArray, value::PackedArray>;

// Builds an associative key from a numeric address, in the shape of the key
// prototype. The key must carry the array's declared index width / signedness
// so it compares equal to the key an ordinary `mem[i]` access builds (the map's
// ordering compares keys of one type, all of the same width).
auto AssocKey(std::int64_t address, const value::PackedArray& key_prototype)
    -> value::PackedArray {
  return value::PackedArray::FromInt(
      address,
      value::PackedType{
          std::array{value::PackedRange{
              .left = static_cast<std::int64_t>(key_prototype.BitWidth()) - 1,
              .right = 0}},
          key_prototype.IsSigned(), key_prototype.IsFourState()});
}

// LRM 21.4.1 associative load. Addressing is by key: an `@key` sets the cursor,
// and consecutive words advance it. A word writes `ElementRef(key)`, which
// creates the entry (seeded with the element prototype, so its shape drives the
// parse) when absent. `start` / `finish` bound the key range; without them the
// keys come entirely from the file.
void ReadMemAssoc(
    RuntimeEffects& runtime, AssocMem& dest, const value::String& filename,
    const value::PackedArray& key_prototype, unsigned base,
    std::optional<std::int64_t> start, std::optional<std::int64_t> finish) {
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

    value::PackedArray& cell = dest.ElementRef(AssocKey(cursor, key_prototype));
    const auto elem = ParseWord(runtime, base, token, cell);
    if (!elem) return;
    cell = *elem;
    cursor += step;
  }
}

// LRM 21.5.3 associative dump. Entries are written in ascending key order (the
// map keeps them sorted), each as an `@key` line followed by the word, so a
// sparse array round-trips through `$readmem`. `start` / `finish` bound the key
// range when supplied.
void WriteMemAssoc(
    RuntimeEffects& runtime, const AssocMem& src, const value::String& filename,
    unsigned base, std::optional<std::int64_t> start,
    std::optional<std::int64_t> finish) {
  auto out = CreateFile(runtime, filename, base);
  if (!out) return;

  const bool bounded = start.has_value() && finish.has_value();
  const std::int64_t lo = bounded ? std::min(*start, *finish) : 0;
  const std::int64_t hi = bounded ? std::max(*start, *finish) : 0;
  src.ForEachEntry(
      [&](const value::PackedArray& key, const value::PackedArray& value) {
        const std::int64_t k = key.ToInt64();
        if (bounded && (k < lo || k > hi)) return;
        *out << '@' << RenderWord(key, 16U) << '\n'
             << RenderWord(value, base) << '\n';
      });
}

}  // namespace

void ReadMemGridCore(
    RuntimeEffects& runtime, const value::String& filename, unsigned base,
    std::int64_t top_lo, std::int64_t top_hi, std::size_t inner_count,
    std::optional<std::int64_t> start, std::optional<std::int64_t> finish,
    const std::function<value::PackedArray&(std::int64_t, std::size_t)>&
        leaf_at) {
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

    value::PackedArray& leaf = leaf_at(cursor, inner);
    const auto elem = ParseWord(runtime, base, token, leaf);
    if (!elem) return;
    leaf = *elem;
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
    const std::function<const value::PackedArray&(std::int64_t, std::size_t)>&
        leaf_get) {
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
      *out << RenderWord(leaf_get(top, i), base) << '\n';
    }
    if (top == last) break;
  }
}

auto ReadMem(
    RuntimeEffects& runtime, DynamicMemory dest, const value::String& filename,
    const value::PackedArray& base, const value::PackedArray& start)
    -> MemoryLoad<DynamicMemory> {
  ReadMemZeroBased(
      runtime, dest, filename, static_cast<unsigned>(base.ToInt64()),
      start.ToInt64(), std::nullopt);
  return MemoryLoad<DynamicMemory>{std::move(dest)};
}

auto ReadMemWithin(
    RuntimeEffects& runtime, DynamicMemory dest, const value::String& filename,
    const value::PackedArray& base, const value::PackedArray& start,
    const value::PackedArray& finish) -> MemoryLoad<DynamicMemory> {
  ReadMemZeroBased(
      runtime, dest, filename, static_cast<unsigned>(base.ToInt64()),
      start.ToInt64(), finish.ToInt64());
  return MemoryLoad<DynamicMemory>{std::move(dest)};
}

void WriteMem(
    RuntimeEffects& runtime, const DynamicMemory& src,
    const value::String& filename, const value::PackedArray& base,
    const value::PackedArray& start) {
  WriteMemZeroBased(
      runtime, src, filename, static_cast<unsigned>(base.ToInt64()),
      start.ToInt64(), std::nullopt);
}

void WriteMemWithin(
    RuntimeEffects& runtime, const DynamicMemory& src,
    const value::String& filename, const value::PackedArray& base,
    const value::PackedArray& start, const value::PackedArray& finish) {
  WriteMemZeroBased(
      runtime, src, filename, static_cast<unsigned>(base.ToInt64()),
      start.ToInt64(), finish.ToInt64());
}

auto ReadMem(
    RuntimeEffects& runtime, QueueMemory dest, const value::String& filename,
    const value::PackedArray& base, const value::PackedArray& start)
    -> MemoryLoad<QueueMemory> {
  ReadMemZeroBased(
      runtime, dest, filename, static_cast<unsigned>(base.ToInt64()),
      start.ToInt64(), std::nullopt);
  return MemoryLoad<QueueMemory>{std::move(dest)};
}

auto ReadMemWithin(
    RuntimeEffects& runtime, QueueMemory dest, const value::String& filename,
    const value::PackedArray& base, const value::PackedArray& start,
    const value::PackedArray& finish) -> MemoryLoad<QueueMemory> {
  ReadMemZeroBased(
      runtime, dest, filename, static_cast<unsigned>(base.ToInt64()),
      start.ToInt64(), finish.ToInt64());
  return MemoryLoad<QueueMemory>{std::move(dest)};
}

void WriteMem(
    RuntimeEffects& runtime, const QueueMemory& src,
    const value::String& filename, const value::PackedArray& base,
    const value::PackedArray& start) {
  WriteMemZeroBased(
      runtime, src, filename, static_cast<unsigned>(base.ToInt64()),
      start.ToInt64(), std::nullopt);
}

void WriteMemWithin(
    RuntimeEffects& runtime, const QueueMemory& src,
    const value::String& filename, const value::PackedArray& base,
    const value::PackedArray& start, const value::PackedArray& finish) {
  WriteMemZeroBased(
      runtime, src, filename, static_cast<unsigned>(base.ToInt64()),
      start.ToInt64(), finish.ToInt64());
}

auto ReadMem(
    RuntimeEffects& runtime, AssociativeMemory dest,
    const value::String& filename, const value::PackedArray& key_prototype,
    const value::PackedArray& base, const value::PackedArray& start)
    -> MemoryLoad<AssociativeMemory> {
  ReadMemAssoc(
      runtime, dest, filename, key_prototype,
      static_cast<unsigned>(base.ToInt64()), start.ToInt64(), std::nullopt);
  return MemoryLoad<AssociativeMemory>{std::move(dest)};
}

auto ReadMemWithin(
    RuntimeEffects& runtime, AssociativeMemory dest,
    const value::String& filename, const value::PackedArray& key_prototype,
    const value::PackedArray& base, const value::PackedArray& start,
    const value::PackedArray& finish) -> MemoryLoad<AssociativeMemory> {
  ReadMemAssoc(
      runtime, dest, filename, key_prototype,
      static_cast<unsigned>(base.ToInt64()), start.ToInt64(), finish.ToInt64());
  return MemoryLoad<AssociativeMemory>{std::move(dest)};
}

void WriteMem(
    RuntimeEffects& runtime, const AssociativeMemory& src,
    const value::String& filename, const value::PackedArray& base,
    const value::PackedArray& start) {
  WriteMemAssoc(
      runtime, src, filename, static_cast<unsigned>(base.ToInt64()),
      start.ToInt64(), std::nullopt);
}

void WriteMemWithin(
    RuntimeEffects& runtime, const AssociativeMemory& src,
    const value::String& filename, const value::PackedArray& base,
    const value::PackedArray& start, const value::PackedArray& finish) {
  WriteMemAssoc(
      runtime, src, filename, static_cast<unsigned>(base.ToInt64()),
      start.ToInt64(), finish.ToInt64());
}

}  // namespace lyra::runtime
