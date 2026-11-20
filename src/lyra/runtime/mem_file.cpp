#include "lyra/runtime/mem_file.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <fstream>
#include <optional>
#include <span>
#include <sstream>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/diag_message.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/runtime/diagnostic.hpp"
#include "lyra/runtime/file_table.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/value/enumeration.hpp"
#include "lyra/value/format.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/string.hpp"
#include "lyra/value/unpacked_range.hpp"

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
// the run goes on (LRM 21.4), so the report is an error or a warning made at
// the call of the task, never a fatal one.
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
  const std::string_view digits = std::string_view{token}.substr(1);
  if (!value::IsDigits(value::DigitRadix::kHex, digits)) {
    Error(
        runtime, "'{}' read a malformed address '{}'",
        TaskName(base, Direction::kLoad), token);
    return std::nullopt;
  }
  value::LongInt address;
  value::FromDigits(address, value::DigitRadix::kHex, digits);
  return address.ToInt64();
}

// LRM 21.4.2: whether the number a token writes is the value of a member. It
// is the number the file writes, read at the width its digits take, so a digit
// a word of the type would lose still counts; over a 2-state base an x or z
// digit is 0.
auto IsMemberNumber(
    const value::Enumeration& members, std::string_view token, unsigned base)
    -> bool {
  const value::DigitRadix radix = MemoryRadix(base);
  const auto digits = static_cast<std::uint64_t>(
      std::ranges::count_if(token, [](char c) { return c != '_'; }));
  const value::StateDomain domain = members.four_state
                                        ? value::StateDomain::kFourState
                                        : value::StateDomain::kTwoState;
  const value::IntegralShape written{
      .width = std::max(digits * value::BitsPerDigit(radix), members.width),
      .domain = domain};
  value::WrittenWords number(written);
  value::FromDigits(number.MutableView(), radix, token);
  value::WrittenWords word({.width = members.width, .domain = domain});
  value::Convert(word.MutableView(), number.View());
  value::WrittenWords kept(written);
  value::Convert(kept.MutableView(), word.View());
  return value::CaseEqual(kept.View(), number.View()) &&
         members.PositionOf(word.View()).has_value();
}

// Whether a token is a word of a memory whose words take `values`, reporting
// why not where it is not; a load reads nothing after such a token (LRM 21.4,
// 21.4.2).
auto AdmitWord(
    RuntimeEffects& runtime, const WordValues& values, unsigned base,
    const std::string& token) -> bool {
  if (!value::IsDigits(MemoryRadix(base), token)) {
    Error(
        runtime, "'{}' read a malformed data word '{}'",
        TaskName(base, Direction::kLoad), token);
    return false;
  }
  return std::visit(
      Overloaded{
          [](const AnyNumber&) { return true; },
          [&](const MemberValues& only) {
            if (IsMemberNumber(only.members, token, base)) {
              return true;
            }
            Error(
                runtime,
                "'{}' read the word '{}', which is no member of the memory's "
                "enumeration",
                TaskName(base, Direction::kLoad), token);
            return false;
          }},
      values);
}

// Whether a token of a memory file is an `@address` rather than a data word
// (LRM 21.4).
auto IsAddress(std::string_view token) -> bool {
  return token.front() == '@';
}

// The addresses a call names, from the first toward the last (LRM 21.4,
// 21.5): from the start the call names, or the memory's lowest, upward toward
// the memory's highest, or toward the finish a window names.
struct AddressRun {
  std::int64_t first;
  std::int64_t last;
};

auto AddressRunOf(
    std::int64_t top_hi, std::int64_t start, std::optional<std::int64_t> finish)
    -> AddressRun {
  if (finish.has_value()) {
    return {.first = start, .last = *finish};
  }
  return {.first = start, .last = std::max(start, top_hi)};
}

}  // namespace

auto RenderedMemoryWord(const value::ConstIntegralView& word, unsigned base)
    -> std::string {
  value::FormatSpec spec;
  spec.kind = base == 2U ? value::FormatKind::kBinary : value::FormatKind::kHex;
  return value::FormatIntegralOperand(spec, word, nullptr);
}

// What one task asked for and the words it moves.
struct MemoryWords::State {
  // Where the words come from and go: the file of radix-`base` words a memory
  // task names (LRM 21.4, 21.5), or the next `count` words of `width` bits the
  // open file `fd` holds, as `$fread` reads them (LRM 21.3.4.4), which answers
  // how many bytes it read.
  struct TextFile {
    RuntimeEffects* runtime;
    value::String filename;
    unsigned base;
  };
  struct OpenFile {
    FileTable* files;
    std::int64_t fd;
    std::int64_t count;
    std::uint64_t width;
    std::int64_t bytes_read = 0;
  };

  WordValues values;
  std::variant<TextFile, OpenFile> source;
  std::vector<value::UnpackedRange> dims;
  std::int64_t start;
  std::optional<std::int64_t> finish;

  // One word of a task: the address it names, which leaf of that address it is
  // where an address expands to several, and its text -- the token a load
  // read, the bytes `$fread` read, or the word a dump writes.
  struct Text {
    std::int64_t address;
    std::size_t ordinal;
    std::string text;
  };
  std::vector<Text> words;
  bool keyed = false;

  // The file of words a memory task names, which a dump and a load by key
  // always have.
  [[nodiscard]] auto Named() -> TextFile& {
    auto* text = std::get_if<TextFile>(&source);
    if (text == nullptr) {
      throw InternalError(
          "memory words: a task naming a file of words is asked of the words "
          "$fread read -- please report this as a bug");
    }
    return *text;
  }

  // The grid's words, `top_lo` to `top_hi` highest-dimension addresses each
  // expanding to `inner_count` leaves.
  void ReadGrid(
      std::int64_t top_lo, std::int64_t top_hi, std::size_t inner_count);
  void ReadText(
      TextFile& file, std::int64_t top_lo, std::int64_t top_hi,
      std::size_t inner_count);
  void ReadBytes(OpenFile& file, std::int64_t top_lo, std::int64_t top_hi);
  void PlanGrid(
      std::int64_t top_lo, std::int64_t top_hi, std::size_t inner_count);
};

MemoryWords::MemoryWords(
    RuntimeEffects& runtime, WordValues values, const value::String& filename,
    std::span<const std::int64_t> bounds, std::int64_t base, std::int64_t start,
    std::optional<std::int64_t> finish)
    : state_(
          std::make_unique<State>(State{
              .values = std::move(values),
              .source =
                  State::TextFile{
                      .runtime = &runtime,
                      .filename = filename,
                      .base = static_cast<unsigned>(base)},
              .dims = value::UnpackedRangesOf(bounds),
              .start = start,
              .finish = finish,
              .words = {}})) {
}

MemoryWords::MemoryWords(
    FileTable& files, std::int64_t fd, std::span<const std::int64_t> bounds,
    std::int64_t start, std::int64_t count, std::int64_t width)
    : state_(
          std::make_unique<State>(State{
              .values = AnyNumber{},
              .source =
                  State::OpenFile{
                      .files = &files,
                      .fd = fd,
                      .count = count,
                      .width = static_cast<std::uint64_t>(width)},
              .dims = value::UnpackedRangesOf(bounds),
              .start = start,
              .finish = std::nullopt,
              .words = {}})) {
}
MemoryWords::MemoryWords(MemoryWords&&) noexcept = default;
auto MemoryWords::operator=(MemoryWords&&) noexcept -> MemoryWords& = default;
MemoryWords::~MemoryWords() = default;

void MemoryWords::ReadGrid(base::SourceLocation at) {
  const base::SourceLocationScope bound(at);
  const std::span<const value::UnpackedRange> dims = state_->dims;
  state_->ReadGrid(
      dims.front().Low(), dims.front().High(),
      value::ElementsSpanned(dims.subspan(1)));
}

void MemoryWords::ReadDense(std::int64_t count, base::SourceLocation at) {
  const base::SourceLocationScope bound(at);
  state_->ReadGrid(0, count - 1, 1);
}

void MemoryWords::PlanGrid() {
  const std::span<const value::UnpackedRange> dims = state_->dims;
  state_->PlanGrid(
      dims.front().Low(), dims.front().High(),
      value::ElementsSpanned(dims.subspan(1)));
}

void MemoryWords::PlanDense(std::int64_t count) {
  state_->PlanGrid(0, count - 1, 1);
}

auto MemoryWords::Count() const -> std::int64_t {
  return static_cast<std::int64_t>(state_->words.size());
}

auto MemoryWords::Ordinal(std::int64_t k, std::int64_t dimension) const
    -> std::int64_t {
  const State::Text& word = state_->words.at(static_cast<std::size_t>(k));
  const std::span<const value::UnpackedRange> dims = state_->dims;
  if (dims.empty()) {
    return word.address;
  }
  const auto d = static_cast<std::size_t>(dimension);
  const std::int64_t address =
      d == 0 ? word.address
             : dims[d].Low() +
                   static_cast<std::int64_t>(
                       value::AscendingPositionIn(dims, d, word.ordinal));
  return dims[d].ToOrdinal(address);
}

auto MemoryWords::Key(std::int64_t k) const -> std::int64_t {
  return state_->words.at(static_cast<std::size_t>(k)).address;
}

auto MemoryWords::BytesRead() const -> std::int64_t {
  const auto* file = std::get_if<State::OpenFile>(&state_->source);
  return file == nullptr ? 0 : file->bytes_read;
}

void MemoryWords::Word(std::int64_t k, const value::IntegralView& out) const {
  const std::string& text = state_->words.at(static_cast<std::size_t>(k)).text;
  std::visit(
      Overloaded{
          [&](const State::TextFile& file) {
            value::FromDigits(out, MemoryRadix(file.base), text);
          },
          // LRM 21.3.4.4: the bytes most significant first, a word the file
          // ended inside holding zeros in the bytes it lacked.
          [&](const State::OpenFile&) { ReadBigEndian(out, text); }},
      state_->source);
}

void MemoryWords::Put(std::int64_t k, const value::ConstIntegralView& word) {
  state_->words.at(static_cast<std::size_t>(k)).text =
      RenderedMemoryWord(word, state_->Named().base);
}

void MemoryWords::PutKeyed(
    std::int64_t key, const value::ConstIntegralView& word) {
  state_->keyed = true;
  state_->words.push_back(
      State::Text{
          .address = key,
          .ordinal = 0,
          .text = RenderedMemoryWord(word, state_->Named().base)});
}

void MemoryWords::Commit(base::SourceLocation at) {
  const base::SourceLocationScope bound(at);
  State& state = *state_;
  const State::TextFile& named = state.Named();
  auto out = CreateFile(*named.runtime, named.filename, named.base);
  if (!out) return;
  // An associative memory's entries are written each as an `@key` line before
  // its word, in key order, so a sparse array round-trips through a load (LRM
  // 21.5.3); a window bounds the keys written.
  const std::int64_t lo =
      state.finish.has_value() ? std::min(state.start, *state.finish) : 0;
  const std::int64_t hi =
      state.finish.has_value() ? std::max(state.start, *state.finish) : 0;
  for (const State::Text& word : state.words) {
    if (state.keyed) {
      if (state.finish.has_value() && (word.address < lo || word.address > hi))
        continue;
      value::WrittenWords key(
          {.width = 64, .domain = value::StateDomain::kTwoState});
      value::FromInt(key.MutableView(), word.address);
      *out << '@' << RenderedMemoryWord(key.View(), 16U) << '\n';
    }
    *out << word.text << '\n';
  }
}

void MemoryWords::ReadKeyed(base::SourceLocation at) {
  const base::SourceLocationScope bound(at);
  State& state = *state_;
  const State::TextFile& named = state.Named();
  RuntimeEffects& runtime = *named.runtime;
  const unsigned base = named.base;
  const auto text = SlurpFile(runtime, named.filename, base);
  if (!text) return;

  const bool bounded = state.finish.has_value();
  const std::int64_t active_lo =
      bounded ? std::min(state.start, *state.finish) : 0;
  const std::int64_t active_hi =
      bounded ? std::max(state.start, *state.finish) : 0;
  std::int64_t cursor = state.start;
  const std::int64_t step = (bounded && state.start > *state.finish) ? -1 : 1;

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

    if (!AdmitWord(runtime, state.values, base, token)) return;
    state.words.push_back(
        State::Text{.address = cursor, .ordinal = 0, .text = token});
    cursor += step;
  }
}

void MemoryWords::State::ReadGrid(
    std::int64_t top_lo, std::int64_t top_hi, std::size_t inner_count) {
  std::visit(
      Overloaded{
          [&](TextFile& text) { ReadText(text, top_lo, top_hi, inner_count); },
          [&](OpenFile& file) {
            // `$fread` reads a memory of one dimension (LRM 21.3.4.4), whose
            // every address is one word.
            if (inner_count != 1) {
              throw InternalError(
                  "memory words: $fread reads a memory of more than one "
                  "dimension -- please report this as a bug");
            }
            ReadBytes(file, top_lo, top_hi);
          }},
      source);
}

void MemoryWords::State::ReadBytes(
    OpenFile& file, std::int64_t top_lo, std::int64_t top_hi) {
  file.bytes_read = file.files->ReadMemoryWords(
      file.fd, file.width,
      value::UnpackedRange{.left = top_lo, .right = top_hi}, start, file.count,
      [&](std::int64_t address, std::span<const char> bytes) {
        words.push_back(
            Text{
                .address = address,
                .ordinal = 0,
                .text = std::string(bytes.begin(), bytes.end())});
      });
}

void MemoryWords::State::ReadText(
    TextFile& file, std::int64_t top_lo, std::int64_t top_hi,
    std::size_t inner_count) {
  RuntimeEffects* runtime = file.runtime;
  const unsigned base = file.base;
  const auto text = SlurpFile(*runtime, file.filename, base);
  if (!text) return;

  // The addresses index the highest dimension.
  const AddressRun run = AddressRunOf(top_hi, start, finish);
  const std::int64_t active_lo = std::min(run.first, run.last);
  const std::int64_t active_hi = std::max(run.first, run.last);
  const std::int64_t step = run.first <= run.last ? 1 : -1;
  std::int64_t cursor = run.first;

  const std::vector<std::string> tokens = Tokenize(*text);
  std::size_t inner = 0;
  bool reported_outside = false;
  for (const std::string& token : tokens) {
    if (IsAddress(token)) {
      const auto a = ParseAtAddress(*runtime, base, token);
      if (!a) return;
      if (*a < active_lo || *a > active_hi) {
        Error(
            *runtime, "'{}' read address {}, which is outside the load range",
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
    // reaching past the memory loads what lies inside it and still reads the
    // words the file gives the rest. That clause lets a tool warn, and a load
    // is told once, at its first word stored nowhere: a file addressing bytes
    // of a memory of wider rows loses all of it this way (LRM 21.4.3).
    if (!AdmitWord(*runtime, values, base, token)) return;
    if (cursor >= top_lo && cursor <= top_hi) {
      words.push_back(Text{.address = cursor, .ordinal = inner, .text = token});
    } else if (!reported_outside) {
      Warn(
          *runtime,
          "'{}' read a word for address {}, which the memory does not hold; "
          "no word for an address outside {} to {} is stored",
          TaskName(base, Direction::kLoad), cursor, top_lo, top_hi);
      reported_outside = true;
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
  if (finish.has_value() && std::ranges::none_of(tokens, IsAddress)) {
    const auto held = static_cast<std::int64_t>(tokens.size());
    const std::int64_t expected =
        (active_hi - active_lo + 1) * static_cast<std::int64_t>(inner_count);
    if (held != expected) {
      Warn(
          *runtime,
          "'{}' read a file holding {} words, while the address range "
          "spans {}",
          TaskName(base, Direction::kLoad), held, expected);
    }
  }
}

void MemoryWords::State::PlanGrid(
    std::int64_t top_lo, std::int64_t top_hi, std::size_t inner_count) {
  // Only the addresses the memory holds are written, since an address is an
  // index into it and a read at one it does not hold names no element (LRM
  // 7.4.5).
  const AddressRun run = AddressRunOf(top_hi, start, finish);
  const std::int64_t lowest = std::max(std::min(run.first, run.last), top_lo);
  const std::int64_t highest = std::min(std::max(run.first, run.last), top_hi);
  const std::int64_t step = run.first <= run.last ? 1 : -1;
  const std::int64_t from = step > 0 ? lowest : highest;
  for (std::int64_t i = 0; i <= highest - lowest; ++i) {
    const std::int64_t top = from + (step * i);
    for (std::size_t ordinal = 0; ordinal < inner_count; ++ordinal) {
      words.push_back(Text{.address = top, .ordinal = ordinal, .text = {}});
    }
  }
}

auto ReadMem(
    RuntimeEffects& runtime, const value::String& filename,
    std::span<const std::int64_t> bounds, std::int64_t base, std::int64_t start)
    -> MemoryWords {
  return {runtime, AnyNumber{}, filename, bounds, base, start, std::nullopt};
}
auto ReadMemWithin(
    RuntimeEffects& runtime, const value::String& filename,
    std::span<const std::int64_t> bounds, std::int64_t base, std::int64_t start,
    std::int64_t finish) -> MemoryWords {
  return {runtime, AnyNumber{}, filename, bounds, base, start, finish};
}
auto ReadMem(
    RuntimeEffects& runtime, const value::Enumeration& members,
    const value::String& filename, std::span<const std::int64_t> bounds,
    std::int64_t base, std::int64_t start) -> MemoryWords {
  return {runtime, MemberValues{members}, filename, bounds, base,
          start,   std::nullopt};
}
auto ReadMemWithin(
    RuntimeEffects& runtime, const value::Enumeration& members,
    const value::String& filename, std::span<const std::int64_t> bounds,
    std::int64_t base, std::int64_t start, std::int64_t finish) -> MemoryWords {
  return {runtime, MemberValues{members}, filename, bounds, base, start,
          finish};
}
auto WriteMem(
    RuntimeEffects& runtime, const value::String& filename,
    std::span<const std::int64_t> bounds, std::int64_t base, std::int64_t start)
    -> MemoryWords {
  return {runtime, AnyNumber{}, filename, bounds, base, start, std::nullopt};
}
auto WriteMemWithin(
    RuntimeEffects& runtime, const value::String& filename,
    std::span<const std::int64_t> bounds, std::int64_t base, std::int64_t start,
    std::int64_t finish) -> MemoryWords {
  return {runtime, AnyNumber{}, filename, bounds, base, start, finish};
}

}  // namespace lyra::runtime
