#pragma once

#include <cstdint>
#include <memory>
#include <optional>
#include <span>
#include <string>
#include <variant>

#include "lyra/base/source_location.hpp"
#include "lyra/value/enumeration.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/string.hpp"

namespace lyra::runtime {

class FileTable;
class RuntimeEffects;

// LRM 21.4 $readmemh / $readmemb load a memory from the text file a call
// names: whitespace-separated radix-`base` words (base 16 for $readmemh, 2 for
// $readmemb), optional `@hex` address directives, `//` and `/* */` comments,
// and per-digit x / z / ?. LRM 21.5 $writememh / $writememb dump a memory in a
// form the load reads back: one radix-`base` word per line, each written at
// full width with per-digit x / z, over any file already there.
//
// Addressing is one of two requests, and the caller says which. The plain form
// runs upward from `start`, which the caller makes the memory's lowest address
// where the source named none -- the clause's no-address form and its
// start-only form are the same run. The windowed form names a `finish` as
// well, which bounds the range an `@address` may reach, lets the run descend,
// and obliges the file to fill the whole window (LRM 21.4).
//
// What an address means is the memory's: an unpacked array of fixed size reads
// its declared bounds, a dynamic array or a queue is the 0-based dense space
// its current size spans, and an associative array is addressed by key (LRM
// 21.4.1). Which numbers a word takes is its type's (LRM 21.4.2), so a load
// into a memory of an enumerated type is handed the enumeration's members.
//
// A file the task cannot read or write, or a word it cannot take, is reported
// and the run goes on (LRM 21.4).

// The radix a memory task's words are written in (LRM 21.4): binary for the
// `b` tasks, whose base is 2, and hexadecimal for the `h` ones.
[[nodiscard]] constexpr auto MemoryRadix(unsigned base) -> value::DigitRadix {
  return base == 2U ? value::DigitRadix::kBinary : value::DigitRadix::kHex;
}

// A word written out at full width in radix `base`, x and z per digit (LRM
// 21.5.1), as `%h` / `%b` display it.
[[nodiscard]] auto RenderedMemoryWord(
    const value::ConstIntegralView& word, unsigned base) -> std::string;

// The values a word of a memory takes from its file (LRM 21.4.2): any number
// the file writes, or, in a memory of an enumerated type, a member's only.
struct AnyNumber {};
struct MemberValues {
  value::Enumeration members;
};
using WordValues = std::variant<AnyNumber, MemberValues>;

// The words one memory task moves between a file and a memory: what the call
// asked for, and, once the memory's own code asks, the words the file holds,
// in the order it holds them, or the words a dump writes, in the order it
// writes them. The file, its tokens, the addresses and the reports are this
// class's, the same for every memory; which element a word is, and walking the
// memory to reach it, are the memory type's own code, which asks this for the
// word at each turn as Verilator's generated code asks its `VlReadMem`.
class MemoryWords {
 public:
  // A load, of any number or of an enumeration's members only, and a dump,
  // each with no window or with one ending at `finish`. `bounds` holds every
  // declared range of a memory of fixed size, highest dimension first, as
  // left and right bounds, and none for any other memory.
  MemoryWords(
      RuntimeEffects& runtime, WordValues values, const value::String& filename,
      std::span<const std::int64_t> bounds, std::int64_t base,
      std::int64_t start, std::optional<std::int64_t> finish);
  // LRM 21.3.4.4 $fread into a memory: at most `count` words, from the
  // address `start` toward the highest, of `width` bits each, the next ones
  // the open file `fd` holds, read as bytes most significant first.
  MemoryWords(
      FileTable& files, std::int64_t fd, std::span<const std::int64_t> bounds,
      std::int64_t start, std::int64_t count, std::int64_t width);
  MemoryWords(const MemoryWords&) = delete;
  auto operator=(const MemoryWords&) -> MemoryWords& = delete;
  MemoryWords(MemoryWords&&) noexcept;
  auto operator=(MemoryWords&&) noexcept -> MemoryWords&;
  ~MemoryWords();

  // The words the file gives a memory of fixed size, by its bounds; a memory
  // of `count` elements addressed from 0; and one addressed by key. Each word
  // the file gives an address the memory does not hold is left out.
  void ReadGrid(base::SourceLocation at);
  void ReadDense(std::int64_t count, base::SourceLocation at);
  void ReadKeyed(base::SourceLocation at);

  // The words a dump writes, in the order it writes them, of a memory of fixed
  // size and of one of `count` elements addressed from 0.
  void PlanGrid();
  void PlanDense(std::int64_t count);

  // How many words there are, the storage ordinal the word `k` names in the
  // memory's dimension `dimension`, and the key it names in a memory addressed
  // by key.
  [[nodiscard]] auto Count() const -> std::int64_t;
  [[nodiscard]] auto Ordinal(std::int64_t k, std::int64_t dimension) const
      -> std::int64_t;
  [[nodiscard]] auto Key(std::int64_t k) const -> std::int64_t;

  // How many bytes `$fread` read into the words, which it answers (LRM
  // 21.3.4.4); none for a memory task's.
  [[nodiscard]] auto BytesRead() const -> std::int64_t;

  // The word `k` of a load, at the type of the memory's words, written over
  // `out`.
  void Word(std::int64_t k, const value::IntegralView& out) const;
  template <value::IntegralValue T>
  [[nodiscard]] auto Word(std::int64_t k) const -> T {
    T word = T::Zero();
    Word(k, word.AsMutableWords().MutableView());
    return word;
  }

  // The word `k` of a dump, and an entry of a memory addressed by key, which
  // a dump of one takes in its key order; then the file written.
  void Put(std::int64_t k, const value::ConstIntegralView& word);
  void PutKeyed(std::int64_t key, const value::ConstIntegralView& word);
  template <value::IntegralValue T>
  void Put(std::int64_t k, const T& word) {
    Put(k, word.AsWords().View());
  }
  template <value::IntegralValue T>
  void PutKeyed(std::int64_t key, const T& word) {
    PutKeyed(key, word.AsWords().View());
  }
  void Commit(base::SourceLocation at);

 private:
  struct State;
  std::unique_ptr<State> state_;
};

// The six requests a memory task's call makes, each answering the words it
// moves; the memory's own code then reads or writes them.
[[nodiscard]] auto ReadMem(
    RuntimeEffects& runtime, const value::String& filename,
    std::span<const std::int64_t> bounds, std::int64_t base, std::int64_t start)
    -> MemoryWords;
[[nodiscard]] auto ReadMemWithin(
    RuntimeEffects& runtime, const value::String& filename,
    std::span<const std::int64_t> bounds, std::int64_t base, std::int64_t start,
    std::int64_t finish) -> MemoryWords;
[[nodiscard]] auto ReadMem(
    RuntimeEffects& runtime, const value::Enumeration& members,
    const value::String& filename, std::span<const std::int64_t> bounds,
    std::int64_t base, std::int64_t start) -> MemoryWords;
[[nodiscard]] auto ReadMemWithin(
    RuntimeEffects& runtime, const value::Enumeration& members,
    const value::String& filename, std::span<const std::int64_t> bounds,
    std::int64_t base, std::int64_t start, std::int64_t finish) -> MemoryWords;
[[nodiscard]] auto WriteMem(
    RuntimeEffects& runtime, const value::String& filename,
    std::span<const std::int64_t> bounds, std::int64_t base, std::int64_t start)
    -> MemoryWords;
[[nodiscard]] auto WriteMemWithin(
    RuntimeEffects& runtime, const value::String& filename,
    std::span<const std::int64_t> bounds, std::int64_t base, std::int64_t start,
    std::int64_t finish) -> MemoryWords;

}  // namespace lyra::runtime
