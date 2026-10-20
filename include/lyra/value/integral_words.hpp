#pragma once

#include <algorithm>
#include <bit>
#include <concepts>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string_view>
#include <utility>
#include <vector>

namespace lyra::value {

// What every operation on an integral value does, stated once over its words.
//
// An integral value of `width` bits is its value plane -- bit i of the value at
// bit i%64 of word i/64 -- and, when it can hold x or z, an unknown plane of
// the same length whose set positions are those holding x or z. A position's
// two bits spell its value: 0 is (0, 0), 1 is (1, 0), z is (0, 1), x is (1, 1).
// Every position above `width` in the top word is clear in both planes, so two
// values are equal exactly when their words are.
//
// Each function here is one operator of the language (LRM 11.4) or one builtin
// over values, settling what the operator does with an x or a z and answering
// as the language says the operator answers. It takes each value as a view of
// its words where they lie, with its width and its signedness, so the one
// statement serves every width. Each function here takes a view by value,
// written or read: through a view handed by reference a compiler cannot tell
// that writing a word leaves the view as it was, and the operation stops
// reducing to the instructions its width leaves. That is a rule of these
// functions, which fold into their callers; code that is called as a function
// takes a view however suits it. A two-state value has no unknown words,
// which read as clear everywhere, and a value converted or extracted into a
// two-state answer leaves an x or a z there as 0.

enum class Signedness : std::uint8_t { kSigned, kUnsigned };

// The scalar one position holds. Its two plane bits spell it, and each
// enumerator is that pair as a number, the value bit at `kScalarValueBit` and
// the unknown bit at `kScalarUnknownBit`. That number is the byte a scalar
// crosses a boundary as, and what LRM Annex H.10.1.1 names `svLogic`.
inline constexpr unsigned kScalarValueBit = 0;
inline constexpr unsigned kScalarUnknownBit = 1;

enum class FourStateBit : std::uint8_t {
  kZero = 0b00,
  kOne = 0b01,
  kHighImpedance = 0b10,
  kUnknown = 0b11
};

// An operation that answers a scalar states its two plane bits, which are what
// the value holding the answer stores.
[[nodiscard]] constexpr auto FourStateBitOf(bool value, bool unknown)
    -> FourStateBit {
  return static_cast<FourStateBit>(
      ((value ? 1U : 0U) << kScalarValueBit) |
      ((unknown ? 1U : 0U) << kScalarUnknownBit));
}

[[nodiscard]] constexpr auto ValueBitOf(FourStateBit bit) -> bool {
  return ((std::to_underlying(bit) >> kScalarValueBit) & 1U) != 0U;
}

[[nodiscard]] constexpr auto UnknownBitOf(FourStateBit bit) -> bool {
  return ((std::to_underlying(bit) >> kScalarUnknownBit) & 1U) != 0U;
}

// LRM 11.4.7 truth value of an integral. A definitively-one bit settles the
// value as nonzero however many unknown bits sit beside it, so `(1, x, x, x)`
// is known nonzero while `(x, x, x, x)` is unknown.
enum class Truthiness : std::uint8_t { kKnownZero, kKnownNonzero, kUnknown };

// How two drivers' contributions to one net combine (LRM 6.6.1 Table 6-2, 6.6.3
// Tables 6-3 and 6-4), which the net's declared type picks: `wire` and `tri`
// pass agreement and make a conflict x, `wand` and `triand` let any 0 win,
// `wor` and `trior` any 1. z is every table's identity and defers to the other
// driver, so it is no table of its own.
enum class NetResolution : std::uint8_t { kTriState, kWiredAnd, kWiredOr };

// LRM 11.4.9: the reduction of every position to one scalar.
enum class ReductionOp : std::uint8_t { kAnd, kOr, kXor, kNand, kNor, kXnor };

// The radix a run of digits is written in where each digit covers a whole
// number of bits (LRM 5.7.1, 21.4).
enum class DigitRadix : std::uint8_t { kBinary, kOctal, kHex };

// LRM 11.4.7 logical negation of one scalar: an x or z stays unknown.
[[nodiscard]] constexpr auto Inverted(FourStateBit bit) -> FourStateBit {
  const unsigned planes = std::to_underlying(bit);
  const unsigned value = (planes >> kScalarValueBit) & 1U;
  const unsigned unknown = (planes >> kScalarUnknownBit) & 1U;
  return FourStateBitOf(((value ^ 1U) | unknown) != 0U, unknown != 0U);
}

// LRM 11.4.7 logical AND of two scalars: 0 where either is, 1 where both are,
// and unknown otherwise.
[[nodiscard]] constexpr auto LogicalAnd(FourStateBit a, FourStateBit b)
    -> FourStateBit {
  const bool nonzero = a != FourStateBit::kZero && b != FourStateBit::kZero;
  const bool one = a == FourStateBit::kOne && b == FourStateBit::kOne;
  return FourStateBitOf(nonzero, nonzero && !one);
}

// The integer one word of a plane is. A value wider than 64 bits is held as
// 64-bit words; a narrower one is held as one integer per plane, the smallest
// of these that holds its width, and that integer is its one word.
template <class T>
concept PlaneWord =
    std::same_as<T, std::uint8_t> || std::same_as<T, std::uint16_t> ||
    std::same_as<T, std::uint32_t> || std::same_as<T, std::uint64_t>;

// The words of one plane where they lie, read and written. How many there are
// is part of the span's type where the code holding the value knows its type,
// and open where it was compiled without it.
template <class T>
inline constexpr bool kIsConstWordSpan = false;
template <PlaneWord Word, std::size_t kCount>
inline constexpr bool kIsConstWordSpan<std::span<const Word, kCount>> = true;
template <PlaneWord Word, std::size_t kCount>
inline constexpr bool kIsConstWordSpan<std::span<Word, kCount>> = true;
template <class T>
concept ConstWordSpan = kIsConstWordSpan<T>;

template <class T>
inline constexpr bool kIsWordSpan = false;
template <PlaneWord Word, std::size_t kCount>
inline constexpr bool kIsWordSpan<std::span<Word, kCount>> = true;
template <class T>
concept WordSpan = kIsWordSpan<T>;

// A plane a value does not have, which reads as clear everywhere.
inline constexpr std::span<const std::uint64_t, 0> kEmptyPlane{};

// The words of both planes of one integral value, read, with nothing said of
// how many bits they are: one member of a table whose width is stated once for
// all of them.
struct ConstPlanes {
  std::span<const std::uint64_t> value;
  std::span<const std::uint64_t> unknown;
};

// An integral value being read: the words of its planes where they lie, how
// many bits it is, and which signedness its bits are read as. Every operation
// below takes its operands this way. It can hold x or z exactly when it has
// unknown words.
//
// Which integer a word is, and how many words each plane has, are part of the
// view's type where the code reading the value knows the value's type, so each
// operation, stated once over a view of any word and any count, is compiled
// for the ones a type fixes: at one word its loops are a single step and a
// compiler reduces the operation to the instructions the width leaves.
template <PlaneWord Word, std::size_t kValueWords, std::size_t kUnknownWords>
struct ConstIntegralViewOf {
  std::span<const Word, kValueWords> value;
  std::span<const Word, kUnknownWords> unknown;
  std::uint64_t width = 0;
  Signedness signedness = Signedness::kUnsigned;

  [[nodiscard]] constexpr auto IsFourState() const -> bool {
    return !unknown.empty();
  }

  // The same value with the count of its words open, which only a value held
  // as 64-bit words has.
  [[nodiscard]] constexpr auto AsDynamic() const -> ConstIntegralViewOf<
      std::uint64_t, std::dynamic_extent, std::dynamic_extent>
    requires std::same_as<Word, std::uint64_t>
  {
    return {
        .value = value,
        .unknown = unknown,
        .width = width,
        .signedness = signedness};
  }
};
using ConstIntegralView = ConstIntegralViewOf<
    std::uint64_t, std::dynamic_extent, std::dynamic_extent>;

// An integral value being written: the words of its planes where they lie,
// and how many bits it is. Every operation below writes its answer through
// one.
template <PlaneWord Word, std::size_t kValueWords, std::size_t kUnknownWords>
struct IntegralViewOf {
  std::span<Word, kValueWords> value;
  std::span<Word, kUnknownWords> unknown;
  std::uint64_t width = 0;

  [[nodiscard]] constexpr auto IsFourState() const -> bool {
    return !unknown.empty();
  }

  // The low `narrower` bits of the value as a value of that many bits, which
  // is how a type laid out in a wider one is written.
  [[nodiscard]] constexpr auto Truncated(std::uint64_t narrower) const
      -> IntegralViewOf {
    return {.value = value, .unknown = unknown, .width = narrower};
  }

  // The value as it stands, read.
  [[nodiscard]] constexpr auto AsConst() const
      -> ConstIntegralViewOf<Word, kValueWords, kUnknownWords> {
    return {
        .value = value,
        .unknown = unknown,
        .width = width,
        .signedness = Signedness::kUnsigned};
  }
};
using IntegralView =
    IntegralViewOf<std::uint64_t, std::dynamic_extent, std::dynamic_extent>;

template <class T>
inline constexpr bool kIsConstIntegralView = false;
template <PlaneWord Word, std::size_t kValueWords, std::size_t kUnknownWords>
inline constexpr bool kIsConstIntegralView<
    ConstIntegralViewOf<Word, kValueWords, kUnknownWords>> = true;
template <class T>
concept AnyConstIntegralView = kIsConstIntegralView<T>;

[[nodiscard]] constexpr auto WordCountForBits(std::uint64_t bit_width)
    -> std::size_t {
  return static_cast<std::size_t>(
      (bit_width / 64U) + (bit_width % 64U == 0U ? 0U : 1U));
}

// The low `count` bits of a word set, all of them from 64 up.
[[nodiscard]] constexpr auto LowBits(std::uint64_t count) -> std::uint64_t {
  return count >= 64U ? ~std::uint64_t{0} : (std::uint64_t{1} << count) - 1U;
}

// The bits of word `word_index` that lie below `bit_width`.
[[nodiscard]] constexpr auto ValidBitsMask(
    std::size_t word_index, std::uint64_t bit_width) -> std::uint64_t {
  const std::uint64_t low = static_cast<std::uint64_t>(word_index) * 64U;
  return LowBits(bit_width > low ? bit_width - low : 0U);
}

// Where some of a value's bits lie: `width` of them from `lsb`, counted from
// the value's least significant bit rather than by the indices a declaration
// gives them.
struct BitPositions {
  std::uint64_t lsb = 0;
  std::uint64_t width = 0;
};

// Word `index` of a plane, which the plane has. Every operation works in 64
// bits whatever integer a word is held in: a word is widened as it is read and
// narrowed as it is written, and a plane held in a narrower integer has no
// position above that integer for the narrowing to lose.
[[nodiscard]] constexpr auto WordAt(ConstWordSpan auto words, std::size_t index)
    -> std::uint64_t {
  return std::uint64_t{words[index]};
}

template <PlaneWord Word, std::size_t kCount>
constexpr void SetWord(
    std::span<Word, kCount> words, std::size_t index, std::uint64_t word) {
  words[index] = static_cast<Word>(word);
}

// Word `index` of a plane, a word the plane does not have reading as clear --
// which is how the unknown plane of a two-state value reads.
[[nodiscard]] constexpr auto WordOrZero(
    ConstWordSpan auto words, std::size_t index) -> std::uint64_t {
  return index < words.size() ? WordAt(words, index) : std::uint64_t{0};
}

// One word's worth of a plane beginning at `offset`, right-aligned, keeping
// `count` positions. A position the plane does not reach reads as clear.
[[nodiscard]] constexpr auto BitsAt(
    ConstWordSpan auto words, std::uint64_t offset, std::uint64_t count)
    -> std::uint64_t {
  // A plane of one word is read as a shift of that word, which is every read
  // of a value no wider than a machine word.
  if (words.size() == 1U) {
    return offset < 64U ? (WordAt(words, 0) >> offset) & LowBits(count) : 0U;
  }
  const auto word = static_cast<std::size_t>(offset / 64U);
  const std::uint64_t shift = offset % 64U;
  std::uint64_t bits = WordOrZero(words, word) >> shift;
  if (shift != 0U) {
    bits |= WordOrZero(words, word + 1U) << (64U - shift);
  }
  return bits & LowBits(count);
}

[[nodiscard]] constexpr auto BitAt(
    ConstWordSpan auto words, std::uint64_t position) -> bool {
  return ((WordOrZero(words, static_cast<std::size_t>(position / 64U)) >>
           (position % 64U)) &
          1U) != 0U;
}

// The scalar one position of a value holds.
[[nodiscard]] constexpr auto ScalarAt(
    AnyConstIntegralView auto bits, std::uint64_t position) -> FourStateBit {
  return FourStateBitOf(
      BitAt(bits.value, position), BitAt(bits.unknown, position));
}

// Every position of a plane at or above `width` cleared, which is the form a
// plane is held in.
constexpr void ClearAboveWidth(WordSpan auto words, std::uint64_t width) {
  if (!words.empty()) {
    const std::size_t top = words.size() - 1U;
    SetWord(words, top, WordAt(words, top) & ValidBitsMask(top, width));
  }
}

namespace detail {

struct WordStep {
  std::size_t word;
  std::uint64_t offset;
  std::uint64_t count;
};

// The part of the `remaining` positions from `position` up that lies in one
// word.
[[nodiscard]] constexpr auto StepAt(
    std::uint64_t position, std::uint64_t remaining) -> WordStep {
  const std::uint64_t offset = position % 64U;
  return WordStep{
      .word = static_cast<std::size_t>(position / 64U),
      .offset = offset,
      .count = std::min<std::uint64_t>(64U - offset, remaining)};
}

[[nodiscard]] constexpr auto AnySet(ConstWordSpan auto words) -> bool {
  std::uint64_t set = 0;
  for (const std::uint64_t word : words) {
    set |= word;
  }
  return set != 0U;
}

constexpr void Fill(WordSpan auto words, std::uint64_t word) {
  for (std::size_t i = 0; i < words.size(); ++i) {
    SetWord(words, i, word);
  }
}

constexpr void Copy(ConstWordSpan auto src, WordSpan auto dst) {
  for (std::size_t i = 0; i < dst.size(); ++i) {
    SetWord(dst, i, WordOrZero(src, i));
  }
}

[[nodiscard]] constexpr auto ScalarOf(bool holds) -> FourStateBit {
  return FourStateBitOf(holds, false);
}

// x where `unknown` holds, and otherwise 1 exactly where `holds` does.
[[nodiscard]] constexpr auto UnknownOr(bool unknown, bool holds)
    -> FourStateBit {
  return FourStateBitOf(unknown || holds, unknown);
}

// `dst = a + b` over the width, with each operand's missing words reading as
// clear.
constexpr void AddWords(
    ConstWordSpan auto a, ConstWordSpan auto b, WordSpan auto dst,
    std::uint64_t width) {
  std::uint64_t carry = 0;
  for (std::size_t i = 0; i < dst.size(); ++i) {
    const std::uint64_t aw = WordOrZero(a, i);
    const std::uint64_t s1 = aw + WordOrZero(b, i);
    const std::uint64_t c1 = s1 < aw ? 1U : 0U;
    const std::uint64_t s2 = s1 + carry;
    const std::uint64_t c2 = s2 < s1 ? 1U : 0U;
    SetWord(dst, i, s2);
    carry = c1 + c2;
  }
  ClearAboveWidth(dst, width);
}

constexpr void SubWords(
    ConstWordSpan auto a, ConstWordSpan auto b, WordSpan auto dst,
    std::uint64_t width) {
  std::uint64_t borrow = 0;
  for (std::size_t i = 0; i < dst.size(); ++i) {
    const std::uint64_t aw = WordOrZero(a, i);
    const std::uint64_t bw = WordOrZero(b, i);
    const std::uint64_t d1 = aw - bw;
    const std::uint64_t b1 = aw < bw ? 1U : 0U;
    const std::uint64_t d2 = d1 - borrow;
    const std::uint64_t b2 = d1 < borrow ? 1U : 0U;
    SetWord(dst, i, d2);
    borrow = b1 + b2;
  }
  ClearAboveWidth(dst, width);
}

// The product of two values of many words, modulo 2^width: schoolbook
// multiplication, each word pair's product taken whole.
constexpr void MulWords(
    std::span<const std::uint64_t> a, std::span<const std::uint64_t> b,
    std::span<std::uint64_t> dst, std::uint64_t width) {
  Fill(dst, 0U);
  for (std::size_t i = 0; i < a.size() && i < dst.size(); ++i) {
    std::uint64_t carry = 0;
    for (std::size_t j = 0; i + j < dst.size(); ++j) {
      if (j >= b.size() && carry == 0U) {
        break;
      }
      const unsigned __int128 product =
          (static_cast<unsigned __int128>(a[i]) * WordOrZero(b, j)) +
          dst[i + j] + carry;
      dst[i + j] = static_cast<std::uint64_t>(product);
      carry = static_cast<std::uint64_t>(product >> 64U);
    }
  }
  ClearAboveWidth(dst, width);
}

// How the numbers two values of `width` bits hold compare: negative, zero or
// positive as the first lies below, at or above the second. A signed value's
// top position weighs negative, which orders two of them as unsigned ones are
// ordered once that position is inverted in both.
[[nodiscard]] constexpr auto Order(
    ConstWordSpan auto a, ConstWordSpan auto b, std::uint64_t width,
    Signedness signedness) -> int {
  const std::uint64_t sign = signedness == Signedness::kSigned
                                 ? std::uint64_t{1} << ((width - 1U) % 64U)
                                 : 0U;
  for (std::size_t i = a.size(); i-- > 0;) {
    const std::uint64_t inverted = i + 1U == a.size() ? sign : 0U;
    const std::uint64_t aw = WordAt(a, i) ^ inverted;
    const std::uint64_t bw = WordOrZero(b, i) ^ inverted;
    if (aw != bw) {
      return aw < bw ? -1 : 1;
    }
  }
  return 0;
}

// The number a shift amount holds, saturated at a value no width reaches.
[[nodiscard]] constexpr auto ShiftCount(ConstWordSpan auto amount)
    -> std::uint64_t {
  std::uint64_t above = 0;
  for (std::size_t i = 1; i < amount.size(); ++i) {
    above |= WordAt(amount, i);
  }
  return above != 0U ? ~std::uint64_t{0} : WordOrZero(amount, 0);
}

constexpr void ShiftLeftWords(
    ConstWordSpan auto src, std::uint64_t amount, WordSpan auto dst,
    std::uint64_t width) {
  Fill(dst, 0U);
  if (amount >= width) {
    return;
  }
  const auto word_shift = static_cast<std::size_t>(amount / 64U);
  const std::uint64_t bit_shift = amount % 64U;
  for (std::size_t i = dst.size(); i-- > word_shift;) {
    const std::size_t from = i - word_shift;
    std::uint64_t word = WordOrZero(src, from) << bit_shift;
    if (bit_shift != 0U && from > 0U) {
      word |= WordOrZero(src, from - 1U) >> (64U - bit_shift);
    }
    SetWord(dst, i, word);
  }
  ClearAboveWidth(dst, width);
}

// Shifts toward the least significant end, filling the vacated top positions
// with `fill`. Each word of the answer is the source read `amount` positions
// up, which is clear past the source's width and so in every vacated position,
// and the fill in the positions of that word the shift vacates.
constexpr void ShiftRightWords(
    ConstWordSpan auto src, std::uint64_t amount, bool fill, WordSpan auto dst,
    std::uint64_t width) {
  const std::uint64_t vacated = std::min(amount, width);
  for (std::size_t i = 0; i < dst.size(); ++i) {
    const std::uint64_t low = static_cast<std::uint64_t>(i) * 64U;
    const std::uint64_t above =
        ValidBitsMask(i, width) & ~ValidBitsMask(i, width - vacated);
    SetWord(dst, i, BitsAt(src, low + vacated, 64U) | (fill ? above : 0U));
  }
}

// A plane moved toward the least significant end by `amount`, each plane of a
// signed value filled from its own top position where the shift is arithmetic,
// so an x at the top extends down.
template <
    PlaneWord WrittenWord, std::size_t kWrittenValueWords,
    std::size_t kWrittenUnknownWords>
constexpr void ShiftRightPlanes(
    IntegralViewOf<WrittenWord, kWrittenValueWords, kWrittenUnknownWords> out,
    AnyConstIntegralView auto a, std::uint64_t amount, bool arithmetic) {
  ShiftRightWords(
      a.value, amount, arithmetic && BitAt(a.value, out.width - 1U), out.value,
      out.width);
  if (!out.unknown.empty()) {
    ShiftRightWords(
        a.unknown, amount, arithmetic && BitAt(a.unknown, out.width - 1U),
        out.unknown, out.width);
  }
}

// The low `width` bits of a word read as a two's-complement number.
[[nodiscard]] constexpr auto SignExtended(
    std::uint64_t word, std::uint64_t width) -> std::int64_t {
  const std::uint64_t unused = 64U - width;
  return static_cast<std::int64_t>(word << unused) >>
         static_cast<std::int64_t>(unused);
}

// The quotient and the remainder of two values of one word whose divisor is not
// zero, each the machine's own division. A signed operand narrower than a word
// divides as the 64-bit number it extends to, which no quotient overflows; at
// 64 bits the one that would -- the most negative number over -1 -- is that
// number again, with nothing left over.
[[nodiscard]] constexpr auto QuotientWord(
    std::uint64_t a, std::uint64_t b, std::uint64_t width,
    Signedness signedness) -> std::uint64_t {
  if (signedness == Signedness::kUnsigned) {
    return a / b;
  }
  const std::int64_t divisor = SignExtended(b, width);
  if (width == 64U && divisor == -1) {
    return std::uint64_t{0} - a;
  }
  return static_cast<std::uint64_t>(SignExtended(a, width) / divisor) &
         LowBits(width);
}

[[nodiscard]] constexpr auto RemainderWord(
    std::uint64_t a, std::uint64_t b, std::uint64_t width,
    Signedness signedness) -> std::uint64_t {
  if (signedness == Signedness::kUnsigned) {
    return a % b;
  }
  const std::int64_t divisor = SignExtended(b, width);
  if (width == 64U && divisor == -1) {
    return 0U;
  }
  return static_cast<std::uint64_t>(SignExtended(a, width) % divisor) &
         LowBits(width);
}

// What long division of two values of many words answers: the quotient's and
// the remainder's magnitudes, and the signs taken off the operands.
struct LongDivision {
  std::vector<std::uint64_t> quotient;
  std::vector<std::uint64_t> remainder;
  bool dividend_negative = false;
  bool divisor_negative = false;
};

// Binary long division of the operands' magnitudes, one quotient bit per
// position of the dividend from the top. The divisor is not zero.
[[nodiscard]] constexpr auto DivideLong(
    std::span<const std::uint64_t> a, std::span<const std::uint64_t> b,
    std::uint64_t width, Signedness signedness) -> LongDivision {
  const std::size_t words = a.size();
  LongDivision division{
      .quotient = std::vector<std::uint64_t>(words, 0U),
      .remainder = std::vector<std::uint64_t>(words, 0U),
      .dividend_negative =
          signedness == Signedness::kSigned && BitAt(a, width - 1U),
      .divisor_negative =
          signedness == Signedness::kSigned && BitAt(b, width - 1U)};
  std::vector<std::uint64_t> dividend_words(words);
  std::vector<std::uint64_t> divisor_words(words);
  const std::span<std::uint64_t> dividend = dividend_words;
  const std::span<std::uint64_t> divisor = divisor_words;
  if (division.dividend_negative) {
    SubWords(kEmptyPlane, a, dividend, width);
  } else {
    Copy(a, dividend);
  }
  if (division.divisor_negative) {
    SubWords(kEmptyPlane, b, divisor, width);
  } else {
    Copy(b, divisor);
  }
  const std::span<std::uint64_t> remainder = division.remainder;
  for (std::uint64_t pos = width; pos-- > 0;) {
    std::uint64_t carry = BitAt(dividend, pos) ? 1U : 0U;
    for (std::uint64_t& word : remainder) {
      const std::uint64_t next = word >> 63U;
      word = (word << 1U) | carry;
      carry = next;
    }
    ClearAboveWidth(remainder, width);
    if (Order(remainder, divisor, width, Signedness::kUnsigned) >= 0) {
      SubWords(remainder, divisor, remainder, width);
      division.quotient[static_cast<std::size_t>(pos / 64U)] |= std::uint64_t{1}
                                                                << (pos % 64U);
    }
  }
  return division;
}

// A magnitude written out as the number of the sign given.
constexpr void WriteSigned(
    std::span<const std::uint64_t> magnitude, bool negative,
    std::span<std::uint64_t> out, std::uint64_t width) {
  if (negative) {
    SubWords(kEmptyPlane, magnitude, out, width);
  } else {
    Copy(magnitude, out);
  }
}

// LRM 11.4.3 over many words: a quotient truncates toward zero, so it is
// negative where the operands' signs differ, and a remainder takes the
// dividend's sign.
constexpr void QuotientWords(
    std::span<const std::uint64_t> a, std::span<const std::uint64_t> b,
    std::span<std::uint64_t> out, std::uint64_t width, Signedness signedness) {
  const LongDivision division = DivideLong(a, b, width, signedness);
  WriteSigned(
      division.quotient,
      division.dividend_negative != division.divisor_negative, out, width);
}

constexpr void RemainderWords(
    std::span<const std::uint64_t> a, std::span<const std::uint64_t> b,
    std::span<std::uint64_t> out, std::uint64_t width, Signedness signedness) {
  const LongDivision division = DivideLong(a, b, width, signedness);
  WriteSigned(division.remainder, division.dividend_negative, out, width);
}

// One word of a value read `offset` positions from its least significant bit,
// where the offset may lie below the value or past it: each plane's bits
// there, and which of the word's positions lie inside the value. A position
// outside it reads as clear in both planes.
struct SourceWord {
  std::uint64_t value;
  std::uint64_t unknown;
  std::uint64_t inside;
};

[[nodiscard]] constexpr auto SourceWordAt(
    AnyConstIntegralView auto src, std::int64_t offset) -> SourceWord {
  if (offset <= -64 || offset >= static_cast<std::int64_t>(src.width)) {
    return SourceWord{.value = 0U, .unknown = 0U, .inside = 0U};
  }
  if (offset < 0) {
    const auto up = static_cast<std::uint64_t>(-offset);
    return SourceWord{
        .value = WordOrZero(src.value, 0) << up,
        .unknown = WordOrZero(src.unknown, 0) << up,
        .inside = LowBits(src.width) << up};
  }
  const auto from = static_cast<std::uint64_t>(offset);
  return SourceWord{
      .value = BitsAt(src.value, from, 64U),
      .unknown = BitsAt(src.unknown, from, 64U),
      .inside = LowBits(src.width - from)};
}

// Writes the positions `run` names of `dst` from `src`, whose least
// significant bit lies at position `start` of `dst`, and leaves every other
// position as it stands. A two-state source landing in four-state storage
// settles the positions it lands on, and an x or z landing in two-state
// storage lands as 0.
template <
    PlaneWord WrittenWord, std::size_t kWrittenValueWords,
    std::size_t kWrittenUnknownWords>
constexpr void WriteRun(
    IntegralViewOf<WrittenWord, kWrittenValueWords, kWrittenUnknownWords> dst,
    AnyConstIntegralView auto src, std::int64_t start, BitPositions run) {
  const std::uint64_t end = run.lsb + run.width;
  const std::size_t last = std::min(
      static_cast<std::size_t>((end - 1U) / 64U), dst.value.size() - 1U);
  for (auto i = static_cast<std::size_t>(run.lsb / 64U); i <= last; ++i) {
    const std::uint64_t low = static_cast<std::uint64_t>(i) * 64U;
    const std::uint64_t from = std::max(run.lsb, low) - low;
    const std::uint64_t to = std::min(end, low + 64U) - low;
    const std::uint64_t mask = LowBits(to - from) << from;
    const SourceWord word =
        SourceWordAt(src, static_cast<std::int64_t>(low) - start);
    if (dst.unknown.empty()) {
      SetWord(
          dst.value, i,
          (WordAt(dst.value, i) & ~mask) | (word.value & ~word.unknown & mask));
    } else {
      SetWord(
          dst.value, i, (WordAt(dst.value, i) & ~mask) | (word.value & mask));
      SetWord(
          dst.unknown, i,
          (WordAt(dst.unknown, i) & ~mask) | (word.unknown & mask));
    }
  }
}

// Whether a value holds exactly `number`, read as a two's-complement number of
// its width.
[[nodiscard]] constexpr auto HoldsNumber(
    std::span<const std::uint64_t> value, std::uint64_t width,
    std::int64_t number) -> bool {
  for (std::size_t i = 0; i < value.size(); ++i) {
    std::uint64_t expected = number < 0 ? ~std::uint64_t{0} : 0U;
    if (i == 0U) {
      expected = static_cast<std::uint64_t>(number);
    }
    if (value[i] != (expected & ValidBitsMask(i, width))) {
      return false;
    }
  }
  return true;
}

// Whether a truth value is settled as zero, and whether as nonzero; it is
// unknown where it is neither.
[[nodiscard]] constexpr auto IsKnownZero(Truthiness truth) -> bool {
  switch (truth) {
    case Truthiness::kKnownZero:
      return true;
    case Truthiness::kKnownNonzero:
    case Truthiness::kUnknown:
      return false;
  }
  std::unreachable();
}

[[nodiscard]] constexpr auto IsKnownNonzero(Truthiness truth) -> bool {
  switch (truth) {
    case Truthiness::kKnownNonzero:
      return true;
    case Truthiness::kKnownZero:
    case Truthiness::kUnknown:
      return false;
  }
  std::unreachable();
}

// LRM 11.4.7: the logical operators' tables over their operands' truth
// values. An answer is unknown only where an operand's truth is and the other
// operand does not settle it.
[[nodiscard]] constexpr auto TruthAnd(Truthiness a, Truthiness b)
    -> FourStateBit {
  const bool zero = IsKnownZero(a) || IsKnownZero(b);
  const bool one = IsKnownNonzero(a) && IsKnownNonzero(b);
  return FourStateBitOf(!zero, !zero && !one);
}

[[nodiscard]] constexpr auto TruthOr(Truthiness a, Truthiness b)
    -> FourStateBit {
  const bool one = IsKnownNonzero(a) || IsKnownNonzero(b);
  const bool zero = IsKnownZero(a) && IsKnownZero(b);
  return FourStateBitOf(!zero, !zero && !one);
}

[[nodiscard]] constexpr auto TruthNot(Truthiness a) -> FourStateBit {
  const bool nonzero = IsKnownNonzero(a);
  const bool zero = IsKnownZero(a);
  return FourStateBitOf(!nonzero, !nonzero && !zero);
}

}  // namespace detail

// Every position of `out` holding one scalar.
template <
    PlaneWord WrittenWord, std::size_t kWrittenValueWords,
    std::size_t kWrittenUnknownWords>
constexpr void FillScalar(
    IntegralViewOf<WrittenWord, kWrittenValueWords, kWrittenUnknownWords> out,
    FourStateBit bit) {
  const bool unknown = UnknownBitOf(bit);
  const bool value = ValueBitOf(bit) && (out.IsFourState() || !unknown);
  for (std::size_t i = 0; i < out.value.size(); ++i) {
    const std::uint64_t valid = ValidBitsMask(i, out.width);
    SetWord(out.value, i, value ? valid : 0U);
    if (!out.unknown.empty()) {
      SetWord(out.unknown, i, unknown ? valid : 0U);
    }
  }
}

// The value a declaration holds before anything writes it (LRM Table 6-7): x in
// every position of a four-state type, 0 in every position of a two-state one.
template <
    PlaneWord WrittenWord, std::size_t kWrittenValueWords,
    std::size_t kWrittenUnknownWords>
constexpr void FillDefault(
    IntegralViewOf<WrittenWord, kWrittenValueWords, kWrittenUnknownWords> out) {
  FillScalar(out, FourStateBit::kUnknown);
}

[[nodiscard]] constexpr auto HasUnknown(AnyConstIntegralView auto a) -> bool {
  return detail::AnySet(a.unknown);
}

// A value read from an integer: the low word is `value`, and every word above
// repeats its sign, which is what makes `-1` fill a wide destination (LRM
// 11.6.1).
template <
    PlaneWord WrittenWord, std::size_t kWrittenValueWords,
    std::size_t kWrittenUnknownWords>
constexpr void FromInt(
    IntegralViewOf<WrittenWord, kWrittenValueWords, kWrittenUnknownWords> out,
    std::int64_t value) {
  detail::Fill(out.value, value < 0 ? ~std::uint64_t{0} : 0U);
  SetWord(out.value, 0, static_cast<std::uint64_t>(value));
  ClearAboveWidth(out.value, out.width);
  detail::Fill(out.unknown, 0U);
}

// The low 64 bits as a signed integer, sign-extended from the width when the
// value is signed. An x or z bit reads as 0 (LRM 6.12.1).
[[nodiscard]] constexpr auto ToInt64(AnyConstIntegralView auto a)
    -> std::int64_t {
  const std::uint64_t raw = WordOrZero(a.value, 0) & ~WordOrZero(a.unknown, 0);
  if (a.signedness == Signedness::kUnsigned || a.width >= 64U) {
    return static_cast<std::int64_t>(raw);
  }
  return detail::SignExtended(raw, a.width);
}

[[nodiscard]] constexpr auto Truth(AnyConstIntegralView auto a) -> Truthiness {
  bool unknown_bit = false;
  for (std::size_t i = 0; i < a.value.size(); ++i) {
    const std::uint64_t unk = WordOrZero(a.unknown, i);
    if ((WordAt(a.value, i) & ~unk) != 0U) {
      return Truthiness::kKnownNonzero;
    }
    unknown_bit = unknown_bit || unk != 0U;
  }
  return unknown_bit ? Truthiness::kUnknown : Truthiness::kKnownZero;
}

// Bit 0's scalar, which is the bit an edge is detected on (LRM 9.4.2).
[[nodiscard]] constexpr auto LeastSignificantBit(AnyConstIntegralView auto a)
    -> FourStateBit {
  return ScalarAt(a, 0);
}

// LRM 11.4.3: x or z anywhere in either operand makes the whole result x.
// Otherwise the result is the operation on the value planes, modulo 2^width.
template <
    PlaneWord WrittenWord, std::size_t kWrittenValueWords,
    std::size_t kWrittenUnknownWords>
constexpr void Add(
    IntegralViewOf<WrittenWord, kWrittenValueWords, kWrittenUnknownWords> out,
    AnyConstIntegralView auto a, AnyConstIntegralView auto b) {
  if (HasUnknown(a) || HasUnknown(b)) {
    FillScalar(out, FourStateBit::kUnknown);
    return;
  }
  detail::AddWords(a.value, b.value, out.value, out.width);
  detail::Fill(out.unknown, 0U);
}

template <
    PlaneWord WrittenWord, std::size_t kWrittenValueWords,
    std::size_t kWrittenUnknownWords>
constexpr void Subtract(
    IntegralViewOf<WrittenWord, kWrittenValueWords, kWrittenUnknownWords> out,
    AnyConstIntegralView auto a, AnyConstIntegralView auto b) {
  if (HasUnknown(a) || HasUnknown(b)) {
    FillScalar(out, FourStateBit::kUnknown);
    return;
  }
  detail::SubWords(a.value, b.value, out.value, out.width);
  detail::Fill(out.unknown, 0U);
}

template <
    PlaneWord WrittenWord, std::size_t kWrittenValueWords,
    std::size_t kWrittenUnknownWords>
constexpr void Negate(
    IntegralViewOf<WrittenWord, kWrittenValueWords, kWrittenUnknownWords> out,
    AnyConstIntegralView auto a) {
  if (HasUnknown(a)) {
    FillScalar(out, FourStateBit::kUnknown);
    return;
  }
  detail::SubWords(kEmptyPlane, a.value, out.value, out.width);
  detail::Fill(out.unknown, 0U);
}

// A value of one word multiplies as the machine multiplies a word.
template <
    PlaneWord WrittenWord, std::size_t kWrittenValueWords,
    std::size_t kWrittenUnknownWords>
constexpr void Multiply(
    IntegralViewOf<WrittenWord, kWrittenValueWords, kWrittenUnknownWords> out,
    AnyConstIntegralView auto a, AnyConstIntegralView auto b) {
  if (HasUnknown(a) || HasUnknown(b)) {
    FillScalar(out, FourStateBit::kUnknown);
    return;
  }
  detail::Fill(out.unknown, 0U);
  if constexpr (std::same_as<WrittenWord, std::uint64_t>) {
    if (out.value.size() != 1U) {
      detail::MulWords(a.value, b.value, out.value, out.width);
      return;
    }
  }
  SetWord(
      out.value, 0,
      (WordAt(a.value, 0) * WordAt(b.value, 0)) & LowBits(out.width));
}

// LRM 11.4.3: integer division truncates toward zero, and a zero divisor makes
// the result x. A value of one word divides as the machine divides a word.
template <
    PlaneWord WrittenWord, std::size_t kWrittenValueWords,
    std::size_t kWrittenUnknownWords>
constexpr void Divide(
    IntegralViewOf<WrittenWord, kWrittenValueWords, kWrittenUnknownWords> out,
    AnyConstIntegralView auto a, AnyConstIntegralView auto b) {
  if (HasUnknown(a) || HasUnknown(b) || !detail::AnySet(b.value)) {
    FillScalar(out, FourStateBit::kUnknown);
    return;
  }
  detail::Fill(out.unknown, 0U);
  if constexpr (std::same_as<WrittenWord, std::uint64_t>) {
    if (out.value.size() != 1U) {
      detail::QuotientWords(
          a.value, b.value, out.value, out.width, a.signedness);
      return;
    }
  }
  SetWord(
      out.value, 0,
      detail::QuotientWord(
          WordAt(a.value, 0), WordAt(b.value, 0), out.width, a.signedness));
}

// LRM 11.4.3: a remainder takes the dividend's sign, and a zero divisor makes
// the result x.
template <
    PlaneWord WrittenWord, std::size_t kWrittenValueWords,
    std::size_t kWrittenUnknownWords>
constexpr void Modulo(
    IntegralViewOf<WrittenWord, kWrittenValueWords, kWrittenUnknownWords> out,
    AnyConstIntegralView auto a, AnyConstIntegralView auto b) {
  if (HasUnknown(a) || HasUnknown(b) || !detail::AnySet(b.value)) {
    FillScalar(out, FourStateBit::kUnknown);
    return;
  }
  detail::Fill(out.unknown, 0U);
  if constexpr (std::same_as<WrittenWord, std::uint64_t>) {
    if (out.value.size() != 1U) {
      detail::RemainderWords(
          a.value, b.value, out.value, out.width, a.signedness);
      return;
    }
  }
  SetWord(
      out.value, 0,
      detail::RemainderWord(
          WordAt(a.value, 0), WordAt(b.value, 0), out.width, a.signedness));
}

// LRM 11.4.3 power. An x or z in either operand makes the result x. A negative
// exponent is a reciprocal, which integer division truncates to zero for every
// base but 1 and -1 and which for 0 divides by zero (Table 11-4); any other
// exponent, read whole however wide it is, multiplies the base that many
// times, so a zero one answers 1.
constexpr void Power(
    IntegralView out, ConstIntegralView base, ConstIntegralView exponent) {
  if (HasUnknown(base) || HasUnknown(exponent)) {
    FillScalar(out, FourStateBit::kUnknown);
    return;
  }
  detail::Fill(out.unknown, 0U);
  if (exponent.signedness == Signedness::kSigned &&
      BitAt(exponent.value, exponent.width - 1U)) {
    if (!detail::AnySet(base.value)) {
      FillScalar(out, FourStateBit::kUnknown);
    } else if (detail::HoldsNumber(base.value, out.width, 1)) {
      FromInt(out, 1);
    } else if (
        base.signedness == Signedness::kSigned &&
        detail::HoldsNumber(base.value, out.width, -1)) {
      FromInt(out, BitAt(exponent.value, 0) ? -1 : 1);
    } else {
      FromInt(out, 0);
    }
    return;
  }
  const std::size_t words = out.value.size();
  std::vector<std::uint64_t> factor_words(base.value.begin(), base.value.end());
  std::vector<std::uint64_t> product_words(words);
  const std::span<std::uint64_t> factor = factor_words;
  const std::span<std::uint64_t> product = product_words;
  detail::Fill(out.value, 0U);
  out.value[0] = 1U;
  ClearAboveWidth(out.value, out.width);
  std::uint64_t remaining = 0;
  for (std::uint64_t at = 0; at < exponent.width; ++at) {
    if (BitAt(exponent.value, at)) {
      remaining = at + 1U;
    }
  }
  for (std::uint64_t at = 0; at < remaining; ++at) {
    if (BitAt(exponent.value, at)) {
      detail::MulWords(out.value, factor, product, out.width);
      detail::Copy(product, out.value);
    }
    if (at + 1U < remaining) {
      detail::MulWords(factor, factor, product, out.width);
      detail::Copy(product, factor);
    }
  }
}

// LRM 11.4.8 Tables 11-11 to 11-15: per position, z reads as x.
template <
    PlaneWord WrittenWord, std::size_t kWrittenValueWords,
    std::size_t kWrittenUnknownWords>
constexpr void BitwiseAnd(
    IntegralViewOf<WrittenWord, kWrittenValueWords, kWrittenUnknownWords> out,
    AnyConstIntegralView auto a, AnyConstIntegralView auto b) {
  for (std::size_t i = 0; i < out.value.size(); ++i) {
    const std::uint64_t av = WordAt(a.value, i);
    const std::uint64_t bv = WordAt(b.value, i);
    const std::uint64_t au = WordOrZero(a.unknown, i);
    const std::uint64_t bu = WordOrZero(b.unknown, i);
    const std::uint64_t zero = (~au & ~av) | (~bu & ~bv);
    const std::uint64_t one = (~au & av) & (~bu & bv);
    const std::uint64_t unknown = ~(zero | one) & (au | bu);
    SetWord(out.value, i, one | unknown);
    if (!out.unknown.empty()) {
      SetWord(out.unknown, i, unknown);
    }
  }
}

template <
    PlaneWord WrittenWord, std::size_t kWrittenValueWords,
    std::size_t kWrittenUnknownWords>
constexpr void BitwiseOr(
    IntegralViewOf<WrittenWord, kWrittenValueWords, kWrittenUnknownWords> out,
    AnyConstIntegralView auto a, AnyConstIntegralView auto b) {
  for (std::size_t i = 0; i < out.value.size(); ++i) {
    const std::uint64_t av = WordAt(a.value, i);
    const std::uint64_t bv = WordAt(b.value, i);
    const std::uint64_t au = WordOrZero(a.unknown, i);
    const std::uint64_t bu = WordOrZero(b.unknown, i);
    const std::uint64_t one = (~au & av) | (~bu & bv);
    const std::uint64_t zero = (~au & ~av) & (~bu & ~bv);
    const std::uint64_t unknown = ~(zero | one) & (au | bu);
    SetWord(out.value, i, one | unknown);
    if (!out.unknown.empty()) {
      SetWord(out.unknown, i, unknown);
    }
  }
}

template <
    PlaneWord WrittenWord, std::size_t kWrittenValueWords,
    std::size_t kWrittenUnknownWords>
constexpr void BitwiseXor(
    IntegralViewOf<WrittenWord, kWrittenValueWords, kWrittenUnknownWords> out,
    AnyConstIntegralView auto a, AnyConstIntegralView auto b) {
  for (std::size_t i = 0; i < out.value.size(); ++i) {
    const std::uint64_t unknown =
        WordOrZero(a.unknown, i) | WordOrZero(b.unknown, i);
    SetWord(out.value, i, unknown | (WordAt(a.value, i) ^ WordAt(b.value, i)));
    if (!out.unknown.empty()) {
      SetWord(out.unknown, i, unknown);
    }
  }
}

template <
    PlaneWord WrittenWord, std::size_t kWrittenValueWords,
    std::size_t kWrittenUnknownWords>
constexpr void BitwiseXnor(
    IntegralViewOf<WrittenWord, kWrittenValueWords, kWrittenUnknownWords> out,
    AnyConstIntegralView auto a, AnyConstIntegralView auto b) {
  for (std::size_t i = 0; i < out.value.size(); ++i) {
    const std::uint64_t unknown =
        WordOrZero(a.unknown, i) | WordOrZero(b.unknown, i);
    SetWord(out.value, i, unknown | ~(WordAt(a.value, i) ^ WordAt(b.value, i)));
    if (!out.unknown.empty()) {
      SetWord(out.unknown, i, unknown);
    }
  }
  ClearAboveWidth(out.value, out.width);
}

template <
    PlaneWord WrittenWord, std::size_t kWrittenValueWords,
    std::size_t kWrittenUnknownWords>
constexpr void BitwiseNot(
    IntegralViewOf<WrittenWord, kWrittenValueWords, kWrittenUnknownWords> out,
    AnyConstIntegralView auto a) {
  for (std::size_t i = 0; i < out.value.size(); ++i) {
    const std::uint64_t unknown = WordOrZero(a.unknown, i);
    SetWord(out.value, i, ~WordAt(a.value, i) | unknown);
    if (!out.unknown.empty()) {
      SetWord(out.unknown, i, unknown);
    }
  }
  ClearAboveWidth(out.value, out.width);
}

// LRM 11.4.5 logical equality: a position both operands know and disagree on
// settles them unequal however many unknown bits sit beside it, so only
// agreement everywhere both are known leaves the answer to those bits.
[[nodiscard]] constexpr auto Equal(
    AnyConstIntegralView auto a, AnyConstIntegralView auto b) -> FourStateBit {
  std::uint64_t differing = 0;
  std::uint64_t unknown = 0;
  for (std::size_t i = 0; i < a.value.size() && differing == 0U; ++i) {
    const std::uint64_t unk =
        WordOrZero(a.unknown, i) | WordOrZero(b.unknown, i);
    differing |= (WordAt(a.value, i) ^ WordAt(b.value, i)) & ~unk;
    unknown |= unk;
  }
  const bool agree = differing == 0U;
  return FourStateBitOf(agree, agree && unknown != 0U);
}

[[nodiscard]] constexpr auto NotEqual(
    AnyConstIntegralView auto a, AnyConstIntegralView auto b) -> FourStateBit {
  return Inverted(Equal(a, b));
}

// LRM 11.4.4: a relation with an x or z anywhere in either operand is x.
// Otherwise the operands compare as numbers of the operation's signedness.
[[nodiscard]] constexpr auto Less(
    AnyConstIntegralView auto a, AnyConstIntegralView auto b) -> FourStateBit {
  return detail::UnknownOr(
      HasUnknown(a) || HasUnknown(b),
      detail::Order(a.value, b.value, a.width, a.signedness) < 0);
}

[[nodiscard]] constexpr auto LessEqual(
    AnyConstIntegralView auto a, AnyConstIntegralView auto b) -> FourStateBit {
  return Inverted(Less(b, a));
}

[[nodiscard]] constexpr auto Greater(
    AnyConstIntegralView auto a, AnyConstIntegralView auto b) -> FourStateBit {
  return Less(b, a);
}

[[nodiscard]] constexpr auto GreaterEqual(
    AnyConstIntegralView auto a, AnyConstIntegralView auto b) -> FourStateBit {
  return Inverted(Less(a, b));
}

// LRM 11.4.5 case equality: both planes identical, so x matches x and z
// matches z. Its answer is never unknown.
[[nodiscard]] constexpr auto CaseEqual(
    AnyConstIntegralView auto a, AnyConstIntegralView auto b) -> bool {
  for (std::size_t i = 0; i < a.value.size(); ++i) {
    if (WordAt(a.value, i) != WordAt(b.value, i) ||
        WordOrZero(a.unknown, i) != WordOrZero(b.unknown, i)) {
      return false;
    }
  }
  return true;
}

// LRM 11.4.6: an x or z in `b` is a wildcard; one in `a` where `b` compares
// leaves the answer unknown unless a known position already disagrees.
[[nodiscard]] constexpr auto WildcardEqual(
    AnyConstIntegralView auto a, AnyConstIntegralView auto b) -> FourStateBit {
  std::uint64_t differing = 0;
  std::uint64_t unknown = 0;
  for (std::size_t i = 0; i < a.value.size() && differing == 0U; ++i) {
    const std::uint64_t au = WordOrZero(a.unknown, i);
    const std::uint64_t compared = ~WordOrZero(b.unknown, i);
    differing |= (WordAt(a.value, i) ^ WordAt(b.value, i)) & ~au & compared;
    unknown |= au & compared;
  }
  const bool agree = differing == 0U;
  return FourStateBitOf(agree, agree && unknown != 0U);
}

// LRM 12.5.1 `casez` label match: z on either side is a wildcard, and every
// other position matches exactly on both planes.
[[nodiscard]] constexpr auto CasezMatch(
    AnyConstIntegralView auto a, AnyConstIntegralView auto b) -> bool {
  for (std::size_t i = 0; i < a.value.size(); ++i) {
    const std::uint64_t av = WordAt(a.value, i);
    const std::uint64_t bv = WordAt(b.value, i);
    const std::uint64_t au = WordOrZero(a.unknown, i);
    const std::uint64_t bu = WordOrZero(b.unknown, i);
    const std::uint64_t compared = ~((au & ~av) | (bu & ~bv));
    if ((((av ^ bv) | (au ^ bu)) & compared) != 0U) {
      return false;
    }
  }
  return true;
}

// LRM 12.5.1 `casex` label match: x or z on either side is a wildcard.
[[nodiscard]] constexpr auto CasexMatch(
    AnyConstIntegralView auto a, AnyConstIntegralView auto b) -> bool {
  for (std::size_t i = 0; i < a.value.size(); ++i) {
    const std::uint64_t compared =
        ~(WordOrZero(a.unknown, i) | WordOrZero(b.unknown, i));
    if (((WordAt(a.value, i) ^ WordAt(b.value, i)) & compared) != 0U) {
      return false;
    }
  }
  return true;
}

// LRM 11.4.7: the logical operators read each operand as its truth value, and
// the two operands need not be of one type.
[[nodiscard]] constexpr auto LogicalAnd(
    AnyConstIntegralView auto a, AnyConstIntegralView auto b) -> FourStateBit {
  return detail::TruthAnd(Truth(a), Truth(b));
}

[[nodiscard]] constexpr auto LogicalOr(
    AnyConstIntegralView auto a, AnyConstIntegralView auto b) -> FourStateBit {
  return detail::TruthOr(Truth(a), Truth(b));
}

[[nodiscard]] constexpr auto LogicalNot(AnyConstIntegralView auto a)
    -> FourStateBit {
  return detail::TruthNot(Truth(a));
}

// LRM 11.4.7 `<->`: `(a -> b) && (b -> a)`, unknown where either truth is.
[[nodiscard]] constexpr auto LogicalEquivalence(
    AnyConstIntegralView auto a, AnyConstIntegralView auto b) -> FourStateBit {
  const Truthiness x = Truth(a);
  const Truthiness y = Truth(b);
  return detail::UnknownOr(
      x == Truthiness::kUnknown || y == Truthiness::kUnknown, x == y);
}

// LRM 11.4.11 Table 11-20: the arms of a conditional operator whose condition
// is ambiguous, combined -- a position both arms know and agree on survives,
// and every other position is x.
template <
    PlaneWord WrittenWord, std::size_t kWrittenValueWords,
    std::size_t kWrittenUnknownWords>
constexpr void MergeConditional(
    IntegralViewOf<WrittenWord, kWrittenValueWords, kWrittenUnknownWords> out,
    AnyConstIntegralView auto a, AnyConstIntegralView auto b) {
  for (std::size_t i = 0; i < out.value.size(); ++i) {
    const std::uint64_t agreed =
        ~(WordAt(a.value, i) ^ WordAt(b.value, i)) &
        ~(WordOrZero(a.unknown, i) | WordOrZero(b.unknown, i));
    const std::uint64_t unknown = ~agreed & ValidBitsMask(i, out.width);
    const std::uint64_t value = WordAt(a.value, i) & agreed;
    if (out.unknown.empty()) {
      SetWord(out.value, i, value);
    } else {
      SetWord(out.value, i, value | unknown);
      SetWord(out.unknown, i, unknown);
    }
  }
}

// Two drivers' contributions to one net folded under the table `fold` names.
template <
    PlaneWord WrittenWord, std::size_t kWrittenValueWords,
    std::size_t kWrittenUnknownWords>
constexpr void Resolve(
    IntegralViewOf<WrittenWord, kWrittenValueWords, kWrittenUnknownWords> out,
    AnyConstIntegralView auto a, AnyConstIntegralView auto b,
    NetResolution fold) {
  for (std::size_t i = 0; i < out.value.size(); ++i) {
    const std::uint64_t av = WordAt(a.value, i);
    const std::uint64_t bv = WordAt(b.value, i);
    const std::uint64_t au = WordOrZero(a.unknown, i);
    const std::uint64_t bu = WordOrZero(b.unknown, i);
    const std::uint64_t a_z = ~av & au;
    const std::uint64_t b_z = ~bv & bu;
    const std::uint64_t take_b = a_z;
    const std::uint64_t take_a = ~a_z & b_z;
    const std::uint64_t both = ~a_z & ~b_z;
    std::uint64_t v = (take_b & bv) | (take_a & av);
    std::uint64_t u = (take_b & bu) | (take_a & au);
    switch (fold) {
      case NetResolution::kTriState: {
        const std::uint64_t eq = ~(av ^ bv) & ~(au ^ bu);
        const std::uint64_t conflict = both & ~eq;
        v |= (both & eq & av) | conflict;
        u |= (both & eq & au) | conflict;
        break;
      }
      case NetResolution::kWiredAnd: {
        const std::uint64_t any_zero = (~av & ~au) | (~bv & ~bu);
        const std::uint64_t any_x = (av & au) | (bv & bu);
        const std::uint64_t x_bits = both & ~any_zero & any_x;
        v |= x_bits | (both & ~any_zero & ~any_x);
        u |= x_bits;
        break;
      }
      case NetResolution::kWiredOr: {
        const std::uint64_t any_one = (av & ~au) | (bv & ~bu);
        const std::uint64_t any_x = (av & au) | (bv & bu);
        const std::uint64_t x_bits = both & ~any_one & any_x;
        v |= (both & any_one) | x_bits;
        u |= x_bits;
        break;
      }
    }
    SetWord(out.value, i, v);
    if (!out.unknown.empty()) {
      SetWord(out.unknown, i, u);
    }
  }
}

// A stronger contribution meeting a weaker one: it determines every position
// it drives, which is every position where it is not z, and the weaker one
// determines the rest (LRM 28.12.1).
template <
    PlaneWord WrittenWord, std::size_t kWrittenValueWords,
    std::size_t kWrittenUnknownWords>
constexpr void Dominate(
    IntegralViewOf<WrittenWord, kWrittenValueWords, kWrittenUnknownWords> out,
    AnyConstIntegralView auto stronger, AnyConstIntegralView auto weaker) {
  for (std::size_t i = 0; i < out.value.size(); ++i) {
    const std::uint64_t sv = WordAt(stronger.value, i);
    const std::uint64_t su = WordOrZero(stronger.unknown, i);
    const std::uint64_t undriven = ~sv & su;
    SetWord(
        out.value, i, (sv & ~undriven) | (WordAt(weaker.value, i) & undriven));
    if (!out.unknown.empty()) {
      SetWord(
          out.unknown, i,
          (su & ~undriven) | (WordOrZero(weaker.unknown, i) & undriven));
    }
  }
}

[[nodiscard]] constexpr auto Reduce(AnyConstIntegralView auto a, ReductionOp op)
    -> FourStateBit {
  bool any_zero = false;
  bool any_one = false;
  bool any_unknown = false;
  bool parity = false;
  for (std::size_t i = 0; i < a.value.size(); ++i) {
    const std::uint64_t valid = ValidBitsMask(i, a.width);
    const std::uint64_t unk = WordOrZero(a.unknown, i) & valid;
    const std::uint64_t known = ~unk & valid;
    any_zero = any_zero || (~WordAt(a.value, i) & known) != 0U;
    any_one = any_one || (WordAt(a.value, i) & known) != 0U;
    any_unknown = any_unknown || unk != 0U;
    parity = parity != ((std::popcount(WordAt(a.value, i) & known) & 1) != 0);
  }
  // A known 0 settles the AND and a known 1 the OR whatever else the value
  // holds; an x or z leaves either unknown only where nothing settles it.
  const FourStateBit and_bit =
      FourStateBitOf(!any_zero, !any_zero && any_unknown);
  const FourStateBit or_bit =
      detail::UnknownOr(!any_one && any_unknown, any_one);
  const FourStateBit xor_bit = detail::UnknownOr(any_unknown, parity);
  switch (op) {
    case ReductionOp::kAnd:
      return and_bit;
    case ReductionOp::kOr:
      return or_bit;
    case ReductionOp::kXor:
      return xor_bit;
    case ReductionOp::kNand:
      return Inverted(and_bit);
    case ReductionOp::kNor:
      return Inverted(or_bit);
    case ReductionOp::kXnor:
      return Inverted(xor_bit);
  }
  std::unreachable();
}

// A value of one integral type read as another (LRM 6.24.1, 10.7): the
// source's bits where it reaches and, above them, its sign bit where a signed
// source widens -- an x or z sign filling with itself into four states.
template <
    PlaneWord WrittenWord, std::size_t kWrittenValueWords,
    std::size_t kWrittenUnknownWords>
constexpr void Convert(
    IntegralViewOf<WrittenWord, kWrittenValueWords, kWrittenUnknownWords> out,
    AnyConstIntegralView auto src) {
  const bool extends =
      src.signedness == Signedness::kSigned && out.width > src.width;
  for (std::size_t i = 0; i < out.value.size(); ++i) {
    const std::uint64_t low = static_cast<std::uint64_t>(i) * 64U;
    const std::uint64_t valid = ValidBitsMask(i, out.width);
    const std::uint64_t inside =
        low < src.width ? LowBits(src.width - low) : 0U;
    const std::uint64_t above = extends ? valid & ~inside : 0U;
    const std::uint64_t value = (BitsAt(src.value, low, 64U) & inside & valid) |
                                (BitAt(src.value, src.width - 1U) ? above : 0U);
    const std::uint64_t unknown =
        (BitsAt(src.unknown, low, 64U) & inside & valid) |
        (BitAt(src.unknown, src.width - 1U) ? above : 0U);
    if (out.unknown.empty()) {
      SetWord(out.value, i, value & ~unknown);
    } else {
      SetWord(out.value, i, value);
      SetWord(out.unknown, i, unknown);
    }
  }
}

// LRM 11.5.1: as many bits of `src` as `out` holds, starting at `start`,
// counted from its least significant bit. A position outside the source reads
// x.
template <
    PlaneWord WrittenWord, std::size_t kWrittenValueWords,
    std::size_t kWrittenUnknownWords>
constexpr void Extract(
    IntegralViewOf<WrittenWord, kWrittenValueWords, kWrittenUnknownWords> out,
    AnyConstIntegralView auto src, std::int64_t start) {
  for (std::size_t i = 0; i < out.value.size(); ++i) {
    const detail::SourceWord word =
        detail::SourceWordAt(src, start + (static_cast<std::int64_t>(i) * 64));
    const std::uint64_t valid = ValidBitsMask(i, out.width);
    if (out.unknown.empty()) {
      SetWord(out.value, i, word.value & ~word.unknown & valid);
    } else {
      const std::uint64_t outside = valid & ~word.inside;
      SetWord(out.value, i, (word.value & valid) | outside);
      SetWord(out.unknown, i, (word.unknown & valid) | outside);
    }
  }
}

// The positions a write of `src_width` bits at `start` lands on in a value of
// `dst_width` bits, among the positions `within` names of it -- as a write
// through a part of a part lands nowhere outside the outer part. None, where
// it reaches none of them.
[[nodiscard]] constexpr auto ClampedBitRange(
    std::uint64_t dst_width, std::uint64_t src_width, std::int64_t start,
    BitPositions within) -> std::optional<BitPositions> {
  const std::int64_t from =
      std::max({start, std::int64_t{0}, static_cast<std::int64_t>(within.lsb)});
  const std::int64_t to = std::min(
      {start + static_cast<std::int64_t>(src_width),
       static_cast<std::int64_t>(dst_width),
       static_cast<std::int64_t>(within.lsb + within.width)});
  if (from >= to) {
    return std::nullopt;
  }
  return BitPositions{
      .lsb = static_cast<std::uint64_t>(from),
      .width = static_cast<std::uint64_t>(to - from)};
}

// LRM 11.5.1: writes `src` into `dst` at `start`, landing only on the
// positions of `dst` that `within` names and leaving every other position as
// it stands. Answers the positions written, none where it reaches none. The
// value written into is left clear above its width even where it was not, as
// it is where another member of an untagged union wrote the storage it lies
// in (LRM 7.3.2): a write leaves the value its type holds.
template <
    PlaneWord WrittenWord, std::size_t kWrittenValueWords,
    std::size_t kWrittenUnknownWords>
constexpr auto InsertWithin(
    IntegralViewOf<WrittenWord, kWrittenValueWords, kWrittenUnknownWords> dst,
    AnyConstIntegralView auto src, std::int64_t start, BitPositions within)
    -> std::optional<BitPositions> {
  const std::optional<BitPositions> written =
      ClampedBitRange(dst.width, src.width, start, within);
  if (written) {
    detail::WriteRun(dst, src, start, *written);
  }
  ClearAboveWidth(dst.value, dst.width);
  ClearAboveWidth(dst.unknown, dst.width);
  return written;
}

// The same write over the whole of `dst`: a position outside the destination
// is not written.
template <
    PlaneWord WrittenWord, std::size_t kWrittenValueWords,
    std::size_t kWrittenUnknownWords>
constexpr auto Insert(
    IntegralViewOf<WrittenWord, kWrittenValueWords, kWrittenUnknownWords> dst,
    AnyConstIntegralView auto src, std::int64_t start)
    -> std::optional<BitPositions> {
  return InsertWithin(
      dst, src, start, BitPositions{.lsb = 0, .width = dst.width});
}

// `into` with the positions `run` names taken from `from`, a value of its
// width: what a procedural continuous assignment holding them shows there (LRM
// 10.6.2).
template <
    PlaneWord WrittenWord, std::size_t kWrittenValueWords,
    std::size_t kWrittenUnknownWords>
constexpr void TakePositions(
    IntegralViewOf<WrittenWord, kWrittenValueWords, kWrittenUnknownWords> out,
    AnyConstIntegralView auto into, AnyConstIntegralView auto from,
    BitPositions run) {
  detail::Copy(into.value, out.value);
  detail::Copy(into.unknown, out.unknown);
  InsertWithin(out, from, 0, run);
}

// LRM 11.4.10: a shift moves every bit by the number the amount holds, read
// unsigned, and an amount holding an x or z makes the whole result x. An
// amount at or past the width moves every bit out.
template <
    PlaneWord WrittenWord, std::size_t kWrittenValueWords,
    std::size_t kWrittenUnknownWords>
constexpr void ShiftLeft(
    IntegralViewOf<WrittenWord, kWrittenValueWords, kWrittenUnknownWords> out,
    AnyConstIntegralView auto a, AnyConstIntegralView auto amount) {
  if (HasUnknown(amount)) {
    FillScalar(out, FourStateBit::kUnknown);
    return;
  }
  const std::uint64_t by = detail::ShiftCount(amount.value);
  detail::ShiftLeftWords(a.value, by, out.value, out.width);
  if (!out.unknown.empty()) {
    detail::ShiftLeftWords(a.unknown, by, out.unknown, out.width);
  }
}

template <
    PlaneWord WrittenWord, std::size_t kWrittenValueWords,
    std::size_t kWrittenUnknownWords>
constexpr void LogicalShiftRight(
    IntegralViewOf<WrittenWord, kWrittenValueWords, kWrittenUnknownWords> out,
    AnyConstIntegralView auto a, AnyConstIntegralView auto amount) {
  if (HasUnknown(amount)) {
    FillScalar(out, FourStateBit::kUnknown);
    return;
  }
  detail::ShiftRightPlanes(out, a, detail::ShiftCount(amount.value), false);
}

// `>>>` fills from the sign of a signed operand and with 0 from an unsigned
// one.
template <
    PlaneWord WrittenWord, std::size_t kWrittenValueWords,
    std::size_t kWrittenUnknownWords>
constexpr void ArithmeticShiftRight(
    IntegralViewOf<WrittenWord, kWrittenValueWords, kWrittenUnknownWords> out,
    AnyConstIntegralView auto a, AnyConstIntegralView auto amount) {
  if (HasUnknown(amount)) {
    FillScalar(out, FourStateBit::kUnknown);
    return;
  }
  detail::ShiftRightPlanes(
      out, a, detail::ShiftCount(amount.value),
      a.signedness == Signedness::kSigned);
}

// Past this magnitude a position lies outside every value: no packed value is
// this wide and no unpacked one holds this many elements. Bounding a position
// here also keeps the arithmetic that composes positions inside a word.
inline constexpr std::int64_t kPositionLimit = std::int64_t{1} << 32;

// How wide the type position arithmetic is done in is: a signed four-state
// value of this many bits, so an index of any width shifts by a declared range
// without wrapping and one holding x or z stays unknown through every step.
inline constexpr std::uint64_t kPositionWidth = 64;

// The position an index names (LRM 11.5.1, 7.4.5): the number it holds, or
// none where it holds x or z or a magnitude past every value.
[[nodiscard]] constexpr auto PositionNamed(AnyConstIntegralView auto index)
    -> std::optional<std::int64_t> {
  if (HasUnknown(index)) {
    return std::nullopt;
  }
  const bool negative = index.signedness == Signedness::kSigned &&
                        BitAt(index.value, index.width - 1U);
  // The number fits a machine word exactly when every bit above the low 63
  // repeats its sign.
  const std::uint64_t low =
      WordOrZero(index.value, 0) |
      (negative && index.width < 64U ? ~LowBits(index.width) : 0U);
  if (index.width >= 64U && ((low >> 63U) != 0U) != negative) {
    return std::nullopt;
  }
  for (std::size_t i = 1; i < index.value.size(); ++i) {
    if (WordAt(index.value, i) !=
        (negative ? ValidBitsMask(i, index.width) : 0U)) {
      return std::nullopt;
    }
  }
  const auto at = static_cast<std::int64_t>(low);
  if (at < -kPositionLimit || at > kPositionLimit) {
    return std::nullopt;
  }
  return at;
}

// The position an index names as a value of the position type: all x where it
// names none.
template <
    PlaneWord WrittenWord, std::size_t kWrittenValueWords,
    std::size_t kWrittenUnknownWords>
constexpr void ToPosition(
    IntegralViewOf<WrittenWord, kWrittenValueWords, kWrittenUnknownWords> out,
    AnyConstIntegralView auto index) {
  const std::optional<std::int64_t> named = PositionNamed(index);
  if (named) {
    FromInt(out, *named);
  } else {
    FillScalar(out, FourStateBit::kUnknown);
  }
}

// LRM 11.5.1: as many bits of `src` as `out` holds from the position named,
// which read as a declaration's default does where the position names none.
template <
    PlaneWord WrittenWord, std::size_t kWrittenValueWords,
    std::size_t kWrittenUnknownWords>
constexpr void Slice(
    IntegralViewOf<WrittenWord, kWrittenValueWords, kWrittenUnknownWords> out,
    AnyConstIntegralView auto src, AnyConstIntegralView auto position) {
  const std::optional<std::int64_t> start = PositionNamed(position);
  if (start) {
    Extract(out, src, *start);
  } else {
    FillDefault(out);
  }
}

// LRM 11.5.1: `bits` written into `dst` at the position named, which writes
// nothing where the position names none. Answers the positions written.
template <
    PlaneWord WrittenWord, std::size_t kWrittenValueWords,
    std::size_t kWrittenUnknownWords>
constexpr auto WithSlice(
    IntegralViewOf<WrittenWord, kWrittenValueWords, kWrittenUnknownWords> dst,
    AnyConstIntegralView auto position, AnyConstIntegralView auto bits)
    -> std::optional<BitPositions> {
  const std::optional<std::int64_t> start = PositionNamed(position);
  if (!start) {
    return std::nullopt;
  }
  return Insert(dst, bits, *start);
}

// LRM 11.4.12: `high` in the most significant positions and `low` below it.
// The answer is as wide as the two together.
template <
    PlaneWord WrittenWord, std::size_t kWrittenValueWords,
    std::size_t kWrittenUnknownWords>
constexpr void Concat(
    IntegralViewOf<WrittenWord, kWrittenValueWords, kWrittenUnknownWords> out,
    AnyConstIntegralView auto high, AnyConstIntegralView auto low) {
  detail::Fill(out.value, 0U);
  detail::Fill(out.unknown, 0U);
  Insert(out, low, 0);
  Insert(out, high, static_cast<std::int64_t>(low.width));
}

// LRM 11.4.12.1: as many copies of `a` as fill the answer, laid end to end.
template <
    PlaneWord WrittenWord, std::size_t kWrittenValueWords,
    std::size_t kWrittenUnknownWords>
constexpr void Replicate(
    IntegralViewOf<WrittenWord, kWrittenValueWords, kWrittenUnknownWords> out,
    AnyConstIntegralView auto a) {
  detail::Fill(out.value, 0U);
  detail::Fill(out.unknown, 0U);
  for (std::uint64_t at = 0; at < out.width; at += a.width) {
    Insert(out, a, static_cast<std::int64_t>(at));
  }
}

// LRM 20.8.1 `$clog2`: the operand read as unsigned, and 0 answering 0. The
// clause does not say what an x or z bit reads as; each reads as 0 here.
[[nodiscard]] constexpr auto CeilLog2(AnyConstIntegralView auto a)
    -> std::int64_t {
  std::int64_t high = -1;
  int set = 0;
  for (std::size_t i = 0; i < a.value.size(); ++i) {
    const std::uint64_t word = WordAt(a.value, i) & ~WordOrZero(a.unknown, i) &
                               ValidBitsMask(i, a.width);
    if (word == 0U) {
      continue;
    }
    set += std::popcount(word);
    high = static_cast<std::int64_t>(
        (64U * i) + 63U - static_cast<std::uint64_t>(std::countl_zero(word)));
  }
  return high < 0 ? 0 : high + (set > 1 ? 1 : 0);
}

// LRM 20.9 `$countbits`: how many positions of `a` hold one of the scalars the
// control bits name. Each position of `control` names the scalar it holds, and
// a scalar several of them name counts once.
[[nodiscard]] constexpr auto CountBits(
    AnyConstIntegralView auto a, AnyConstIntegralView auto control)
    -> std::int64_t {
  bool zero = false;
  bool one = false;
  bool high_impedance = false;
  bool unknown = false;
  for (std::size_t i = 0; i < control.value.size(); ++i) {
    const std::uint64_t valid = ValidBitsMask(i, control.width);
    const std::uint64_t v = WordAt(control.value, i);
    const std::uint64_t u = WordOrZero(control.unknown, i);
    zero = zero || (~v & ~u & valid) != 0U;
    one = one || (v & ~u) != 0U;
    high_impedance = high_impedance || (~v & u) != 0U;
    unknown = unknown || (v & u) != 0U;
  }
  std::int64_t count = 0;
  for (std::size_t i = 0; i < a.value.size(); ++i) {
    const std::uint64_t valid = ValidBitsMask(i, a.width);
    const std::uint64_t v = WordAt(a.value, i);
    const std::uint64_t u = WordOrZero(a.unknown, i);
    const std::uint64_t counted =
        (zero ? ~v & ~u & valid : 0U) | (one ? v & ~u : 0U) |
        (high_impedance ? ~v & u : 0U) | (unknown ? v & u : 0U);
    count += std::popcount(counted);
  }
  return count;
}

// LRM 5.9: `bytes` read as a value whose last byte is the least significant,
// held at `width` bits -- a shorter run leaves the positions above it 0 and a
// longer one loses its leading bytes. Bytes carry no unknown state, so the
// unknown plane is clear.
constexpr void FromBytes(IntegralView out, std::span<const char> bytes) {
  detail::Fill(out.value, 0U);
  detail::Fill(out.unknown, 0U);
  for (std::size_t k = 0; k < bytes.size(); ++k) {
    const std::uint64_t at = 8U * static_cast<std::uint64_t>(k);
    if (at >= out.width) {
      break;
    }
    const auto byte = static_cast<std::uint64_t>(
        static_cast<unsigned char>(bytes[bytes.size() - 1U - k]));
    const std::uint64_t count = std::min<std::uint64_t>(8U, out.width - at);
    const std::uint64_t word = byte & LowBits(count);
    out.value[static_cast<std::size_t>(at / 64U)] |= word << (at % 64U);
  }
}

// How many bits one digit of a radix covers.
[[nodiscard]] constexpr auto BitsPerDigit(DigitRadix radix) -> std::uint64_t {
  switch (radix) {
    case DigitRadix::kBinary:
      return 1;
    case DigitRadix::kOctal:
      return 3;
    case DigitRadix::kHex:
      return 4;
  }
  std::unreachable();
}

// The number a character holds as a digit, in whichever radix up to sixteen
// has that digit; none for a character that is a digit of no radix.
[[nodiscard]] constexpr auto DigitOf(char c) -> std::optional<std::uint64_t> {
  if (c >= '0' && c <= '9') {
    return static_cast<std::uint64_t>(c - '0');
  }
  if (c >= 'a' && c <= 'f') {
    return static_cast<std::uint64_t>(c - 'a') + 10U;
  }
  if (c >= 'A' && c <= 'F') {
    return static_cast<std::uint64_t>(c - 'A') + 10U;
  }
  return std::nullopt;
}

// The number the digit `c` holds in `radix`, where it is a digit of it.
[[nodiscard]] constexpr auto DigitOf(char c, DigitRadix radix)
    -> std::optional<std::uint64_t> {
  const std::optional<std::uint64_t> number = DigitOf(c);
  if (!number || (*number >> BitsPerDigit(radix)) != 0U) {
    return std::nullopt;
  }
  return number;
}

// Whether `c` stands for a whole digit of x or of z (LRM 5.7.1), `?` being z.
[[nodiscard]] constexpr auto IsUnknownDigit(char c) -> bool {
  return c == 'x' || c == 'X';
}
[[nodiscard]] constexpr auto IsHighImpedanceDigit(char c) -> bool {
  return c == 'z' || c == 'Z' || c == '?';
}

// Whether `c` is a digit of `radix`, an x, z or ? digit among them.
[[nodiscard]] constexpr auto IsDigit(char c, DigitRadix radix) -> bool {
  return IsUnknownDigit(c) || IsHighImpedanceDigit(c) ||
         DigitOf(c, radix).has_value();
}

// LRM 21.4: whether a word is a number of `radix` -- at least one digit of it,
// separated by any `_`.
[[nodiscard]] constexpr auto IsDigits(DigitRadix radix, std::string_view digits)
    -> bool {
  bool any_digit = false;
  for (const char c : digits) {
    if (c == '_') {
      continue;
    }
    if (!IsDigit(c, radix)) {
      return false;
    }
    any_digit = true;
  }
  return any_digit;
}

// LRM 21.4: a word of digits read as the value `out` is, its last digit the
// least significant -- a shorter word leaves the positions above it 0 and a
// longer one loses its leading digits. `_` separates digits; an x digit is x
// in every bit it covers and a z or ? digit z in every one. The word is a
// number of the radix, which whoever took it from text has already asked.
constexpr void FromDigits(
    IntegralView out, DigitRadix radix, std::string_view digits) {
  const std::uint64_t bits_per_digit = BitsPerDigit(radix);
  const std::uint64_t all = LowBits(bits_per_digit);
  const bool four_state = out.IsFourState();
  detail::Fill(out.value, 0U);
  detail::Fill(out.unknown, 0U);
  // One digit's bits set where the digit lies, which may cross into the next
  // word.
  const auto place = [&](std::span<std::uint64_t> plane, std::uint64_t at,
                         std::uint64_t bits) {
    const detail::WordStep step = detail::StepAt(at, bits_per_digit);
    plane[step.word] |= bits << step.offset;
    if (step.count < bits_per_digit && step.word + 1U < plane.size()) {
      plane[step.word + 1U] |= bits >> step.count;
    }
  };
  std::uint64_t at = 0;
  for (std::size_t k = digits.size(); k-- > 0;) {
    const char c = digits[k];
    if (c == '_') {
      continue;
    }
    std::uint64_t value_bits = 0;
    std::uint64_t unknown_bits = 0;
    if (IsUnknownDigit(c)) {
      value_bits = all;
      unknown_bits = all;
    } else if (IsHighImpedanceDigit(c)) {
      unknown_bits = all;
    } else {
      value_bits = DigitOf(c, radix).value_or(0U);
    }
    if (at < out.width && four_state) {
      place(out.value, at, value_bits);
      place(out.unknown, at, unknown_bits);
    } else if (at < out.width && unknown_bits == 0U) {
      place(out.value, at, value_bits);
    }
    at += bits_per_digit;
  }
  ClearAboveWidth(out.value, out.width);
  ClearAboveWidth(out.unknown, out.width);
}

// The byte `index` positions up from a value's least significant end, a
// position past the width reading 0; none where one of its bits holds x or z.
[[nodiscard]] constexpr auto ByteAt(
    AnyConstIntegralView auto a, std::uint64_t index)
    -> std::optional<std::uint8_t> {
  const std::uint64_t at = 8U * index;
  if (at >= a.width) {
    return std::uint8_t{0};
  }
  const std::uint64_t count = std::min<std::uint64_t>(8U, a.width - at);
  if (BitsAt(a.unknown, at, count) != 0U) {
    return std::nullopt;
  }
  return static_cast<std::uint8_t>(BitsAt(a.value, at, count));
}

// LRM 11.4.14.2: `a`'s `block`-wide blocks in reversed order, counted from its
// least significant bit, the bits inside each left where they are. The last
// block is whatever the width leaves over and is not padded.
constexpr void ReverseBlocks(
    IntegralView out, ConstIntegralView a, std::uint64_t block) {
  detail::Fill(out.value, 0U);
  detail::Fill(out.unknown, 0U);
  for (std::uint64_t low = 0; low < out.width; low += block) {
    const std::uint64_t count = std::min(block, out.width - low);
    const std::uint64_t to = out.width - low - count;
    InsertWithin(
        out, a, static_cast<std::int64_t>(to) - static_cast<std::int64_t>(low),
        BitPositions{.lsb = to, .width = count});
  }
}

}  // namespace lyra::value
