#pragma once

#include <algorithm>
#include <bit>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string_view>
#include <utility>
#include <vector>

// Whether an operation is folded into what calls it is decided by the build
// that compiles the caller, because the two builds want opposite things of the
// same text. An optimized build inlines every short operation, so that one on
// a value no wider than a machine word reduces to the instructions its width
// leaves: the planes are arrays whose size the type fixes, and nothing folds
// across a call. An unoptimized build folds nothing, so inlining there would
// only copy an operation's body into each use; it calls, and a unit holds one
// copy of each operation it uses. An operation whose work grows with the value
// -- a product or a quotient of many words, a power, a run of digits -- is
// never forced in either build.
#ifdef __OPTIMIZE__
#define LYRA_FOLDED [[gnu::always_inline]]
#else
#define LYRA_FOLDED
#endif

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
// over planes and a width, settling what the operator does with an x or a z
// and answering as the language says the operator answers. It takes the planes
// and the width as arguments, so the one statement serves every width:
// generated code for a value the width of a machine word reaches it with a
// single word, and a value wider than one with as many as its type fixes. A
// two-state value passes an empty unknown plane, which reads as clear
// everywhere, and a two-state answer holds an x or a z as 0.

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
  switch (bit) {
    case FourStateBit::kZero:
      return FourStateBit::kOne;
    case FourStateBit::kOne:
      return FourStateBit::kZero;
    case FourStateBit::kHighImpedance:
    case FourStateBit::kUnknown:
      return FourStateBit::kUnknown;
  }
  std::unreachable();
}

// LRM 11.4.7 logical AND of two scalars: 0 where either is, 1 where both are,
// and unknown otherwise.
[[nodiscard]] constexpr auto LogicalAnd(FourStateBit a, FourStateBit b)
    -> FourStateBit {
  if (a == FourStateBit::kZero || b == FourStateBit::kZero) {
    return FourStateBit::kZero;
  }
  if (a == FourStateBit::kOne && b == FourStateBit::kOne) {
    return FourStateBit::kOne;
  }
  return FourStateBit::kUnknown;
}

// The planes of one integral value, read.
struct ConstPlanes {
  std::span<const std::uint64_t> value;
  std::span<const std::uint64_t> unknown;
};

// The planes of one integral value, written.
struct Planes {
  std::span<std::uint64_t> value;
  std::span<std::uint64_t> unknown;

  [[nodiscard]] LYRA_FOLDED constexpr auto AsConst() const -> ConstPlanes {
    return ConstPlanes{.value = value, .unknown = unknown};
  }
};

// An integral value of a type the code reading it was compiled without: its
// planes, its width and its signedness. It can hold x or z exactly when it has
// an unknown plane. Code compiled once for every integral type -- the
// formatter, a file read, a memory image -- takes a value this way, and the
// caller, which knows the type, states it.
struct ConstIntegralView {
  ConstPlanes planes;
  std::uint64_t width = 0;
  Signedness signedness = Signedness::kUnsigned;

  [[nodiscard]] constexpr auto IsFourState() const -> bool {
    return !planes.unknown.empty();
  }
};

// The same for a value such code writes, which is told how many bits to write
// and reads nothing as a number.
struct IntegralView {
  Planes planes;
  std::uint64_t width = 0;

  [[nodiscard]] constexpr auto IsFourState() const -> bool {
    return !planes.unknown.empty();
  }
};

[[nodiscard]] LYRA_FOLDED constexpr auto WordCountForBits(
    std::uint64_t bit_width) -> std::size_t {
  return static_cast<std::size_t>(
      (bit_width / 64U) + (bit_width % 64U == 0U ? 0U : 1U));
}

// The low `count` bits of a word set, all of them from 64 up.
[[nodiscard]] LYRA_FOLDED constexpr auto LowBits(std::uint64_t count)
    -> std::uint64_t {
  return count >= 64U ? ~std::uint64_t{0} : (std::uint64_t{1} << count) - 1U;
}

// The bits of word `word_index` that lie below `bit_width`.
[[nodiscard]] LYRA_FOLDED constexpr auto ValidBitsMask(
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

// Word `index` of a plane, a word the plane does not have reading as clear --
// which is how the unknown plane of a two-state value reads.
[[nodiscard]] LYRA_FOLDED constexpr auto WordAt(
    std::span<const std::uint64_t> words, std::size_t index) -> std::uint64_t {
  return index < words.size() ? words[index] : std::uint64_t{0};
}

// One word's worth of a plane beginning at `offset`, right-aligned, keeping
// `count` positions. A position the plane does not reach reads as clear.
[[nodiscard]] LYRA_FOLDED constexpr auto BitsAt(
    std::span<const std::uint64_t> words, std::uint64_t offset,
    std::uint64_t count) -> std::uint64_t {
  // A plane of one word is read as a shift of that word, which is every read
  // of a value no wider than a machine word.
  if (words.size() == 1U) {
    return offset < 64U ? (words[0] >> offset) & LowBits(count) : 0U;
  }
  const auto word = static_cast<std::size_t>(offset / 64U);
  const std::uint64_t shift = offset % 64U;
  std::uint64_t bits = WordAt(words, word) >> shift;
  if (shift != 0U) {
    bits |= WordAt(words, word + 1U) << (64U - shift);
  }
  return bits & LowBits(count);
}

[[nodiscard]] LYRA_FOLDED constexpr auto BitAt(
    std::span<const std::uint64_t> words, std::uint64_t position) -> bool {
  return ((WordAt(words, static_cast<std::size_t>(position / 64U)) >>
           (position % 64U)) &
          1U) != 0U;
}

// Every position of a plane at or above `width` cleared, which is the form a
// plane is held in.
LYRA_FOLDED constexpr void ClearAboveWidth(
    std::span<std::uint64_t> words, std::uint64_t width) {
  if (!words.empty()) {
    words.back() &= ValidBitsMask(words.size() - 1U, width);
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
[[nodiscard]] LYRA_FOLDED constexpr auto StepAt(
    std::uint64_t position, std::uint64_t remaining) -> WordStep {
  const std::uint64_t offset = position % 64U;
  return WordStep{
      .word = static_cast<std::size_t>(position / 64U),
      .offset = offset,
      .count = std::min<std::uint64_t>(64U - offset, remaining)};
}

[[nodiscard]] LYRA_FOLDED constexpr auto AnySet(
    std::span<const std::uint64_t> words) -> bool {
  std::uint64_t set = 0;
  for (const std::uint64_t word : words) {
    set |= word;
  }
  return set != 0U;
}

LYRA_FOLDED constexpr void Fill(
    std::span<std::uint64_t> words, std::uint64_t word) {
  for (std::uint64_t& filled : words) {
    filled = word;
  }
}

LYRA_FOLDED constexpr void Copy(
    std::span<const std::uint64_t> src, std::span<std::uint64_t> dst) {
  for (std::size_t i = 0; i < dst.size(); ++i) {
    dst[i] = WordAt(src, i);
  }
}

[[nodiscard]] LYRA_FOLDED constexpr auto ScalarOf(bool holds) -> FourStateBit {
  return holds ? FourStateBit::kOne : FourStateBit::kZero;
}

// `dst = a + b` over the width, with each operand's missing words reading as
// clear.
LYRA_FOLDED constexpr void AddWords(
    std::span<const std::uint64_t> a, std::span<const std::uint64_t> b,
    std::span<std::uint64_t> dst, std::uint64_t width) {
  std::uint64_t carry = 0;
  for (std::size_t i = 0; i < dst.size(); ++i) {
    const std::uint64_t aw = WordAt(a, i);
    const std::uint64_t s1 = aw + WordAt(b, i);
    const std::uint64_t c1 = s1 < aw ? 1U : 0U;
    const std::uint64_t s2 = s1 + carry;
    const std::uint64_t c2 = s2 < s1 ? 1U : 0U;
    dst[i] = s2;
    carry = c1 + c2;
  }
  ClearAboveWidth(dst, width);
}

LYRA_FOLDED constexpr void SubWords(
    std::span<const std::uint64_t> a, std::span<const std::uint64_t> b,
    std::span<std::uint64_t> dst, std::uint64_t width) {
  std::uint64_t borrow = 0;
  for (std::size_t i = 0; i < dst.size(); ++i) {
    const std::uint64_t aw = WordAt(a, i);
    const std::uint64_t bw = WordAt(b, i);
    const std::uint64_t d1 = aw - bw;
    const std::uint64_t b1 = aw < bw ? 1U : 0U;
    const std::uint64_t d2 = d1 - borrow;
    const std::uint64_t b2 = d1 < borrow ? 1U : 0U;
    dst[i] = d2;
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
          (static_cast<unsigned __int128>(a[i]) * WordAt(b, j)) + dst[i + j] +
          carry;
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
[[nodiscard]] LYRA_FOLDED constexpr auto Order(
    std::span<const std::uint64_t> a, std::span<const std::uint64_t> b,
    std::uint64_t width, Signedness signedness) -> int {
  const std::uint64_t sign = signedness == Signedness::kSigned
                                 ? std::uint64_t{1} << ((width - 1U) % 64U)
                                 : 0U;
  for (std::size_t i = a.size(); i-- > 0;) {
    const std::uint64_t inverted = i + 1U == a.size() ? sign : 0U;
    const std::uint64_t aw = a[i] ^ inverted;
    const std::uint64_t bw = WordAt(b, i) ^ inverted;
    if (aw != bw) {
      return aw < bw ? -1 : 1;
    }
  }
  return 0;
}

// The number a shift amount holds, saturated at a value no width reaches.
[[nodiscard]] LYRA_FOLDED constexpr auto ShiftCount(
    std::span<const std::uint64_t> amount) -> std::uint64_t {
  std::uint64_t above = 0;
  for (std::size_t i = 1; i < amount.size(); ++i) {
    above |= amount[i];
  }
  return above != 0U ? ~std::uint64_t{0} : WordAt(amount, 0);
}

LYRA_FOLDED constexpr void ShiftLeftWords(
    std::span<const std::uint64_t> src, std::uint64_t amount,
    std::span<std::uint64_t> dst, std::uint64_t width) {
  Fill(dst, 0U);
  if (amount >= width) {
    return;
  }
  const auto word_shift = static_cast<std::size_t>(amount / 64U);
  const std::uint64_t bit_shift = amount % 64U;
  for (std::size_t i = dst.size(); i-- > word_shift;) {
    const std::size_t from = i - word_shift;
    std::uint64_t word = WordAt(src, from) << bit_shift;
    if (bit_shift != 0U && from > 0U) {
      word |= WordAt(src, from - 1U) >> (64U - bit_shift);
    }
    dst[i] = word;
  }
  ClearAboveWidth(dst, width);
}

// Shifts toward the least significant end, filling the vacated top positions
// with `fill`.
LYRA_FOLDED constexpr void ShiftRightWords(
    std::span<const std::uint64_t> src, std::uint64_t amount, bool fill,
    std::span<std::uint64_t> dst, std::uint64_t width) {
  const std::uint64_t filled = std::min(amount, width);
  for (std::size_t i = 0; i < dst.size(); ++i) {
    const std::uint64_t low = static_cast<std::uint64_t>(i) * 64U;
    dst[i] = filled == width || low >= width - filled
                 ? 0U
                 : BitsAt(src, low + filled, 64U);
  }
  ClearAboveWidth(dst, width);
  if (!fill) {
    return;
  }
  for (std::uint64_t set = 0U; set < filled;) {
    const WordStep step = StepAt(width - filled + set, filled - set);
    dst[step.word] |= LowBits(step.count) << step.offset;
    set += step.count;
  }
}

// A plane moved toward the least significant end by `amount`, each plane of a
// signed value filled from its own top position where the shift is arithmetic,
// so an x at the top extends down.
LYRA_FOLDED constexpr void ShiftRightPlanes(
    Planes out, ConstPlanes a, std::uint64_t amount, std::uint64_t width,
    bool arithmetic) {
  ShiftRightWords(
      a.value, amount, arithmetic && BitAt(a.value, width - 1U), out.value,
      width);
  if (!out.unknown.empty()) {
    ShiftRightWords(
        a.unknown, amount, arithmetic && BitAt(a.unknown, width - 1U),
        out.unknown, width);
  }
}

// The low `width` bits of a word read as a two's-complement number.
[[nodiscard]] LYRA_FOLDED constexpr auto SignExtended(
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
[[nodiscard]] LYRA_FOLDED constexpr auto QuotientWord(
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

[[nodiscard]] LYRA_FOLDED constexpr auto RemainderWord(
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
  std::vector<std::uint64_t> dividend(words);
  std::vector<std::uint64_t> divisor(words);
  if (division.dividend_negative) {
    SubWords({}, a, dividend, width);
  } else {
    Copy(a, dividend);
  }
  if (division.divisor_negative) {
    SubWords({}, b, divisor, width);
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
    SubWords({}, magnitude, out, width);
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

[[nodiscard]] LYRA_FOLDED constexpr auto SourceWordAt(
    ConstPlanes src, std::uint64_t width, std::int64_t offset) -> SourceWord {
  if (offset <= -64 || offset >= static_cast<std::int64_t>(width)) {
    return SourceWord{.value = 0U, .unknown = 0U, .inside = 0U};
  }
  if (offset < 0) {
    const auto up = static_cast<std::uint64_t>(-offset);
    return SourceWord{
        .value = WordAt(src.value, 0) << up,
        .unknown = WordAt(src.unknown, 0) << up,
        .inside = LowBits(width) << up};
  }
  const auto from = static_cast<std::uint64_t>(offset);
  return SourceWord{
      .value = BitsAt(src.value, from, 64U),
      .unknown = BitsAt(src.unknown, from, 64U),
      .inside = LowBits(width - from)};
}

// Writes the positions `run` names of `dst` from `src`, a value of `src_width`
// bits whose least significant bit lies at position `start` of `dst`, and
// leaves every other position as it stands. A two-state source landing in
// four-state storage settles the positions it lands on, and an x or z landing
// in two-state storage lands as 0.
LYRA_FOLDED constexpr void WriteRun(
    Planes dst, ConstPlanes src, std::uint64_t src_width, std::int64_t start,
    BitPositions run) {
  const std::uint64_t end = run.lsb + run.width;
  const std::size_t last = std::min(
      static_cast<std::size_t>((end - 1U) / 64U), dst.value.size() - 1U);
  for (auto i = static_cast<std::size_t>(run.lsb / 64U); i <= last; ++i) {
    const std::uint64_t low = static_cast<std::uint64_t>(i) * 64U;
    const std::uint64_t from = std::max(run.lsb, low) - low;
    const std::uint64_t to = std::min(end, low + 64U) - low;
    const std::uint64_t mask = LowBits(to - from) << from;
    const SourceWord word =
        SourceWordAt(src, src_width, static_cast<std::int64_t>(low) - start);
    if (dst.unknown.empty()) {
      dst.value[i] =
          (dst.value[i] & ~mask) | (word.value & ~word.unknown & mask);
    } else {
      dst.value[i] = (dst.value[i] & ~mask) | (word.value & mask);
      dst.unknown[i] = (dst.unknown[i] & ~mask) | (word.unknown & mask);
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

// LRM 11.4.7: the logical operators' tables over their operands' truth
// values. An answer is unknown only where an operand's truth is and the other
// operand does not settle it.
[[nodiscard]] LYRA_FOLDED constexpr auto TruthAnd(Truthiness a, Truthiness b)
    -> FourStateBit {
  if (a == Truthiness::kKnownZero || b == Truthiness::kKnownZero) {
    return FourStateBit::kZero;
  }
  if (a == Truthiness::kKnownNonzero && b == Truthiness::kKnownNonzero) {
    return FourStateBit::kOne;
  }
  return FourStateBit::kUnknown;
}

[[nodiscard]] LYRA_FOLDED constexpr auto TruthOr(Truthiness a, Truthiness b)
    -> FourStateBit {
  if (a == Truthiness::kKnownNonzero || b == Truthiness::kKnownNonzero) {
    return FourStateBit::kOne;
  }
  if (a == Truthiness::kKnownZero && b == Truthiness::kKnownZero) {
    return FourStateBit::kZero;
  }
  return FourStateBit::kUnknown;
}

[[nodiscard]] LYRA_FOLDED constexpr auto TruthNot(Truthiness a)
    -> FourStateBit {
  switch (a) {
    case Truthiness::kKnownZero:
      return FourStateBit::kOne;
    case Truthiness::kKnownNonzero:
      return FourStateBit::kZero;
    case Truthiness::kUnknown:
      return FourStateBit::kUnknown;
  }
  std::unreachable();
}

}  // namespace detail

// Every position of `out` holding one scalar.
LYRA_FOLDED constexpr void FillScalar(
    Planes out, std::uint64_t width, FourStateBit bit) {
  const bool value = ValueBitOf(bit);
  const bool unknown = UnknownBitOf(bit);
  if (out.unknown.empty()) {
    detail::Fill(out.value, value && !unknown ? ~std::uint64_t{0} : 0U);
    ClearAboveWidth(out.value, width);
    return;
  }
  detail::Fill(out.value, value ? ~std::uint64_t{0} : 0U);
  detail::Fill(out.unknown, unknown ? ~std::uint64_t{0} : 0U);
  ClearAboveWidth(out.value, width);
  ClearAboveWidth(out.unknown, width);
}

// The value a declaration holds before anything writes it (LRM Table 6-7): x in
// every position of a four-state type, 0 in every position of a two-state one.
LYRA_FOLDED constexpr void FillDefault(Planes out, std::uint64_t width) {
  FillScalar(out, width, FourStateBit::kUnknown);
}

[[nodiscard]] LYRA_FOLDED constexpr auto HasUnknown(ConstPlanes a) -> bool {
  return detail::AnySet(a.unknown);
}

// A value read from an integer: the low word is `value`, and every word above
// repeats its sign, which is what makes `-1` fill a wide destination (LRM
// 11.6.1).
LYRA_FOLDED constexpr void FromInt(
    Planes out, std::uint64_t width, std::int64_t value) {
  detail::Fill(out.value, value < 0 ? ~std::uint64_t{0} : 0U);
  out.value[0] = static_cast<std::uint64_t>(value);
  ClearAboveWidth(out.value, width);
  detail::Fill(out.unknown, 0U);
}

// The low 64 bits as a signed integer, sign-extended from the width when the
// value is signed. An x or z bit reads as 0 (LRM 6.12.1).
[[nodiscard]] LYRA_FOLDED constexpr auto ToInt64(
    ConstPlanes a, std::uint64_t width, Signedness signedness) -> std::int64_t {
  const std::uint64_t raw = WordAt(a.value, 0) & ~WordAt(a.unknown, 0);
  if (signedness == Signedness::kUnsigned || width >= 64U) {
    return static_cast<std::int64_t>(raw);
  }
  return detail::SignExtended(raw, width);
}

[[nodiscard]] LYRA_FOLDED constexpr auto Truth(ConstPlanes a) -> Truthiness {
  bool unknown_bit = false;
  for (std::size_t i = 0; i < a.value.size(); ++i) {
    const std::uint64_t unk = WordAt(a.unknown, i);
    if ((a.value[i] & ~unk) != 0U) {
      return Truthiness::kKnownNonzero;
    }
    unknown_bit = unknown_bit || unk != 0U;
  }
  return unknown_bit ? Truthiness::kUnknown : Truthiness::kKnownZero;
}

// Bit 0's scalar, which is the bit an edge is detected on (LRM 9.4.2).
[[nodiscard]] LYRA_FOLDED constexpr auto LeastSignificantBit(ConstPlanes a)
    -> FourStateBit {
  return FourStateBitOf(
      (WordAt(a.value, 0) & 1U) != 0U, (WordAt(a.unknown, 0) & 1U) != 0U);
}

// LRM 11.4.3: x or z anywhere in either operand makes the whole result x.
// Otherwise the result is the operation on the value planes, modulo 2^width.
LYRA_FOLDED constexpr void Add(
    Planes out, ConstPlanes a, ConstPlanes b, std::uint64_t w) {
  if (HasUnknown(a) || HasUnknown(b)) {
    FillScalar(out, w, FourStateBit::kUnknown);
    return;
  }
  detail::AddWords(a.value, b.value, out.value, w);
  detail::Fill(out.unknown, 0U);
}

LYRA_FOLDED constexpr void Subtract(
    Planes out, ConstPlanes a, ConstPlanes b, std::uint64_t w) {
  if (HasUnknown(a) || HasUnknown(b)) {
    FillScalar(out, w, FourStateBit::kUnknown);
    return;
  }
  detail::SubWords(a.value, b.value, out.value, w);
  detail::Fill(out.unknown, 0U);
}

LYRA_FOLDED constexpr void Negate(Planes out, ConstPlanes a, std::uint64_t w) {
  if (HasUnknown(a)) {
    FillScalar(out, w, FourStateBit::kUnknown);
    return;
  }
  detail::SubWords({}, a.value, out.value, w);
  detail::Fill(out.unknown, 0U);
}

// A value of one word multiplies as the machine multiplies a word.
LYRA_FOLDED constexpr void Multiply(
    Planes out, ConstPlanes a, ConstPlanes b, std::uint64_t w) {
  if (HasUnknown(a) || HasUnknown(b)) {
    FillScalar(out, w, FourStateBit::kUnknown);
    return;
  }
  if (out.value.size() == 1U) {
    out.value[0] = (a.value[0] * b.value[0]) & LowBits(w);
  } else {
    detail::MulWords(a.value, b.value, out.value, w);
  }
  detail::Fill(out.unknown, 0U);
}

// LRM 11.4.3: integer division truncates toward zero, and a zero divisor makes
// the result x. A value of one word divides as the machine divides a word.
LYRA_FOLDED constexpr void Divide(
    Planes out, ConstPlanes a, ConstPlanes b, std::uint64_t w,
    Signedness signedness) {
  if (HasUnknown(a) || HasUnknown(b) || !detail::AnySet(b.value)) {
    FillScalar(out, w, FourStateBit::kUnknown);
    return;
  }
  if (out.value.size() == 1U) {
    out.value[0] = detail::QuotientWord(a.value[0], b.value[0], w, signedness);
  } else {
    detail::QuotientWords(a.value, b.value, out.value, w, signedness);
  }
  detail::Fill(out.unknown, 0U);
}

// LRM 11.4.3: a remainder takes the dividend's sign, and a zero divisor makes
// the result x.
LYRA_FOLDED constexpr void Modulo(
    Planes out, ConstPlanes a, ConstPlanes b, std::uint64_t w,
    Signedness signedness) {
  if (HasUnknown(a) || HasUnknown(b) || !detail::AnySet(b.value)) {
    FillScalar(out, w, FourStateBit::kUnknown);
    return;
  }
  if (out.value.size() == 1U) {
    out.value[0] = detail::RemainderWord(a.value[0], b.value[0], w, signedness);
  } else {
    detail::RemainderWords(a.value, b.value, out.value, w, signedness);
  }
  detail::Fill(out.unknown, 0U);
}

// LRM 11.4.3 power. An x or z in either operand makes the result x. A negative
// exponent is a reciprocal, which integer division truncates to zero for every
// base but 1 and -1 and which for 0 divides by zero (Table 11-4); any other
// exponent, read whole however wide it is, multiplies the base that many
// times, so a zero one answers 1.
constexpr void Power(
    Planes out, ConstPlanes base, std::uint64_t w, Signedness signedness,
    ConstPlanes exponent, std::uint64_t exponent_w,
    Signedness exponent_signedness) {
  if (HasUnknown(base) || HasUnknown(exponent)) {
    FillScalar(out, w, FourStateBit::kUnknown);
    return;
  }
  detail::Fill(out.unknown, 0U);
  if (exponent_signedness == Signedness::kSigned &&
      BitAt(exponent.value, exponent_w - 1U)) {
    if (!detail::AnySet(base.value)) {
      FillScalar(out, w, FourStateBit::kUnknown);
    } else if (detail::HoldsNumber(base.value, w, 1)) {
      FromInt(out, w, 1);
    } else if (
        signedness == Signedness::kSigned &&
        detail::HoldsNumber(base.value, w, -1)) {
      FromInt(out, w, BitAt(exponent.value, 0) ? -1 : 1);
    } else {
      FromInt(out, w, 0);
    }
    return;
  }
  const std::size_t words = out.value.size();
  std::vector<std::uint64_t> factor(base.value.begin(), base.value.end());
  std::vector<std::uint64_t> product(words);
  FromInt(Planes{.value = out.value, .unknown = {}}, w, 1);
  std::uint64_t remaining = 0;
  for (std::uint64_t at = 0; at < exponent_w; ++at) {
    if (BitAt(exponent.value, at)) {
      remaining = at + 1U;
    }
  }
  for (std::uint64_t at = 0; at < remaining; ++at) {
    if (BitAt(exponent.value, at)) {
      detail::MulWords(out.value, factor, product, w);
      detail::Copy(product, out.value);
    }
    if (at + 1U < remaining) {
      detail::MulWords(factor, factor, product, w);
      detail::Copy(product, factor);
    }
  }
}

// LRM 11.4.8 Tables 11-11 to 11-15: per position, z reads as x.
LYRA_FOLDED constexpr void BitwiseAnd(
    Planes out, ConstPlanes a, ConstPlanes b) {
  for (std::size_t i = 0; i < out.value.size(); ++i) {
    const std::uint64_t av = a.value[i];
    const std::uint64_t bv = b.value[i];
    const std::uint64_t au = WordAt(a.unknown, i);
    const std::uint64_t bu = WordAt(b.unknown, i);
    const std::uint64_t zero = (~au & ~av) | (~bu & ~bv);
    const std::uint64_t one = (~au & av) & (~bu & bv);
    const std::uint64_t unknown = ~(zero | one) & (au | bu);
    out.value[i] = one | unknown;
    if (!out.unknown.empty()) {
      out.unknown[i] = unknown;
    }
  }
}

LYRA_FOLDED constexpr void BitwiseOr(Planes out, ConstPlanes a, ConstPlanes b) {
  for (std::size_t i = 0; i < out.value.size(); ++i) {
    const std::uint64_t av = a.value[i];
    const std::uint64_t bv = b.value[i];
    const std::uint64_t au = WordAt(a.unknown, i);
    const std::uint64_t bu = WordAt(b.unknown, i);
    const std::uint64_t one = (~au & av) | (~bu & bv);
    const std::uint64_t zero = (~au & ~av) & (~bu & ~bv);
    const std::uint64_t unknown = ~(zero | one) & (au | bu);
    out.value[i] = one | unknown;
    if (!out.unknown.empty()) {
      out.unknown[i] = unknown;
    }
  }
}

LYRA_FOLDED constexpr void BitwiseXor(
    Planes out, ConstPlanes a, ConstPlanes b) {
  for (std::size_t i = 0; i < out.value.size(); ++i) {
    const std::uint64_t unknown = WordAt(a.unknown, i) | WordAt(b.unknown, i);
    out.value[i] = unknown | (a.value[i] ^ b.value[i]);
    if (!out.unknown.empty()) {
      out.unknown[i] = unknown;
    }
  }
}

LYRA_FOLDED constexpr void BitwiseXnor(
    Planes out, ConstPlanes a, ConstPlanes b, std::uint64_t w) {
  for (std::size_t i = 0; i < out.value.size(); ++i) {
    const std::uint64_t unknown = WordAt(a.unknown, i) | WordAt(b.unknown, i);
    out.value[i] = unknown | ~(a.value[i] ^ b.value[i]);
    if (!out.unknown.empty()) {
      out.unknown[i] = unknown;
    }
  }
  ClearAboveWidth(out.value, w);
}

LYRA_FOLDED constexpr void BitwiseNot(
    Planes out, ConstPlanes a, std::uint64_t w) {
  for (std::size_t i = 0; i < out.value.size(); ++i) {
    const std::uint64_t unknown = WordAt(a.unknown, i);
    out.value[i] = ~a.value[i] | unknown;
    if (!out.unknown.empty()) {
      out.unknown[i] = unknown;
    }
  }
  ClearAboveWidth(out.value, w);
}

// LRM 11.4.5 logical equality: a position both operands know and disagree on
// settles them unequal however many unknown bits sit beside it, so only
// agreement everywhere both are known leaves the answer to those bits.
[[nodiscard]] LYRA_FOLDED constexpr auto Equal(ConstPlanes a, ConstPlanes b)
    -> FourStateBit {
  bool unknown_bit = false;
  for (std::size_t i = 0; i < a.value.size(); ++i) {
    const std::uint64_t unk = WordAt(a.unknown, i) | WordAt(b.unknown, i);
    if (((a.value[i] ^ b.value[i]) & ~unk) != 0U) {
      return FourStateBit::kZero;
    }
    unknown_bit = unknown_bit || unk != 0U;
  }
  return unknown_bit ? FourStateBit::kUnknown : FourStateBit::kOne;
}

[[nodiscard]] LYRA_FOLDED constexpr auto NotEqual(ConstPlanes a, ConstPlanes b)
    -> FourStateBit {
  return Inverted(Equal(a, b));
}

// LRM 11.4.4: a relation with an x or z anywhere in either operand is x.
// Otherwise the operands compare as numbers of the operation's signedness.
[[nodiscard]] LYRA_FOLDED constexpr auto Less(
    ConstPlanes a, ConstPlanes b, std::uint64_t w, Signedness signedness)
    -> FourStateBit {
  if (HasUnknown(a) || HasUnknown(b)) {
    return FourStateBit::kUnknown;
  }
  return detail::ScalarOf(detail::Order(a.value, b.value, w, signedness) < 0);
}

[[nodiscard]] LYRA_FOLDED constexpr auto LessEqual(
    ConstPlanes a, ConstPlanes b, std::uint64_t w, Signedness signedness)
    -> FourStateBit {
  return Inverted(Less(b, a, w, signedness));
}

[[nodiscard]] LYRA_FOLDED constexpr auto Greater(
    ConstPlanes a, ConstPlanes b, std::uint64_t w, Signedness signedness)
    -> FourStateBit {
  return Less(b, a, w, signedness);
}

[[nodiscard]] LYRA_FOLDED constexpr auto GreaterEqual(
    ConstPlanes a, ConstPlanes b, std::uint64_t w, Signedness signedness)
    -> FourStateBit {
  return Inverted(Less(a, b, w, signedness));
}

// LRM 11.4.5 case equality: both planes identical, so x matches x and z
// matches z. Its answer is never unknown.
[[nodiscard]] LYRA_FOLDED constexpr auto CaseEqual(ConstPlanes a, ConstPlanes b)
    -> bool {
  for (std::size_t i = 0; i < a.value.size(); ++i) {
    if (a.value[i] != b.value[i] ||
        WordAt(a.unknown, i) != WordAt(b.unknown, i)) {
      return false;
    }
  }
  return true;
}

// LRM 11.4.6: an x or z in `b` is a wildcard; one in `a` where `b` compares
// leaves the answer unknown unless a known position already disagrees.
[[nodiscard]] LYRA_FOLDED constexpr auto WildcardEqual(
    ConstPlanes a, ConstPlanes b) -> FourStateBit {
  bool unknown_at_compare = false;
  for (std::size_t i = 0; i < a.value.size(); ++i) {
    const std::uint64_t au = WordAt(a.unknown, i);
    const std::uint64_t compared = ~WordAt(b.unknown, i);
    if (((a.value[i] ^ b.value[i]) & ~au & compared) != 0U) {
      return FourStateBit::kZero;
    }
    unknown_at_compare = unknown_at_compare || (au & compared) != 0U;
  }
  return unknown_at_compare ? FourStateBit::kUnknown : FourStateBit::kOne;
}

// LRM 12.5.1 `casez` label match: z on either side is a wildcard, and every
// other position matches exactly on both planes.
[[nodiscard]] LYRA_FOLDED constexpr auto CasezMatch(
    ConstPlanes a, ConstPlanes b) -> bool {
  for (std::size_t i = 0; i < a.value.size(); ++i) {
    const std::uint64_t av = a.value[i];
    const std::uint64_t bv = b.value[i];
    const std::uint64_t au = WordAt(a.unknown, i);
    const std::uint64_t bu = WordAt(b.unknown, i);
    const std::uint64_t compared = ~((au & ~av) | (bu & ~bv));
    if ((((av ^ bv) | (au ^ bu)) & compared) != 0U) {
      return false;
    }
  }
  return true;
}

// LRM 12.5.1 `casex` label match: x or z on either side is a wildcard.
[[nodiscard]] LYRA_FOLDED constexpr auto CasexMatch(
    ConstPlanes a, ConstPlanes b) -> bool {
  for (std::size_t i = 0; i < a.value.size(); ++i) {
    const std::uint64_t compared =
        ~(WordAt(a.unknown, i) | WordAt(b.unknown, i));
    if (((a.value[i] ^ b.value[i]) & compared) != 0U) {
      return false;
    }
  }
  return true;
}

// LRM 11.4.7: the logical operators read each operand as its truth value, and
// the two operands need not be of one type.
[[nodiscard]] LYRA_FOLDED constexpr auto LogicalAnd(
    ConstPlanes a, ConstPlanes b) -> FourStateBit {
  return detail::TruthAnd(Truth(a), Truth(b));
}

[[nodiscard]] LYRA_FOLDED constexpr auto LogicalOr(ConstPlanes a, ConstPlanes b)
    -> FourStateBit {
  return detail::TruthOr(Truth(a), Truth(b));
}

[[nodiscard]] LYRA_FOLDED constexpr auto LogicalNot(ConstPlanes a)
    -> FourStateBit {
  return detail::TruthNot(Truth(a));
}

// LRM 11.4.7 `<->`: `(a -> b) && (b -> a)`, unknown where either truth is.
[[nodiscard]] LYRA_FOLDED constexpr auto LogicalEquivalence(
    ConstPlanes a, ConstPlanes b) -> FourStateBit {
  const Truthiness x = Truth(a);
  const Truthiness y = Truth(b);
  if (x == Truthiness::kUnknown || y == Truthiness::kUnknown) {
    return FourStateBit::kUnknown;
  }
  return detail::ScalarOf(x == y);
}

// LRM 11.4.11 Table 11-20: the arms of a conditional operator whose condition
// is ambiguous, combined -- a position both arms know and agree on survives,
// and every other position is x.
LYRA_FOLDED constexpr void MergeConditional(
    Planes out, ConstPlanes a, ConstPlanes b, std::uint64_t w) {
  for (std::size_t i = 0; i < out.value.size(); ++i) {
    const std::uint64_t agreed = ~(a.value[i] ^ b.value[i]) &
                                 ~(WordAt(a.unknown, i) | WordAt(b.unknown, i));
    const std::uint64_t unknown = ~agreed & ValidBitsMask(i, w);
    const std::uint64_t value = a.value[i] & agreed;
    if (out.unknown.empty()) {
      out.value[i] = value;
    } else {
      out.value[i] = value | unknown;
      out.unknown[i] = unknown;
    }
  }
}

// Two drivers' contributions to one net folded under the table `fold` names.
LYRA_FOLDED constexpr void Resolve(
    Planes out, ConstPlanes a, ConstPlanes b, NetResolution fold) {
  for (std::size_t i = 0; i < out.value.size(); ++i) {
    const std::uint64_t av = a.value[i];
    const std::uint64_t bv = b.value[i];
    const std::uint64_t au = WordAt(a.unknown, i);
    const std::uint64_t bu = WordAt(b.unknown, i);
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
    out.value[i] = v;
    if (!out.unknown.empty()) {
      out.unknown[i] = u;
    }
  }
}

// A stronger contribution meeting a weaker one: it determines every position
// it drives, which is every position where it is not z, and the weaker one
// determines the rest (LRM 28.12.1).
LYRA_FOLDED constexpr void Dominate(
    Planes out, ConstPlanes stronger, ConstPlanes weaker) {
  for (std::size_t i = 0; i < out.value.size(); ++i) {
    const std::uint64_t sv = stronger.value[i];
    const std::uint64_t su = WordAt(stronger.unknown, i);
    const std::uint64_t undriven = ~sv & su;
    out.value[i] = (sv & ~undriven) | (weaker.value[i] & undriven);
    if (!out.unknown.empty()) {
      out.unknown[i] =
          (su & ~undriven) | (WordAt(weaker.unknown, i) & undriven);
    }
  }
}

[[nodiscard]] LYRA_FOLDED constexpr auto Reduce(
    ConstPlanes a, std::uint64_t w, ReductionOp op) -> FourStateBit {
  bool any_zero = false;
  bool any_one = false;
  bool any_unknown = false;
  bool parity = false;
  for (std::size_t i = 0; i < a.value.size(); ++i) {
    const std::uint64_t valid = ValidBitsMask(i, w);
    const std::uint64_t unk = WordAt(a.unknown, i) & valid;
    const std::uint64_t known = ~unk & valid;
    any_zero = any_zero || (~a.value[i] & known) != 0U;
    any_one = any_one || (a.value[i] & known) != 0U;
    any_unknown = any_unknown || unk != 0U;
    parity = parity != ((std::popcount(a.value[i] & known) & 1) != 0);
  }
  FourStateBit and_bit = FourStateBit::kOne;
  if (any_zero) {
    and_bit = FourStateBit::kZero;
  } else if (any_unknown) {
    and_bit = FourStateBit::kUnknown;
  }
  FourStateBit or_bit = FourStateBit::kZero;
  if (any_one) {
    or_bit = FourStateBit::kOne;
  } else if (any_unknown) {
    or_bit = FourStateBit::kUnknown;
  }
  const FourStateBit xor_bit =
      any_unknown ? FourStateBit::kUnknown : detail::ScalarOf(parity);
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
LYRA_FOLDED constexpr void Convert(
    Planes out, std::uint64_t out_width, ConstPlanes src,
    std::uint64_t src_width, Signedness src_signedness) {
  const bool extends =
      src_signedness == Signedness::kSigned && out_width > src_width;
  for (std::size_t i = 0; i < out.value.size(); ++i) {
    const std::uint64_t low = static_cast<std::uint64_t>(i) * 64U;
    const std::uint64_t valid = ValidBitsMask(i, out_width);
    const std::uint64_t inside =
        low < src_width ? LowBits(src_width - low) : 0U;
    const std::uint64_t above = extends ? valid & ~inside : 0U;
    const std::uint64_t value = (BitsAt(src.value, low, 64U) & inside & valid) |
                                (BitAt(src.value, src_width - 1U) ? above : 0U);
    const std::uint64_t unknown =
        (BitsAt(src.unknown, low, 64U) & inside & valid) |
        (BitAt(src.unknown, src_width - 1U) ? above : 0U);
    if (out.unknown.empty()) {
      out.value[i] = value & ~unknown;
    } else {
      out.value[i] = value;
      out.unknown[i] = unknown;
    }
  }
}

// LRM 11.5.1: `out_width` bits of `src` starting at `start`, counted from its
// least significant bit. A position outside the source reads x.
LYRA_FOLDED constexpr void Extract(
    Planes out, std::uint64_t out_width, ConstPlanes src,
    std::uint64_t src_width, std::int64_t start) {
  for (std::size_t i = 0; i < out.value.size(); ++i) {
    const detail::SourceWord word = detail::SourceWordAt(
        src, src_width, start + (static_cast<std::int64_t>(i) * 64));
    const std::uint64_t valid = ValidBitsMask(i, out_width);
    if (out.unknown.empty()) {
      out.value[i] = word.value & ~word.unknown & valid;
    } else {
      const std::uint64_t outside = valid & ~word.inside;
      out.value[i] = (word.value & valid) | outside;
      out.unknown[i] = (word.unknown & valid) | outside;
    }
  }
}

// The positions of `dst` a write of `src_width` bits at `start` lands on,
// inside the destination. None, where it lies wholly outside.
[[nodiscard]] LYRA_FOLDED constexpr auto Reached(
    std::uint64_t dst_width, std::uint64_t src_width, std::int64_t start)
    -> std::optional<BitPositions> {
  const auto dst_signed = static_cast<std::int64_t>(dst_width);
  const std::int64_t from = std::max<std::int64_t>(start, 0);
  const std::int64_t to = std::min<std::int64_t>(
      start + static_cast<std::int64_t>(src_width), dst_signed);
  if (from >= to) {
    return std::nullopt;
  }
  return BitPositions{
      .lsb = static_cast<std::uint64_t>(from),
      .width = static_cast<std::uint64_t>(to - from)};
}

// LRM 11.5.1: writes `src` into `dst` at `start`, landing only on the
// positions of `dst` that `within` names -- as a write through a part of a
// part lands nowhere outside the outer part -- and leaving every other
// position as it stands. Answers the positions written, none where it reaches
// none.
LYRA_FOLDED constexpr auto InsertWithin(
    Planes dst, std::uint64_t dst_width, ConstPlanes src,
    std::uint64_t src_width, std::int64_t start, BitPositions within)
    -> std::optional<BitPositions> {
  const std::optional<BitPositions> reached =
      Reached(dst_width, src_width, start);
  if (!reached) {
    return std::nullopt;
  }
  const std::uint64_t from = std::max(reached->lsb, within.lsb);
  const std::uint64_t to =
      std::min(reached->lsb + reached->width, within.lsb + within.width);
  if (from >= to) {
    return std::nullopt;
  }
  const BitPositions written{.lsb = from, .width = to - from};
  detail::WriteRun(dst, src, src_width, start, written);
  return written;
}

// The same write over the whole of `dst`: a position outside the destination
// is not written.
LYRA_FOLDED constexpr auto Insert(
    Planes dst, std::uint64_t dst_width, ConstPlanes src,
    std::uint64_t src_width, std::int64_t start)
    -> std::optional<BitPositions> {
  return InsertWithin(
      dst, dst_width, src, src_width, start,
      BitPositions{.lsb = 0, .width = dst_width});
}

// LRM 11.4.10: a shift moves every bit by the number the amount holds, read
// unsigned, and an amount holding an x or z makes the whole result x. An
// amount at or past the width moves every bit out.
LYRA_FOLDED constexpr void ShiftLeft(
    Planes out, ConstPlanes a, std::uint64_t w, ConstPlanes amount) {
  if (HasUnknown(amount)) {
    FillScalar(out, w, FourStateBit::kUnknown);
    return;
  }
  const std::uint64_t by = detail::ShiftCount(amount.value);
  detail::ShiftLeftWords(a.value, by, out.value, w);
  if (!out.unknown.empty()) {
    detail::ShiftLeftWords(a.unknown, by, out.unknown, w);
  }
}

LYRA_FOLDED constexpr void LogicalShiftRight(
    Planes out, ConstPlanes a, std::uint64_t w, ConstPlanes amount) {
  if (HasUnknown(amount)) {
    FillScalar(out, w, FourStateBit::kUnknown);
    return;
  }
  detail::ShiftRightPlanes(out, a, detail::ShiftCount(amount.value), w, false);
}

// `>>>` fills from the sign of a signed operand and with 0 from an unsigned
// one.
LYRA_FOLDED constexpr void ArithmeticShiftRight(
    Planes out, ConstPlanes a, std::uint64_t w, Signedness signedness,
    ConstPlanes amount) {
  if (HasUnknown(amount)) {
    FillScalar(out, w, FourStateBit::kUnknown);
    return;
  }
  detail::ShiftRightPlanes(
      out, a, detail::ShiftCount(amount.value), w,
      signedness == Signedness::kSigned);
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
[[nodiscard]] LYRA_FOLDED constexpr auto PositionNamed(
    ConstPlanes index, std::uint64_t width, Signedness signedness)
    -> std::optional<std::int64_t> {
  if (HasUnknown(index)) {
    return std::nullopt;
  }
  const bool negative =
      signedness == Signedness::kSigned && BitAt(index.value, width - 1U);
  // The number fits a machine word exactly when every bit above the low 63
  // repeats its sign.
  const std::uint64_t low =
      WordAt(index.value, 0) | (negative && width < 64U ? ~LowBits(width) : 0U);
  if (width >= 64U && ((low >> 63U) != 0U) != negative) {
    return std::nullopt;
  }
  for (std::size_t i = 1; i < index.value.size(); ++i) {
    if (index.value[i] != (negative ? ValidBitsMask(i, width) : 0U)) {
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
LYRA_FOLDED constexpr void ToPosition(
    Planes out, ConstPlanes index, std::uint64_t index_w,
    Signedness index_signedness) {
  const std::optional<std::int64_t> named =
      PositionNamed(index, index_w, index_signedness);
  if (named) {
    FromInt(out, kPositionWidth, *named);
  } else {
    FillScalar(out, kPositionWidth, FourStateBit::kUnknown);
  }
}

// LRM 11.5.1: the `out_width` bits of `src` from the position named, which
// read as a declaration's default does where the position names none.
LYRA_FOLDED constexpr void Slice(
    Planes out, std::uint64_t out_width, ConstPlanes src,
    std::uint64_t src_width, ConstPlanes position, std::uint64_t position_w,
    Signedness position_signedness) {
  const std::optional<std::int64_t> start =
      PositionNamed(position, position_w, position_signedness);
  if (start) {
    Extract(out, out_width, src, src_width, *start);
  } else {
    FillDefault(out, out_width);
  }
}

// LRM 11.5.1: `bits` written into `dst` at the position named, which writes
// nothing where the position names none. Answers the positions written.
LYRA_FOLDED constexpr auto WithSlice(
    Planes dst, std::uint64_t dst_width, ConstPlanes position,
    std::uint64_t position_w, Signedness position_signedness, ConstPlanes bits,
    std::uint64_t bits_width) -> std::optional<BitPositions> {
  const std::optional<std::int64_t> start =
      PositionNamed(position, position_w, position_signedness);
  if (!start) {
    return std::nullopt;
  }
  return Insert(dst, dst_width, bits, bits_width, *start);
}

// LRM 11.4.12: `high` in the most significant positions and `low` below it.
// The answer is as wide as the two together.
LYRA_FOLDED constexpr void Concat(
    Planes out, ConstPlanes high, std::uint64_t high_width, ConstPlanes low,
    std::uint64_t low_width) {
  detail::Fill(out.value, 0U);
  detail::Fill(out.unknown, 0U);
  const std::uint64_t width = high_width + low_width;
  Insert(out, width, low, low_width, 0);
  Insert(out, width, high, high_width, static_cast<std::int64_t>(low_width));
}

// LRM 11.4.12.1: as many copies of `a` as fill the `out_width` bits of the
// answer, laid end to end. Its work grows with how many copies that is.
constexpr void Replicate(
    Planes out, std::uint64_t out_width, ConstPlanes a, std::uint64_t w) {
  detail::Fill(out.value, 0U);
  detail::Fill(out.unknown, 0U);
  for (std::uint64_t at = 0; at < out_width; at += w) {
    Insert(out, out_width, a, w, static_cast<std::int64_t>(at));
  }
}

// LRM 20.8.1 `$clog2`: the operand read as unsigned, and 0 answering 0. The
// clause does not say what an x or z bit reads as; each reads as 0 here.
[[nodiscard]] LYRA_FOLDED constexpr auto CeilLog2(
    ConstPlanes a, std::uint64_t w) -> std::int64_t {
  std::int64_t high = -1;
  int set = 0;
  for (std::size_t i = 0; i < a.value.size(); ++i) {
    const std::uint64_t word =
        a.value[i] & ~WordAt(a.unknown, i) & ValidBitsMask(i, w);
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
[[nodiscard]] LYRA_FOLDED constexpr auto CountBits(
    ConstPlanes a, std::uint64_t w, ConstPlanes control,
    std::uint64_t control_w) -> std::int64_t {
  bool zero = false;
  bool one = false;
  bool high_impedance = false;
  bool unknown = false;
  for (std::size_t i = 0; i < control.value.size(); ++i) {
    const std::uint64_t valid = ValidBitsMask(i, control_w);
    const std::uint64_t v = control.value[i];
    const std::uint64_t u = WordAt(control.unknown, i);
    zero = zero || (~v & ~u & valid) != 0U;
    one = one || (v & ~u) != 0U;
    high_impedance = high_impedance || (~v & u) != 0U;
    unknown = unknown || (v & u) != 0U;
  }
  std::int64_t count = 0;
  for (std::size_t i = 0; i < a.value.size(); ++i) {
    const std::uint64_t valid = ValidBitsMask(i, w);
    const std::uint64_t v = a.value[i];
    const std::uint64_t u = WordAt(a.unknown, i);
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
constexpr void FromBytes(
    Planes out, std::uint64_t width, std::span<const char> bytes) {
  detail::Fill(out.value, 0U);
  detail::Fill(out.unknown, 0U);
  for (std::size_t k = 0; k < bytes.size(); ++k) {
    const std::uint64_t at = 8U * static_cast<std::uint64_t>(k);
    if (at >= width) {
      break;
    }
    const auto byte = static_cast<std::uint64_t>(
        static_cast<unsigned char>(bytes[bytes.size() - 1U - k]));
    const std::uint64_t count = std::min<std::uint64_t>(8U, width - at);
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

// LRM 21.4: a word of digits read as a value, its last digit the least
// significant, held at `width` bits -- a shorter word leaves the positions
// above it 0 and a longer one loses its leading digits. `_` separates digits;
// an x digit is x in every bit it covers and a z or ? digit z in every one.
// Answers false where a character is no digit of the radix, or there is no
// digit at all.
[[nodiscard]] constexpr auto FromDigits(
    Planes out, std::uint64_t width, DigitRadix radix, std::string_view digits)
    -> bool {
  const std::uint64_t bits_per_digit = BitsPerDigit(radix);
  const std::uint64_t all = LowBits(bits_per_digit);
  const bool four_state = !out.unknown.empty();
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
  bool any_digit = false;
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
    } else if (const std::optional<std::uint64_t> number = DigitOf(c, radix)) {
      value_bits = *number;
    } else {
      return false;
    }
    any_digit = true;
    if (at < width && four_state) {
      place(out.value, at, value_bits);
      place(out.unknown, at, unknown_bits);
    } else if (at < width && unknown_bits == 0U) {
      place(out.value, at, value_bits);
    }
    at += bits_per_digit;
  }
  ClearAboveWidth(out.value, width);
  ClearAboveWidth(out.unknown, width);
  return any_digit;
}

// The byte `index` positions up from a value's least significant end, a
// position past the width reading 0; none where one of its bits holds x or z.
[[nodiscard]] LYRA_FOLDED constexpr auto ByteAt(
    ConstPlanes a, std::uint64_t width, std::uint64_t index)
    -> std::optional<std::uint8_t> {
  const std::uint64_t at = 8U * index;
  if (at >= width) {
    return std::uint8_t{0};
  }
  const std::uint64_t count = std::min<std::uint64_t>(8U, width - at);
  if (BitsAt(a.unknown, at, count) != 0U) {
    return std::nullopt;
  }
  return static_cast<std::uint8_t>(BitsAt(a.value, at, count));
}

// LRM 11.4.14.2: `a`'s `block`-wide blocks in reversed order, counted from its
// least significant bit, the bits inside each left where they are. The last
// block is whatever the width leaves over and is not padded.
constexpr void ReverseBlocks(
    Planes out, ConstPlanes a, std::uint64_t w, std::uint64_t block) {
  detail::Fill(out.value, 0U);
  detail::Fill(out.unknown, 0U);
  for (std::uint64_t low = 0; low < w; low += block) {
    const std::uint64_t count = std::min(block, w - low);
    const std::uint64_t to = w - low - count;
    InsertWithin(
        out, w, a, w,
        static_cast<std::int64_t>(to) - static_cast<std::int64_t>(low),
        BitPositions{.lsb = to, .width = count});
  }
}

}  // namespace lyra::value
