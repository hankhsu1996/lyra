#include "lyra/value/packed_array.hpp"

#include <algorithm>
#include <array>
#include <bit>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/value/packed.hpp"
#include "lyra/value/packed_bitwise.hpp"
#include "lyra/value/packed_convert.hpp"
#include "lyra/value/packed_internal.hpp"
#include "lyra/value/packed_reduction.hpp"
#include "lyra/value/position.hpp"
#include "lyra/value/string.hpp"

namespace lyra::value {

namespace {

// As many words as `bit_width` needs, every position clear -- a value's own
// plane, or a run of words an operation works in before it has a result.
auto ZeroedWords(std::uint64_t bit_width) -> PackedWordArray {
  return PackedWordArray{WordCountForBits(bit_width), std::uint64_t{0}};
}

// Every plane a value of this shape carries, one after the other, every
// position clear.
auto ZeroedPlanes(std::uint64_t bit_width, bool is_four_state)
    -> PackedWordArray {
  const std::size_t plane_count = is_four_state ? 2U : 1U;
  return PackedWordArray{
      WordCountForBits(bit_width) * plane_count, std::uint64_t{0}};
}

// The mask of a value that fits in one word, which is that value's own word
// zero.
auto MaskForWidth(std::uint64_t bit_width) -> std::uint64_t {
  return ValidBitsMask(0, bit_width);
}

auto SignExtendToInt64(std::uint64_t bits, std::uint64_t bit_width)
    -> std::int64_t {
  if (bit_width == 0U || bit_width >= 64U) {
    return static_cast<std::int64_t>(bits);
  }
  const std::uint64_t sign_bit = std::uint64_t{1} << (bit_width - 1U);
  if ((bits & sign_bit) != 0U) {
    return static_cast<std::int64_t>(bits | ~MaskForWidth(bit_width));
  }
  return static_cast<std::int64_t>(bits);
}

// The two operands occupy the same raw bit-plane storage: identical width and
// identical state domain (the state domain fixes the plane count). This is the
// precondition for a word-parallel operation or a low-level bit copy. It is
// deliberately weaker than same-representation -- it does not compare the
// dimension stack or signedness, neither of which a word-parallel bit op reads.
auto RequireSameStorageDomain(
    const PackedArray& lhs, const PackedArray& rhs, std::string_view op)
    -> void {
  detail::RequireSameWidth(op, lhs.BitWidth(), rhs.BitWidth());
  detail::RequireSameStateDomain(op, lhs.IsFourState(), rhs.IsFourState());
}

// What words assembled outside a packed operation must satisfy before they are
// taken as a value's bits. An empty unknown plane stands for no unknown bit at
// all, which a two-state shape may also state.
auto RequirePlanes(
    std::string_view where, std::uint64_t bit_width, bool is_four_state,
    std::span<const std::uint64_t> value_words,
    std::span<const std::uint64_t> unknown_words) -> void {
  if (bit_width == 0U) {
    throw InternalError(std::string{where} + ": bit_width must be >= 1");
  }
  const std::size_t expected = WordCountForBits(bit_width);
  if (value_words.size() != expected) {
    throw InternalError(std::string{where} + ": value word count mismatch");
  }
  if (!is_four_state && !unknown_words.empty()) {
    throw InternalError(
        std::string{where} + ": 2-state shape forbids unknown words");
  }
  if (is_four_state && !unknown_words.empty() &&
      unknown_words.size() != expected) {
    throw InternalError(
        std::string{where} + ": 4-state unknown word count mismatch");
  }
}

}  // namespace

PackedArray::PackedArray() = default;

PackedArray::PackedArray(PackedType type)
    : PackedArray(type.bit_width, type.is_signed, type.is_four_state) {
}

PackedArray::PackedArray(
    std::uint64_t bit_width, bool is_signed, bool is_four_state,
    PackedWordArray planes)
    : bit_width_(bit_width),
      is_signed_(is_signed),
      is_four_state_(is_four_state),
      planes_(std::move(planes)) {
}

auto PackedArray::Blank(
    std::uint64_t bit_width, bool is_signed, bool is_four_state)
    -> PackedArray {
  return PackedArray{
      bit_width, is_signed, is_four_state,
      ZeroedPlanes(bit_width, is_four_state)};
}

// Read from the run itself rather than from the width, so that a value whose
// words were moved away reads as holding none.
auto PackedArray::WordsPerPlane() const -> std::size_t {
  return is_four_state_ ? planes_.size() / 2U : planes_.size();
}

auto PackedArray::MutableValueWords() -> std::span<std::uint64_t> {
  return std::span{planes_.data(), planes_.size()}.first(WordsPerPlane());
}

auto PackedArray::MutableUnknownWords() -> std::span<std::uint64_t> {
  return std::span{planes_.data(), planes_.size()}.subspan(WordsPerPlane());
}

PackedArray::PackedArray(
    std::uint64_t bit_width, bool is_signed, bool is_four_state)
    : bit_width_(bit_width),
      is_signed_(is_signed),
      is_four_state_(is_four_state),
      planes_(ZeroedPlanes(bit_width, is_four_state)) {
  // LRM Table 6-7: a four-state declaration reads as x until something drives
  // it. A two-state one reads as the zero the planes already carry.
  if (is_four_state_) {
    SetAllValidBits(MutableValueWords(), bit_width_);
    SetAllValidBits(MutableUnknownWords(), bit_width_);
  }
}

auto PackedArray::Int(std::int32_t value) -> PackedArray {
  return FromInt(value, 32U, true, false);
}

auto PackedArray::IntUnsigned(std::uint32_t value) -> PackedArray {
  return FromInt(static_cast<std::int64_t>(value), 32U, false, false);
}

auto PackedArray::Byte(std::int8_t value) -> PackedArray {
  return FromInt(value, 8U, true, false);
}

auto PackedArray::Integer(std::int32_t value) -> PackedArray {
  return FromInt(value, 32U, true, true);
}

auto PackedArray::Bit(bool value) -> PackedArray {
  return FromInt(value ? 1 : 0, 1U, false, false);
}

auto PackedArray::FromBool(bool value) -> PackedArray {
  return Bit(value);
}

auto PackedArray::HighImpedanceScalar() -> PackedArray {
  const std::array<std::uint64_t, 1> value_words = {0};
  const std::array<std::uint64_t, 1> unknown_words = {1};
  return FromWords(value_words, unknown_words, 1, false, true);
}

auto PackedArray::MakeFromWordPlanes(
    std::uint64_t bit_width, bool is_signed, bool is_four_state,
    std::span<const std::uint64_t> value_words,
    std::span<const std::uint64_t> unknown_words) -> PackedArray {
  RequirePlanes(
      "PackedArray::MakeFromWordPlanes", bit_width, is_four_state, value_words,
      unknown_words);

  PackedArray p = Blank(bit_width, is_signed, is_four_state);
  p.InstallPlanes(value_words, unknown_words);
  return p;
}

auto PackedArray::InstallPlanes(
    std::span<const std::uint64_t> value_words,
    std::span<const std::uint64_t> unknown_words) -> void {
  auto value_dst = MutableValueWords();
  std::ranges::copy(value_words, value_dst.begin());
  MaskUnusedTopBits(value_dst, bit_width_);
  if (unknown_words.empty()) {
    return;
  }
  auto unknown_dst = MutableUnknownWords();
  std::ranges::copy(unknown_words, unknown_dst.begin());
  MaskUnusedTopBits(unknown_dst, bit_width_);
}

auto PackedArray::FromInt(std::int64_t value, const PackedType& type)
    -> PackedArray {
  return FromInt(value, type.bit_width, type.is_signed, type.is_four_state);
}

auto PackedArray::FromInt(
    std::int64_t value, std::uint64_t bit_width, bool is_signed,
    bool is_four_state) -> PackedArray {
  if (bit_width == 0U) {
    throw InternalError("PackedArray::FromInt: bit_width must be >= 1");
  }
  PackedArray result = Blank(bit_width, is_signed, is_four_state);
  auto words = result.MutableValueWords();
  words[0] = static_cast<std::uint64_t>(value);
  // A carrier narrower than the value is sign-extended from its own sign bit,
  // which is what makes `-1` fill a wide destination (LRM 11.6.1).
  if (bit_width > 64U) {
    const std::uint64_t fill = (value < 0) ? ~std::uint64_t{0} : 0U;
    std::ranges::fill(words.subspan(1), fill);
  }
  MaskUnusedTopBits(words, bit_width);
  return result;
}

auto PackedArray::FromWords(
    std::span<const std::uint64_t> value_words,
    std::span<const std::uint64_t> unknown_words, const PackedType& type)
    -> PackedArray {
  return MakeFromWordPlanes(
      type.bit_width, type.is_signed, type.is_four_state, value_words,
      unknown_words);
}

auto PackedArray::FromWords(
    std::span<const std::uint64_t> value_words,
    std::span<const std::uint64_t> unknown_words, std::uint64_t bit_width,
    bool is_signed, bool is_four_state) -> PackedArray {
  return MakeFromWordPlanes(
      bit_width, is_signed, is_four_state, value_words, unknown_words);
}

auto PackedArray::FromBytes(
    std::span<const char> bytes, std::uint64_t bit_width, bool is_signed,
    bool is_four_state) -> PackedArray {
  const auto word_count = WordCountForBits(bit_width);
  std::vector<std::uint64_t> val_words(word_count, 0U);
  const auto total_input_bits = static_cast<std::uint64_t>(bytes.size()) * 8U;
  const auto bits_to_use = std::min<std::uint64_t>(total_input_bits, bit_width);
  // LRM 5.9 / 21.3.4.4: walk input bits MSB-first. The i-th input bit lands
  // at destination bit position (bit_width - 1 - i); the source bit is byte
  // (i / 8)'s (7 - i % 8) position. Excess input bits never enter the loop;
  // shortfalls leave trailing val_words bits at zero from construction.
  for (std::uint64_t i = 0; i < bits_to_use; ++i) {
    const auto byte =
        static_cast<std::uint64_t>(static_cast<unsigned char>(bytes[i / 8U]));
    const auto src_shift = 7U - (i % 8U);
    if (((byte >> src_shift) & 1U) != 0U) {
      const auto bit_pos = bit_width - 1U - i;
      const auto word_ix = static_cast<std::size_t>(bit_pos / 64U);
      const auto bit_ix = static_cast<std::size_t>(bit_pos % 64U);
      val_words[word_ix] |= std::uint64_t{1} << bit_ix;
    }
  }
  return PackedArray::FromWords(
      std::span<const std::uint64_t>{val_words},
      std::span<const std::uint64_t>{}, bit_width, is_signed, is_four_state);
}

namespace {

// One scanned bit's two-plane encoding: `unk` marks x or z, and for an unknown
// bit `val` distinguishes x (1) from z (0), matching the 4-state convention
// (all-x is val1/unk1, all-z is val0/unk1).
struct DigitBit {
  bool val;
  bool unk;
};

auto HexDigitValue(char c) -> int {
  if (c >= '0' && c <= '9') return c - '0';
  if (c >= 'a' && c <= 'f') return (c - 'a') + 10;
  if (c >= 'A' && c <= 'F') return (c - 'A') + 10;
  return -1;
}

}  // namespace

auto PackedArray::FromDigits(
    std::string_view digits, unsigned base, std::uint64_t bit_width,
    bool is_signed, bool is_four_state) -> std::optional<PackedArray> {
  unsigned bits_per_digit = 0;
  switch (base) {
    case 2:
      bits_per_digit = 1;
      break;
    case 8:
      bits_per_digit = 3;
      break;
    case 16:
      bits_per_digit = 4;
      break;
    default:
      throw InternalError("PackedArray::FromDigits: base must be 2, 8, or 16");
  }

  // Each digit expands to `bits_per_digit` bits, MSB-first, so the string is
  // right-justified into the target below. An x / z / ? digit marks its whole
  // span unknown (LRM 21.4: an unknown digit covers all the bits it names).
  std::vector<DigitBit> bits;
  bits.reserve(digits.size() * bits_per_digit);
  bool any_digit = false;
  for (const char c : digits) {
    if (c == '_') continue;
    bool unknown = false;
    bool unknown_val = false;
    unsigned digit_value = 0;
    if (c == 'x' || c == 'X') {
      unknown = true;
      unknown_val = true;
    } else if (c == 'z' || c == 'Z' || c == '?') {
      unknown = true;
    } else {
      const int d = HexDigitValue(c);
      if (d < 0 || std::cmp_greater_equal(d, base)) return std::nullopt;
      digit_value = static_cast<unsigned>(d);
    }
    any_digit = true;
    for (unsigned b = 0; b < bits_per_digit; ++b) {
      const unsigned shift = bits_per_digit - 1U - b;
      if (unknown) {
        bits.push_back(DigitBit{.val = unknown_val, .unk = true});
      } else {
        bits.push_back(
            DigitBit{.val = ((digit_value >> shift) & 1U) != 0U, .unk = false});
      }
    }
  }
  if (!any_digit) return std::nullopt;

  const auto word_count = WordCountForBits(bit_width);
  std::vector<std::uint64_t> val_words(word_count, 0U);
  std::vector<std::uint64_t> unk_words(word_count, 0U);
  const auto bits_to_use =
      std::min<std::size_t>(bits.size(), static_cast<std::size_t>(bit_width));
  const std::size_t skip = bits.size() - bits_to_use;
  for (std::size_t i = 0; i < bits_to_use; ++i) {
    const std::size_t bit_pos = bits_to_use - 1U - i;
    const DigitBit b = bits[skip + i];
    const std::size_t word_ix = bit_pos / 64U;
    const std::size_t bit_ix = bit_pos % 64U;
    if (b.val) val_words[word_ix] |= std::uint64_t{1} << bit_ix;
    if (b.unk) unk_words[word_ix] |= std::uint64_t{1} << bit_ix;
  }

  if (!is_four_state) {
    for (std::size_t w = 0; w < word_count; ++w) {
      val_words[w] &= ~unk_words[w];
    }
    return PackedArray::FromWords(
        std::span<const std::uint64_t>{val_words},
        std::span<const std::uint64_t>{}, bit_width, is_signed, false);
  }
  return PackedArray::FromWords(
      std::span<const std::uint64_t>{val_words},
      std::span<const std::uint64_t>{unk_words}, bit_width, is_signed, true);
}

auto PackedArray::BitWidth() const -> std::uint64_t {
  return bit_width_;
}

auto PackedArray::IsSigned() const -> bool {
  return is_signed_;
}

auto PackedArray::IsFourState() const -> bool {
  return is_four_state_;
}

auto PackedArray::ResetToDefault() -> void {
  // LRM Table 6-7 gives a state domain that has an unknown state a default of
  // x, and one that has not a default of zero.
  if (is_four_state_) {
    SetAllValidBits(MutableValueWords(), bit_width_);
    SetAllValidBits(MutableUnknownWords(), bit_width_);
    return;
  }
  std::ranges::fill(MutableValueWords(), std::uint64_t{0});
}

auto PackedArray::FilledLike(
    const PackedArray& prototype, const PackedArray& fill) -> PackedArray {
  const auto low_bit = [](std::span<const std::uint64_t> words) -> bool {
    return !words.empty() && (words.front() & 1U) != 0U;
  };
  const bool value_bit = low_bit(fill.ValueWords());
  const bool unknown_bit = low_bit(fill.UnknownWords());

  const std::uint64_t width = prototype.bit_width_;
  const auto words = WordCountForBits(width);
  const std::uint64_t all_ones = ~std::uint64_t{0};

  if (!prototype.is_four_state_) {
    // A 2-state shape has neither an unknown nor a high-impedance state, so
    // both collapse to the all-zero canonical default and only 1 fills.
    const std::vector<std::uint64_t> value_words(
        words, (value_bit && !unknown_bit) ? all_ones : 0);
    return FromWords(value_words, {}, width, prototype.is_signed_, false);
  }
  // The two planes spell the four scalars: 0 and 1 are the value plane with the
  // unknown plane clear, x is both set, and z is the unknown plane alone.
  // Construction masks the bits above the declared width in the top word.
  const std::vector<std::uint64_t> value_words(words, value_bit ? all_ones : 0);
  const std::vector<std::uint64_t> unknown_words(
      words, unknown_bit ? all_ones : 0);
  return FromWords(
      value_words, unknown_words, width, prototype.is_signed_, true);
}

auto PackedArray::ValueWords() const -> std::span<const std::uint64_t> {
  return std::span{planes_.data(), planes_.size()}.first(WordsPerPlane());
}

auto PackedArray::UnknownWords() const -> std::span<const std::uint64_t> {
  return std::span{planes_.data(), planes_.size()}.subspan(WordsPerPlane());
}

auto PackedArray::ByteString() const -> std::string {
  const std::uint64_t bit_width = BitWidth();
  const std::uint64_t byte_count = (bit_width + 7U) / 8U;
  std::string out;
  out.reserve(static_cast<std::size_t>(byte_count));
  const auto val_words = ValueWords();
  const auto unk_words = UnknownWords();
  for (std::uint64_t byte_i = 0; byte_i < byte_count; ++byte_i) {
    // Counted up from the value's own least significant bit, so a width that
    // does not fill its top byte leaves the bits above it clear.
    const std::uint64_t byte_base = (byte_count - 1U - byte_i) * 8U;
    unsigned char byte = 0;
    bool any_unknown = false;
    for (std::uint64_t bit_in_byte = 0; bit_in_byte < 8U; ++bit_in_byte) {
      const std::uint64_t bit_pos = byte_base + bit_in_byte;
      if (bit_pos >= bit_width) break;
      const auto word_ix = static_cast<std::size_t>(bit_pos / 64U);
      const auto bit_ix = static_cast<std::size_t>(bit_pos % 64U);
      if (!unk_words.empty() && ((unk_words[word_ix] >> bit_ix) & 1U) != 0U) {
        any_unknown = true;
      }
      if (((val_words[word_ix] >> bit_ix) & 1U) != 0U) {
        byte |= static_cast<unsigned char>(1U << bit_in_byte);
      }
    }
    out.push_back(any_unknown ? '\0' : static_cast<char>(byte));
  }
  return out;
}

auto PackedArray::IsBitIdentical(const PackedArray& other) const -> bool {
  if (bit_width_ != other.bit_width_) return false;
  if (is_four_state_ != other.is_four_state_) return false;
  const auto vw_a = ValueWords();
  const auto vw_b = other.ValueWords();
  if (vw_a.size() != vw_b.size()) return false;
  if (!std::ranges::equal(vw_a, vw_b)) return false;
  return std::ranges::equal(UnknownWords(), other.UnknownWords());
}

auto PackedArray::CaseEqual(const PackedArray& other) const -> PackedArray {
  return FromInt(IsBitIdentical(other) ? 1 : 0, 1, false, false);
}

auto PackedArray::Lsb() const -> FourStateBit {
  const auto vw = ValueWords();
  const bool value_bit = (vw[0] & std::uint64_t{1}) != 0U;
  if (!is_four_state_) {
    return value_bit ? FourStateBit::kOne : FourStateBit::kZero;
  }
  const auto uw = UnknownWords();
  const bool unknown_bit = !uw.empty() && (uw[0] & std::uint64_t{1}) != 0U;
  if (unknown_bit) {
    return value_bit ? FourStateBit::kUnknown : FourStateBit::kHighImpedance;
  }
  return value_bit ? FourStateBit::kOne : FourStateBit::kZero;
}

auto PackedArray::AsBitView() -> BitView {
  if (is_four_state_) {
    throw InternalError("PackedArray::AsBitView: storage is 4-state");
  }
  return BitView{MutableValueWords(), bit_width_};
}

auto PackedArray::AsBitView() const -> ConstBitView {
  if (is_four_state_) {
    throw InternalError("PackedArray::AsBitView: storage is 4-state");
  }
  return ConstBitView{ValueWords(), bit_width_};
}

auto PackedArray::AsLogicView() -> LogicView {
  if (!is_four_state_) {
    throw InternalError("PackedArray::AsLogicView: storage is 2-state");
  }
  return LogicView{MutableValueWords(), MutableUnknownWords(), bit_width_};
}

auto PackedArray::AsLogicView() const -> ConstLogicView {
  if (!is_four_state_) {
    throw InternalError("PackedArray::AsLogicView: storage is 2-state");
  }
  return ConstLogicView{ValueWords(), UnknownWords(), bit_width_};
}

auto PackedArray::IsUninitialized() const -> bool {
  return bit_width_ == 0;
}

auto PackedArray::SameRepresentation(const PackedArray& other) const -> bool {
  return bit_width_ == other.bit_width_ && is_signed_ == other.is_signed_ &&
         is_four_state_ == other.is_four_state_;
}

auto PackedArray::ToInt64() const -> std::int64_t {
  if (bit_width_ > 64U) {
    throw InternalError("PackedArray::ToInt64: bit_width > 64");
  }
  // LRM 6.12.1 / 6.19: when a 4-state value is read into a 2-state context,
  // X/Z bits collapse to 0. The unknown plane marks those positions, so the
  // value plane is masked by `~unknown` before any further interpretation.
  const auto value = ValueWords()[0];
  const auto unk =
      UnknownWords().empty() ? std::uint64_t{0} : UnknownWords()[0];
  const auto raw = (value & ~unk) & MaskForWidth(bit_width_);
  return is_signed_ ? SignExtendToInt64(raw, bit_width_)
                    : static_cast<std::int64_t>(raw);
}

auto PackedArray::IsTruthy() const -> bool {
  // LRM 12.4: X/Z bits do not count as truthy; only a definitively-one
  // bit (value=1 with unknown=0) makes an `if`/`while`/ternary take its
  // true branch.
  const auto vw = ValueWords();
  const auto uw = UnknownWords();
  for (std::size_t i = 0; i < vw.size(); ++i) {
    const std::uint64_t unk = i < uw.size() ? uw[i] : 0U;
    if ((vw[i] & ~unk) != 0U) {
      return true;
    }
  }
  return false;
}

auto PackedArray::HasUnknown() const -> bool {
  return std::ranges::any_of(
      UnknownWords(), [](std::uint64_t w) { return w != 0U; });
}

auto PackedArray::Truth() const -> Truthiness {
  const auto value_words = ValueWords();
  const auto unknown_words = UnknownWords();
  bool has_unknown_bit = false;
  for (std::size_t i = 0; i < value_words.size(); ++i) {
    const std::uint64_t unk = i < unknown_words.size() ? unknown_words[i] : 0U;
    if ((value_words[i] & ~unk) != 0U) {
      return Truthiness::kKnownNonzero;
    }
    if (unk != 0U) {
      has_unknown_bit = true;
    }
  }
  return has_unknown_bit ? Truthiness::kUnknown : Truthiness::kKnownZero;
}

auto PackedArray::Clog2() const -> PackedArray {
  // ceil(log2(n)) is the index of n's highest set bit, plus one unless n is an
  // exact power of two; n in {0, 1} yields 0. Read the operand as unsigned:
  // X/Z bits collapse to 0 (value plane masked by ~unknown) and the declared
  // width bounds the top word.
  const auto value_words = ValueWords();
  const auto unknown_words = UnknownWords();
  std::int64_t high_bit = -1;
  int set_bits = 0;
  for (std::size_t i = 0; i < value_words.size(); ++i) {
    const std::uint64_t unk =
        i < unknown_words.size() ? unknown_words[i] : std::uint64_t{0};
    const std::uint64_t word =
        value_words[i] & ~unk & ValidBitsMask(i, bit_width_);
    if (word == 0U) {
      continue;
    }
    set_bits += std::popcount(word);
    high_bit =
        static_cast<std::int64_t>((64U * i) + 63U - std::countl_zero(word));
  }
  const std::int32_t result =
      high_bit < 0
          ? 0
          : static_cast<std::int32_t>(high_bit + (set_bits > 1 ? 1 : 0));
  return Integer(result);
}

namespace {

// The four-state bit values a count admits, one flag per value at that value's
// own index. LRM 20.9 admits a value by naming it as a control bit, and naming
// it twice admits it once, which is what a set of flags records.
using AdmittedBitValues = std::uint8_t;

auto FlagOf(FourStateBit value) -> AdmittedBitValues {
  return static_cast<AdmittedBitValues>(1U << static_cast<unsigned>(value));
}

auto Admits(AdmittedBitValues admitted, FourStateBit value) -> bool {
  return (admitted & FlagOf(value)) != 0U;
}

auto BitValueOfPlanes(bool value, bool unknown) -> FourStateBit {
  if (!unknown) {
    return value ? FourStateBit::kOne : FourStateBit::kZero;
  }
  return value ? FourStateBit::kUnknown : FourStateBit::kHighImpedance;
}

auto AdmittedBy(const PackedArray& control_bits) -> AdmittedBitValues {
  AdmittedBitValues admitted = 0;
  const auto value_words = control_bits.ValueWords();
  const auto unknown_words = control_bits.UnknownWords();
  for (std::uint64_t i = 0; i < control_bits.BitWidth(); ++i) {
    const std::size_t word = i / 64U;
    const std::uint64_t bit = std::uint64_t{1} << (i % 64U);
    const bool unknown =
        word < unknown_words.size() && (unknown_words[word] & bit) != 0U;
    admitted = static_cast<AdmittedBitValues>(
        admitted |
        FlagOf(BitValueOfPlanes((value_words[word] & bit) != 0U, unknown)));
  }
  return admitted;
}

}  // namespace

auto PackedArray::CountBits(const PackedArray& control_bits) const
    -> PackedArray {
  const AdmittedBitValues admitted = AdmittedBy(control_bits);
  const auto value_words = ValueWords();
  const auto unknown_words = UnknownWords();
  std::int64_t count = 0;
  for (std::size_t i = 0; i < value_words.size(); ++i) {
    // The top word's bits above the declared width are storage, not value, so
    // they are excluded before counting -- otherwise they would all read as
    // zeros and inflate a `$countbits(v, '0)`.
    const std::uint64_t valid = ValidBitsMask(i, bit_width_);
    const std::uint64_t value = value_words[i] & valid;
    const std::uint64_t unknown =
        (i < unknown_words.size() ? unknown_words[i] : std::uint64_t{0}) &
        valid;
    if (Admits(admitted, FourStateBit::kZero)) {
      count += std::popcount(~value & ~unknown & valid);
    }
    if (Admits(admitted, FourStateBit::kOne)) {
      count += std::popcount(value & ~unknown);
    }
    if (Admits(admitted, FourStateBit::kHighImpedance)) {
      count += std::popcount(~value & unknown);
    }
    if (Admits(admitted, FourStateBit::kUnknown)) {
      count += std::popcount(value & unknown);
    }
  }
  return Int(static_cast<std::int32_t>(count));
}

auto PackedArray::Concat(const PackedArray& rhs) const -> PackedArray {
  const std::uint64_t total = BitWidth() + rhs.BitWidth();
  const bool any_four_state = IsFourState() || rhs.IsFourState();
  if (total == 0U) {
    throw InternalError("PackedArray::Concat: total bit width is zero");
  }
  PackedArray result = Blank(total, false, any_four_state);
  auto dst_value = result.MutableValueWords();
  auto dst_unknown = result.MutableUnknownWords();
  std::uint64_t cursor = 0;
  for (const PackedArray* op : {&rhs, this}) {
    MoveBitRun(op->ValueWords(), 0U, dst_value, cursor, op->BitWidth());
    // A two-state operand carries no unknown plane, and moving from an absent
    // one leaves the result's own positions clear, which is what it holds.
    if (any_four_state) {
      MoveBitRun(op->UnknownWords(), 0U, dst_unknown, cursor, op->BitWidth());
    }
    cursor += op->BitWidth();
  }
  return result;
}

auto PackedArray::Replicate(std::int64_t count) const -> PackedArray {
  if (count <= 0) {
    throw InternalError(
        "PackedArray::Replicate: a packed multiplier is a constant expression "
        "the front end has already checked, so a count that is not positive "
        "here would name a zero-width result");
  }
  const auto repeats = static_cast<std::uint64_t>(count);
  PackedArray result = Blank(BitWidth() * repeats, false, IsFourState());
  auto dst_value = result.MutableValueWords();
  auto dst_unknown = result.MutableUnknownWords();
  for (std::uint64_t i = 0; i < repeats; ++i) {
    const std::uint64_t cursor = i * BitWidth();
    MoveBitRun(ValueWords(), 0U, dst_value, cursor, BitWidth());
    // The result is four-state exactly when this value is, so the plane the
    // copy lands in exists on the same condition the copy has.
    if (IsFourState()) {
      MoveBitRun(UnknownWords(), 0U, dst_unknown, cursor, BitWidth());
    }
  }
  return result;
}

auto PackedArray::ToBitstream() const -> PackedArray {
  return ConvertFrom(*this, BitWidth(), false, IsFourState());
}

auto PackedArray::FromBitstream(
    const PackedArray& bits, const PackedArray& prototype) -> PackedArray {
  if (bits.BitWidth() != prototype.BitWidth()) {
    throw InternalError(
        "PackedArray::FromBitstream: the stream reaching a part is the part's "
        "own width, which whoever divided the stream owed -- please report "
        "this as a bug");
  }
  return ConvertFrom(
      bits, prototype.BitWidth(), prototype.IsSigned(),
      prototype.IsFourState());
}

auto PackedArray::ReverseBlocks(std::int64_t block_bits) const -> PackedArray {
  if (block_bits <= 0) {
    throw InternalError(
        "PackedArray::ReverseBlocks: a block size is a constant expression the "
        "front end has already checked to be positive -- please report this as "
        "a bug");
  }
  const auto block = static_cast<std::uint64_t>(block_bits);
  const std::uint64_t width = BitWidth();
  auto block_at = [&](std::uint64_t low) {
    return ConvertFrom(
        LogicalShiftRight(PackedArray::Int(static_cast<std::int32_t>(low))),
        std::min(block, width - low), false, IsFourState());
  };
  // The block starting at bit zero ends up most significant, so the blocks
  // compose in the order they are taken.
  PackedArray result = block_at(0);
  for (std::uint64_t low = block; low < width; low += block) {
    result = result.Concat(block_at(low));
  }
  return result;
}

auto BitstreamSegment(
    const PackedArray& bits, std::uint64_t consumed, std::uint64_t width)
    -> PackedArray {
  if (consumed + width > bits.BitWidth()) {
    throw InternalError(
        "BitstreamSegment: a part asked for more bits than the stream has "
        "left, which the caller that sized the stream owed -- please report "
        "this as a bug");
  }
  const std::uint64_t low = bits.BitWidth() - consumed - width;
  return PackedArray::ConvertFrom(
      bits.LogicalShiftRight(PackedArray::Int(static_cast<std::int32_t>(low))),
      width, false, bits.IsFourState());
}

auto PackedArray::ConvertBitsInto(PackedArray dst, const PackedArray& src)
    -> PackedArray {
  const Signedness src_signedness =
      src.IsSigned() ? Signedness::kSigned : Signedness::kUnsigned;
  if (dst.is_four_state_) {
    LogicView dst_view = dst.AsLogicView();
    if (src.IsFourState()) {
      ConvertToLogic(src.AsLogicView(), dst_view, src_signedness);
    } else {
      ConvertToLogic(src.AsBitView(), dst_view, src_signedness);
    }
  } else {
    BitView dst_view = dst.AsBitView();
    if (src.IsFourState()) {
      ConvertToBit(src.AsLogicView(), dst_view, src_signedness);
    } else {
      ConvertToBit(src.AsBitView(), dst_view, src_signedness);
    }
  }
  return dst;
}

auto PackedArray::ConvertFrom(
    const PackedArray& src, std::uint64_t dst_bit_width, bool dst_is_signed,
    bool dst_is_four_state) -> PackedArray {
  PackedArray dst = Blank(dst_bit_width, dst_is_signed, dst_is_four_state);
  if (src.bit_width_ > 64U || dst_bit_width > 64U) {
    return ConvertBitsInto(std::move(dst), src);
  }
  // Nearly every conversion a design performs is between two values of one
  // word each, and the rules below are the same ones the general form applies
  // word by word (LRM 6.11.2, 6.11.3, 6.12.1): the source's bits where it
  // reaches, and above them its sign bit where a signed source widens -- an x
  // or z sign filling with itself into four states, and with the 0 it becomes
  // into two.
  const std::uint64_t sign = src.bit_width_ - 1U;
  const bool pads = dst_bit_width > src.bit_width_ && src.is_signed_;
  const std::uint64_t reached = MaskForWidth(src.bit_width_);
  const auto fill = [&](std::uint64_t bits) {
    const bool set = pads && ((bits >> sign) & 1U) != 0U;
    return ((bits & reached) | (set ? ~reached : 0U)) &
           MaskForWidth(dst_bit_width);
  };
  const std::uint64_t value = src.ValueWords()[0];
  const std::uint64_t unknown =
      src.is_four_state_ ? src.UnknownWords()[0] : std::uint64_t{0};
  if (dst_is_four_state) {
    dst.MutableValueWords()[0] = fill(value);
    dst.MutableUnknownWords()[0] = fill(unknown);
  } else {
    dst.MutableValueWords()[0] = fill(value & ~unknown);
  }
  return dst;
}

auto PackedArray::ConvertFrom(const PackedArray& src, const PackedType& type)
    -> PackedArray {
  return ConvertFrom(src, type.bit_width, type.is_signed, type.is_four_state);
}

auto PackedArray::FromString(const String& text, const PackedType& type)
    -> PackedArray {
  const std::string_view chars = text.View();
  // An empty string carries no bytes, so it takes one zero byte and conforms to
  // zero -- the same path every other length follows. Bytes carry no sign and
  // no unknown state, so the text's own value is unsigned 2-state.
  const std::uint64_t bit_width = 8 * std::max<std::size_t>(chars.size(), 1);
  return ConvertFrom(
      FromBytes(
          std::span<const char>{chars.data(), chars.size()}, bit_width, false,
          false),
      type);
}

namespace {

auto NarrowWordOf(const PackedArray& v) -> std::uint64_t {
  return v.ValueWords()[0] & MaskForWidth(v.BitWidth());
}

// Encodes a known 0/1 only; for results that must be X under LRM 11.4
// propagation, use AllX(1U, false) instead.
auto OneBitResult(bool flag, bool is_four_state) -> PackedArray {
  return PackedArray::FromInt(flag ? 1 : 0, 1U, false, is_four_state);
}

// The answer of a predicate that cannot be unknown, whatever it compared: it
// carries no unknown plane, so it is two-state even over four-state operands.
auto DeterministicBit(bool flag) -> PackedArray {
  return OneBitResult(flag, false);
}

// LRM 11.4 X/Z propagation result. A four-state value of a declared shape
// reads as all-x before anything drives it (LRM Table 6-7), which is what an
// operand carrying an unknown bit propagates.
auto AllX(std::uint64_t bit_width, bool is_signed) -> PackedArray {
  return PackedArray{bit_width, is_signed, true};
}

auto AddWordsInto(
    std::span<const std::uint64_t> a, std::span<const std::uint64_t> b,
    std::span<std::uint64_t> dst, std::uint64_t bit_width) -> void {
  std::uint64_t carry = 0;
  for (std::size_t i = 0; i < dst.size(); ++i) {
    const std::uint64_t aw = i < a.size() ? a[i] : 0U;
    const std::uint64_t bw = i < b.size() ? b[i] : 0U;
    const std::uint64_t s1 = aw + bw;
    const std::uint64_t c1 = s1 < aw ? 1U : 0U;
    const std::uint64_t s2 = s1 + carry;
    const std::uint64_t c2 = s2 < s1 ? 1U : 0U;
    dst[i] = s2;
    carry = c1 + c2;
  }
  MaskUnusedTopBits(dst, bit_width);
}

auto SubWordsInto(
    std::span<const std::uint64_t> a, std::span<const std::uint64_t> b,
    std::span<std::uint64_t> dst, std::uint64_t bit_width) -> void {
  std::uint64_t borrow = 0;
  for (std::size_t i = 0; i < dst.size(); ++i) {
    const std::uint64_t aw = i < a.size() ? a[i] : 0U;
    const std::uint64_t bw = i < b.size() ? b[i] : 0U;
    const std::uint64_t d1 = aw - bw;
    const std::uint64_t b1 = aw < bw ? 1U : 0U;
    const std::uint64_t d2 = d1 - borrow;
    const std::uint64_t b2 = d1 < borrow ? 1U : 0U;
    dst[i] = d2;
    borrow = b1 + b2;
  }
  MaskUnusedTopBits(dst, bit_width);
}

auto Mul64x64(std::uint64_t a, std::uint64_t b)
    -> std::pair<std::uint64_t, std::uint64_t> {
  const std::uint64_t a_lo = a & 0xFFFFFFFFULL;
  const std::uint64_t a_hi = a >> 32U;
  const std::uint64_t b_lo = b & 0xFFFFFFFFULL;
  const std::uint64_t b_hi = b >> 32U;
  const std::uint64_t ll = a_lo * b_lo;
  const std::uint64_t lh = a_lo * b_hi;
  const std::uint64_t hl = a_hi * b_lo;
  const std::uint64_t hh = a_hi * b_hi;
  const std::uint64_t mid =
      (ll >> 32U) + (lh & 0xFFFFFFFFULL) + (hl & 0xFFFFFFFFULL);
  const std::uint64_t low = (ll & 0xFFFFFFFFULL) | (mid << 32U);
  const std::uint64_t high = hh + (lh >> 32U) + (hl >> 32U) + (mid >> 32U);
  return {low, high};
}

auto MulWordsInto(
    std::span<const std::uint64_t> a, std::span<const std::uint64_t> b,
    std::span<std::uint64_t> dst, std::uint64_t bit_width) -> void {
  std::ranges::fill(dst, std::uint64_t{0});
  for (std::size_t i = 0; i < a.size() && i < dst.size(); ++i) {
    std::uint64_t carry = 0;
    for (std::size_t j = 0; i + j < dst.size(); ++j) {
      if (j >= b.size() && carry == 0U) {
        break;
      }
      const std::uint64_t bw = j < b.size() ? b[j] : 0U;
      const auto [lo, hi] = Mul64x64(a[i], bw);
      const std::uint64_t s1 = dst[i + j] + lo;
      const std::uint64_t c1 = s1 < lo ? 1U : 0U;
      const std::uint64_t s2 = s1 + carry;
      const std::uint64_t c2 = s2 < s1 ? 1U : 0U;
      dst[i + j] = s2;
      carry = hi + c1 + c2;
    }
  }
  MaskUnusedTopBits(dst, bit_width);
}

auto UnsignedCompare(
    std::span<const std::uint64_t> a, std::span<const std::uint64_t> b) -> int {
  for (std::size_t i = a.size(); i-- > 0;) {
    const std::uint64_t aw = a[i];
    const std::uint64_t bw = i < b.size() ? b[i] : 0U;
    if (aw != bw) {
      return aw < bw ? -1 : 1;
    }
  }
  return 0;
}

auto SignedCompare(
    std::span<const std::uint64_t> a, std::span<const std::uint64_t> b,
    std::uint64_t bit_width) -> int {
  if (a.empty()) {
    return 0;
  }
  const std::uint64_t rem = bit_width % 64U;
  const std::uint64_t sign_bit =
      rem == 0U ? std::uint64_t{1} << 63U : std::uint64_t{1} << (rem - 1U);
  const std::uint64_t a_top = a.back() ^ sign_bit;
  const std::uint64_t b_top = b.back() ^ sign_bit;
  if (a_top != b_top) {
    return a_top < b_top ? -1 : 1;
  }
  for (std::size_t i = a.size() - 1U; i-- > 0;) {
    const std::uint64_t aw = a[i];
    const std::uint64_t bw = i < b.size() ? b[i] : 0U;
    if (aw != bw) {
      return aw < bw ? -1 : 1;
    }
  }
  return 0;
}

auto ShiftLeftWordsInto(
    std::span<const std::uint64_t> src, std::uint64_t amount,
    std::span<std::uint64_t> dst, std::uint64_t bit_width) -> void {
  std::ranges::fill(dst, std::uint64_t{0});
  if (amount >= bit_width) {
    return;
  }
  const std::uint64_t word_shift = amount / 64U;
  const std::uint64_t bit_shift = amount % 64U;
  if (bit_shift == 0U) {
    for (std::size_t i = 0; i < src.size() && (i + word_shift) < dst.size();
         ++i) {
      dst[i + word_shift] = src[i];
    }
  } else {
    const std::uint64_t rev = 64U - bit_shift;
    std::uint64_t prev_high = 0U;
    for (std::size_t i = 0; i < src.size() && (i + word_shift) < dst.size();
         ++i) {
      const std::uint64_t cur = src[i];
      dst[i + word_shift] = (cur << bit_shift) | prev_high;
      prev_high = cur >> rev;
    }
  }
  MaskUnusedTopBits(dst, bit_width);
}

auto LogicalShiftRightWordsInto(
    std::span<const std::uint64_t> src, std::uint64_t amount,
    std::span<std::uint64_t> dst, std::uint64_t bit_width) -> void {
  std::ranges::fill(dst, std::uint64_t{0});
  if (amount >= bit_width) {
    return;
  }
  const std::uint64_t word_shift = amount / 64U;
  const std::uint64_t bit_shift = amount % 64U;
  if (bit_shift == 0U) {
    for (std::size_t i = word_shift; i < src.size(); ++i) {
      dst[i - word_shift] = src[i];
    }
  } else {
    const std::uint64_t rev = 64U - bit_shift;
    for (std::size_t i = word_shift; i < src.size(); ++i) {
      const std::uint64_t cur = src[i];
      const std::uint64_t next = (i + 1U) < src.size() ? src[i + 1U] : 0U;
      dst[i - word_shift] = (cur >> bit_shift) | (next << rev);
    }
  }
  MaskUnusedTopBits(dst, bit_width);
}

auto SetBitAt(std::span<std::uint64_t> words, std::uint64_t pos) -> void {
  const auto idx = static_cast<std::size_t>(pos / 64U);
  words[idx] |= std::uint64_t{1} << (pos % 64U);
}

auto IsZero(std::span<const std::uint64_t> words) -> bool {
  return std::ranges::all_of(words, [](std::uint64_t w) { return w == 0U; });
}

auto ShiftLeftOneInPlace(
    std::span<std::uint64_t> words, std::uint64_t bit_width) -> void {
  std::uint64_t carry = 0;
  for (auto& w : words) {
    const std::uint64_t new_carry = w >> 63U;
    w = (w << 1U) | carry;
    carry = new_carry;
  }
  MaskUnusedTopBits(words, bit_width);
}

// Bit-by-bit unsigned long division. `numerator` and `divisor` are read-only
// word arrays of width `bit_width`. `divisor` must be non-zero (caller
// handles div-by-zero). Writes quotient to `quot` and remainder to `rem`.
auto LongDivideUnsigned(
    std::span<const std::uint64_t> numerator,
    std::span<const std::uint64_t> divisor, std::span<std::uint64_t> quot,
    std::span<std::uint64_t> rem, std::uint64_t bit_width) -> void {
  std::ranges::fill(quot, std::uint64_t{0});
  std::ranges::fill(rem, std::uint64_t{0});
  for (std::uint64_t pos = bit_width; pos-- > 0;) {
    ShiftLeftOneInPlace(rem, bit_width);
    rem[0] |= BitAt(numerator, pos) ? std::uint64_t{1} : std::uint64_t{0};
    if (UnsignedCompare(rem, divisor) >= 0) {
      SubWordsInto(rem, divisor, rem, bit_width);
      SetBitAt(quot, pos);
    }
  }
}

auto FillTopBits(
    std::span<std::uint64_t> dst, std::uint64_t bit_width, std::uint64_t amount)
    -> void {
  if (amount == 0U) {
    return;
  }
  if (amount >= bit_width) {
    std::ranges::fill(dst, ~std::uint64_t{0});
    MaskUnusedTopBits(dst, bit_width);
    return;
  }
  const std::uint64_t start = bit_width - amount;
  auto i = static_cast<std::size_t>(start / 64U);
  const std::uint64_t bit_offset = start % 64U;
  if (bit_offset != 0U) {
    dst[i] |= ~((std::uint64_t{1} << bit_offset) - 1U);
    ++i;
  }
  for (; i < dst.size(); ++i) {
    dst[i] = ~std::uint64_t{0};
  }
  MaskUnusedTopBits(dst, bit_width);
}

// LRM 11.4.5 logical equality, which is x only where the unknown bits leave
// the relation ambiguous: a position both operands know and disagree on
// settles them as unequal however many unknown bits sit beside it, so only
// agreement everywhere both are known leaves the answer to those bits.
auto KnownBitsEqual(const PackedArray& a, const PackedArray& b)
    -> std::optional<bool> {
  const auto a_val = a.ValueWords();
  const auto b_val = b.ValueWords();
  const auto a_unk = a.UnknownWords();
  const auto b_unk = b.UnknownWords();
  bool has_unknown_bit = false;
  for (std::size_t i = 0; i < a_val.size(); ++i) {
    const std::uint64_t unk =
        (i < a_unk.size() ? a_unk[i] : 0U) | (i < b_unk.size() ? b_unk[i] : 0U);
    if (((a_val[i] ^ b_val[i]) & ~unk) != 0U) {
      return false;
    }
    if (unk != 0U) {
      has_unknown_bit = true;
    }
  }
  if (has_unknown_bit) {
    return std::nullopt;
  }
  return true;
}

// Both answers of one division, taken over the operands' magnitudes, with what
// each operand's sign was. LRM 11.4.4 signs the two answers by different rules
// -- a quotient is negative when the operands disagree, a remainder takes the
// dividend's sign alone -- so the division runs once and each answer is signed
// by whoever asked for it.
struct DividedMagnitudes {
  PackedWordArray quotient;
  PackedWordArray remainder;
  bool dividend_negative;
  bool divisor_negative;
};

auto DivideMagnitudes(
    std::span<const std::uint64_t> dividend,
    std::span<const std::uint64_t> divisor, std::uint64_t bit_width,
    bool is_signed) -> DividedMagnitudes {
  const bool neg_a = is_signed && BitAt(dividend, bit_width - 1U);
  const bool neg_b = is_signed && BitAt(divisor, bit_width - 1U);
  auto magnitude = [bit_width](std::span<const std::uint64_t> words, bool neg) {
    auto out = ZeroedWords(bit_width);
    if (neg) {
      SubWordsInto({}, words, out, bit_width);
    } else {
      std::ranges::copy(words, out.begin());
    }
    return out;
  };
  DividedMagnitudes result{
      .quotient = ZeroedWords(bit_width),
      .remainder = ZeroedWords(bit_width),
      .dividend_negative = neg_a,
      .divisor_negative = neg_b};
  LongDivideUnsigned(
      magnitude(dividend, neg_a), magnitude(divisor, neg_b), result.quotient,
      result.remainder, bit_width);
  return result;
}

// The negation of `words`, or `words` itself where the answer is positive.
auto SignedAs(PackedWordArray words, bool negative, std::uint64_t bit_width)
    -> PackedWordArray {
  if (!negative) {
    return words;
  }
  auto negated = ZeroedWords(bit_width);
  SubWordsInto({}, words, negated, bit_width);
  return negated;
}

}  // namespace

auto PackedArray::operator+(const PackedArray& other) const -> PackedArray {
  RequireSameStorageDomain(*this, other, "operator+");
  if (HasUnknown() || other.HasUnknown()) {
    return AllX(bit_width_, is_signed_);
  }
  PackedArray result = Blank(bit_width_, is_signed_, is_four_state_);
  AddWordsInto(
      ValueWords(), other.ValueWords(), result.MutableValueWords(), bit_width_);
  return result;
}

auto PackedArray::operator-(const PackedArray& other) const -> PackedArray {
  RequireSameStorageDomain(*this, other, "operator-");
  if (HasUnknown() || other.HasUnknown()) {
    return AllX(bit_width_, is_signed_);
  }
  PackedArray result = Blank(bit_width_, is_signed_, is_four_state_);
  SubWordsInto(
      ValueWords(), other.ValueWords(), result.MutableValueWords(), bit_width_);
  return result;
}

auto PackedArray::operator*(const PackedArray& other) const -> PackedArray {
  RequireSameStorageDomain(*this, other, "operator*");
  if (HasUnknown() || other.HasUnknown()) {
    return AllX(bit_width_, is_signed_);
  }
  PackedArray result = Blank(bit_width_, is_signed_, is_four_state_);
  MulWordsInto(
      ValueWords(), other.ValueWords(), result.MutableValueWords(), bit_width_);
  return result;
}

auto PackedArray::operator/(const PackedArray& other) const -> PackedArray {
  RequireSameStorageDomain(*this, other, "operator/");
  if (HasUnknown() || other.HasUnknown()) {
    return AllX(bit_width_, is_signed_);
  }
  if (IsZero(other.ValueWords())) {
    // LRM 11.4.4: integer division by zero on 4-state yields X for every bit;
    // on 2-state it is implementation-defined and we pick zero. Both are the
    // declared shape's own default value (LRM Table 6-7), so naming the shape
    // states them.
    return PackedArray{bit_width_, is_signed_, is_four_state_};
  }
  auto divided = DivideMagnitudes(
      ValueWords(), other.ValueWords(), bit_width_, is_signed_);
  const auto quotient = SignedAs(
      std::move(divided.quotient),
      divided.dividend_negative != divided.divisor_negative, bit_width_);
  return MakeFromWordPlanes(
      bit_width_, is_signed_, is_four_state_,
      std::span<const std::uint64_t>{quotient.data(), quotient.size()}, {});
}

auto PackedArray::operator%(const PackedArray& other) const -> PackedArray {
  RequireSameStorageDomain(*this, other, "operator%");
  if (HasUnknown() || other.HasUnknown()) {
    return AllX(bit_width_, is_signed_);
  }
  if (IsZero(other.ValueWords())) {
    // LRM 11.4.4 again, with the same reading as division by zero: the
    // declared shape's own default value, all-X on 4-state and zero on 2.
    return PackedArray{bit_width_, is_signed_, is_four_state_};
  }
  auto divided = DivideMagnitudes(
      ValueWords(), other.ValueWords(), bit_width_, is_signed_);
  const auto remainder = SignedAs(
      std::move(divided.remainder), divided.dividend_negative, bit_width_);
  return MakeFromWordPlanes(
      bit_width_, is_signed_, is_four_state_,
      std::span<const std::uint64_t>{remainder.data(), remainder.size()}, {});
}

auto PackedArray::operator&(const PackedArray& other) const -> PackedArray {
  RequireSameStorageDomain(*this, other, "operator&");
  if (bit_width_ <= 64U && !is_four_state_) {
    const auto mask = MaskForWidth(bit_width_);
    return FromInt(
        static_cast<std::int64_t>(
            (NarrowWordOf(*this) & NarrowWordOf(other)) & mask),
        bit_width_, is_signed_, is_four_state_);
  }
  PackedArray result = Blank(bit_width_, is_signed_, is_four_state_);
  if (is_four_state_) {
    BitwiseAnd(AsLogicView(), other.AsLogicView(), result.AsLogicView());
  } else {
    BitwiseAnd(AsBitView(), other.AsBitView(), result.AsBitView());
  }
  return result;
}

auto PackedArray::operator|(const PackedArray& other) const -> PackedArray {
  RequireSameStorageDomain(*this, other, "operator|");
  if (bit_width_ <= 64U && !is_four_state_) {
    const auto mask = MaskForWidth(bit_width_);
    return FromInt(
        static_cast<std::int64_t>(
            (NarrowWordOf(*this) | NarrowWordOf(other)) & mask),
        bit_width_, is_signed_, is_four_state_);
  }
  PackedArray result = Blank(bit_width_, is_signed_, is_four_state_);
  if (is_four_state_) {
    BitwiseOr(AsLogicView(), other.AsLogicView(), result.AsLogicView());
  } else {
    BitwiseOr(AsBitView(), other.AsBitView(), result.AsBitView());
  }
  return result;
}

auto PackedArray::operator^(const PackedArray& other) const -> PackedArray {
  RequireSameStorageDomain(*this, other, "operator^");
  if (bit_width_ <= 64U && !is_four_state_) {
    const auto mask = MaskForWidth(bit_width_);
    return FromInt(
        static_cast<std::int64_t>(
            (NarrowWordOf(*this) ^ NarrowWordOf(other)) & mask),
        bit_width_, is_signed_, is_four_state_);
  }
  PackedArray result = Blank(bit_width_, is_signed_, is_four_state_);
  if (is_four_state_) {
    BitwiseXor(AsLogicView(), other.AsLogicView(), result.AsLogicView());
  } else {
    BitwiseXor(AsBitView(), other.AsBitView(), result.AsBitView());
  }
  return result;
}

auto PackedArray::BitwiseXnor(const PackedArray& other) const -> PackedArray {
  RequireSameStorageDomain(*this, other, "BitwiseXnor");
  if (bit_width_ <= 64U && !is_four_state_) {
    const auto mask = MaskForWidth(bit_width_);
    return FromInt(
        static_cast<std::int64_t>(
            (~(NarrowWordOf(*this) ^ NarrowWordOf(other))) & mask),
        bit_width_, is_signed_, is_four_state_);
  }
  PackedArray result = Blank(bit_width_, is_signed_, is_four_state_);
  if (is_four_state_) {
    value::BitwiseXnor(
        AsLogicView(), other.AsLogicView(), result.AsLogicView());
  } else {
    value::BitwiseXnor(AsBitView(), other.AsBitView(), result.AsBitView());
  }
  return result;
}

auto PackedArray::operator==(const PackedArray& other) const -> PackedArray {
  RequireSameStorageDomain(*this, other, "operator==");
  const std::optional<bool> equal = KnownBitsEqual(*this, other);
  if (!equal.has_value()) {
    return AllX(1U, false);
  }
  return OneBitResult(*equal, is_four_state_);
}

auto PackedArray::operator!=(const PackedArray& other) const -> PackedArray {
  RequireSameStorageDomain(*this, other, "operator!=");
  const std::optional<bool> equal = KnownBitsEqual(*this, other);
  if (!equal.has_value()) {
    return AllX(1U, false);
  }
  return OneBitResult(!*equal, is_four_state_);
}

auto PackedArray::WildcardEquals(const PackedArray& other) const
    -> PackedArray {
  RequireSameStorageDomain(*this, other, "WildcardEquals");
  const auto words = WordCountForBits(bit_width_);
  const auto a_val = ValueWords();
  const auto b_val = other.ValueWords();
  const auto a_unk = UnknownWords();
  const auto b_unk = other.UnknownWords();
  bool definite_mismatch = false;
  bool lhs_unknown_at_compare = false;
  for (std::size_t w = 0; w < words; ++w) {
    const std::uint64_t bw_unk = w < b_unk.size() ? b_unk[w] : 0U;
    const std::uint64_t aw_unk = w < a_unk.size() ? a_unk[w] : 0U;
    const std::uint64_t cmp_mask = ~bw_unk & ValidBitsMask(w, bit_width_);
    const std::uint64_t diff = a_val[w] ^ b_val[w];
    if ((diff & ~aw_unk & cmp_mask) != 0U) {
      definite_mismatch = true;
    }
    if ((aw_unk & cmp_mask) != 0U) {
      lhs_unknown_at_compare = true;
    }
  }
  if (definite_mismatch) {
    return OneBitResult(false, is_four_state_);
  }
  if (lhs_unknown_at_compare) {
    return AllX(1U, false);
  }
  return OneBitResult(true, is_four_state_);
}

auto PackedArray::CasezEquals(const PackedArray& other) const -> PackedArray {
  RequireSameStorageDomain(*this, other, "CasezEquals");
  const auto words = WordCountForBits(bit_width_);
  const auto a_val = ValueWords();
  const auto b_val = other.ValueWords();
  const auto a_unk = UnknownWords();
  const auto b_unk = other.UnknownWords();
  for (std::size_t w = 0; w < words; ++w) {
    const std::uint64_t aw_val = a_val[w];
    const std::uint64_t bw_val = b_val[w];
    const std::uint64_t aw_unk = w < a_unk.size() ? a_unk[w] : 0U;
    const std::uint64_t bw_unk = w < b_unk.size() ? b_unk[w] : 0U;
    // Lyra encoding (see packed.cpp): X = (value=1, unknown=1);
    // Z = (value=0, unknown=1). casez masks out Z bits on either side; X bits
    // on either side are NOT wildcards and must still match exactly.
    const std::uint64_t a_z = aw_unk & ~aw_val;
    const std::uint64_t b_z = bw_unk & ~bw_val;
    const std::uint64_t cmp_mask = ~(a_z | b_z) & ValidBitsMask(w, bit_width_);
    if (((aw_val ^ bw_val) & cmp_mask) != 0U ||
        ((aw_unk ^ bw_unk) & cmp_mask) != 0U) {
      return DeterministicBit(false);
    }
  }
  return DeterministicBit(true);
}

auto PackedArray::CasexEquals(const PackedArray& other) const -> PackedArray {
  RequireSameStorageDomain(*this, other, "CasexEquals");
  const auto words = WordCountForBits(bit_width_);
  const auto a_val = ValueWords();
  const auto b_val = other.ValueWords();
  const auto a_unk = UnknownWords();
  const auto b_unk = other.UnknownWords();
  for (std::size_t w = 0; w < words; ++w) {
    const std::uint64_t aw_val = a_val[w];
    const std::uint64_t bw_val = b_val[w];
    const std::uint64_t aw_unk = w < a_unk.size() ? a_unk[w] : 0U;
    const std::uint64_t bw_unk = w < b_unk.size() ? b_unk[w] : 0U;
    // casex masks out any-unknown bits on either side (X or Z); the value
    // plane carries the comparison on the remaining bits.
    const std::uint64_t cmp_mask =
        ~(aw_unk | bw_unk) & ValidBitsMask(w, bit_width_);
    if (((aw_val ^ bw_val) & cmp_mask) != 0U) {
      return DeterministicBit(false);
    }
  }
  return DeterministicBit(true);
}

auto PackedArray::MergeConditional(const PackedArray& other) const
    -> PackedArray {
  RequireSameStorageDomain(*this, other, "MergeConditional");
  const auto words = WordCountForBits(bit_width_);
  const auto a_val = ValueWords();
  const auto b_val = other.ValueWords();
  const auto a_unk = UnknownWords();
  const auto b_unk = other.UnknownWords();
  std::vector<std::uint64_t> res_val(words, 0U);
  std::vector<std::uint64_t> res_unk(words, 0U);
  for (std::size_t w = 0; w < words; ++w) {
    const std::uint64_t au = w < a_unk.size() ? a_unk[w] : 0U;
    const std::uint64_t bu = w < b_unk.size() ? b_unk[w] : 0U;
    // X is (value=1, unknown=1), which is what every bit the two arms do not
    // both know and agree on becomes.
    const std::uint64_t agreed = ~(a_val[w] ^ b_val[w]) & ~(au | bu);
    const std::uint64_t valid = ValidBitsMask(w, bit_width_);
    res_val[w] = ((a_val[w] & agreed) | ~agreed) & valid;
    res_unk[w] = ~agreed & valid;
  }
  return FromWords(res_val, res_unk, bit_width_, is_signed_, true);
}

auto PackedArray::ResolveNet(const PackedArray& other, NetResolution fold) const
    -> PackedArray {
  RequireSameStorageDomain(*this, other, "ResolveNet");
  const auto words = WordCountForBits(bit_width_);
  const auto a_val = ValueWords();
  const auto b_val = other.ValueWords();
  const auto a_unk = UnknownWords();
  const auto b_unk = other.UnknownWords();
  std::vector<std::uint64_t> res_val(words, 0U);
  std::vector<std::uint64_t> res_unk(words, 0U);
  for (std::size_t w = 0; w < words; ++w) {
    const std::uint64_t av = a_val[w];
    const std::uint64_t bv = b_val[w];
    const std::uint64_t au = w < a_unk.size() ? a_unk[w] : 0U;
    const std::uint64_t bu = w < b_unk.size() ? b_unk[w] : 0U;
    // Lyra encoding (see packed.cpp): Z = (value=0, unknown=1) is every fold's
    // identity and defers to the other driver; X = (value=1, unknown=1). The
    // folds differ only in how two driving bits combine. A bit only one driver
    // drives takes that driver's value; `both` is where the fold decides.
    const std::uint64_t a_z = ~av & au;
    const std::uint64_t b_z = ~bv & bu;
    const std::uint64_t take_b = a_z;
    const std::uint64_t take_a = ~a_z & b_z;
    const std::uint64_t both = ~a_z & ~b_z;
    std::uint64_t v = 0U;
    std::uint64_t u = 0U;
    switch (fold) {
      case NetResolution::kTriState: {
        // LRM 6.6.1 Table 6-2: equal drivers pass through, a 0/1 conflict is X.
        const std::uint64_t eq = ~(av ^ bv) & ~(au ^ bu);
        const std::uint64_t pass = both & eq;
        const std::uint64_t conflict = both & ~eq;
        v = (take_b & bv) | ((take_a | pass) & av) | conflict;
        u = (take_b & bu) | ((take_a | pass) & au) | conflict;
        break;
      }
      case NetResolution::kWiredAnd: {
        // LRM 6.6.3 Table 6-3: any 0 wins, else X dominates, else both drive 1.
        const std::uint64_t any_zero = (~av & ~au) | (~bv & ~bu);
        const std::uint64_t any_x = (av & au) | (bv & bu);
        const std::uint64_t x_bits = both & ~any_zero & any_x;
        const std::uint64_t one_bits = both & ~any_zero & ~any_x;
        v = (take_b & bv) | (take_a & av) | x_bits | one_bits;
        u = (take_b & bu) | (take_a & au) | x_bits;
        break;
      }
      case NetResolution::kWiredOr: {
        // LRM 6.6.3 Table 6-4: any 1 wins, else X dominates, else both drive 0.
        const std::uint64_t any_one = (av & ~au) | (bv & ~bu);
        const std::uint64_t any_x = (av & au) | (bv & bu);
        const std::uint64_t x_bits = both & ~any_one & any_x;
        const std::uint64_t one_bits = both & any_one;
        v = (take_b & bv) | (take_a & av) | one_bits | x_bits;
        u = (take_b & bu) | (take_a & au) | x_bits;
        break;
      }
    }
    const std::uint64_t valid = ValidBitsMask(w, bit_width_);
    res_val[w] = v & valid;
    res_unk[w] = u & valid;
  }
  return FromWords(res_val, res_unk, bit_width_, is_signed_, is_four_state_);
}

auto PackedArray::Dominating(const PackedArray& weaker) const -> PackedArray {
  RequireSameStorageDomain(*this, weaker, "Dominating");
  const auto words = WordCountForBits(bit_width_);
  const auto a_val = ValueWords();
  const auto b_val = weaker.ValueWords();
  const auto a_unk = UnknownWords();
  const auto b_unk = weaker.UnknownWords();
  std::vector<std::uint64_t> res_val(words, 0U);
  std::vector<std::uint64_t> res_unk(words, 0U);
  for (std::size_t w = 0; w < words; ++w) {
    const std::uint64_t av = a_val[w];
    const std::uint64_t bv = b_val[w];
    const std::uint64_t au = w < a_unk.size() ? a_unk[w] : 0U;
    const std::uint64_t bu = w < b_unk.size() ? b_unk[w] : 0U;
    // A position is undriven exactly where this value is Z (value 0, unknown
    // 1); everywhere else it determines the result, so a 2-state shape, which
    // has no Z, determines all of it.
    const std::uint64_t undriven = ~av & au;
    res_val[w] = (av & ~undriven) | (bv & undriven);
    res_unk[w] = (au & ~undriven) | (bu & undriven);
  }
  return FromWords(res_val, res_unk, bit_width_, is_signed_, is_four_state_);
}

namespace {

// Where a run named at `start` meets a value `value_width` bits wide, said the
// way a move asks for it: how far into the run the two overlap, how far into
// the value, and how many positions they share. A run the value does not reach
// shares nothing, which is a count of zero rather than a case of its own.
struct RunOverlap {
  std::uint64_t in_run;
  std::uint64_t in_value;
  std::uint64_t count;
};

auto OverlapOf(
    std::int64_t start, std::int64_t run_width, std::int64_t value_width)
    -> RunOverlap {
  // Off either end the two share nothing, and settling that first is what
  // keeps the arithmetic below inside its range: a start beyond a run's own
  // width is the only way these sums overflow.
  if (start <= -run_width || start >= value_width) {
    return RunOverlap{};
  }
  const std::int64_t in_run = start < 0 ? -start : 0;
  const std::int64_t end = std::min(run_width, value_width - start);
  return RunOverlap{
      .in_run = static_cast<std::uint64_t>(in_run),
      .in_value = static_cast<std::uint64_t>(start + in_run),
      .count = static_cast<std::uint64_t>(end - in_run)};
}

}  // namespace

auto PackedArray::ExtractRun(std::int64_t start, std::uint64_t bit_width) const
    -> PackedArray {
  if (bit_width == 0U) {
    throw InternalError("PackedArray::ExtractRun: bit_width must be >= 1");
  }
  const RunOverlap overlap = OverlapOf(
      start, static_cast<std::int64_t>(bit_width),
      static_cast<std::int64_t>(bit_width_));

  // A run of bits taken out of a value is unsigned whatever the value was (LRM
  // 11.5.1).
  PackedArray result = Blank(bit_width, false, is_four_state_);
  MoveBitRun(
      ValueWords(), overlap.in_value, result.MutableValueWords(),
      overlap.in_run, overlap.count);
  if (!is_four_state_) {
    return result;
  }
  MoveBitRun(
      UnknownWords(), overlap.in_value, result.MutableUnknownWords(),
      overlap.in_run, overlap.count);
  // Whatever the run reaches for outside the value is x (LRM 11.5.1), which is
  // both planes set. A two-state value reads those positions as the zero the
  // blank already carries.
  const std::uint64_t reached = overlap.in_run + overlap.count;
  const std::uint64_t above = bit_width - reached;
  SetBitRun(result.MutableValueWords(), 0U, overlap.in_run);
  SetBitRun(result.MutableUnknownWords(), 0U, overlap.in_run);
  SetBitRun(result.MutableValueWords(), reached, above);
  SetBitRun(result.MutableUnknownWords(), reached, above);
  return result;
}

auto PackedArray::AssignRun(
    std::int64_t start, std::uint64_t bit_width, const PackedArray& value)
    -> void {
  if (bit_width == 0U) {
    throw InternalError("PackedArray::AssignRun: bit_width must be >= 1");
  }
  if (value.BitWidth() != bit_width) {
    throw InternalError(
        "PackedArray::AssignRun: value width does not match the run's width");
  }
  // A 2-state field inside a 4-state aggregate (LRM 7.2.1) writes a 2-state
  // value into a 4-state slot; the loop below clears the unknown plane at the
  // written positions (the value carries none), widening it. The reverse never
  // occurs: a 2-state aggregate has only 2-state fields, so a 4-state value
  // reaching 2-state storage is a missing upstream coercion.
  if (value.IsFourState() && !is_four_state_) {
    throw InternalError(
        "PackedArray::AssignRun: a 4-state value cannot be written into "
        "2-state storage");
  }
  const RunOverlap overlap = OverlapOf(
      start, static_cast<std::int64_t>(bit_width),
      static_cast<std::int64_t>(bit_width_));

  MoveBitRun(
      value.ValueWords(), overlap.in_run, MutableValueWords(), overlap.in_value,
      overlap.count);
  if (is_four_state_) {
    // A two-state value carries no unknown plane at all, so moving from it
    // settles the positions it lands on, which is what the widening above
    // requires.
    MoveBitRun(
        value.UnknownWords(), overlap.in_run, MutableUnknownWords(),
        overlap.in_value, overlap.count);
  }
}

namespace {

// A run of `width` bits wide in a value of the given domain, every one of them
// what a read outside a value yields: x for four-state, 0 for two-state (LRM
// 11.5.1).
auto Unreached(std::uint64_t width, bool is_four_state) -> PackedArray {
  return PackedArray{width, false, is_four_state};
}

// A run's width as the count a select states. A select never names an empty
// run, so a count below one is a lowering defect rather than a value.
auto RunWidth(std::int64_t width) -> std::uint64_t {
  if (width < 1) {
    throw InternalError("a packed select names a run of at least one bit");
  }
  return static_cast<std::uint64_t>(width);
}

}  // namespace

auto PackedArray::Slice(const PackedArray& position, std::int64_t width) const
    -> PackedArray {
  const std::uint64_t run = RunWidth(width);
  const std::optional<std::int64_t> start = ReadPosition(position);
  if (!start) {
    return Unreached(run, is_four_state_);
  }
  return ExtractRun(*start, run);
}

auto PackedArray::SliceRef(const PackedArray& position, std::int64_t width)
    -> PackedArrayRef {
  return PackedArrayRef{*this, ReadPosition(position), RunWidth(width)};
}

auto PackedArray::WithSlice(
    const PackedArray& position, std::int64_t width,
    const PackedArray& value) const -> PackedArray {
  PackedArray result{*this};
  result.SliceRef(position, width) = value;
  return result;
}

// Defined with the value's own accessors: every select reads a position, and
// here each accessor it calls is a load rather than a call.
auto ReadPosition(const PackedArray& position) -> std::optional<std::int64_t> {
  if (position.HasUnknown()) {
    return std::nullopt;
  }
  const auto words = position.ValueWords();
  const std::uint64_t width = position.BitWidth();
  const bool negative =
      position.IsSigned() &&
      ((words[(width - 1U) / 64U] >> ((width - 1U) % 64U)) & 1U) != 0U;
  // The value fits a machine word exactly when every bit above the low 63
  // repeats its sign, which a narrower value does by being narrower.
  const std::uint64_t low_word =
      words[0] | (negative ? ~MaskForWidth(width) : std::uint64_t{0});
  if (width >= 64U && ((low_word >> 63U) != 0U) != negative) {
    return std::nullopt;
  }
  for (std::size_t i = 1; i < words.size(); ++i) {
    const std::uint64_t expected =
        negative ? ValidBitsMask(i, width) : std::uint64_t{0};
    if (words[i] != expected) {
      return std::nullopt;
    }
  }
  const auto value = static_cast<std::int64_t>(low_word);
  if (value < -kPositionLimit || value > kPositionLimit) {
    return std::nullopt;
  }
  return value;
}

auto PackedArray::ToPosition(const PackedArray& index) -> PackedArray {
  const std::optional<std::int64_t> named = ReadPosition(index);
  if (!named) {
    return PackedArray{64U, true, true};
  }
  return FromInt(*named, 64U, true, true);
}

PackedArrayRef::PackedArrayRef(
    PackedArray& root, std::optional<std::int64_t> start,
    std::uint64_t bit_width)
    : root_(&root), start_(start), bit_width_(bit_width) {
}

auto PackedArrayRef::ToOwned() const -> PackedArray {
  if (!start_) {
    return Unreached(bit_width_, root_->IsFourState());
  }
  return std::as_const(*root_).ExtractRun(*start_, bit_width_);
}

auto PackedArrayRef::operator=(const PackedArray& value) -> PackedArrayRef& {
  if (start_) {
    root_->AssignRun(*start_, bit_width_, value);
  }
  return *this;
}

auto PackedArrayRef::SliceRef(
    const PackedArray& position, std::int64_t width) const -> PackedArrayRef {
  const std::optional<std::int64_t> inner = ReadPosition(position);
  std::optional<std::int64_t> start;
  if (start_ && inner) {
    start = *start_ + *inner;
  }
  return PackedArrayRef{*root_, start, RunWidth(width)};
}

auto PackedArray::operator<(const PackedArray& other) const -> PackedArray {
  RequireSameStorageDomain(*this, other, "operator<");
  if (HasUnknown() || other.HasUnknown()) {
    return AllX(1U, false);
  }
  const int cmp =
      is_signed_ ? SignedCompare(ValueWords(), other.ValueWords(), bit_width_)
                 : UnsignedCompare(ValueWords(), other.ValueWords());
  return OneBitResult(cmp < 0, is_four_state_);
}

auto PackedArray::operator<=(const PackedArray& other) const -> PackedArray {
  RequireSameStorageDomain(*this, other, "operator<=");
  if (HasUnknown() || other.HasUnknown()) {
    return AllX(1U, false);
  }
  const int cmp =
      is_signed_ ? SignedCompare(ValueWords(), other.ValueWords(), bit_width_)
                 : UnsignedCompare(ValueWords(), other.ValueWords());
  return OneBitResult(cmp <= 0, is_four_state_);
}

auto PackedArray::operator>(const PackedArray& other) const -> PackedArray {
  RequireSameStorageDomain(*this, other, "operator>");
  if (HasUnknown() || other.HasUnknown()) {
    return AllX(1U, false);
  }
  const int cmp =
      is_signed_ ? SignedCompare(ValueWords(), other.ValueWords(), bit_width_)
                 : UnsignedCompare(ValueWords(), other.ValueWords());
  return OneBitResult(cmp > 0, is_four_state_);
}

auto PackedArray::operator>=(const PackedArray& other) const -> PackedArray {
  RequireSameStorageDomain(*this, other, "operator>=");
  if (HasUnknown() || other.HasUnknown()) {
    return AllX(1U, false);
  }
  const int cmp =
      is_signed_ ? SignedCompare(ValueWords(), other.ValueWords(), bit_width_)
                 : UnsignedCompare(ValueWords(), other.ValueWords());
  return OneBitResult(cmp >= 0, is_four_state_);
}

auto PackedArray::operator-() const -> PackedArray {
  if (HasUnknown()) {
    return AllX(bit_width_, is_signed_);
  }
  PackedArray result = Blank(bit_width_, is_signed_, is_four_state_);
  SubWordsInto({}, ValueWords(), result.MutableValueWords(), bit_width_);
  return result;
}

auto PackedArray::operator~() const -> PackedArray {
  if (bit_width_ <= 64U && !is_four_state_) {
    const auto mask = MaskForWidth(bit_width_);
    return FromInt(
        static_cast<std::int64_t>((~NarrowWordOf(*this)) & mask), bit_width_,
        is_signed_, false);
  }
  PackedArray result = Blank(bit_width_, is_signed_, is_four_state_);
  if (is_four_state_) {
    BitwiseNot(AsLogicView(), result.AsLogicView());
  } else {
    BitwiseNot(AsBitView(), result.AsBitView());
  }
  return result;
}

auto PackedArray::operator&&(const PackedArray& other) const -> PackedArray {
  const auto a = Truth();
  const auto b = other.Truth();
  const bool result_four_state = is_four_state_ || other.is_four_state_;
  if (a == Truthiness::kKnownZero || b == Truthiness::kKnownZero) {
    return OneBitResult(false, result_four_state);
  }
  if (a == Truthiness::kKnownNonzero && b == Truthiness::kKnownNonzero) {
    return OneBitResult(true, result_four_state);
  }
  return AllX(1U, false);
}

auto PackedArray::operator||(const PackedArray& other) const -> PackedArray {
  const auto a = Truth();
  const auto b = other.Truth();
  const bool result_four_state = is_four_state_ || other.is_four_state_;
  if (a == Truthiness::kKnownNonzero || b == Truthiness::kKnownNonzero) {
    return OneBitResult(true, result_four_state);
  }
  if (a == Truthiness::kKnownZero && b == Truthiness::kKnownZero) {
    return OneBitResult(false, result_four_state);
  }
  return AllX(1U, false);
}

auto PackedArray::operator!() const -> PackedArray {
  switch (Truth()) {
    case Truthiness::kKnownZero:
      return OneBitResult(true, is_four_state_);
    case Truthiness::kKnownNonzero:
      return OneBitResult(false, is_four_state_);
    case Truthiness::kUnknown:
      return AllX(1U, false);
  }
  throw InternalError("operator!: unhandled Truthiness");
}

auto PackedArray::LogicalImplication(const PackedArray& other) const
    -> PackedArray {
  const auto a = Truth();
  const auto b = other.Truth();
  const bool result_four_state = is_four_state_ || other.is_four_state_;
  if (a == Truthiness::kKnownZero || b == Truthiness::kKnownNonzero) {
    return OneBitResult(true, result_four_state);
  }
  if (a == Truthiness::kKnownNonzero && b == Truthiness::kKnownZero) {
    return OneBitResult(false, result_four_state);
  }
  return AllX(1U, false);
}

auto PackedArray::LogicalEquivalence(const PackedArray& other) const
    -> PackedArray {
  const auto a = Truth();
  const auto b = other.Truth();
  const bool result_four_state = is_four_state_ || other.is_four_state_;
  if (a == Truthiness::kUnknown || b == Truthiness::kUnknown) {
    return AllX(1U, false);
  }
  return OneBitResult(a == b, result_four_state);
}

namespace {

// Capacity-saturating fold of a shift amount to a single word. Any amount
// at or beyond `bit_width` of the operand becomes a no-op (`amt >=
// bit_width` -> zero result), so saturating to ~0 for wide-amount values is
// safe: callers only compare against `bit_width`.
auto ShiftAmountAsUint(const PackedArray& amount) -> std::uint64_t {
  const auto words = amount.ValueWords();
  for (std::size_t i = 1; i < words.size(); ++i) {
    if (words[i] != 0U) {
      return ~std::uint64_t{0};
    }
  }
  return words.empty() ? 0U : words[0];
}

}  // namespace

auto PackedArray::ShiftLeft(const PackedArray& amount) const -> PackedArray {
  // LRM 11.4.10: X/Z in the amount yields an all-X result; X/Z in the
  // value rides along by shifting the unknown plane in parallel.
  if (amount.HasUnknown()) {
    return AllX(bit_width_, is_signed_);
  }
  const auto amt = ShiftAmountAsUint(amount);
  PackedArray result = Blank(bit_width_, is_signed_, is_four_state_);
  ShiftLeftWordsInto(ValueWords(), amt, result.MutableValueWords(), bit_width_);
  if (is_four_state_) {
    ShiftLeftWordsInto(
        UnknownWords(), amt, result.MutableUnknownWords(), bit_width_);
  }
  return result;
}

auto PackedArray::LogicalShiftRight(const PackedArray& amount) const
    -> PackedArray {
  if (amount.HasUnknown()) {
    return AllX(bit_width_, is_signed_);
  }
  const auto amt = ShiftAmountAsUint(amount);
  PackedArray result = Blank(bit_width_, is_signed_, is_four_state_);
  LogicalShiftRightWordsInto(
      ValueWords(), amt, result.MutableValueWords(), bit_width_);
  if (is_four_state_) {
    LogicalShiftRightWordsInto(
        UnknownWords(), amt, result.MutableUnknownWords(), bit_width_);
  }
  return result;
}

auto PackedArray::ArithmeticShiftRight(const PackedArray& amount) const
    -> PackedArray {
  if (!is_signed_) {
    return LogicalShiftRight(amount);
  }
  if (amount.HasUnknown()) {
    return AllX(bit_width_, is_signed_);
  }
  const auto amt = ShiftAmountAsUint(amount);
  // Each plane shifts with its own MSB as the fill, so an X at the top
  // extends down into the new top bits instead of degenerating to the
  // value-plane sign.
  const bool value_sign = BitAt(ValueWords(), bit_width_ - 1U);
  const std::uint64_t filled = std::min(amt, bit_width_);
  PackedArray result = Blank(bit_width_, true, is_four_state_);
  auto value_dst = result.MutableValueWords();
  LogicalShiftRightWordsInto(ValueWords(), amt, value_dst, bit_width_);
  if (value_sign) {
    FillTopBits(value_dst, bit_width_, filled);
  }
  if (is_four_state_) {
    const bool unk_sign = BitAt(UnknownWords(), bit_width_ - 1U);
    auto unknown_dst = result.MutableUnknownWords();
    LogicalShiftRightWordsInto(UnknownWords(), amt, unknown_dst, bit_width_);
    if (unk_sign) {
      FillTopBits(unknown_dst, bit_width_, filled);
    }
  }
  return result;
}

auto PackedArray::Pow(const PackedArray& exponent) const -> PackedArray {
  // The exponent is checked before the base so that `X ** 0 = 1` wins
  // over X-propagation from the base (LRM 11.4.10).
  if (exponent.HasUnknown()) {
    return AllX(bit_width_, is_signed_);
  }
  const auto exp_words = exponent.ValueWords();
  for (std::size_t i = 1; i < exp_words.size(); ++i) {
    if (exp_words[i] != 0U && exp_words[i] != ~std::uint64_t{0}) {
      throw SimulationError(
          "the ** operator's exponent magnitude exceeds 64 bits, which is not "
          "yet supported");
    }
  }
  const std::uint64_t exp_low = exp_words.empty() ? 0U : exp_words[0];
  const std::int64_t exp = exponent.IsSigned()
                               ? SignExtendToInt64(exp_low, exponent.BitWidth())
                               : static_cast<std::int64_t>(exp_low);
  if (exp == 0) {
    return FromInt(1, bit_width_, is_signed_, is_four_state_);
  }
  if (HasUnknown()) {
    return AllX(bit_width_, is_signed_);
  }
  if (exp < 0) {
    // LRM Table 11-4: a negative exponent is a reciprocal, which integer
    // division truncates to zero for every base but +-1 -- and for a zero base
    // is a division by zero, so the whole result is x.
    if (IsZero(ValueWords())) {
      return AllX(bit_width_, is_signed_);
    }
    const auto one = FromInt(1, bit_width_, is_signed_, is_four_state_);
    if (UnsignedCompare(ValueWords(), one.ValueWords()) == 0) {
      return one;
    }
    if (is_signed_) {
      const auto neg_one = FromInt(-1, bit_width_, true, is_four_state_);
      if (UnsignedCompare(ValueWords(), neg_one.ValueWords()) == 0) {
        return FromInt(
            (exp & 1) != 0 ? -1 : 1, bit_width_, is_signed_, is_four_state_);
      }
    }
    return FromInt(0, bit_width_, is_signed_, is_four_state_);
  }
  PackedArray result = FromInt(1, bit_width_, is_signed_, is_four_state_);
  PackedArray base = *this;
  std::int64_t e = exp;
  while (e > 0) {
    if ((e & 1) != 0) {
      result = result * base;
    }
    e >>= 1;
    if (e > 0) {
      base = base * base;
    }
  }
  return result;
}

auto PackedArray::ReductionAnd() const -> PackedArray {
  if (bit_width_ <= 64U && !is_four_state_) {
    return OneBitResult(
        NarrowWordOf(*this) == MaskForWidth(bit_width_), is_four_state_);
  }
  PackedArray result = Blank(1U, false, is_four_state_);
  if (is_four_state_) {
    value::ReductionAnd(AsLogicView(), result.AsLogicView());
  } else {
    value::ReductionAnd(AsBitView(), result.AsBitView());
  }
  return result;
}

auto PackedArray::ReductionOr() const -> PackedArray {
  if (bit_width_ <= 64U && !is_four_state_) {
    return OneBitResult(NarrowWordOf(*this) != 0U, is_four_state_);
  }
  PackedArray result = Blank(1U, false, is_four_state_);
  if (is_four_state_) {
    value::ReductionOr(AsLogicView(), result.AsLogicView());
  } else {
    value::ReductionOr(AsBitView(), result.AsBitView());
  }
  return result;
}

auto PackedArray::ReductionXor() const -> PackedArray {
  if (bit_width_ <= 64U && !is_four_state_) {
    return OneBitResult(
        (std::popcount(NarrowWordOf(*this)) & 1) != 0, is_four_state_);
  }
  PackedArray result = Blank(1U, false, is_four_state_);
  if (is_four_state_) {
    value::ReductionXor(AsLogicView(), result.AsLogicView());
  } else {
    value::ReductionXor(AsBitView(), result.AsBitView());
  }
  return result;
}

auto PackedArray::ReductionNand() const -> PackedArray {
  if (bit_width_ <= 64U && !is_four_state_) {
    return OneBitResult(
        NarrowWordOf(*this) != MaskForWidth(bit_width_), is_four_state_);
  }
  PackedArray result = Blank(1U, false, is_four_state_);
  if (is_four_state_) {
    value::ReductionNand(AsLogicView(), result.AsLogicView());
  } else {
    value::ReductionNand(AsBitView(), result.AsBitView());
  }
  return result;
}

auto PackedArray::ReductionNor() const -> PackedArray {
  if (bit_width_ <= 64U && !is_four_state_) {
    return OneBitResult(NarrowWordOf(*this) == 0U, is_four_state_);
  }
  PackedArray result = Blank(1U, false, is_four_state_);
  if (is_four_state_) {
    value::ReductionNor(AsLogicView(), result.AsLogicView());
  } else {
    value::ReductionNor(AsBitView(), result.AsBitView());
  }
  return result;
}

auto PackedArray::ReductionXnor() const -> PackedArray {
  if (bit_width_ <= 64U && !is_four_state_) {
    return OneBitResult(
        (std::popcount(NarrowWordOf(*this)) & 1) == 0, is_four_state_);
  }
  PackedArray result = Blank(1U, false, is_four_state_);
  if (is_four_state_) {
    value::ReductionXnor(AsLogicView(), result.AsLogicView());
  } else {
    value::ReductionXnor(AsBitView(), result.AsBitView());
  }
  return result;
}

}  // namespace lyra::value
