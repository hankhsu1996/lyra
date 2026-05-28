#include "lyra/value/packed_array.hpp"

#include <algorithm>
#include <bit>
#include <cstdint>
#include <span>
#include <string>
#include <string_view>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/value/packed.hpp"
#include "lyra/value/packed_bitwise.hpp"
#include "lyra/value/packed_convert.hpp"
#include "lyra/value/packed_internal.hpp"
#include "lyra/value/packed_reduction.hpp"

namespace lyra::value {

namespace {

auto MakeStorage(std::uint64_t bit_width, bool is_four_state)
    -> std::variant<BitValue, LogicValue> {
  if (is_four_state) {
    return LogicValue{bit_width};
  }
  return BitValue{bit_width};
}

auto MaskForWidth(std::uint64_t bit_width) -> std::uint64_t {
  if (bit_width >= 64U) {
    return ~std::uint64_t{0};
  }
  return (std::uint64_t{1} << bit_width) - 1U;
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

auto ExpectSameShape(
    const PackedArray& lhs, const PackedArray& rhs, std::string_view op)
    -> void {
  if (lhs.BitWidth() != rhs.BitWidth()) {
    throw InternalError(
        std::string{op} +
        ": operand bit_width mismatch (left=" + std::to_string(lhs.BitWidth()) +
        ", right=" + std::to_string(rhs.BitWidth()) + ")");
  }
  if (lhs.IsFourState() != rhs.IsFourState()) {
    throw InternalError(std::string{op} + ": operand state-kind mismatch");
  }
}

}  // namespace

PackedArray::PackedArray(
    std::uint64_t bit_width, bool is_signed, bool is_four_state)
    : bit_width_(bit_width),
      is_signed_(is_signed),
      is_four_state_(is_four_state),
      storage_(MakeStorage(bit_width, is_four_state)) {
}

auto PackedArray::Int(std::int32_t value) -> PackedArray {
  return FromInt(value, 32U, true, false);
}

auto PackedArray::MakeFromWordPlanes(
    std::uint64_t bit_width, bool is_signed, bool is_four_state,
    std::span<const std::uint64_t> value_words,
    std::span<const std::uint64_t> unknown_words) -> PackedArray {
  if (bit_width == 0U) {
    throw InternalError(
        "PackedArray::MakeFromWordPlanes: bit_width must be >= 1");
  }
  const std::size_t expected = (bit_width + 63U) / 64U;
  if (value_words.size() != expected) {
    throw InternalError(
        "PackedArray::MakeFromWordPlanes: value word count mismatch");
  }
  if (!is_four_state && !unknown_words.empty()) {
    throw InternalError(
        "PackedArray::MakeFromWordPlanes: 2-state shape forbids unknown "
        "words");
  }
  if (is_four_state && !unknown_words.empty() &&
      unknown_words.size() != expected) {
    throw InternalError(
        "PackedArray::MakeFromWordPlanes: 4-state unknown word count "
        "mismatch");
  }

  PackedArray p{bit_width, is_signed, is_four_state};
  auto value_dst = std::visit(
      [](auto& v) { return detail::PackedAccess::ValueWords(v.View()); },
      p.storage_);
  std::ranges::copy(value_words, value_dst.begin());
  MaskUnusedTopBits(value_dst, bit_width);
  if (is_four_state) {
    auto& lv = std::get<LogicValue>(p.storage_);
    auto unknown_dst = detail::PackedAccess::UnknownWords(lv.View());
    if (unknown_words.empty()) {
      std::ranges::fill(unknown_dst, std::uint64_t{0});
    } else {
      std::ranges::copy(unknown_words, unknown_dst.begin());
      MaskUnusedTopBits(unknown_dst, bit_width);
    }
  }
  return p;
}

auto PackedArray::FromInt(
    std::int64_t value, std::uint64_t bit_width, bool is_signed,
    bool is_four_state) -> PackedArray {
  const std::size_t n = (bit_width + 63U) / 64U;
  absl::InlinedVector<std::uint64_t, kPackedWordsInlineCapacity> words(n);
  if (bit_width <= 64U) {
    words[0] = static_cast<std::uint64_t>(value);
  } else {
    const std::uint64_t fill = (value < 0) ? ~std::uint64_t{0} : 0U;
    words[0] = static_cast<std::uint64_t>(value);
    for (std::size_t i = 1; i < n; ++i) {
      words[i] = fill;
    }
  }
  return MakeFromWordPlanes(
      bit_width, is_signed, is_four_state,
      std::span<const std::uint64_t>{words.data(), words.size()}, {});
}

auto PackedArray::FromWords(
    std::initializer_list<std::uint64_t> value_words,
    std::initializer_list<std::uint64_t> unknown_words, std::uint64_t bit_width,
    bool is_signed, bool is_four_state) -> PackedArray {
  return MakeFromWordPlanes(
      bit_width, is_signed, is_four_state,
      std::span<const std::uint64_t>{value_words.begin(), value_words.size()},
      std::span<const std::uint64_t>{
          unknown_words.begin(), unknown_words.size()});
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

auto PackedArray::ValueWords() const -> std::span<const std::uint64_t> {
  return std::visit(
      [](const auto& v) -> std::span<const std::uint64_t> {
        return detail::PackedAccess::ValueWords(v.View());
      },
      storage_);
}

auto PackedArray::UnknownWords() const -> std::span<const std::uint64_t> {
  if (const auto* lv = std::get_if<LogicValue>(&storage_)) {
    return detail::PackedAccess::UnknownWords(lv->View());
  }
  return {};
}

auto PackedArray::IsCaseEqual(const PackedArray& other) const -> bool {
  if (bit_width_ != other.bit_width_) return false;
  if (is_four_state_ != other.is_four_state_) return false;
  const auto vw_a = ValueWords();
  const auto vw_b = other.ValueWords();
  if (vw_a.size() != vw_b.size()) return false;
  if (!std::ranges::equal(vw_a, vw_b)) return false;
  return std::ranges::equal(UnknownWords(), other.UnknownWords());
}

auto PackedArray::CaseEqual(const PackedArray& other) const -> PackedArray {
  return FromInt(IsCaseEqual(other) ? 1 : 0, 1, false, false);
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
  auto* bv = std::get_if<BitValue>(&storage_);
  if (bv == nullptr) {
    throw InternalError("PackedArray::AsBitView: storage is 4-state");
  }
  return bv->View();
}

auto PackedArray::AsBitView() const -> ConstBitView {
  const auto* bv = std::get_if<BitValue>(&storage_);
  if (bv == nullptr) {
    throw InternalError("PackedArray::AsBitView: storage is 4-state");
  }
  return bv->View();
}

auto PackedArray::AsLogicView() -> LogicView {
  auto* lv = std::get_if<LogicValue>(&storage_);
  if (lv == nullptr) {
    throw InternalError("PackedArray::AsLogicView: storage is 2-state");
  }
  return lv->View();
}

auto PackedArray::AsLogicView() const -> ConstLogicView {
  const auto* lv = std::get_if<LogicValue>(&storage_);
  if (lv == nullptr) {
    throw InternalError("PackedArray::AsLogicView: storage is 2-state");
  }
  return lv->View();
}

auto PackedArray::operator=(const PackedArray& other) -> PackedArray& {
  if (this != &other) {
    AssignFrom(other);
  }
  return *this;
}

auto PackedArray::operator=(PackedArray&& other) noexcept(false)
    -> PackedArray& {
  if (this != &other) {
    AssignFrom(other);
  }
  return *this;
}

auto PackedArray::AssignFrom(const PackedArray& other) -> void {
  ExpectSameShape(*this, other, "PackedArray::AssignFrom");
  auto dst = std::visit(
      [](auto& v) { return detail::PackedAccess::ValueWords(v.View()); },
      storage_);
  const auto src = other.ValueWords();
  for (std::size_t i = 0; i < dst.size(); ++i) {
    dst[i] = src[i];
  }
  if (is_four_state_) {
    auto& dst_logic = std::get<LogicValue>(storage_);
    const auto& src_logic = std::get<LogicValue>(other.storage_);
    auto dst_unk = detail::PackedAccess::UnknownWords(dst_logic.View());
    const auto src_unk = detail::PackedAccess::UnknownWords(src_logic.View());
    for (std::size_t i = 0; i < dst_unk.size(); ++i) {
      dst_unk[i] = src_unk[i];
    }
  }
}

auto PackedArray::ToInt64() const -> std::int64_t {
  if (bit_width_ > 64U) {
    throw InternalError("PackedArray::ToInt64: bit_width > 64");
  }
  const auto raw = ValueWords()[0] & MaskForWidth(bit_width_);
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

auto PackedArray::ConvertFrom(
    const PackedArray& src, std::uint64_t dst_bit_width, bool dst_is_signed,
    bool dst_is_four_state) -> PackedArray {
  PackedArray dst{dst_bit_width, dst_is_signed, dst_is_four_state};
  const Signedness src_signedness =
      src.IsSigned() ? Signedness::kSigned : Signedness::kUnsigned;
  if (dst_is_four_state) {
    auto& dst_lv = std::get<LogicValue>(dst.storage_);
    LogicView dst_view = dst_lv.View();
    if (src.IsFourState()) {
      const auto& src_lv = std::get<LogicValue>(src.storage_);
      ConvertToLogic(src_lv.View(), dst_view, src_signedness);
    } else {
      const auto& src_bv = std::get<BitValue>(src.storage_);
      ConvertToLogic(src_bv.View(), dst_view, src_signedness);
    }
  } else {
    auto& dst_bv = std::get<BitValue>(dst.storage_);
    BitView dst_view = dst_bv.View();
    if (src.IsFourState()) {
      const auto& src_lv = std::get<LogicValue>(src.storage_);
      ConvertToBit(src_lv.View(), dst_view, src_signedness);
    } else {
      const auto& src_bv = std::get<BitValue>(src.storage_);
      ConvertToBit(src_bv.View(), dst_view, src_signedness);
    }
  }
  return dst;
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

// LRM 11.4 X/Z propagation result: a 4-state default ctor leaves the
// unknown plane all-1, which is the all-X value.
auto AllX(std::uint64_t bit_width, bool is_signed) -> PackedArray {
  return PackedArray{bit_width, is_signed, true};
}

using WordBuffer =
    absl::InlinedVector<std::uint64_t, kPackedWordsInlineCapacity>;

auto MakeWordBuffer(std::uint64_t bit_width) -> WordBuffer {
  return WordBuffer(WordCountForBits(bit_width), std::uint64_t{0});
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

auto BitAt(std::span<const std::uint64_t> words, std::uint64_t pos)
    -> std::uint64_t {
  const auto idx = static_cast<std::size_t>(pos / 64U);
  if (idx >= words.size()) {
    return 0U;
  }
  return (words[idx] >> (pos % 64U)) & std::uint64_t{1};
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
    rem[0] |= BitAt(numerator, pos);
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

enum class Truthiness : std::uint8_t { kKnownZero, kKnownNonzero, kUnknown };

// LRM 11.4.7 truth value. kKnownNonzero requires a definitively-one bit
// (no other 1 dominates), so `(1, X, X, X)` is kKnownNonzero but
// `(X, X, X, X)` is kUnknown.
auto TruthinessOf(const PackedArray& v) -> Truthiness {
  const auto vw = v.ValueWords();
  const auto uw = v.UnknownWords();
  bool has_unknown_bit = false;
  for (std::size_t i = 0; i < vw.size(); ++i) {
    const std::uint64_t unk = i < uw.size() ? uw[i] : 0U;
    if ((vw[i] & ~unk) != 0U) {
      return Truthiness::kKnownNonzero;
    }
    if (unk != 0U) {
      has_unknown_bit = true;
    }
  }
  return has_unknown_bit ? Truthiness::kUnknown : Truthiness::kKnownZero;
}

}  // namespace

auto PackedArray::operator+(const PackedArray& other) const -> PackedArray {
  ExpectSameShape(*this, other, "operator+");
  if (HasUnknown() || other.HasUnknown()) {
    return AllX(bit_width_, is_signed_);
  }
  auto buf = MakeWordBuffer(bit_width_);
  AddWordsInto(ValueWords(), other.ValueWords(), buf, bit_width_);
  return MakeFromWordPlanes(
      bit_width_, is_signed_, is_four_state_,
      std::span<const std::uint64_t>{buf.data(), buf.size()}, {});
}

auto PackedArray::operator-(const PackedArray& other) const -> PackedArray {
  ExpectSameShape(*this, other, "operator-");
  if (HasUnknown() || other.HasUnknown()) {
    return AllX(bit_width_, is_signed_);
  }
  auto buf = MakeWordBuffer(bit_width_);
  SubWordsInto(ValueWords(), other.ValueWords(), buf, bit_width_);
  return MakeFromWordPlanes(
      bit_width_, is_signed_, is_four_state_,
      std::span<const std::uint64_t>{buf.data(), buf.size()}, {});
}

auto PackedArray::operator*(const PackedArray& other) const -> PackedArray {
  ExpectSameShape(*this, other, "operator*");
  if (HasUnknown() || other.HasUnknown()) {
    return AllX(bit_width_, is_signed_);
  }
  auto buf = MakeWordBuffer(bit_width_);
  MulWordsInto(ValueWords(), other.ValueWords(), buf, bit_width_);
  return MakeFromWordPlanes(
      bit_width_, is_signed_, is_four_state_,
      std::span<const std::uint64_t>{buf.data(), buf.size()}, {});
}

auto PackedArray::operator/(const PackedArray& other) const -> PackedArray {
  ExpectSameShape(*this, other, "operator/");
  if (HasUnknown() || other.HasUnknown()) {
    return AllX(bit_width_, is_signed_);
  }
  if (IsZero(other.ValueWords())) {
    // SV LRM 11.4.4: integer division by zero on 4-state yields X for every
    // bit; on 2-state it is implementation-defined and we pick zero. The
    // default ctor encodes both directly: LogicValue defaults to all-X
    // (matching the SV `logic` default), BitValue defaults to all-zero.
    return PackedArray{bit_width_, is_signed_, is_four_state_};
  }
  const bool neg_a = is_signed_ && BitAt(ValueWords(), bit_width_ - 1U) != 0U;
  const bool neg_b =
      is_signed_ && BitAt(other.ValueWords(), bit_width_ - 1U) != 0U;
  auto a_abs = MakeWordBuffer(bit_width_);
  auto b_abs = MakeWordBuffer(bit_width_);
  if (neg_a) {
    SubWordsInto({}, ValueWords(), a_abs, bit_width_);
  } else {
    std::ranges::copy(ValueWords(), a_abs.begin());
  }
  if (neg_b) {
    SubWordsInto({}, other.ValueWords(), b_abs, bit_width_);
  } else {
    std::ranges::copy(other.ValueWords(), b_abs.begin());
  }
  auto quot = MakeWordBuffer(bit_width_);
  auto rem = MakeWordBuffer(bit_width_);
  LongDivideUnsigned(a_abs, b_abs, quot, rem, bit_width_);
  if (neg_a != neg_b) {
    auto neg_quot = MakeWordBuffer(bit_width_);
    SubWordsInto({}, quot, neg_quot, bit_width_);
    quot = std::move(neg_quot);
  }
  return MakeFromWordPlanes(
      bit_width_, is_signed_, is_four_state_,
      std::span<const std::uint64_t>{quot.data(), quot.size()}, {});
}

auto PackedArray::operator%(const PackedArray& other) const -> PackedArray {
  ExpectSameShape(*this, other, "operator%");
  if (HasUnknown() || other.HasUnknown()) {
    return AllX(bit_width_, is_signed_);
  }
  if (IsZero(other.ValueWords())) {
    // See operator/'s div-by-zero note: deliberate use of the default ctor's
    // shape-default (all-X for 4-state, zero for 2-state).
    return PackedArray{bit_width_, is_signed_, is_four_state_};
  }
  const bool neg_a = is_signed_ && BitAt(ValueWords(), bit_width_ - 1U) != 0U;
  const bool neg_b =
      is_signed_ && BitAt(other.ValueWords(), bit_width_ - 1U) != 0U;
  auto a_abs = MakeWordBuffer(bit_width_);
  auto b_abs = MakeWordBuffer(bit_width_);
  if (neg_a) {
    SubWordsInto({}, ValueWords(), a_abs, bit_width_);
  } else {
    std::ranges::copy(ValueWords(), a_abs.begin());
  }
  if (neg_b) {
    SubWordsInto({}, other.ValueWords(), b_abs, bit_width_);
  } else {
    std::ranges::copy(other.ValueWords(), b_abs.begin());
  }
  auto quot = MakeWordBuffer(bit_width_);
  auto rem = MakeWordBuffer(bit_width_);
  LongDivideUnsigned(a_abs, b_abs, quot, rem, bit_width_);
  // SV/C truncated modulo: remainder takes the sign of the dividend.
  if (neg_a) {
    auto neg_rem = MakeWordBuffer(bit_width_);
    SubWordsInto({}, rem, neg_rem, bit_width_);
    rem = std::move(neg_rem);
  }
  return MakeFromWordPlanes(
      bit_width_, is_signed_, is_four_state_,
      std::span<const std::uint64_t>{rem.data(), rem.size()}, {});
}

auto PackedArray::operator&(const PackedArray& other) const -> PackedArray {
  ExpectSameShape(*this, other, "operator&");
  if (bit_width_ <= 64U && !is_four_state_) {
    const auto mask = MaskForWidth(bit_width_);
    return FromInt(
        static_cast<std::int64_t>(
            (NarrowWordOf(*this) & NarrowWordOf(other)) & mask),
        bit_width_, is_signed_, is_four_state_);
  }
  PackedArray result{bit_width_, is_signed_, is_four_state_};
  if (is_four_state_) {
    BitwiseAnd(
        std::get<LogicValue>(storage_).View(),
        std::get<LogicValue>(other.storage_).View(),
        std::get<LogicValue>(result.storage_).View());
  } else {
    BitwiseAnd(
        std::get<BitValue>(storage_).View(),
        std::get<BitValue>(other.storage_).View(),
        std::get<BitValue>(result.storage_).View());
  }
  return result;
}

auto PackedArray::operator|(const PackedArray& other) const -> PackedArray {
  ExpectSameShape(*this, other, "operator|");
  if (bit_width_ <= 64U && !is_four_state_) {
    const auto mask = MaskForWidth(bit_width_);
    return FromInt(
        static_cast<std::int64_t>(
            (NarrowWordOf(*this) | NarrowWordOf(other)) & mask),
        bit_width_, is_signed_, is_four_state_);
  }
  PackedArray result{bit_width_, is_signed_, is_four_state_};
  if (is_four_state_) {
    BitwiseOr(
        std::get<LogicValue>(storage_).View(),
        std::get<LogicValue>(other.storage_).View(),
        std::get<LogicValue>(result.storage_).View());
  } else {
    BitwiseOr(
        std::get<BitValue>(storage_).View(),
        std::get<BitValue>(other.storage_).View(),
        std::get<BitValue>(result.storage_).View());
  }
  return result;
}

auto PackedArray::operator^(const PackedArray& other) const -> PackedArray {
  ExpectSameShape(*this, other, "operator^");
  if (bit_width_ <= 64U && !is_four_state_) {
    const auto mask = MaskForWidth(bit_width_);
    return FromInt(
        static_cast<std::int64_t>(
            (NarrowWordOf(*this) ^ NarrowWordOf(other)) & mask),
        bit_width_, is_signed_, is_four_state_);
  }
  PackedArray result{bit_width_, is_signed_, is_four_state_};
  if (is_four_state_) {
    BitwiseXor(
        std::get<LogicValue>(storage_).View(),
        std::get<LogicValue>(other.storage_).View(),
        std::get<LogicValue>(result.storage_).View());
  } else {
    BitwiseXor(
        std::get<BitValue>(storage_).View(),
        std::get<BitValue>(other.storage_).View(),
        std::get<BitValue>(result.storage_).View());
  }
  return result;
}

auto PackedArray::BitwiseXnor(const PackedArray& other) const -> PackedArray {
  ExpectSameShape(*this, other, "BitwiseXnor");
  if (bit_width_ <= 64U && !is_four_state_) {
    const auto mask = MaskForWidth(bit_width_);
    return FromInt(
        static_cast<std::int64_t>(
            (~(NarrowWordOf(*this) ^ NarrowWordOf(other))) & mask),
        bit_width_, is_signed_, is_four_state_);
  }
  PackedArray result{bit_width_, is_signed_, is_four_state_};
  if (is_four_state_) {
    value::BitwiseXnor(
        std::get<LogicValue>(storage_).View(),
        std::get<LogicValue>(other.storage_).View(),
        std::get<LogicValue>(result.storage_).View());
  } else {
    value::BitwiseXnor(
        std::get<BitValue>(storage_).View(),
        std::get<BitValue>(other.storage_).View(),
        std::get<BitValue>(result.storage_).View());
  }
  return result;
}

auto PackedArray::operator==(const PackedArray& other) const -> PackedArray {
  ExpectSameShape(*this, other, "operator==");
  if (HasUnknown() || other.HasUnknown()) {
    return AllX(1U, false);
  }
  return OneBitResult(
      UnsignedCompare(ValueWords(), other.ValueWords()) == 0, is_four_state_);
}

auto PackedArray::operator!=(const PackedArray& other) const -> PackedArray {
  ExpectSameShape(*this, other, "operator!=");
  if (HasUnknown() || other.HasUnknown()) {
    return AllX(1U, false);
  }
  return OneBitResult(
      UnsignedCompare(ValueWords(), other.ValueWords()) != 0, is_four_state_);
}

auto PackedArray::WildcardEquals(const PackedArray& other) const
    -> PackedArray {
  ExpectSameShape(*this, other, "WildcardEquals");
  const auto words = WordCountForBits(bit_width_);
  const auto a_val = ValueWords();
  const auto b_val = other.ValueWords();
  const auto a_unk = UnknownWords();
  const auto b_unk = other.UnknownWords();
  const auto top_mask = MaskForWidth(bit_width_ - ((words - 1U) * 64U));
  bool definite_mismatch = false;
  bool lhs_unknown_at_compare = false;
  for (std::size_t w = 0; w < words; ++w) {
    const std::uint64_t bw_unk = w < b_unk.size() ? b_unk[w] : 0U;
    const std::uint64_t aw_unk = w < a_unk.size() ? a_unk[w] : 0U;
    std::uint64_t cmp_mask = ~bw_unk;
    if (w + 1U == words) {
      cmp_mask &= top_mask;
    }
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

auto PackedArray::Index(const PackedArray& index) const -> PackedArray {
  return Slice(index, 1U);
}

auto PackedArray::Slice(const PackedArray& lsb_bit, std::uint32_t width) const
    -> PackedArray {
  if (width == 0U) {
    throw InternalError("PackedArray::Slice: width must be >= 1");
  }
  if (lsb_bit.HasUnknown() || lsb_bit.BitWidth() > 64U) {
    if (is_four_state_) {
      return AllX(width, false);
    }
    return PackedArray{width, false, false};
  }
  const std::int64_t start = lsb_bit.ToInt64();
  const auto src_value = ValueWords();
  const auto src_unknown = UnknownWords();
  const auto bw_signed = static_cast<std::int64_t>(bit_width_);
  auto val_buf = MakeWordBuffer(width);
  auto unk_buf = is_four_state_ ? MakeWordBuffer(width) : WordBuffer{};
  for (std::uint32_t i = 0; i < width; ++i) {
    const std::int64_t pos = start + static_cast<std::int64_t>(i);
    const std::uint64_t out_mask = std::uint64_t{1} << (i % 64U);
    if (pos < 0 || pos >= bw_signed) {
      if (is_four_state_) {
        val_buf[i / 64U] |= out_mask;
        unk_buf[i / 64U] |= out_mask;
      }
      continue;
    }
    const auto w_idx = static_cast<std::size_t>(pos / 64);
    const auto b_idx = static_cast<std::uint64_t>(pos % 64);
    if (((src_value[w_idx] >> b_idx) & 1U) != 0U) {
      val_buf[i / 64U] |= out_mask;
    }
    if (is_four_state_ && w_idx < src_unknown.size() &&
        ((src_unknown[w_idx] >> b_idx) & 1U) != 0U) {
      unk_buf[i / 64U] |= out_mask;
    }
  }
  return MakeFromWordPlanes(
      width, false, is_four_state_,
      std::span<const std::uint64_t>{val_buf.data(), val_buf.size()},
      is_four_state_
          ? std::span<const std::uint64_t>{unk_buf.data(), unk_buf.size()}
          : std::span<const std::uint64_t>{});
}

auto PackedArray::operator<(const PackedArray& other) const -> PackedArray {
  ExpectSameShape(*this, other, "operator<");
  if (HasUnknown() || other.HasUnknown()) {
    return AllX(1U, false);
  }
  const int cmp =
      is_signed_ ? SignedCompare(ValueWords(), other.ValueWords(), bit_width_)
                 : UnsignedCompare(ValueWords(), other.ValueWords());
  return OneBitResult(cmp < 0, is_four_state_);
}

auto PackedArray::operator<=(const PackedArray& other) const -> PackedArray {
  ExpectSameShape(*this, other, "operator<=");
  if (HasUnknown() || other.HasUnknown()) {
    return AllX(1U, false);
  }
  const int cmp =
      is_signed_ ? SignedCompare(ValueWords(), other.ValueWords(), bit_width_)
                 : UnsignedCompare(ValueWords(), other.ValueWords());
  return OneBitResult(cmp <= 0, is_four_state_);
}

auto PackedArray::operator>(const PackedArray& other) const -> PackedArray {
  ExpectSameShape(*this, other, "operator>");
  if (HasUnknown() || other.HasUnknown()) {
    return AllX(1U, false);
  }
  const int cmp =
      is_signed_ ? SignedCompare(ValueWords(), other.ValueWords(), bit_width_)
                 : UnsignedCompare(ValueWords(), other.ValueWords());
  return OneBitResult(cmp > 0, is_four_state_);
}

auto PackedArray::operator>=(const PackedArray& other) const -> PackedArray {
  ExpectSameShape(*this, other, "operator>=");
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
  auto buf = MakeWordBuffer(bit_width_);
  SubWordsInto({}, ValueWords(), buf, bit_width_);
  return MakeFromWordPlanes(
      bit_width_, is_signed_, is_four_state_,
      std::span<const std::uint64_t>{buf.data(), buf.size()}, {});
}

auto PackedArray::operator~() const -> PackedArray {
  if (bit_width_ <= 64U && !is_four_state_) {
    const auto mask = MaskForWidth(bit_width_);
    return FromInt(
        static_cast<std::int64_t>((~NarrowWordOf(*this)) & mask), bit_width_,
        is_signed_, false);
  }
  PackedArray result{bit_width_, is_signed_, is_four_state_};
  if (is_four_state_) {
    BitwiseNot(
        std::get<LogicValue>(storage_).View(),
        std::get<LogicValue>(result.storage_).View());
  } else {
    BitwiseNot(
        std::get<BitValue>(storage_).View(),
        std::get<BitValue>(result.storage_).View());
  }
  return result;
}

auto PackedArray::LogicalAnd(const PackedArray& other) const -> PackedArray {
  const auto a = TruthinessOf(*this);
  const auto b = TruthinessOf(other);
  const bool result_four_state = is_four_state_ || other.is_four_state_;
  if (a == Truthiness::kKnownZero || b == Truthiness::kKnownZero) {
    return OneBitResult(false, result_four_state);
  }
  if (a == Truthiness::kKnownNonzero && b == Truthiness::kKnownNonzero) {
    return OneBitResult(true, result_four_state);
  }
  return AllX(1U, false);
}

auto PackedArray::LogicalOr(const PackedArray& other) const -> PackedArray {
  const auto a = TruthinessOf(*this);
  const auto b = TruthinessOf(other);
  const bool result_four_state = is_four_state_ || other.is_four_state_;
  if (a == Truthiness::kKnownNonzero || b == Truthiness::kKnownNonzero) {
    return OneBitResult(true, result_four_state);
  }
  if (a == Truthiness::kKnownZero && b == Truthiness::kKnownZero) {
    return OneBitResult(false, result_four_state);
  }
  return AllX(1U, false);
}

auto PackedArray::LogicalNot() const -> PackedArray {
  switch (TruthinessOf(*this)) {
    case Truthiness::kKnownZero:
      return OneBitResult(true, is_four_state_);
    case Truthiness::kKnownNonzero:
      return OneBitResult(false, is_four_state_);
    case Truthiness::kUnknown:
      return AllX(1U, false);
  }
  throw InternalError("LogicalNot: unhandled Truthiness");
}

auto PackedArray::LogicalImplication(const PackedArray& other) const
    -> PackedArray {
  const auto a = TruthinessOf(*this);
  const auto b = TruthinessOf(other);
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
  const auto a = TruthinessOf(*this);
  const auto b = TruthinessOf(other);
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
  auto value_buf = MakeWordBuffer(bit_width_);
  ShiftLeftWordsInto(ValueWords(), amt, value_buf, bit_width_);
  if (!is_four_state_) {
    return MakeFromWordPlanes(
        bit_width_, is_signed_, false,
        std::span<const std::uint64_t>{value_buf.data(), value_buf.size()}, {});
  }
  auto unk_buf = MakeWordBuffer(bit_width_);
  ShiftLeftWordsInto(UnknownWords(), amt, unk_buf, bit_width_);
  return MakeFromWordPlanes(
      bit_width_, is_signed_, true,
      std::span<const std::uint64_t>{value_buf.data(), value_buf.size()},
      std::span<const std::uint64_t>{unk_buf.data(), unk_buf.size()});
}

auto PackedArray::LogicalShiftRight(const PackedArray& amount) const
    -> PackedArray {
  if (amount.HasUnknown()) {
    return AllX(bit_width_, is_signed_);
  }
  const auto amt = ShiftAmountAsUint(amount);
  auto value_buf = MakeWordBuffer(bit_width_);
  LogicalShiftRightWordsInto(ValueWords(), amt, value_buf, bit_width_);
  if (!is_four_state_) {
    return MakeFromWordPlanes(
        bit_width_, is_signed_, false,
        std::span<const std::uint64_t>{value_buf.data(), value_buf.size()}, {});
  }
  auto unk_buf = MakeWordBuffer(bit_width_);
  LogicalShiftRightWordsInto(UnknownWords(), amt, unk_buf, bit_width_);
  return MakeFromWordPlanes(
      bit_width_, is_signed_, true,
      std::span<const std::uint64_t>{value_buf.data(), value_buf.size()},
      std::span<const std::uint64_t>{unk_buf.data(), unk_buf.size()});
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
  const std::uint64_t value_sign = BitAt(ValueWords(), bit_width_ - 1U);
  auto value_buf = MakeWordBuffer(bit_width_);
  LogicalShiftRightWordsInto(ValueWords(), amt, value_buf, bit_width_);
  if (value_sign != 0U) {
    FillTopBits(value_buf, bit_width_, std::min(amt, bit_width_));
  }
  if (!is_four_state_) {
    return MakeFromWordPlanes(
        bit_width_, true, false,
        std::span<const std::uint64_t>{value_buf.data(), value_buf.size()}, {});
  }
  const std::uint64_t unk_sign = BitAt(UnknownWords(), bit_width_ - 1U);
  auto unk_buf = MakeWordBuffer(bit_width_);
  LogicalShiftRightWordsInto(UnknownWords(), amt, unk_buf, bit_width_);
  if (unk_sign != 0U) {
    FillTopBits(unk_buf, bit_width_, std::min(amt, bit_width_));
  }
  return MakeFromWordPlanes(
      bit_width_, true, true,
      std::span<const std::uint64_t>{value_buf.data(), value_buf.size()},
      std::span<const std::uint64_t>{unk_buf.data(), unk_buf.size()});
}

auto PackedArray::Power(const PackedArray& exponent) const -> PackedArray {
  // The exponent is checked before the base so that `X ** 0 = 1` wins
  // over X-propagation from the base (LRM 11.4.10).
  if (exponent.HasUnknown()) {
    return AllX(bit_width_, is_signed_);
  }
  const auto exp_words = exponent.ValueWords();
  for (std::size_t i = 1; i < exp_words.size(); ++i) {
    if (exp_words[i] != 0U && exp_words[i] != ~std::uint64_t{0}) {
      throw InternalError(
          "PackedArray::Power: exponent magnitude exceeds 64 bits");
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
    // Negative exp on integer base rounds to zero except for base == +-1.
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
  PackedArray result{1U, false, is_four_state_};
  if (is_four_state_) {
    value::ReductionAnd(
        std::get<LogicValue>(storage_).View(),
        std::get<LogicValue>(result.storage_).View());
  } else {
    value::ReductionAnd(
        std::get<BitValue>(storage_).View(),
        std::get<BitValue>(result.storage_).View());
  }
  return result;
}

auto PackedArray::ReductionOr() const -> PackedArray {
  if (bit_width_ <= 64U && !is_four_state_) {
    return OneBitResult(NarrowWordOf(*this) != 0U, is_four_state_);
  }
  PackedArray result{1U, false, is_four_state_};
  if (is_four_state_) {
    value::ReductionOr(
        std::get<LogicValue>(storage_).View(),
        std::get<LogicValue>(result.storage_).View());
  } else {
    value::ReductionOr(
        std::get<BitValue>(storage_).View(),
        std::get<BitValue>(result.storage_).View());
  }
  return result;
}

auto PackedArray::ReductionXor() const -> PackedArray {
  if (bit_width_ <= 64U && !is_four_state_) {
    return OneBitResult(
        (std::popcount(NarrowWordOf(*this)) & 1) != 0, is_four_state_);
  }
  PackedArray result{1U, false, is_four_state_};
  if (is_four_state_) {
    value::ReductionXor(
        std::get<LogicValue>(storage_).View(),
        std::get<LogicValue>(result.storage_).View());
  } else {
    value::ReductionXor(
        std::get<BitValue>(storage_).View(),
        std::get<BitValue>(result.storage_).View());
  }
  return result;
}

auto PackedArray::ReductionNand() const -> PackedArray {
  if (bit_width_ <= 64U && !is_four_state_) {
    return OneBitResult(
        NarrowWordOf(*this) != MaskForWidth(bit_width_), is_four_state_);
  }
  PackedArray result{1U, false, is_four_state_};
  if (is_four_state_) {
    value::ReductionNand(
        std::get<LogicValue>(storage_).View(),
        std::get<LogicValue>(result.storage_).View());
  } else {
    value::ReductionNand(
        std::get<BitValue>(storage_).View(),
        std::get<BitValue>(result.storage_).View());
  }
  return result;
}

auto PackedArray::ReductionNor() const -> PackedArray {
  if (bit_width_ <= 64U && !is_four_state_) {
    return OneBitResult(NarrowWordOf(*this) == 0U, is_four_state_);
  }
  PackedArray result{1U, false, is_four_state_};
  if (is_four_state_) {
    value::ReductionNor(
        std::get<LogicValue>(storage_).View(),
        std::get<LogicValue>(result.storage_).View());
  } else {
    value::ReductionNor(
        std::get<BitValue>(storage_).View(),
        std::get<BitValue>(result.storage_).View());
  }
  return result;
}

auto PackedArray::ReductionXnor() const -> PackedArray {
  if (bit_width_ <= 64U && !is_four_state_) {
    return OneBitResult(
        (std::popcount(NarrowWordOf(*this)) & 1) == 0, is_four_state_);
  }
  PackedArray result{1U, false, is_four_state_};
  if (is_four_state_) {
    value::ReductionXnor(
        std::get<LogicValue>(storage_).View(),
        std::get<LogicValue>(result.storage_).View());
  } else {
    value::ReductionXnor(
        std::get<BitValue>(storage_).View(),
        std::get<BitValue>(result.storage_).View());
  }
  return result;
}

}  // namespace lyra::value
