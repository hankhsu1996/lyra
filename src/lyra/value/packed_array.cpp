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

auto WidthOnlyNarrow(const PackedArray& v, std::string_view op) -> void {
  if (v.BitWidth() > 64U) {
    throw InternalError(
        std::string{op} +
        ": wide (>64-bit) packed integral operator not yet implemented");
  }
}

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
  WidthOnlyNarrow(*this, "operator+");
  if (HasUnknown() || other.HasUnknown()) {
    return AllX(bit_width_, is_signed_);
  }
  const auto mask = MaskForWidth(bit_width_);
  const auto sum = (NarrowWordOf(*this) + NarrowWordOf(other)) & mask;
  return FromInt(
      static_cast<std::int64_t>(sum), bit_width_, is_signed_, is_four_state_);
}

auto PackedArray::operator-(const PackedArray& other) const -> PackedArray {
  ExpectSameShape(*this, other, "operator-");
  WidthOnlyNarrow(*this, "operator-");
  if (HasUnknown() || other.HasUnknown()) {
    return AllX(bit_width_, is_signed_);
  }
  const auto mask = MaskForWidth(bit_width_);
  const auto diff = (NarrowWordOf(*this) - NarrowWordOf(other)) & mask;
  return FromInt(
      static_cast<std::int64_t>(diff), bit_width_, is_signed_, is_four_state_);
}

auto PackedArray::operator*(const PackedArray& other) const -> PackedArray {
  ExpectSameShape(*this, other, "operator*");
  WidthOnlyNarrow(*this, "operator*");
  if (HasUnknown() || other.HasUnknown()) {
    return AllX(bit_width_, is_signed_);
  }
  const auto mask = MaskForWidth(bit_width_);
  const auto product = (NarrowWordOf(*this) * NarrowWordOf(other)) & mask;
  return FromInt(
      static_cast<std::int64_t>(product), bit_width_, is_signed_,
      is_four_state_);
}

auto PackedArray::operator/(const PackedArray& other) const -> PackedArray {
  ExpectSameShape(*this, other, "operator/");
  WidthOnlyNarrow(*this, "operator/");
  if (HasUnknown() || other.HasUnknown()) {
    return AllX(bit_width_, is_signed_);
  }
  const auto rhs = other.ToInt64();
  if (rhs == 0) {
    // SV LRM 11.4.4: integer division by zero on 4-state yields X for every
    // bit; on 2-state it is implementation-defined and we pick zero. The
    // default ctor encodes both directly: LogicValue defaults to all-X
    // (matching the SV `logic` default), BitValue defaults to all-zero.
    return PackedArray{bit_width_, is_signed_, is_four_state_};
  }
  if (is_signed_) {
    return FromInt(ToInt64() / rhs, bit_width_, true, is_four_state_);
  }
  const auto au = NarrowWordOf(*this);
  const auto bu = static_cast<std::uint64_t>(rhs);
  return FromInt(
      static_cast<std::int64_t>(au / bu), bit_width_, false, is_four_state_);
}

auto PackedArray::operator%(const PackedArray& other) const -> PackedArray {
  ExpectSameShape(*this, other, "operator%");
  WidthOnlyNarrow(*this, "operator%");
  if (HasUnknown() || other.HasUnknown()) {
    return AllX(bit_width_, is_signed_);
  }
  const auto rhs = other.ToInt64();
  if (rhs == 0) {
    // See operator/'s div-by-zero note: deliberate use of the default ctor's
    // shape-default (all-X for 4-state, zero for 2-state).
    return PackedArray{bit_width_, is_signed_, is_four_state_};
  }
  if (is_signed_) {
    return FromInt(ToInt64() % rhs, bit_width_, true, is_four_state_);
  }
  const auto au = NarrowWordOf(*this);
  const auto bu = static_cast<std::uint64_t>(rhs);
  return FromInt(
      static_cast<std::int64_t>(au % bu), bit_width_, false, is_four_state_);
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
  WidthOnlyNarrow(*this, "operator==");
  if (HasUnknown() || other.HasUnknown()) {
    return AllX(1U, false);
  }
  return OneBitResult(
      NarrowWordOf(*this) == NarrowWordOf(other), is_four_state_);
}

auto PackedArray::operator!=(const PackedArray& other) const -> PackedArray {
  ExpectSameShape(*this, other, "operator!=");
  WidthOnlyNarrow(*this, "operator!=");
  if (HasUnknown() || other.HasUnknown()) {
    return AllX(1U, false);
  }
  return OneBitResult(
      NarrowWordOf(*this) != NarrowWordOf(other), is_four_state_);
}

auto PackedArray::operator<(const PackedArray& other) const -> PackedArray {
  ExpectSameShape(*this, other, "operator<");
  WidthOnlyNarrow(*this, "operator<");
  if (HasUnknown() || other.HasUnknown()) {
    return AllX(1U, false);
  }
  if (is_signed_) {
    return OneBitResult(ToInt64() < other.ToInt64(), is_four_state_);
  }
  return OneBitResult(
      NarrowWordOf(*this) < NarrowWordOf(other), is_four_state_);
}

auto PackedArray::operator<=(const PackedArray& other) const -> PackedArray {
  ExpectSameShape(*this, other, "operator<=");
  WidthOnlyNarrow(*this, "operator<=");
  if (HasUnknown() || other.HasUnknown()) {
    return AllX(1U, false);
  }
  if (is_signed_) {
    return OneBitResult(ToInt64() <= other.ToInt64(), is_four_state_);
  }
  return OneBitResult(
      NarrowWordOf(*this) <= NarrowWordOf(other), is_four_state_);
}

auto PackedArray::operator>(const PackedArray& other) const -> PackedArray {
  ExpectSameShape(*this, other, "operator>");
  WidthOnlyNarrow(*this, "operator>");
  if (HasUnknown() || other.HasUnknown()) {
    return AllX(1U, false);
  }
  if (is_signed_) {
    return OneBitResult(ToInt64() > other.ToInt64(), is_four_state_);
  }
  return OneBitResult(
      NarrowWordOf(*this) > NarrowWordOf(other), is_four_state_);
}

auto PackedArray::operator>=(const PackedArray& other) const -> PackedArray {
  ExpectSameShape(*this, other, "operator>=");
  WidthOnlyNarrow(*this, "operator>=");
  if (HasUnknown() || other.HasUnknown()) {
    return AllX(1U, false);
  }
  if (is_signed_) {
    return OneBitResult(ToInt64() >= other.ToInt64(), is_four_state_);
  }
  return OneBitResult(
      NarrowWordOf(*this) >= NarrowWordOf(other), is_four_state_);
}

auto PackedArray::operator-() const -> PackedArray {
  WidthOnlyNarrow(*this, "operator- (unary)");
  if (HasUnknown()) {
    return AllX(bit_width_, is_signed_);
  }
  const auto mask = MaskForWidth(bit_width_);
  return FromInt(
      static_cast<std::int64_t>((-NarrowWordOf(*this)) & mask), bit_width_,
      is_signed_, is_four_state_);
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

auto ShiftAmountAsUint(const PackedArray& amount) -> std::uint64_t {
  if (amount.BitWidth() <= 64U) {
    return amount.ValueWords()[0] & MaskForWidth(amount.BitWidth());
  }
  return ~std::uint64_t{0};
}

}  // namespace

auto PackedArray::ShiftLeft(const PackedArray& amount) const -> PackedArray {
  WidthOnlyNarrow(*this, "ShiftLeft");
  // LRM 11.4.10: X/Z in the amount yields an all-X result; X/Z in the
  // value rides along by shifting the unknown plane in parallel.
  if (amount.HasUnknown()) {
    return AllX(bit_width_, is_signed_);
  }
  const auto amt = ShiftAmountAsUint(amount);
  if (amt >= bit_width_) {
    return FromInt(0, bit_width_, is_signed_, is_four_state_);
  }
  const auto mask = MaskForWidth(bit_width_);
  const auto new_value = (NarrowWordOf(*this) << amt) & mask;
  if (!is_four_state_) {
    return FromInt(
        static_cast<std::int64_t>(new_value), bit_width_, is_signed_, false);
  }
  const auto unk_word = UnknownWords()[0] & mask;
  const auto new_unknown = (unk_word << amt) & mask;
  return FromWords({new_value}, {new_unknown}, bit_width_, is_signed_, true);
}

auto PackedArray::LogicalShiftRight(const PackedArray& amount) const
    -> PackedArray {
  WidthOnlyNarrow(*this, "LogicalShiftRight");
  if (amount.HasUnknown()) {
    return AllX(bit_width_, is_signed_);
  }
  const auto amt = ShiftAmountAsUint(amount);
  if (amt >= bit_width_) {
    return FromInt(0, bit_width_, is_signed_, is_four_state_);
  }
  const auto new_value = NarrowWordOf(*this) >> amt;
  if (!is_four_state_) {
    return FromInt(
        static_cast<std::int64_t>(new_value), bit_width_, is_signed_, false);
  }
  const auto unk_word = UnknownWords()[0] & MaskForWidth(bit_width_);
  const auto new_unknown = unk_word >> amt;
  return FromWords({new_value}, {new_unknown}, bit_width_, is_signed_, true);
}

auto PackedArray::ArithmeticShiftRight(const PackedArray& amount) const
    -> PackedArray {
  WidthOnlyNarrow(*this, "ArithmeticShiftRight");
  if (!is_signed_) {
    return LogicalShiftRight(amount);
  }
  if (amount.HasUnknown()) {
    return AllX(bit_width_, is_signed_);
  }
  const auto amt = ShiftAmountAsUint(amount);
  const auto signed_value = ToInt64();
  if (amt >= bit_width_) {
    return FromInt(signed_value < 0 ? -1 : 0, bit_width_, true, is_four_state_);
  }
  if (!is_four_state_) {
    return FromInt(signed_value >> amt, bit_width_, true, false);
  }
  // Each plane shifts with its own MSB as the fill, so an X at the top
  // extends down into the new top bits instead of degenerating to the
  // value-plane sign.
  const auto mask = MaskForWidth(bit_width_);
  const auto value_shifted =
      static_cast<std::uint64_t>(signed_value >> amt) & mask;
  const auto unk_word = UnknownWords()[0] & mask;
  std::uint64_t unk_shifted = unk_word >> amt;
  if ((unk_word >> (bit_width_ - 1U)) != 0U) {
    const auto fill_mask = mask & ~MaskForWidth(bit_width_ - amt);
    unk_shifted |= fill_mask;
  }
  unk_shifted &= mask;
  return FromWords({value_shifted}, {unk_shifted}, bit_width_, true, true);
}

auto PackedArray::Power(const PackedArray& exponent) const -> PackedArray {
  WidthOnlyNarrow(*this, "Power");
  WidthOnlyNarrow(exponent, "Power");
  // The exponent is checked before the base so that `X ** 0 = 1` wins
  // over X-propagation from the base (LRM 11.4.10).
  if (exponent.HasUnknown()) {
    return AllX(bit_width_, is_signed_);
  }
  const auto exp = exponent.ToInt64();
  if (exp == 0) {
    return FromInt(1, bit_width_, is_signed_, is_four_state_);
  }
  if (HasUnknown()) {
    return AllX(bit_width_, is_signed_);
  }
  const auto base = ToInt64();
  if (exp < 0) {
    if (base == 1) return FromInt(1, bit_width_, is_signed_, is_four_state_);
    if (base == -1) {
      return FromInt(
          (exp & 1) != 0 ? -1 : 1, bit_width_, is_signed_, is_four_state_);
    }
    return FromInt(0, bit_width_, is_signed_, is_four_state_);
  }
  std::int64_t result = 1;
  std::int64_t b = base;
  std::int64_t e = exp;
  while (e > 0) {
    if ((e & 1) != 0) {
      result *= b;
    }
    b *= b;
    e >>= 1;
  }
  return FromInt(result, bit_width_, is_signed_, is_four_state_);
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
