#include "lyra/value/packed_array.hpp"

#include <algorithm>
#include <bit>
#include <cstdint>
#include <ranges>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

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

auto TotalBitWidthOf(std::span<const PackedRange> dims) -> std::uint64_t {
  std::uint64_t total = 1;
  for (const auto& d : dims) {
    total *= d.ElementCount();
  }
  return total;
}

}  // namespace

PackedArray::PackedArray()
    : bit_width_(0),
      is_signed_(false),
      is_four_state_(false),
      storage_(BitValue{0}),
      dims_() {
}

PackedArray::PackedArray(
    std::initializer_list<PackedRange> dims, bool is_signed, bool is_four_state)
    : bit_width_(TotalBitWidthOf(
          std::span<const PackedRange>{dims.begin(), dims.size()})),
      is_signed_(is_signed),
      is_four_state_(is_four_state),
      storage_(MakeStorage(bit_width_, is_four_state)),
      dims_(dims) {
}

PackedArray::PackedArray(
    std::uint64_t bit_width, bool is_signed, bool is_four_state)
    : bit_width_(bit_width),
      is_signed_(is_signed),
      is_four_state_(is_four_state),
      storage_(MakeStorage(bit_width, is_four_state)),
      dims_({PackedRange{
          .left = static_cast<std::int64_t>(bit_width) - 1, .right = 0}}) {
}

auto PackedArray::Int(std::int32_t value) -> PackedArray {
  return FromInt(value, 32U, true, false);
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
  PackedWordVector words(n);
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
  const auto word_count = static_cast<std::size_t>((bit_width + 63U) / 64U);
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
  // Re-construct the storage in place. `BitValue(W)`'s default state is
  // all-zero and `LogicValue(W)`'s is all-X, which match the LRM Table 6-7
  // canonical defaults for 2-state and 4-state respectively. Routing through
  // the value-type ctors keeps the LRM rule encoded in exactly one place
  // (the ctors themselves). `bit_width_`, `is_signed_`, `is_four_state_`,
  // and `dims_` are preserved.
  if (is_four_state_) {
    storage_.emplace<LogicValue>(bit_width_);
  } else {
    storage_.emplace<BitValue>(bit_width_);
  }
}

auto PackedArray::Dims() const -> std::span<const PackedRange> {
  return std::span<const PackedRange>{dims_.data(), dims_.size()};
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

auto PackedArray::GetBit(std::uint64_t flat_offset) const -> FourStateBit {
  if (flat_offset >= bit_width_) {
    return is_four_state_ ? FourStateBit::kUnknown : FourStateBit::kZero;
  }
  const auto w_idx = static_cast<std::size_t>(flat_offset / 64U);
  const auto b_idx = static_cast<std::uint64_t>(flat_offset % 64U);
  const auto vw = ValueWords();
  const bool value_bit = ((vw[w_idx] >> b_idx) & std::uint64_t{1}) != 0U;
  if (!is_four_state_) {
    return value_bit ? FourStateBit::kOne : FourStateBit::kZero;
  }
  const auto uw = UnknownWords();
  const bool unknown_bit =
      w_idx < uw.size() && ((uw[w_idx] >> b_idx) & std::uint64_t{1}) != 0U;
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

// A moved-from PackedArray must stay a fully formed value of the same shape,
// not an empty husk: STL relocation (deque / vector insert / erase)
// move-constructs an element and then assigns into the vacated slot, and
// AssignFrom requires valid storage and dims on its destination. So the source
// keeps its dims (copied, not moved) and its storage is rebuilt to the
// canonical zero of its width. ResetToDefault can allocate for wide values; on
// allocation failure the noexcept move terminates, which is the codebase's
// treatment of OOM anyway.
PackedArray::PackedArray(PackedArray&& other) noexcept
    : bit_width_(other.bit_width_),
      is_signed_(other.is_signed_),
      is_four_state_(other.is_four_state_),
      storage_(std::move(other.storage_)),
      dims_(other.dims_) {
  other.ResetToDefault();
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
  // A 0-bit destination is the default-constructed "uninitialized" sentinel
  // state (e.g., an `Var<PackedArray>` field that has not yet received its
  // first MIR-level assignment). Adopt the source's shape entirely so the
  // first store into a freshly declared variable succeeds; subsequent stores
  // run through the normal LRM 10.6.1 shape-preserving path.
  if (bit_width_ == 0) {
    bit_width_ = other.bit_width_;
    is_signed_ = other.is_signed_;
    is_four_state_ = other.is_four_state_;
    storage_ = MakeStorage(bit_width_, is_four_state_);
    dims_ = other.dims_;
  }
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

namespace {

// Word-level shift-and-OR copy: writes `src_bit_width` bits from `src` into
// `dst` starting at bit position `dst_lsb`. Caller guarantees `dst` is wide
// enough. Avoids the per-bit loop pattern in AssignSlice; complexity is
// O(src_words) regardless of width.
auto BlitBits(
    std::span<std::uint64_t> dst, std::uint64_t dst_lsb,
    std::span<const std::uint64_t> src, std::uint64_t src_bit_width) -> void {
  if (src_bit_width == 0U) return;
  const auto word_off = static_cast<std::size_t>(dst_lsb / 64U);
  const auto bit_off = static_cast<std::uint64_t>(dst_lsb % 64U);
  const auto src_words = WordCountForBits(src_bit_width);
  const auto top_bits = src_bit_width % 64U;
  const std::uint64_t top_mask =
      top_bits == 0U ? ~std::uint64_t{0} : (std::uint64_t{1} << top_bits) - 1U;
  for (std::size_t i = 0; i < src_words; ++i) {
    std::uint64_t w = src[i];
    if (i + 1U == src_words) w &= top_mask;
    dst[word_off + i] |= w << bit_off;
    if (bit_off != 0U && (word_off + i + 1U) < dst.size()) {
      dst[word_off + i + 1U] |= w >> (64U - bit_off);
    }
  }
}

}  // namespace

auto PackedArray::Concat(std::span<const PackedArray* const> operands)
    -> PackedArray {
  if (operands.empty()) {
    throw InternalError("PackedArray::Concat: empty operand list");
  }
  std::uint64_t total = 0;
  bool any_four_state = false;
  for (const auto* op : operands) {
    total += op->BitWidth();
    any_four_state = any_four_state || op->IsFourState();
  }
  if (total == 0U) {
    throw InternalError("PackedArray::Concat: total bit width is zero");
  }
  PackedArray result{total, false, any_four_state};
  auto dst_value = std::visit(
      [](auto& v) { return detail::PackedAccess::ValueWords(v.View()); },
      result.storage_);
  std::span<std::uint64_t> dst_unknown;
  if (any_four_state) {
    auto& lv = std::get<LogicValue>(result.storage_);
    dst_unknown = detail::PackedAccess::UnknownWords(lv.View());
  }
  // `PackedArray` 4-state default is all-X (value=1, unknown=1) so a freshly
  // constructed result for `any_four_state` has stale 1s in both planes. The
  // bit-blit loop uses |= and would not clear them, so zero both spans before
  // depositing operand bits.
  std::ranges::fill(dst_value, std::uint64_t{0});
  if (any_four_state) {
    std::ranges::fill(dst_unknown, std::uint64_t{0});
  }
  std::uint64_t cursor = 0;
  for (const auto* op : operands | std::views::reverse) {
    BlitBits(dst_value, cursor, op->ValueWords(), op->BitWidth());
    if (any_four_state && op->IsFourState()) {
      BlitBits(dst_unknown, cursor, op->UnknownWords(), op->BitWidth());
    }
    cursor += op->BitWidth();
  }
  return result;
}

auto PackedArray::Replicate(const PackedArray& operand, std::uint64_t count)
    -> PackedArray {
  const std::uint64_t total = operand.BitWidth() * count;
  if (total == 0U) {
    throw InternalError(
        "PackedArray::Replicate: zero-width result (count == 0 is only legal "
        "inside an outer concat with positive-sized siblings)");
  }
  PackedArray result{total, false, operand.IsFourState()};
  auto dst_value = std::visit(
      [](auto& v) { return detail::PackedAccess::ValueWords(v.View()); },
      result.storage_);
  std::span<std::uint64_t> dst_unknown;
  if (operand.IsFourState()) {
    auto& lv = std::get<LogicValue>(result.storage_);
    dst_unknown = detail::PackedAccess::UnknownWords(lv.View());
  }
  // Same stale-X concern as `Concat`: zero both planes before BlitBits ORs in
  // the operand bits.
  std::ranges::fill(dst_value, std::uint64_t{0});
  if (operand.IsFourState()) {
    std::ranges::fill(dst_unknown, std::uint64_t{0});
  }
  for (std::uint64_t i = 0; i < count; ++i) {
    const std::uint64_t cursor = i * operand.BitWidth();
    BlitBits(dst_value, cursor, operand.ValueWords(), operand.BitWidth());
    if (operand.IsFourState()) {
      BlitBits(dst_unknown, cursor, operand.UnknownWords(), operand.BitWidth());
    }
  }
  return result;
}

auto PackedArray::ConvertFrom(
    const PackedArrayRef& src, std::uint64_t dst_bit_width, bool dst_is_signed,
    bool dst_is_four_state) -> PackedArray {
  return ConvertFrom(
      src.ToOwned(), dst_bit_width, dst_is_signed, dst_is_four_state);
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

auto MakeWordBuffer(std::uint64_t bit_width) -> PackedWordVector {
  return PackedWordVector(WordCountForBits(bit_width), std::uint64_t{0});
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

auto PackedArray::CasezEquals(const PackedArray& other) const -> PackedArray {
  ExpectSameShape(*this, other, "CasezEquals");
  const auto words = WordCountForBits(bit_width_);
  const auto a_val = ValueWords();
  const auto b_val = other.ValueWords();
  const auto a_unk = UnknownWords();
  const auto b_unk = other.UnknownWords();
  const auto top_mask = MaskForWidth(bit_width_ - ((words - 1U) * 64U));
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
    std::uint64_t cmp_mask = ~(a_z | b_z);
    if (w + 1U == words) {
      cmp_mask &= top_mask;
    }
    if (((aw_val ^ bw_val) & cmp_mask) != 0U ||
        ((aw_unk ^ bw_unk) & cmp_mask) != 0U) {
      return OneBitResult(false, is_four_state_);
    }
  }
  return OneBitResult(true, is_four_state_);
}

auto PackedArray::CasexEquals(const PackedArray& other) const -> PackedArray {
  ExpectSameShape(*this, other, "CasexEquals");
  const auto words = WordCountForBits(bit_width_);
  const auto a_val = ValueWords();
  const auto b_val = other.ValueWords();
  const auto a_unk = UnknownWords();
  const auto b_unk = other.UnknownWords();
  const auto top_mask = MaskForWidth(bit_width_ - ((words - 1U) * 64U));
  for (std::size_t w = 0; w < words; ++w) {
    const std::uint64_t aw_val = a_val[w];
    const std::uint64_t bw_val = b_val[w];
    const std::uint64_t aw_unk = w < a_unk.size() ? a_unk[w] : 0U;
    const std::uint64_t bw_unk = w < b_unk.size() ? b_unk[w] : 0U;
    // casex masks out any-unknown bits on either side (X or Z); the value
    // plane carries the comparison on the remaining bits.
    std::uint64_t cmp_mask = ~(aw_unk | bw_unk);
    if (w + 1U == words) {
      cmp_mask &= top_mask;
    }
    if (((aw_val ^ bw_val) & cmp_mask) != 0U) {
      return OneBitResult(false, is_four_state_);
    }
  }
  return OneBitResult(true, is_four_state_);
}

auto PackedArray::ExtractBits(
    const PackedArray& lsb_bit, std::uint32_t bit_width) const -> PackedArray {
  if (bit_width == 0U) {
    throw InternalError("PackedArray::ExtractBits: bit_width must be >= 1");
  }
  if (lsb_bit.HasUnknown() || lsb_bit.BitWidth() > 64U) {
    if (is_four_state_) {
      return AllX(bit_width, false);
    }
    return PackedArray{bit_width, false, false};
  }
  const std::int64_t start = lsb_bit.ToInt64();
  const auto src_value = ValueWords();
  const auto src_unknown = UnknownWords();
  const auto bw_signed = static_cast<std::int64_t>(bit_width_);
  auto val_buf = MakeWordBuffer(bit_width);
  auto unk_buf =
      is_four_state_ ? MakeWordBuffer(bit_width) : PackedWordVector{};
  for (std::uint32_t i = 0; i < bit_width; ++i) {
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
      bit_width, false, is_four_state_,
      std::span<const std::uint64_t>{val_buf.data(), val_buf.size()},
      is_four_state_
          ? std::span<const std::uint64_t>{unk_buf.data(), unk_buf.size()}
          : std::span<const std::uint64_t>{});
}

auto PackedArray::AssignSlice(
    const PackedArray& lsb_bit, std::uint32_t bit_width,
    const PackedArray& value) -> void {
  if (bit_width == 0U) {
    throw InternalError("PackedArray::AssignSlice: bit_width must be >= 1");
  }
  if (value.BitWidth() != bit_width) {
    throw InternalError(
        "PackedArray::AssignSlice: value width does not match slice width");
  }
  if (value.IsFourState() != is_four_state_) {
    throw InternalError(
        "PackedArray::AssignSlice: value state kind does not match target");
  }
  if (lsb_bit.HasUnknown() || lsb_bit.BitWidth() > 64U) {
    return;
  }
  const std::int64_t start = lsb_bit.ToInt64();
  const auto bw_signed = static_cast<std::int64_t>(bit_width_);
  auto dst_value = std::visit(
      [](auto& v) { return detail::PackedAccess::ValueWords(v.View()); },
      storage_);
  std::span<std::uint64_t> dst_unknown;
  if (is_four_state_) {
    auto& dst_lv = std::get<LogicValue>(storage_);
    dst_unknown = detail::PackedAccess::UnknownWords(dst_lv.View());
  }
  const auto src_value = value.ValueWords();
  const auto src_unknown = value.UnknownWords();
  for (std::uint32_t i = 0; i < bit_width; ++i) {
    const std::int64_t pos = start + static_cast<std::int64_t>(i);
    if (pos < 0 || pos >= bw_signed) {
      continue;
    }
    const auto dst_w = static_cast<std::size_t>(pos / 64);
    const std::uint64_t dst_mask = std::uint64_t{1}
                                   << (static_cast<std::uint64_t>(pos) % 64U);
    const auto src_w = static_cast<std::size_t>(i / 64U);
    const std::uint64_t src_mask = std::uint64_t{1} << (i % 64U);
    if ((src_value[src_w] & src_mask) != 0U) {
      dst_value[dst_w] |= dst_mask;
    } else {
      dst_value[dst_w] &= ~dst_mask;
    }
    if (is_four_state_) {
      const bool src_unk_bit =
          src_w < src_unknown.size() && (src_unknown[src_w] & src_mask) != 0U;
      if (src_unk_bit) {
        dst_unknown[dst_w] |= dst_mask;
      } else {
        dst_unknown[dst_w] &= ~dst_mask;
      }
    }
  }
}

namespace {

// Canonical shape for PackedArrayRef bit offsets: 64-bit signed 4-state.
// Wide enough to hold any valid bit position, signed so negative-OOB indices
// stay negative through arithmetic, 4-state so X/Z propagates from any layer
// to the final write (LRM 11.5.1 "X/Z position is a no-op").
constexpr std::uint64_t kOffsetBitWidth = 64;
constexpr bool kOffsetSigned = true;
constexpr bool kOffsetFourState = true;

auto Canonicalize(const PackedArray& p) -> PackedArray {
  if (p.BitWidth() == kOffsetBitWidth && p.IsSigned() == kOffsetSigned &&
      p.IsFourState() == kOffsetFourState) {
    return p;
  }
  return PackedArray::ConvertFrom(
      p, kOffsetBitWidth, kOffsetSigned, kOffsetFourState);
}

// Bit width of one element of `dims_view`'s outer dim. Caller guarantees
// `dims_view.size() >= 1` and `total_bit_width % outer_count == 0`.
auto OuterElementBitWidth(
    std::uint64_t total_bit_width, std::span<const PackedRange> dims_view)
    -> std::uint32_t {
  if (dims_view.empty()) {
    throw InternalError(
        "OuterElementBitWidth: empty dim stack (selector applied to a "
        "scalar, which the frontend should reject)");
  }
  return static_cast<std::uint32_t>(
      total_bit_width / dims_view.front().ElementCount());
}

// Pops the outer dim from `dims_view` and returns the remainder as a fresh
// vector. For 1-element results (single-bit / element-select on innermost
// dim) the result is empty -- subsequent chain ops on it are illegal and
// the frontend rejects them, so this stays well-defined as long as we never
// recurse into an empty dim stack.
auto PopOuterDim(std::span<const PackedRange> dims_view)
    -> std::vector<PackedRange> {
  if (dims_view.empty()) {
    throw InternalError(
        "PopOuterDim: empty dim stack (selector applied to a scalar)");
  }
  return std::vector<PackedRange>{dims_view.begin() + 1, dims_view.end()};
}

// Replaces the outer dim's count with `new_count`, preserving inner dims.
// Used by `Slice` to construct the slice result's dim stack.
auto ReplaceOuterDimCount(
    std::span<const PackedRange> dims_view, std::uint32_t new_count)
    -> std::vector<PackedRange> {
  if (dims_view.empty()) {
    throw InternalError(
        "ReplaceOuterDimCount: empty dim stack (range select on a scalar)");
  }
  std::vector<PackedRange> result;
  result.reserve(dims_view.size());
  result.push_back(
      PackedRange{
          .left = static_cast<std::int64_t>(new_count) - 1, .right = 0});
  for (std::size_t i = 1; i < dims_view.size(); ++i) {
    result.push_back(dims_view[i]);
  }
  return result;
}

}  // namespace

auto PackedArray::ElementAt(const PackedArray& idx) -> PackedArrayRef {
  const auto element_bw = OuterElementBitWidth(bit_width_, dims_);
  return PackedArrayRef{
      *this,
      element_bw == 1U
          ? Canonicalize(idx)
          : (Canonicalize(idx) * PackedArray::FromInt(
                                     static_cast<std::int64_t>(element_bw),
                                     kOffsetBitWidth, kOffsetSigned,
                                     kOffsetFourState)),
      element_bw, PopOuterDim(dims_)};
}

auto PackedArray::ElementAt(const PackedArray& idx) const -> PackedArray {
  const auto element_bw = OuterElementBitWidth(bit_width_, dims_);
  const auto bit_offset =
      element_bw == 1U
          ? Canonicalize(idx)
          : (Canonicalize(idx) * PackedArray::FromInt(
                                     static_cast<std::int64_t>(element_bw),
                                     kOffsetBitWidth, kOffsetSigned,
                                     kOffsetFourState));
  return ExtractBits(bit_offset, element_bw);
}

auto PackedArray::Slice(
    const PackedArray& offset_in_outer_elements,
    std::uint32_t count_in_outer_elements) -> PackedArrayRef {
  const auto element_bw = OuterElementBitWidth(bit_width_, dims_);
  return PackedArrayRef{
      *this,
      element_bw == 1U
          ? Canonicalize(offset_in_outer_elements)
          : (Canonicalize(offset_in_outer_elements) *
             PackedArray::FromInt(
                 static_cast<std::int64_t>(element_bw), kOffsetBitWidth,
                 kOffsetSigned, kOffsetFourState)),
      count_in_outer_elements * element_bw,
      ReplaceOuterDimCount(dims_, count_in_outer_elements)};
}

auto PackedArray::Slice(
    const PackedArray& offset_in_outer_elements,
    std::uint32_t count_in_outer_elements) const -> PackedArray {
  const auto element_bw = OuterElementBitWidth(bit_width_, dims_);
  const auto bit_offset =
      element_bw == 1U
          ? Canonicalize(offset_in_outer_elements)
          : (Canonicalize(offset_in_outer_elements) *
             PackedArray::FromInt(
                 static_cast<std::int64_t>(element_bw), kOffsetBitWidth,
                 kOffsetSigned, kOffsetFourState));
  return ExtractBits(bit_offset, count_in_outer_elements * element_bw);
}

PackedArrayRef::PackedArrayRef(
    PackedArray& root, const PackedArray& bit_offset, std::uint32_t bit_width,
    std::vector<PackedRange> dims)
    : root_(&root),
      bit_offset_(Canonicalize(bit_offset)),
      bit_width_(bit_width),
      dims_(std::move(dims)) {
}

auto PackedArrayRef::ToOwned() const -> PackedArray {
  return std::as_const(*root_).ExtractBits(bit_offset_, bit_width_);
}

auto PackedArrayRef::operator=(const PackedArray& value) -> PackedArrayRef& {
  root_->AssignSlice(bit_offset_, bit_width_, value);
  return *this;
}

auto PackedArrayRef::ElementAt(const PackedArray& idx) const -> PackedArrayRef {
  const auto element_bw = OuterElementBitWidth(bit_width_, dims_);
  return PackedArrayRef{
      *root_,
      element_bw == 1U
          ? (bit_offset_ + Canonicalize(idx))
          : (bit_offset_ +
             (Canonicalize(idx) * PackedArray::FromInt(
                                      static_cast<std::int64_t>(element_bw),
                                      kOffsetBitWidth, kOffsetSigned,
                                      kOffsetFourState))),
      element_bw, PopOuterDim(dims_)};
}

auto PackedArrayRef::Slice(
    const PackedArray& offset_in_outer_elements,
    std::uint32_t count_in_outer_elements) const -> PackedArrayRef {
  const auto element_bw = OuterElementBitWidth(bit_width_, dims_);
  return PackedArrayRef{
      *root_,
      element_bw == 1U
          ? (bit_offset_ + Canonicalize(offset_in_outer_elements))
          : (bit_offset_ +
             (Canonicalize(offset_in_outer_elements) *
              PackedArray::FromInt(
                  static_cast<std::int64_t>(element_bw), kOffsetBitWidth,
                  kOffsetSigned, kOffsetFourState))),
      count_in_outer_elements * element_bw,
      ReplaceOuterDimCount(dims_, count_in_outer_elements)};
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

auto PackedArray::operator&&(const PackedArray& other) const -> PackedArray {
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

auto PackedArray::operator||(const PackedArray& other) const -> PackedArray {
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

auto PackedArray::operator!() const -> PackedArray {
  switch (TruthinessOf(*this)) {
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
