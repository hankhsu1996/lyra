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
#include "lyra/value/slice_selector.hpp"
#include "lyra/value/string.hpp"

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

// The two operands occupy the same raw bit-plane storage: identical width and
// identical state domain (the state domain fixes the plane count). This is the
// precondition for a word-parallel operation or a low-level bit copy. It is
// deliberately weaker than same-representation -- it does not compare the
// dimension stack or signedness, neither of which a word-parallel bit op reads.
auto RequireSameStorageDomain(
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

PackedArray::PackedArray() : type_({}, false, false), storage_(BitValue{0}) {
}

PackedArray::PackedArray(PackedType type)
    : type_(std::move(type)),
      storage_(MakeStorage(type_.bit_width, type_.is_four_state)) {
}

PackedArray::PackedArray(
    std::uint64_t bit_width, bool is_signed, bool is_four_state)
    : PackedArray(
          PackedType{
              {PackedRange{
                  .left = static_cast<std::int64_t>(bit_width) - 1,
                  .right = 0}},
              is_signed,
              is_four_state}) {
}

PackedArray::PackedArray(
    std::span<const PackedRange> dims, bool is_signed, bool is_four_state)
    : PackedArray(
          PackedType{
              std::vector<PackedRange>(dims.begin(), dims.end()), is_signed,
              is_four_state}) {
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

auto PackedArray::FromBool(bool value) -> PackedArray {
  return Bit(value);
}

auto PackedArray::MakeFromWordPlanesShaped(
    std::span<const PackedRange> dims, bool is_signed, bool is_four_state,
    std::span<const std::uint64_t> value_words,
    std::span<const std::uint64_t> unknown_words) -> PackedArray {
  const auto bit_width = PackedType::WidthOf(dims);
  if (bit_width == 0U) {
    throw InternalError(
        "PackedArray::MakeFromWordPlanesShaped: bit_width must be >= 1");
  }
  const std::size_t expected = (bit_width + 63U) / 64U;
  if (value_words.size() != expected) {
    throw InternalError(
        "PackedArray::MakeFromWordPlanesShaped: value word count mismatch");
  }
  if (!is_four_state && !unknown_words.empty()) {
    throw InternalError(
        "PackedArray::MakeFromWordPlanesShaped: 2-state shape forbids unknown "
        "words");
  }
  if (is_four_state && !unknown_words.empty() &&
      unknown_words.size() != expected) {
    throw InternalError(
        "PackedArray::MakeFromWordPlanesShaped: 4-state unknown word count "
        "mismatch");
  }

  PackedArray p{dims, is_signed, is_four_state};
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

auto PackedArray::MakeFromWordPlanes(
    std::uint64_t bit_width, bool is_signed, bool is_four_state,
    std::span<const std::uint64_t> value_words,
    std::span<const std::uint64_t> unknown_words) -> PackedArray {
  if (bit_width == 0U) {
    throw InternalError(
        "PackedArray::MakeFromWordPlanes: bit_width must be >= 1");
  }
  const std::array<PackedRange, 1> dims{PackedRange{
      .left = static_cast<std::int64_t>(bit_width) - 1, .right = 0}};
  return MakeFromWordPlanesShaped(
      std::span<const PackedRange>{dims}, is_signed, is_four_state, value_words,
      unknown_words);
}

auto PackedArray::FromInt(std::int64_t value, const PackedType& type)
    -> PackedArray {
  const std::size_t n = (type.bit_width + 63U) / 64U;
  PackedWordVector words(n);
  if (type.bit_width <= 64U) {
    words[0] = static_cast<std::uint64_t>(value);
  } else {
    const std::uint64_t fill = (value < 0) ? ~std::uint64_t{0} : 0U;
    words[0] = static_cast<std::uint64_t>(value);
    for (std::size_t i = 1; i < n; ++i) {
      words[i] = fill;
    }
  }
  return MakeFromWordPlanesShaped(
      std::span<const PackedRange>{type.dims}, type.is_signed,
      type.is_four_state,
      std::span<const std::uint64_t>{words.data(), words.size()}, {});
}

auto PackedArray::FromInt(
    std::int64_t value, std::uint64_t bit_width, bool is_signed,
    bool is_four_state) -> PackedArray {
  return FromInt(
      value,
      PackedType{
          {PackedRange{
              .left = static_cast<std::int64_t>(bit_width) - 1, .right = 0}},
          is_signed,
          is_four_state});
}

auto PackedArray::FromInt(std::int64_t value, const PackedArray& prototype)
    -> PackedArray {
  return FromInt(value, prototype.type_);
}

auto PackedArray::FromWords(
    std::span<const std::uint64_t> value_words,
    std::span<const std::uint64_t> unknown_words, const PackedType& type)
    -> PackedArray {
  return MakeFromWordPlanesShaped(
      std::span<const PackedRange>{type.dims}, type.is_signed,
      type.is_four_state, value_words, unknown_words);
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
  return type_.bit_width;
}

auto PackedArray::IsSigned() const -> bool {
  return type_.is_signed;
}

auto PackedArray::IsFourState() const -> bool {
  return type_.is_four_state;
}

auto PackedArray::ResetToDefault() -> void {
  // Re-construct the storage in place. `BitValue(W)`'s default state is
  // all-zero and `LogicValue(W)`'s is all-X, which match the LRM Table 6-7
  // canonical defaults for 2-state and 4-state respectively. Routing through
  // the value-type ctors keeps the LRM rule encoded in exactly one place
  // (the ctors themselves). The declared type is preserved; only the bits are
  // reset.
  if (type_.is_four_state) {
    storage_.emplace<LogicValue>(type_.bit_width);
  } else {
    storage_.emplace<BitValue>(type_.bit_width);
  }
}

auto PackedArray::HighImpedanceLike(const PackedArray& prototype)
    -> PackedArray {
  if (!prototype.type_.is_four_state) {
    // A 2-state shape has no high-impedance state; the undriven value is the
    // all-zero canonical default (z collapses to 0 outside the 4-state domain).
    return PackedArray(prototype.type_);
  }
  // z is the value plane all-0 with the unknown plane all-1 (x is value 1,
  // unknown 1). Construction masks the bits above the declared width in the top
  // word.
  const std::uint64_t width = prototype.type_.bit_width;
  const std::size_t words = static_cast<std::size_t>((width + 63) / 64);
  const std::vector<std::uint64_t> value_words(words, 0);
  const std::vector<std::uint64_t> unknown_words(words, ~std::uint64_t{0});
  return FromWords(value_words, unknown_words, prototype.type_);
}

auto PackedArray::Dims() const -> std::span<const PackedRange> {
  return std::span<const PackedRange>{type_.dims.data(), type_.dims.size()};
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

auto PackedArray::ByteString() const -> std::string {
  const std::uint64_t bit_width = BitWidth();
  const std::uint64_t byte_count = bit_width / 8U;
  std::string out;
  out.reserve(static_cast<std::size_t>(byte_count));
  const auto val_words = ValueWords();
  const auto unk_words = UnknownWords();
  for (std::uint64_t byte_i = 0; byte_i < byte_count; ++byte_i) {
    unsigned char byte = 0;
    bool any_unknown = false;
    for (std::uint64_t bit_in_byte = 0; bit_in_byte < 8U; ++bit_in_byte) {
      const std::uint64_t bit_pos =
          (bit_width - 1U) - (byte_i * 8U) - bit_in_byte;
      const auto word_ix = static_cast<std::size_t>(bit_pos / 64U);
      const auto bit_ix = static_cast<std::size_t>(bit_pos % 64U);
      if (!unk_words.empty() && ((unk_words[word_ix] >> bit_ix) & 1U) != 0U) {
        any_unknown = true;
      }
      if (((val_words[word_ix] >> bit_ix) & 1U) != 0U) {
        byte |= static_cast<unsigned char>(1U << (7U - bit_in_byte));
      }
    }
    out.push_back(any_unknown ? '\0' : static_cast<char>(byte));
  }
  return out;
}

auto PackedArray::IsBitIdentical(const PackedArray& other) const -> bool {
  if (type_.bit_width != other.type_.bit_width) return false;
  if (type_.is_four_state != other.type_.is_four_state) return false;
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
  if (!type_.is_four_state) {
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
  if (flat_offset >= type_.bit_width) {
    return type_.is_four_state ? FourStateBit::kUnknown : FourStateBit::kZero;
  }
  const auto w_idx = static_cast<std::size_t>(flat_offset / 64U);
  const auto b_idx = static_cast<std::uint64_t>(flat_offset % 64U);
  const auto vw = ValueWords();
  const bool value_bit = ((vw[w_idx] >> b_idx) & std::uint64_t{1}) != 0U;
  if (!type_.is_four_state) {
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

auto PackedArray::IsUninitialized() const -> bool {
  return type_.bit_width == 0;
}

auto PackedArray::SameRepresentation(const PackedArray& other) const -> bool {
  return type_.dims == other.type_.dims &&
         type_.is_signed == other.type_.is_signed &&
         type_.is_four_state == other.type_.is_four_state;
}

auto PackedArray::ToInt64() const -> std::int64_t {
  if (type_.bit_width > 64U) {
    throw InternalError("PackedArray::ToInt64: bit_width > 64");
  }
  // LRM 6.12.1 / 6.19: when a 4-state value is read into a 2-state context,
  // X/Z bits collapse to 0. The unknown plane marks those positions, so the
  // value plane is masked by `~unknown` before any further interpretation.
  const auto value = ValueWords()[0];
  const auto unk =
      UnknownWords().empty() ? std::uint64_t{0} : UnknownWords()[0];
  const auto raw = (value & ~unk) & MaskForWidth(type_.bit_width);
  return type_.is_signed ? SignExtendToInt64(raw, type_.bit_width)
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
    std::uint64_t word = value_words[i] & ~unk;
    if (i + 1U == value_words.size()) {
      word &= MaskForWidth(type_.bit_width - (64U * i));
    }
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

auto PackedArray::ConvertBitsInto(PackedArray dst, const PackedArray& src)
    -> PackedArray {
  const Signedness src_signedness =
      src.IsSigned() ? Signedness::kSigned : Signedness::kUnsigned;
  if (dst.type_.is_four_state) {
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

auto PackedArray::ConvertFrom(
    const PackedArray& src, std::uint64_t dst_bit_width, bool dst_is_signed,
    bool dst_is_four_state) -> PackedArray {
  return ConvertBitsInto(
      PackedArray{dst_bit_width, dst_is_signed, dst_is_four_state}, src);
}

auto PackedArray::ConvertFrom(const PackedArray& src, PackedArray prototype)
    -> PackedArray {
  return ConvertBitsInto(std::move(prototype), src);
}

auto PackedArray::FromString(const String& text, const PackedArray& prototype)
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
      prototype);
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
  RequireSameStorageDomain(*this, other, "operator+");
  if (HasUnknown() || other.HasUnknown()) {
    return AllX(type_.bit_width, type_.is_signed);
  }
  auto buf = MakeWordBuffer(type_.bit_width);
  AddWordsInto(ValueWords(), other.ValueWords(), buf, type_.bit_width);
  return MakeFromWordPlanes(
      type_.bit_width, type_.is_signed, type_.is_four_state,
      std::span<const std::uint64_t>{buf.data(), buf.size()}, {});
}

auto PackedArray::operator-(const PackedArray& other) const -> PackedArray {
  RequireSameStorageDomain(*this, other, "operator-");
  if (HasUnknown() || other.HasUnknown()) {
    return AllX(type_.bit_width, type_.is_signed);
  }
  auto buf = MakeWordBuffer(type_.bit_width);
  SubWordsInto(ValueWords(), other.ValueWords(), buf, type_.bit_width);
  return MakeFromWordPlanes(
      type_.bit_width, type_.is_signed, type_.is_four_state,
      std::span<const std::uint64_t>{buf.data(), buf.size()}, {});
}

auto PackedArray::operator*(const PackedArray& other) const -> PackedArray {
  RequireSameStorageDomain(*this, other, "operator*");
  if (HasUnknown() || other.HasUnknown()) {
    return AllX(type_.bit_width, type_.is_signed);
  }
  auto buf = MakeWordBuffer(type_.bit_width);
  MulWordsInto(ValueWords(), other.ValueWords(), buf, type_.bit_width);
  return MakeFromWordPlanes(
      type_.bit_width, type_.is_signed, type_.is_four_state,
      std::span<const std::uint64_t>{buf.data(), buf.size()}, {});
}

auto PackedArray::operator/(const PackedArray& other) const -> PackedArray {
  RequireSameStorageDomain(*this, other, "operator/");
  if (HasUnknown() || other.HasUnknown()) {
    return AllX(type_.bit_width, type_.is_signed);
  }
  if (IsZero(other.ValueWords())) {
    // SV LRM 11.4.4: integer division by zero on 4-state yields X for every
    // bit; on 2-state it is implementation-defined and we pick zero. The
    // default ctor encodes both directly: LogicValue defaults to all-X
    // (matching the SV `logic` default), BitValue defaults to all-zero.
    return PackedArray{type_.bit_width, type_.is_signed, type_.is_four_state};
  }
  const bool neg_a =
      type_.is_signed && BitAt(ValueWords(), type_.bit_width - 1U) != 0U;
  const bool neg_b =
      type_.is_signed && BitAt(other.ValueWords(), type_.bit_width - 1U) != 0U;
  auto a_abs = MakeWordBuffer(type_.bit_width);
  auto b_abs = MakeWordBuffer(type_.bit_width);
  if (neg_a) {
    SubWordsInto({}, ValueWords(), a_abs, type_.bit_width);
  } else {
    std::ranges::copy(ValueWords(), a_abs.begin());
  }
  if (neg_b) {
    SubWordsInto({}, other.ValueWords(), b_abs, type_.bit_width);
  } else {
    std::ranges::copy(other.ValueWords(), b_abs.begin());
  }
  auto quot = MakeWordBuffer(type_.bit_width);
  auto rem = MakeWordBuffer(type_.bit_width);
  LongDivideUnsigned(a_abs, b_abs, quot, rem, type_.bit_width);
  if (neg_a != neg_b) {
    auto neg_quot = MakeWordBuffer(type_.bit_width);
    SubWordsInto({}, quot, neg_quot, type_.bit_width);
    quot = std::move(neg_quot);
  }
  return MakeFromWordPlanes(
      type_.bit_width, type_.is_signed, type_.is_four_state,
      std::span<const std::uint64_t>{quot.data(), quot.size()}, {});
}

auto PackedArray::operator%(const PackedArray& other) const -> PackedArray {
  RequireSameStorageDomain(*this, other, "operator%");
  if (HasUnknown() || other.HasUnknown()) {
    return AllX(type_.bit_width, type_.is_signed);
  }
  if (IsZero(other.ValueWords())) {
    // See operator/'s div-by-zero note: deliberate use of the default ctor's
    // shape-default (all-X for 4-state, zero for 2-state).
    return PackedArray{type_.bit_width, type_.is_signed, type_.is_four_state};
  }
  const bool neg_a =
      type_.is_signed && BitAt(ValueWords(), type_.bit_width - 1U) != 0U;
  const bool neg_b =
      type_.is_signed && BitAt(other.ValueWords(), type_.bit_width - 1U) != 0U;
  auto a_abs = MakeWordBuffer(type_.bit_width);
  auto b_abs = MakeWordBuffer(type_.bit_width);
  if (neg_a) {
    SubWordsInto({}, ValueWords(), a_abs, type_.bit_width);
  } else {
    std::ranges::copy(ValueWords(), a_abs.begin());
  }
  if (neg_b) {
    SubWordsInto({}, other.ValueWords(), b_abs, type_.bit_width);
  } else {
    std::ranges::copy(other.ValueWords(), b_abs.begin());
  }
  auto quot = MakeWordBuffer(type_.bit_width);
  auto rem = MakeWordBuffer(type_.bit_width);
  LongDivideUnsigned(a_abs, b_abs, quot, rem, type_.bit_width);
  // SV/C truncated modulo: remainder takes the sign of the dividend.
  if (neg_a) {
    auto neg_rem = MakeWordBuffer(type_.bit_width);
    SubWordsInto({}, rem, neg_rem, type_.bit_width);
    rem = std::move(neg_rem);
  }
  return MakeFromWordPlanes(
      type_.bit_width, type_.is_signed, type_.is_four_state,
      std::span<const std::uint64_t>{rem.data(), rem.size()}, {});
}

auto PackedArray::operator&(const PackedArray& other) const -> PackedArray {
  RequireSameStorageDomain(*this, other, "operator&");
  if (type_.bit_width <= 64U && !type_.is_four_state) {
    const auto mask = MaskForWidth(type_.bit_width);
    return FromInt(
        static_cast<std::int64_t>(
            (NarrowWordOf(*this) & NarrowWordOf(other)) & mask),
        type_.bit_width, type_.is_signed, type_.is_four_state);
  }
  PackedArray result{type_.bit_width, type_.is_signed, type_.is_four_state};
  if (type_.is_four_state) {
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
  RequireSameStorageDomain(*this, other, "operator|");
  if (type_.bit_width <= 64U && !type_.is_four_state) {
    const auto mask = MaskForWidth(type_.bit_width);
    return FromInt(
        static_cast<std::int64_t>(
            (NarrowWordOf(*this) | NarrowWordOf(other)) & mask),
        type_.bit_width, type_.is_signed, type_.is_four_state);
  }
  PackedArray result{type_.bit_width, type_.is_signed, type_.is_four_state};
  if (type_.is_four_state) {
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
  RequireSameStorageDomain(*this, other, "operator^");
  if (type_.bit_width <= 64U && !type_.is_four_state) {
    const auto mask = MaskForWidth(type_.bit_width);
    return FromInt(
        static_cast<std::int64_t>(
            (NarrowWordOf(*this) ^ NarrowWordOf(other)) & mask),
        type_.bit_width, type_.is_signed, type_.is_four_state);
  }
  PackedArray result{type_.bit_width, type_.is_signed, type_.is_four_state};
  if (type_.is_four_state) {
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
  RequireSameStorageDomain(*this, other, "BitwiseXnor");
  if (type_.bit_width <= 64U && !type_.is_four_state) {
    const auto mask = MaskForWidth(type_.bit_width);
    return FromInt(
        static_cast<std::int64_t>(
            (~(NarrowWordOf(*this) ^ NarrowWordOf(other))) & mask),
        type_.bit_width, type_.is_signed, type_.is_four_state);
  }
  PackedArray result{type_.bit_width, type_.is_signed, type_.is_four_state};
  if (type_.is_four_state) {
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
  RequireSameStorageDomain(*this, other, "operator==");
  if (HasUnknown() || other.HasUnknown()) {
    return AllX(1U, false);
  }
  return OneBitResult(
      UnsignedCompare(ValueWords(), other.ValueWords()) == 0,
      type_.is_four_state);
}

auto PackedArray::operator!=(const PackedArray& other) const -> PackedArray {
  RequireSameStorageDomain(*this, other, "operator!=");
  if (HasUnknown() || other.HasUnknown()) {
    return AllX(1U, false);
  }
  return OneBitResult(
      UnsignedCompare(ValueWords(), other.ValueWords()) != 0,
      type_.is_four_state);
}

auto PackedArray::WildcardEquals(const PackedArray& other) const
    -> PackedArray {
  RequireSameStorageDomain(*this, other, "WildcardEquals");
  const auto words = WordCountForBits(type_.bit_width);
  const auto a_val = ValueWords();
  const auto b_val = other.ValueWords();
  const auto a_unk = UnknownWords();
  const auto b_unk = other.UnknownWords();
  const auto top_mask = MaskForWidth(type_.bit_width - ((words - 1U) * 64U));
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
    return OneBitResult(false, type_.is_four_state);
  }
  if (lhs_unknown_at_compare) {
    return AllX(1U, false);
  }
  return OneBitResult(true, type_.is_four_state);
}

auto PackedArray::CasezEquals(const PackedArray& other) const -> PackedArray {
  RequireSameStorageDomain(*this, other, "CasezEquals");
  const auto words = WordCountForBits(type_.bit_width);
  const auto a_val = ValueWords();
  const auto b_val = other.ValueWords();
  const auto a_unk = UnknownWords();
  const auto b_unk = other.UnknownWords();
  const auto top_mask = MaskForWidth(type_.bit_width - ((words - 1U) * 64U));
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
      return OneBitResult(false, type_.is_four_state);
    }
  }
  return OneBitResult(true, type_.is_four_state);
}

auto PackedArray::CasexEquals(const PackedArray& other) const -> PackedArray {
  RequireSameStorageDomain(*this, other, "CasexEquals");
  const auto words = WordCountForBits(type_.bit_width);
  const auto a_val = ValueWords();
  const auto b_val = other.ValueWords();
  const auto a_unk = UnknownWords();
  const auto b_unk = other.UnknownWords();
  const auto top_mask = MaskForWidth(type_.bit_width - ((words - 1U) * 64U));
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
      return OneBitResult(false, type_.is_four_state);
    }
  }
  return OneBitResult(true, type_.is_four_state);
}

auto PackedArray::ResolveTriState(const PackedArray& other) const
    -> PackedArray {
  RequireSameStorageDomain(*this, other, "ResolveTriState");
  const auto words = WordCountForBits(type_.bit_width);
  const auto a_val = ValueWords();
  const auto b_val = other.ValueWords();
  const auto a_unk = UnknownWords();
  const auto b_unk = other.UnknownWords();
  const auto top_mask = MaskForWidth(type_.bit_width - ((words - 1U) * 64U));
  std::vector<std::uint64_t> res_val(words, 0U);
  std::vector<std::uint64_t> res_unk(words, 0U);
  for (std::size_t w = 0; w < words; ++w) {
    const std::uint64_t av = a_val[w];
    const std::uint64_t bv = b_val[w];
    const std::uint64_t au = w < a_unk.size() ? a_unk[w] : 0U;
    const std::uint64_t bu = w < b_unk.size() ? b_unk[w] : 0U;
    // Lyra encoding (see packed.cpp): Z = (value=0, unknown=1) and is the
    // resolution identity; equal contributions pass through; any remaining
    // disagreement is a conflict resolving to X = (value=1, unknown=1).
    const std::uint64_t a_z = ~av & au;
    const std::uint64_t b_z = ~bv & bu;
    const std::uint64_t eq = ~(av ^ bv) & ~(au ^ bu);
    const std::uint64_t take_b = a_z;
    const std::uint64_t take_a = ~a_z & (b_z | eq);
    const std::uint64_t conflict = ~a_z & ~b_z & ~eq;
    std::uint64_t v = (take_b & bv) | (take_a & av) | conflict;
    std::uint64_t u = (take_b & bu) | (take_a & au) | conflict;
    if (w + 1U == words) {
      v &= top_mask;
      u &= top_mask;
    }
    res_val[w] = v;
    res_unk[w] = u;
  }
  return FromWords(res_val, res_unk, type_);
}

auto PackedArray::ExtractBits(
    const PackedArray& lsb_bit, std::span<const PackedRange> dims) const
    -> PackedArray {
  const auto bit_width = static_cast<std::uint32_t>(PackedType::WidthOf(dims));
  if (bit_width == 0U) {
    throw InternalError("PackedArray::ExtractBits: bit_width must be >= 1");
  }
  // A fully out-of-range start (X/Z lsb, or a magnitude beyond the 64-bit
  // position carrier) yields an all-X value for a 4-state source, all-zero for
  // a 2-state one. The shape is carried by constructing with `dims`.
  if (lsb_bit.HasUnknown() || lsb_bit.BitWidth() > 64U) {
    return PackedArray{dims, false, type_.is_four_state};
  }
  const std::int64_t start = lsb_bit.ToInt64();
  const auto src_value = ValueWords();
  const auto src_unknown = UnknownWords();
  const auto bw_signed = static_cast<std::int64_t>(type_.bit_width);
  auto val_buf = MakeWordBuffer(bit_width);
  auto unk_buf =
      type_.is_four_state ? MakeWordBuffer(bit_width) : PackedWordVector{};
  for (std::uint32_t i = 0; i < bit_width; ++i) {
    const std::int64_t pos = start + static_cast<std::int64_t>(i);
    const std::uint64_t out_mask = std::uint64_t{1} << (i % 64U);
    if (pos < 0 || pos >= bw_signed) {
      if (type_.is_four_state) {
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
    if (type_.is_four_state && w_idx < src_unknown.size() &&
        ((src_unknown[w_idx] >> b_idx) & 1U) != 0U) {
      unk_buf[i / 64U] |= out_mask;
    }
  }
  return MakeFromWordPlanesShaped(
      dims, false, type_.is_four_state,
      std::span<const std::uint64_t>{val_buf.data(), val_buf.size()},
      type_.is_four_state
          ? std::span<const std::uint64_t>{unk_buf.data(), unk_buf.size()}
          : std::span<const std::uint64_t>{});
}

auto PackedArray::ExtractBits(
    const PackedArray& lsb_bit, std::uint32_t bit_width) const -> PackedArray {
  if (bit_width == 0U) {
    throw InternalError("PackedArray::ExtractBits: bit_width must be >= 1");
  }
  const std::array<PackedRange, 1> dims{PackedRange{
      .left = static_cast<std::int64_t>(bit_width) - 1, .right = 0}};
  return ExtractBits(lsb_bit, std::span<const PackedRange>{dims});
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
  // A 2-state field inside a 4-state aggregate (LRM 7.2.1) writes a 2-state
  // value into a 4-state slot; the loop below clears the unknown plane at the
  // written positions (the value carries none), widening it. The reverse never
  // occurs: a 2-state aggregate has only 2-state fields, so a 4-state value
  // reaching 2-state storage is a missing upstream coercion.
  if (value.IsFourState() && !type_.is_four_state) {
    throw InternalError(
        "PackedArray::AssignSlice: a 4-state value cannot be written into "
        "2-state storage");
  }
  if (lsb_bit.HasUnknown() || lsb_bit.BitWidth() > 64U) {
    return;
  }
  const std::int64_t start = lsb_bit.ToInt64();
  const auto bw_signed = static_cast<std::int64_t>(type_.bit_width);
  auto dst_value = std::visit(
      [](auto& v) { return detail::PackedAccess::ValueWords(v.View()); },
      storage_);
  std::span<std::uint64_t> dst_unknown;
  if (type_.is_four_state) {
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
    if (type_.is_four_state) {
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

// Bit width of one element of `dim_stack`'s outer dim. Caller guarantees
// `dim_stack.size() >= 1` and `total_bit_width % outer_count == 0`.
auto OuterElementBitWidth(
    std::uint64_t total_bit_width, std::span<const PackedRange> dim_stack)
    -> std::uint32_t {
  if (dim_stack.empty()) {
    throw InternalError(
        "OuterElementBitWidth: empty dim stack (selector applied to a "
        "scalar, which the frontend should reject)");
  }
  return static_cast<std::uint32_t>(
      total_bit_width / dim_stack.front().ElementCount());
}

// Pops the outer dim from `dim_stack` and returns the remainder as a fresh
// vector. The element of a one-dimensional vector is a scalar, carried as the
// canonical one-element dimension `[0:0]` -- the same shape a declared scalar
// bit / logic holds -- rather than the empty (0-bit) stack, so a materialized
// element matches its declared element type when stored.
auto PopOuterDim(std::span<const PackedRange> dim_stack)
    -> std::vector<PackedRange> {
  if (dim_stack.empty()) {
    throw InternalError(
        "PopOuterDim: empty dim stack (selector applied to a scalar)");
  }
  std::vector<PackedRange> inner{dim_stack.begin() + 1, dim_stack.end()};
  if (inner.empty()) {
    inner.push_back(PackedRange{.left = 0, .right = 0});
  }
  return inner;
}

// Replaces the outer dim's count with `new_count`, preserving inner dims.
// Used by `Slice` to construct the slice result's dim stack.
auto ReplaceOuterDimCount(
    std::span<const PackedRange> dim_stack, std::uint32_t new_count)
    -> std::vector<PackedRange> {
  if (dim_stack.empty()) {
    throw InternalError(
        "ReplaceOuterDimCount: empty dim stack (range select on a scalar)");
  }
  std::vector<PackedRange> result;
  result.reserve(dim_stack.size());
  result.push_back(
      PackedRange{
          .left = static_cast<std::int64_t>(new_count) - 1, .right = 0});
  for (std::size_t i = 1; i < dim_stack.size(); ++i) {
    result.push_back(dim_stack[i]);
  }
  return result;
}

// A resolved sub-region of a packed value: the LSB-relative bit offset of the
// region within the source's flat storage, and the region's dim stack (width is
// the product of the dims). Both the value selectors (`Element` / `Slice` ->
// `ExtractBits`) and the reference selectors (`ElementRef` / `SliceRef` ->
// `PackedArrayRef`) consume one of these, so offset, width, and shape of a
// selection are derived in exactly one place -- the read and write sides cannot
// drift.
struct PackedSelection {
  PackedArray bit_offset;
  std::vector<PackedRange> dims;
};

// Maps a declared-coordinate index onto the outer dimension's zero-based
// position (LRM 11.5.1): a descending range subtracts its right (least-
// significant) endpoint, an ascending range subtracts the index from it. The
// arithmetic runs in the canonical 64-bit offset domain, so a caller's index of
// any width and state domain composes without a storage-domain clash, and an
// x / z index propagates to an out-of-range offset that reads the element's
// default. A descending zero-based range is the identity.
auto RebaseToZeroBased(const PackedArray& idx, const PackedRange& outer)
    -> PackedArray {
  const auto canon = Canonicalize(idx);
  const bool descending = outer.left >= outer.right;
  if (descending && outer.right == 0) {
    return canon;
  }
  const auto right = PackedArray::FromInt(
      outer.right, kOffsetBitWidth, kOffsetSigned, kOffsetFourState);
  return descending ? canon - right : right - canon;
}

// Scales an outer-element position to a flat-bit offset. One outer element is
// `element_bw` bits; when that is 1 (selecting the innermost dimension), the
// position is already a bit offset. X/Z in the position propagates.
auto ScaledOuterOffset(const PackedArray& outer_units, std::uint32_t element_bw)
    -> PackedArray {
  if (element_bw == 1U) {
    return Canonicalize(outer_units);
  }
  return Canonicalize(outer_units) * PackedArray::FromInt(
                                         static_cast<std::int64_t>(element_bw),
                                         kOffsetBitWidth, kOffsetSigned,
                                         kOffsetFourState);
}

auto ResolveElement(
    std::uint64_t source_bit_width, std::span<const PackedRange> source_dims,
    const PackedArray& idx) -> PackedSelection {
  const auto element_bw = OuterElementBitWidth(source_bit_width, source_dims);
  const auto zero_based = RebaseToZeroBased(idx, source_dims.front());
  return PackedSelection{
      .bit_offset = ScaledOuterOffset(zero_based, element_bw),
      .dims = PopOuterDim(source_dims)};
}

// `anchor` is the SV-declared endpoint the slice hangs from; `shift` is how
// many outer elements the low end sits below its rebased position. A constant
// range and an indexed part-select whose width grows toward the MSB pass `shift
// == 0` (the anchor rebases straight to the low end); an indexed part-select
// growing toward the LSB passes `shift == count - 1`, so the anchor rebases to
// the high end and the low end is `count - 1` below it (LRM 11.5.1). The
// subtraction runs in the canonical offset domain, so an anchor of any width or
// state domain composes and an x / z anchor propagates to an out-of-range read.
auto ResolveSlice(
    std::uint64_t source_bit_width, std::span<const PackedRange> source_dims,
    const PackedArray& anchor, std::uint32_t count, const PackedArray& shift)
    -> PackedSelection {
  const auto element_bw = OuterElementBitWidth(source_bit_width, source_dims);
  const auto low =
      RebaseToZeroBased(anchor, source_dims.front()) - Canonicalize(shift);
  return PackedSelection{
      .bit_offset = ScaledOuterOffset(low, element_bw),
      .dims = ReplaceOuterDimCount(source_dims, count)};
}

struct RawRangeSelector {
  PackedArray anchor;
  std::uint32_t count;
  PackedArray shift;
};

// Derive the (low-endpoint anchor, count, shift) the bit-level `ResolveSlice`
// consumes from a raw range selector `(a, b, form)` and the outer dim's
// orientation. A constant range `[l:r]` gives the oriented-low endpoint and the
// element count; an indexed part-select's base and width give the anchor and a
// direction-dependent shift (LRM 11.5.1). No coordinate is rebased here --
// `ResolveSlice` rebases in the value's own X/Z-aware domain.
auto ResolveRawRangeSelector(
    const PackedRange& outer, const PackedArray& a, const PackedArray& b,
    const PackedArray& form) -> RawRangeSelector {
  const bool descending = outer.left >= outer.right;
  if (static_cast<SliceForm>(form.ToInt64()) == SliceForm::kConstant) {
    const std::int64_t l = a.ToInt64();
    const std::int64_t r = b.ToInt64();
    const std::int64_t lo_endpoint = l < r ? l : r;
    const std::int64_t hi_endpoint = l < r ? r : l;
    const std::int64_t low = descending ? lo_endpoint : hi_endpoint;
    const auto count =
        static_cast<std::uint32_t>((hi_endpoint - lo_endpoint) + 1);
    return RawRangeSelector{
        .anchor = PackedArray::Int(static_cast<std::int32_t>(low)),
        .count = count,
        .shift = PackedArray::Int(0)};
  }
  const auto count = static_cast<std::uint32_t>(b.ToInt64());
  const bool extend_up = (static_cast<SliceForm>(form.ToInt64()) ==
                          SliceForm::kIndexedUp) == descending;
  const std::int64_t shift =
      extend_up ? 0 : static_cast<std::int64_t>(count) - 1;
  return RawRangeSelector{
      .anchor = a,
      .count = count,
      .shift = PackedArray::Int(static_cast<std::int32_t>(shift))};
}

}  // namespace

auto PackedArray::ElementRef(const PackedArray& idx) -> PackedArrayRef {
  auto sel = ResolveElement(type_.bit_width, type_.dims, idx);
  return PackedArrayRef{*this, sel.bit_offset, std::move(sel.dims)};
}

auto PackedArray::Element(const PackedArray& idx) const -> PackedArray {
  auto sel = ResolveElement(type_.bit_width, type_.dims, idx);
  return ExtractBits(sel.bit_offset, sel.dims);
}

auto PackedArray::SliceRef(
    const PackedArray& a, const PackedArray& b, const PackedArray& form)
    -> PackedArrayRef {
  const auto raw = ResolveRawRangeSelector(type_.dims.front(), a, b, form);
  auto sel = ResolveSlice(
      type_.bit_width, type_.dims, raw.anchor, raw.count, raw.shift);
  return PackedArrayRef{*this, sel.bit_offset, std::move(sel.dims)};
}

auto PackedArray::Slice(
    const PackedArray& a, const PackedArray& b, const PackedArray& form) const
    -> PackedArray {
  const auto raw = ResolveRawRangeSelector(type_.dims.front(), a, b, form);
  auto sel = ResolveSlice(
      type_.bit_width, type_.dims, raw.anchor, raw.count, raw.shift);
  return ExtractBits(sel.bit_offset, sel.dims);
}

PackedArrayRef::PackedArrayRef(
    PackedArray& root, const PackedArray& bit_offset,
    std::vector<PackedRange> dims)
    : root_(&root),
      bit_offset_(Canonicalize(bit_offset)),
      bit_width_(static_cast<std::uint32_t>(PackedType::WidthOf(dims))),
      dims_(std::move(dims)) {
}

auto PackedArrayRef::ToOwned() const -> PackedArray {
  return std::as_const(*root_).ExtractBits(bit_offset_, dims_);
}

auto PackedArrayRef::operator=(const PackedArray& value) -> PackedArrayRef& {
  root_->AssignSlice(bit_offset_, bit_width_, value);
  return *this;
}

auto PackedArrayRef::ElementRef(const PackedArray& idx) const
    -> PackedArrayRef {
  auto sel = ResolveElement(bit_width_, dims_, idx);
  return PackedArrayRef{
      *root_, bit_offset_ + sel.bit_offset, std::move(sel.dims)};
}

auto PackedArrayRef::SliceRef(
    const PackedArray& a, const PackedArray& b, const PackedArray& form) const
    -> PackedArrayRef {
  const auto raw = ResolveRawRangeSelector(dims_.front(), a, b, form);
  auto sel = ResolveSlice(bit_width_, dims_, raw.anchor, raw.count, raw.shift);
  return PackedArrayRef{
      *root_, bit_offset_ + sel.bit_offset, std::move(sel.dims)};
}

auto PackedArray::operator<(const PackedArray& other) const -> PackedArray {
  RequireSameStorageDomain(*this, other, "operator<");
  if (HasUnknown() || other.HasUnknown()) {
    return AllX(1U, false);
  }
  const int cmp =
      type_.is_signed
          ? SignedCompare(ValueWords(), other.ValueWords(), type_.bit_width)
          : UnsignedCompare(ValueWords(), other.ValueWords());
  return OneBitResult(cmp < 0, type_.is_four_state);
}

auto PackedArray::operator<=(const PackedArray& other) const -> PackedArray {
  RequireSameStorageDomain(*this, other, "operator<=");
  if (HasUnknown() || other.HasUnknown()) {
    return AllX(1U, false);
  }
  const int cmp =
      type_.is_signed
          ? SignedCompare(ValueWords(), other.ValueWords(), type_.bit_width)
          : UnsignedCompare(ValueWords(), other.ValueWords());
  return OneBitResult(cmp <= 0, type_.is_four_state);
}

auto PackedArray::operator>(const PackedArray& other) const -> PackedArray {
  RequireSameStorageDomain(*this, other, "operator>");
  if (HasUnknown() || other.HasUnknown()) {
    return AllX(1U, false);
  }
  const int cmp =
      type_.is_signed
          ? SignedCompare(ValueWords(), other.ValueWords(), type_.bit_width)
          : UnsignedCompare(ValueWords(), other.ValueWords());
  return OneBitResult(cmp > 0, type_.is_four_state);
}

auto PackedArray::operator>=(const PackedArray& other) const -> PackedArray {
  RequireSameStorageDomain(*this, other, "operator>=");
  if (HasUnknown() || other.HasUnknown()) {
    return AllX(1U, false);
  }
  const int cmp =
      type_.is_signed
          ? SignedCompare(ValueWords(), other.ValueWords(), type_.bit_width)
          : UnsignedCompare(ValueWords(), other.ValueWords());
  return OneBitResult(cmp >= 0, type_.is_four_state);
}

auto PackedArray::operator-() const -> PackedArray {
  if (HasUnknown()) {
    return AllX(type_.bit_width, type_.is_signed);
  }
  auto buf = MakeWordBuffer(type_.bit_width);
  SubWordsInto({}, ValueWords(), buf, type_.bit_width);
  return MakeFromWordPlanes(
      type_.bit_width, type_.is_signed, type_.is_four_state,
      std::span<const std::uint64_t>{buf.data(), buf.size()}, {});
}

auto PackedArray::operator~() const -> PackedArray {
  if (type_.bit_width <= 64U && !type_.is_four_state) {
    const auto mask = MaskForWidth(type_.bit_width);
    return FromInt(
        static_cast<std::int64_t>((~NarrowWordOf(*this)) & mask),
        type_.bit_width, type_.is_signed, false);
  }
  PackedArray result{type_.bit_width, type_.is_signed, type_.is_four_state};
  if (type_.is_four_state) {
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
  const bool result_four_state =
      type_.is_four_state || other.type_.is_four_state;
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
  const bool result_four_state =
      type_.is_four_state || other.type_.is_four_state;
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
      return OneBitResult(true, type_.is_four_state);
    case Truthiness::kKnownNonzero:
      return OneBitResult(false, type_.is_four_state);
    case Truthiness::kUnknown:
      return AllX(1U, false);
  }
  throw InternalError("operator!: unhandled Truthiness");
}

auto PackedArray::LogicalImplication(const PackedArray& other) const
    -> PackedArray {
  const auto a = TruthinessOf(*this);
  const auto b = TruthinessOf(other);
  const bool result_four_state =
      type_.is_four_state || other.type_.is_four_state;
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
  const bool result_four_state =
      type_.is_four_state || other.type_.is_four_state;
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
    return AllX(type_.bit_width, type_.is_signed);
  }
  const auto amt = ShiftAmountAsUint(amount);
  auto value_buf = MakeWordBuffer(type_.bit_width);
  ShiftLeftWordsInto(ValueWords(), amt, value_buf, type_.bit_width);
  if (!type_.is_four_state) {
    return MakeFromWordPlanes(
        type_.bit_width, type_.is_signed, false,
        std::span<const std::uint64_t>{value_buf.data(), value_buf.size()}, {});
  }
  auto unk_buf = MakeWordBuffer(type_.bit_width);
  ShiftLeftWordsInto(UnknownWords(), amt, unk_buf, type_.bit_width);
  return MakeFromWordPlanes(
      type_.bit_width, type_.is_signed, true,
      std::span<const std::uint64_t>{value_buf.data(), value_buf.size()},
      std::span<const std::uint64_t>{unk_buf.data(), unk_buf.size()});
}

auto PackedArray::LogicalShiftRight(const PackedArray& amount) const
    -> PackedArray {
  if (amount.HasUnknown()) {
    return AllX(type_.bit_width, type_.is_signed);
  }
  const auto amt = ShiftAmountAsUint(amount);
  auto value_buf = MakeWordBuffer(type_.bit_width);
  LogicalShiftRightWordsInto(ValueWords(), amt, value_buf, type_.bit_width);
  if (!type_.is_four_state) {
    return MakeFromWordPlanes(
        type_.bit_width, type_.is_signed, false,
        std::span<const std::uint64_t>{value_buf.data(), value_buf.size()}, {});
  }
  auto unk_buf = MakeWordBuffer(type_.bit_width);
  LogicalShiftRightWordsInto(UnknownWords(), amt, unk_buf, type_.bit_width);
  return MakeFromWordPlanes(
      type_.bit_width, type_.is_signed, true,
      std::span<const std::uint64_t>{value_buf.data(), value_buf.size()},
      std::span<const std::uint64_t>{unk_buf.data(), unk_buf.size()});
}

auto PackedArray::ArithmeticShiftRight(const PackedArray& amount) const
    -> PackedArray {
  if (!type_.is_signed) {
    return LogicalShiftRight(amount);
  }
  if (amount.HasUnknown()) {
    return AllX(type_.bit_width, type_.is_signed);
  }
  const auto amt = ShiftAmountAsUint(amount);
  // Each plane shifts with its own MSB as the fill, so an X at the top
  // extends down into the new top bits instead of degenerating to the
  // value-plane sign.
  const std::uint64_t value_sign = BitAt(ValueWords(), type_.bit_width - 1U);
  auto value_buf = MakeWordBuffer(type_.bit_width);
  LogicalShiftRightWordsInto(ValueWords(), amt, value_buf, type_.bit_width);
  if (value_sign != 0U) {
    FillTopBits(value_buf, type_.bit_width, std::min(amt, type_.bit_width));
  }
  if (!type_.is_four_state) {
    return MakeFromWordPlanes(
        type_.bit_width, true, false,
        std::span<const std::uint64_t>{value_buf.data(), value_buf.size()}, {});
  }
  const std::uint64_t unk_sign = BitAt(UnknownWords(), type_.bit_width - 1U);
  auto unk_buf = MakeWordBuffer(type_.bit_width);
  LogicalShiftRightWordsInto(UnknownWords(), amt, unk_buf, type_.bit_width);
  if (unk_sign != 0U) {
    FillTopBits(unk_buf, type_.bit_width, std::min(amt, type_.bit_width));
  }
  return MakeFromWordPlanes(
      type_.bit_width, true, true,
      std::span<const std::uint64_t>{value_buf.data(), value_buf.size()},
      std::span<const std::uint64_t>{unk_buf.data(), unk_buf.size()});
}

auto PackedArray::Pow(const PackedArray& exponent) const -> PackedArray {
  // The exponent is checked before the base so that `X ** 0 = 1` wins
  // over X-propagation from the base (LRM 11.4.10).
  if (exponent.HasUnknown()) {
    return AllX(type_.bit_width, type_.is_signed);
  }
  const auto exp_words = exponent.ValueWords();
  for (std::size_t i = 1; i < exp_words.size(); ++i) {
    if (exp_words[i] != 0U && exp_words[i] != ~std::uint64_t{0}) {
      throw InternalError(
          "PackedArray::Pow: exponent magnitude exceeds 64 bits");
    }
  }
  const std::uint64_t exp_low = exp_words.empty() ? 0U : exp_words[0];
  const std::int64_t exp = exponent.IsSigned()
                               ? SignExtendToInt64(exp_low, exponent.BitWidth())
                               : static_cast<std::int64_t>(exp_low);
  if (exp == 0) {
    return FromInt(1, type_.bit_width, type_.is_signed, type_.is_four_state);
  }
  if (HasUnknown()) {
    return AllX(type_.bit_width, type_.is_signed);
  }
  if (exp < 0) {
    // Negative exp on integer base rounds to zero except for base == +-1.
    const auto one =
        FromInt(1, type_.bit_width, type_.is_signed, type_.is_four_state);
    if (UnsignedCompare(ValueWords(), one.ValueWords()) == 0) {
      return one;
    }
    if (type_.is_signed) {
      const auto neg_one =
          FromInt(-1, type_.bit_width, true, type_.is_four_state);
      if (UnsignedCompare(ValueWords(), neg_one.ValueWords()) == 0) {
        return FromInt(
            (exp & 1) != 0 ? -1 : 1, type_.bit_width, type_.is_signed,
            type_.is_four_state);
      }
    }
    return FromInt(0, type_.bit_width, type_.is_signed, type_.is_four_state);
  }
  PackedArray result =
      FromInt(1, type_.bit_width, type_.is_signed, type_.is_four_state);
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
  if (type_.bit_width <= 64U && !type_.is_four_state) {
    return OneBitResult(
        NarrowWordOf(*this) == MaskForWidth(type_.bit_width),
        type_.is_four_state);
  }
  PackedArray result{1U, false, type_.is_four_state};
  if (type_.is_four_state) {
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
  if (type_.bit_width <= 64U && !type_.is_four_state) {
    return OneBitResult(NarrowWordOf(*this) != 0U, type_.is_four_state);
  }
  PackedArray result{1U, false, type_.is_four_state};
  if (type_.is_four_state) {
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
  if (type_.bit_width <= 64U && !type_.is_four_state) {
    return OneBitResult(
        (std::popcount(NarrowWordOf(*this)) & 1) != 0, type_.is_four_state);
  }
  PackedArray result{1U, false, type_.is_four_state};
  if (type_.is_four_state) {
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
  if (type_.bit_width <= 64U && !type_.is_four_state) {
    return OneBitResult(
        NarrowWordOf(*this) != MaskForWidth(type_.bit_width),
        type_.is_four_state);
  }
  PackedArray result{1U, false, type_.is_four_state};
  if (type_.is_four_state) {
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
  if (type_.bit_width <= 64U && !type_.is_four_state) {
    return OneBitResult(NarrowWordOf(*this) == 0U, type_.is_four_state);
  }
  PackedArray result{1U, false, type_.is_four_state};
  if (type_.is_four_state) {
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
  if (type_.bit_width <= 64U && !type_.is_four_state) {
    return OneBitResult(
        (std::popcount(NarrowWordOf(*this)) & 1) == 0, type_.is_four_state);
  }
  PackedArray result{1U, false, type_.is_four_state};
  if (type_.is_four_state) {
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
