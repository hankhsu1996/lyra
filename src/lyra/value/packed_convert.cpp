#include "lyra/value/packed_convert.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <span>
#include <string_view>

#include "lyra/value/packed.hpp"
#include "lyra/value/packed_internal.hpp"

namespace lyra::value {
namespace {

// A read that is total over the destination's word range. Past the source's
// last word no bit of what this answers is kept, so making it answerable at all
// is what lets one loop cover a destination wider than its source.
auto WordAt(std::span<const std::uint64_t> words, std::size_t index)
    -> std::uint64_t {
  return index < words.size() ? words[index] : std::uint64_t{0};
}

auto PaddingWord(bool set) -> std::uint64_t {
  return set ? ~std::uint64_t{0} : std::uint64_t{0};
}

// One destination plane: the source's own bits where the source reaches, the
// padding bit everywhere above. Every destination word is written, so nothing
// it holds beforehand is read.
auto WritePlane(
    std::span<const std::uint64_t> src, std::span<std::uint64_t> dst,
    std::uint64_t copy_width, std::uint64_t padding, std::uint64_t dst_width)
    -> void {
  for (std::size_t i = 0; i < dst.size(); ++i) {
    const std::uint64_t reached = ValidBitsMask(i, copy_width);
    dst[i] = (WordAt(src, i) & reached) | (padding & ~reached);
  }
  MaskUnusedTopBits(dst, dst_width);
}

// Everything else pads with zero -- an unsigned source and a narrowing one
// alike (LRM 6.11.2, 6.11.3).
auto PadsFromTheSignBit(
    std::uint64_t src_width, std::uint64_t dst_width, Signedness signedness)
    -> bool {
  return dst_width > src_width && signedness == Signedness::kSigned;
}

}  // namespace

auto ConvertToBit(ConstBitView src, BitView dst, Signedness src_signedness)
    -> void {
  constexpr std::string_view kWhere = "ConvertToBit(Bit)";
  detail::RequireAligned(kWhere, detail::PackedAccess::BitOffset(src));
  detail::RequireAligned(kWhere, detail::PackedAccess::BitOffset(dst));
  const auto src_words = detail::PackedAccess::ValueWords(src);
  const auto dst_words = detail::PackedAccess::ValueWords(dst);
  detail::RequireWordCount(kWhere, src_words, src.Width());
  detail::RequireWordCount(kWhere, dst_words, dst.Width());

  const bool pad_ones =
      PadsFromTheSignBit(src.Width(), dst.Width(), src_signedness) &&
      BitAt(src_words, src.Width() - 1U);
  WritePlane(
      src_words, dst_words, std::min(src.Width(), dst.Width()),
      PaddingWord(pad_ones), dst.Width());
}

auto ConvertToBit(ConstLogicView src, BitView dst, Signedness src_signedness)
    -> void {
  constexpr std::string_view kWhere = "ConvertToBit(Logic)";
  detail::RequireAligned(kWhere, detail::PackedAccess::BitOffset(src));
  detail::RequireAligned(kWhere, detail::PackedAccess::BitOffset(dst));
  const auto src_value = detail::PackedAccess::ValueWords(src);
  const auto src_unknown = detail::PackedAccess::UnknownWords(src);
  const auto dst_words = detail::PackedAccess::ValueWords(dst);
  detail::RequireWordCount(kWhere, src_value, src.Width());
  detail::RequireWordCount(kWhere, src_unknown, src.Width());
  detail::RequireWordCount(kWhere, dst_words, dst.Width());

  const std::uint64_t sign = src.Width() - 1U;
  // A sign bit that is unknown or high-impedance is not a one, so it pads with
  // the zero it itself becomes here (LRM 6.11.2).
  const bool pad_ones =
      PadsFromTheSignBit(src.Width(), dst.Width(), src_signedness) &&
      BitAt(src_value, sign) && !BitAt(src_unknown, sign);
  const std::uint64_t padding = PaddingWord(pad_ones);

  // A two-state destination has one plane and takes the source's known bits,
  // so this is the one conversion that reads two source planes into one.
  const std::uint64_t copy_width = std::min(src.Width(), dst.Width());
  for (std::size_t i = 0; i < dst_words.size(); ++i) {
    const std::uint64_t reached = ValidBitsMask(i, copy_width);
    const std::uint64_t known = WordAt(src_value, i) & ~WordAt(src_unknown, i);
    dst_words[i] = (known & reached) | (padding & ~reached);
  }
  MaskUnusedTopBits(dst_words, dst.Width());
}

auto ConvertToLogic(ConstBitView src, LogicView dst, Signedness src_signedness)
    -> void {
  constexpr std::string_view kWhere = "ConvertToLogic(Bit)";
  detail::RequireAligned(kWhere, detail::PackedAccess::BitOffset(src));
  detail::RequireAligned(kWhere, detail::PackedAccess::BitOffset(dst));
  const auto src_words = detail::PackedAccess::ValueWords(src);
  const auto dst_value = detail::PackedAccess::ValueWords(dst);
  const auto dst_unknown = detail::PackedAccess::UnknownWords(dst);
  detail::RequireWordCount(kWhere, src_words, src.Width());
  detail::RequireWordCount(kWhere, dst_value, dst.Width());
  detail::RequireWordCount(kWhere, dst_unknown, dst.Width());

  const bool pad_ones =
      PadsFromTheSignBit(src.Width(), dst.Width(), src_signedness) &&
      BitAt(src_words, src.Width() - 1U);
  WritePlane(
      src_words, dst_value, std::min(src.Width(), dst.Width()),
      PaddingWord(pad_ones), dst.Width());
  std::ranges::fill(dst_unknown, std::uint64_t{0});
}

auto ConvertToLogic(
    ConstLogicView src, LogicView dst, Signedness src_signedness) -> void {
  constexpr std::string_view kWhere = "ConvertToLogic(Logic)";
  detail::RequireAligned(kWhere, detail::PackedAccess::BitOffset(src));
  detail::RequireAligned(kWhere, detail::PackedAccess::BitOffset(dst));
  const auto src_value = detail::PackedAccess::ValueWords(src);
  const auto src_unknown = detail::PackedAccess::UnknownWords(src);
  const auto dst_value = detail::PackedAccess::ValueWords(dst);
  const auto dst_unknown = detail::PackedAccess::UnknownWords(dst);
  detail::RequireWordCount(kWhere, src_value, src.Width());
  detail::RequireWordCount(kWhere, src_unknown, src.Width());
  detail::RequireWordCount(kWhere, dst_value, dst.Width());
  detail::RequireWordCount(kWhere, dst_unknown, dst.Width());

  const std::uint64_t sign = src.Width() - 1U;
  // Both planes pad with the sign bit's own two halves, which is what makes a
  // sign bit of x or z fill with x or z (LRM 6.11.3).
  const bool from_sign =
      PadsFromTheSignBit(src.Width(), dst.Width(), src_signedness);
  const std::uint64_t copy_width = std::min(src.Width(), dst.Width());
  WritePlane(
      src_value, dst_value, copy_width,
      PaddingWord(from_sign && BitAt(src_value, sign)), dst.Width());
  WritePlane(
      src_unknown, dst_unknown, copy_width,
      PaddingWord(from_sign && BitAt(src_unknown, sign)), dst.Width());
}

}  // namespace lyra::value
