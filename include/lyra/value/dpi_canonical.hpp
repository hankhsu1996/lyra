#pragma once

#include <cstddef>
#include <cstdint>
#include <span>
#include <utility>
#include <vector>

#include "lyra/value/integral_fwd.hpp"
#include "lyra/value/integral_words.hpp"

// The DPI-C ABI names an emitted artifact needs, spelled here so it compiles
// without the standard header while still agreeing with a user's svdpi.h at the
// ABI level. The canonical representation of packed values (LRM Annex H.7.7 /
// H.10.1.2) is a sequence of 32-bit groups: a `svBitVecVal` group is 32 bits of
// a 2-state value; a `svLogicVecVal` group is 32 bits of a 4-state value, with
// `aval` the value plane and `bval` the unknown (X/Z) plane -- the two planes a
// value is held as, split into 32-bit groups. An open array crosses as an
// opaque handle instead (LRM Annex H.8.6).
using svBitVecVal = std::uint32_t;
struct svLogicVecVal {
  std::uint32_t aval;
  std::uint32_t bval;
};
using svOpenArrayHandle = void*;

namespace lyra::value {

// The 32-bit groups a value of `width` bits takes in canonical form (LRM Annex
// H.7.7).
[[nodiscard]] constexpr auto CanonicalGroups(std::uint64_t width)
    -> std::size_t {
  return static_cast<std::size_t>((width + 31U) / 32U);
}

namespace detail {

// Group `group` of a plane: groups 2k and 2k+1 are the low and the high half
// of word k.
[[nodiscard]] constexpr auto CanonicalGroupOf(
    ConstWordSpan auto plane, std::size_t group) -> std::uint32_t {
  return static_cast<std::uint32_t>(BitsAt(plane, 32U * group, 32U));
}

}  // namespace detail

// A 1-bit value as an `svLogic` (LRM Annex H.10.1.1), which is the number its
// scalar is.
[[nodiscard]] constexpr auto ToSvLogic(AnyConstIntegralView auto sv)
    -> unsigned char {
  return std::to_underlying(LeastSignificantBit(sv));
}
template <IntegralValue T>
[[nodiscard]] constexpr auto ToSvLogic(const T& sv) -> unsigned char {
  return ToSvLogic(sv.View());
}

// A canonical vector read into a value. A two-state value has no group to
// read an unknown plane from, and holds each x or z a four-state vector
// carries as 0. The bits a vector holds above the value's width are
// undetermined and are not read.
template <
    PlaneWord WrittenWord, std::size_t kWrittenValueWords,
    std::size_t kWrittenUnknownWords>
constexpr void ReadCanonicalBitVec(
    const svBitVecVal* src,
    IntegralViewOf<WrittenWord, kWrittenValueWords, kWrittenUnknownWords> out) {
  detail::Fill(out.value, 0U);
  detail::Fill(out.unknown, 0U);
  const std::span<const svBitVecVal> groups{src, CanonicalGroups(out.width)};
  for (std::size_t group = 0; group < groups.size(); ++group) {
    const std::size_t word = group / 2U;
    SetWord(
        out.value, word,
        WordAt(out.value, word) | (static_cast<std::uint64_t>(groups[group])
                                   << (32U * (group % 2U))));
  }
  ClearAboveWidth(out.value, out.width);
}

// The groups of a four-state canonical vector read into a value, which reads
// 0 where it is wider than the groups given.
template <
    PlaneWord WrittenWord, std::size_t kWrittenValueWords,
    std::size_t kWrittenUnknownWords>
constexpr void ReadCanonicalLogicGroups(
    std::span<const svLogicVecVal> groups,
    IntegralViewOf<WrittenWord, kWrittenValueWords, kWrittenUnknownWords> out) {
  for (std::size_t i = 0; i < out.value.size(); ++i) {
    std::uint64_t value = 0;
    std::uint64_t unknown = 0;
    for (std::size_t half = 0; half < 2U; ++half) {
      const std::size_t group = (2U * i) + half;
      if (group < groups.size()) {
        value |= static_cast<std::uint64_t>(groups[group].aval) << (32U * half);
        unknown |= static_cast<std::uint64_t>(groups[group].bval)
                   << (32U * half);
      }
    }
    const std::uint64_t valid = ValidBitsMask(i, out.width);
    if (out.unknown.empty()) {
      SetWord(out.value, i, value & ~unknown & valid);
    } else {
      SetWord(out.value, i, value & valid);
      SetWord(out.unknown, i, unknown & valid);
    }
  }
}

template <
    PlaneWord WrittenWord, std::size_t kWrittenValueWords,
    std::size_t kWrittenUnknownWords>
constexpr void ReadCanonicalLogicVec(
    const svLogicVecVal* src,
    IntegralViewOf<WrittenWord, kWrittenValueWords, kWrittenUnknownWords> out) {
  ReadCanonicalLogicGroups(
      std::span<const svLogicVecVal>{src, CanonicalGroups(out.width)}, out);
}

template <IntegralValue R>
[[nodiscard]] constexpr auto ReadCanonicalBitVec(const svBitVecVal* src) -> R {
  R out = R::Zero();
  ReadCanonicalBitVec(src, out.MutableView());
  return out;
}
template <IntegralValue R>
[[nodiscard]] constexpr auto ReadCanonicalLogicVec(const svLogicVecVal* src)
    -> R {
  R out = R::Zero();
  ReadCanonicalLogicVec(src, out.MutableView());
  return out;
}

// A 1-bit value read from its `svLogic` scalar encoding, as the canonical
// vector of one group that encoding is.
template <
    PlaneWord WrittenWord, std::size_t kWrittenValueWords,
    std::size_t kWrittenUnknownWords>
constexpr void FromSvLogic(
    unsigned char encoded,
    IntegralViewOf<WrittenWord, kWrittenValueWords, kWrittenUnknownWords> out) {
  const svLogicVecVal group{
      .aval = (encoded >> kScalarValueBit) & 1U,
      .bval = (encoded >> kScalarUnknownBit) & 1U};
  ReadCanonicalLogicGroups(std::span<const svLogicVecVal>{&group, 1U}, out);
}
template <IntegralValue R>
[[nodiscard]] constexpr auto FromSvLogic(unsigned char encoded) -> R {
  R out = R::Zero();
  FromSvLogic(encoded, out.MutableView());
  return out;
}

// A value written into a canonical vector at `dst`, which is either a
// boundary buffer or a foreign caller's own storage. A two-state vector has
// no group to carry an unknown plane, so an x or z bit of a four-state value
// is written as 0.
constexpr void WriteCanonicalBitVec(
    svBitVecVal* dst, AnyConstIntegralView auto sv) {
  const std::span<svBitVecVal> groups{dst, CanonicalGroups(sv.width)};
  for (std::size_t group = 0; group < groups.size(); ++group) {
    groups[group] = detail::CanonicalGroupOf(sv.value, group) &
                    ~detail::CanonicalGroupOf(sv.unknown, group);
  }
}
constexpr void WriteCanonicalLogicVec(
    svLogicVecVal* dst, AnyConstIntegralView auto sv) {
  const std::span<svLogicVecVal> groups{dst, CanonicalGroups(sv.width)};
  for (std::size_t group = 0; group < groups.size(); ++group) {
    groups[group] = svLogicVecVal{
        .aval = detail::CanonicalGroupOf(sv.value, group),
        .bval = detail::CanonicalGroupOf(sv.unknown, group)};
  }
}

template <IntegralValue T>
constexpr void WriteCanonicalBitVec(svBitVecVal* dst, const T& sv) {
  WriteCanonicalBitVec(dst, sv.View());
}
template <IntegralValue T>
constexpr void WriteCanonicalLogicVec(svLogicVecVal* dst, const T& sv) {
  WriteCanonicalLogicVec(dst, sv.View());
}

// A DPI boundary buffer: a canonical vector the foreign side reads and writes
// by pointer. It is an ABI temporary, not an SV value -- it exists only inside
// one foreign-call lowering window. It sizes itself from the SV value it is
// constructed from, which fills it, and exposes a writable group pointer for
// the foreign call and the copy-back read.
class DpiBitBuffer {
 public:
  explicit DpiBitBuffer(const ConstIntegralView& sv);
  template <IntegralValue T>
  explicit DpiBitBuffer(const T& sv) : DpiBitBuffer(sv.AsWords().View()) {
  }
  [[nodiscard]] auto Data() -> svBitVecVal*;

 private:
  std::vector<svBitVecVal> groups_;
};

class DpiLogicBuffer {
 public:
  explicit DpiLogicBuffer(const ConstIntegralView& sv);
  template <IntegralValue T>
  explicit DpiLogicBuffer(const T& sv) : DpiLogicBuffer(sv.AsWords().View()) {
  }
  [[nodiscard]] auto Data() -> svLogicVecVal*;

 private:
  std::vector<svLogicVecVal> groups_;
};

}  // namespace lyra::value
