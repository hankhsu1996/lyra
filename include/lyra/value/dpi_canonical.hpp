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
#ifndef LYRA_SV_CANONICAL_DEFINED
#define LYRA_SV_CANONICAL_DEFINED
using svBitVecVal = std::uint32_t;
struct svLogicVecVal {
  std::uint32_t aval;
  std::uint32_t bval;
};
using svOpenArrayHandle = void*;
#endif

namespace lyra::value {

// The 32-bit groups a value of `width` bits takes in canonical form (LRM Annex
// H.7.7).
[[nodiscard]] LYRA_FOLDED constexpr auto CanonicalGroups(std::uint64_t width)
    -> std::size_t {
  return static_cast<std::size_t>((width + 31U) / 32U);
}

namespace detail {

// Group `group` of a plane: groups 2k and 2k+1 are the low and the high half
// of word k.
[[nodiscard]] LYRA_FOLDED constexpr auto CanonicalGroupOf(
    std::span<const std::uint64_t> plane, std::size_t group) -> std::uint32_t {
  return static_cast<std::uint32_t>(BitsAt(plane, 32U * group, 32U));
}

}  // namespace detail

// A 1-bit value as an `svLogic` (LRM Annex H.10.1.1), which is the number its
// scalar is.
[[nodiscard]] LYRA_FOLDED constexpr auto ToSvLogic(ConstPlanes sv)
    -> unsigned char {
  return std::to_underlying(LeastSignificantBit(sv));
}
template <IntegralValue T>
[[nodiscard]] LYRA_FOLDED constexpr auto ToSvLogic(const T& sv)
    -> unsigned char {
  return ToSvLogic(sv.Load().Read());
}

// A canonical vector read into the planes of a value of `width` bits. A
// two-state value has no group to read an unknown plane from, and holds each
// x or z a four-state vector carries as 0. The bits a vector holds above the
// width are undetermined and are not read.
LYRA_FOLDED constexpr void ReadCanonicalBitVec(
    const svBitVecVal* src, Planes out, std::uint64_t width) {
  for (std::uint64_t& word : out.value) {
    word = 0U;
  }
  for (std::uint64_t& word : out.unknown) {
    word = 0U;
  }
  const std::span<const svBitVecVal> groups{src, CanonicalGroups(width)};
  for (std::size_t group = 0; group < groups.size(); ++group) {
    out.value[group / 2U] |= static_cast<std::uint64_t>(groups[group])
                             << (32U * (group % 2U));
  }
  ClearAboveWidth(out.value, width);
}

LYRA_FOLDED constexpr void ReadCanonicalLogicVec(
    const svLogicVecVal* src, Planes out, std::uint64_t width) {
  const std::span<const svLogicVecVal> groups{src, CanonicalGroups(width)};
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
    const std::uint64_t valid = ValidBitsMask(i, width);
    if (out.unknown.empty()) {
      out.value[i] = value & ~unknown & valid;
    } else {
      out.value[i] = value & valid;
      out.unknown[i] = unknown & valid;
    }
  }
}

template <IntegralValue R>
[[nodiscard]] LYRA_FOLDED constexpr auto ReadCanonicalBitVec(
    const svBitVecVal* src) -> R {
  typename R::Words built;
  ReadCanonicalBitVec(src, built.Write(), R::kWidth);
  return R::FromWords(built);
}
template <IntegralValue R>
[[nodiscard]] LYRA_FOLDED constexpr auto ReadCanonicalLogicVec(
    const svLogicVecVal* src) -> R {
  typename R::Words built;
  ReadCanonicalLogicVec(src, built.Write(), R::kWidth);
  return R::FromWords(built);
}

// A 1-bit value read from its `svLogic` scalar encoding, as the canonical
// vector of one group that encoding is.
LYRA_FOLDED constexpr void FromSvLogic(unsigned char encoded, Planes out) {
  const svLogicVecVal group{
      .aval = (encoded >> kScalarValueBit) & 1U,
      .bval = (encoded >> kScalarUnknownBit) & 1U};
  ReadCanonicalLogicVec(&group, out, 1);
}
template <IntegralValue R>
[[nodiscard]] LYRA_FOLDED constexpr auto FromSvLogic(unsigned char encoded)
    -> R {
  typename R::Words built;
  FromSvLogic(encoded, built.Write());
  return R::FromWords(built);
}

// A value of `width` bits written into a canonical vector at `dst`, which is
// either a boundary buffer or a foreign caller's own storage. A two-state
// vector has no group to carry an unknown plane, so an x or z bit of a
// four-state value is written as 0.
LYRA_FOLDED constexpr void WriteCanonicalBitVec(
    svBitVecVal* dst, ConstPlanes sv, std::uint64_t width) {
  const std::span<svBitVecVal> groups{dst, CanonicalGroups(width)};
  for (std::size_t group = 0; group < groups.size(); ++group) {
    groups[group] = detail::CanonicalGroupOf(sv.value, group) &
                    ~detail::CanonicalGroupOf(sv.unknown, group);
  }
}
LYRA_FOLDED constexpr void WriteCanonicalLogicVec(
    svLogicVecVal* dst, ConstPlanes sv, std::uint64_t width) {
  const std::span<svLogicVecVal> groups{dst, CanonicalGroups(width)};
  for (std::size_t group = 0; group < groups.size(); ++group) {
    groups[group] = svLogicVecVal{
        .aval = detail::CanonicalGroupOf(sv.value, group),
        .bval = detail::CanonicalGroupOf(sv.unknown, group)};
  }
}

template <IntegralValue T>
LYRA_FOLDED constexpr void WriteCanonicalBitVec(svBitVecVal* dst, const T& sv) {
  WriteCanonicalBitVec(dst, sv.Load().Read(), T::kWidth);
}
template <IntegralValue T>
LYRA_FOLDED constexpr void WriteCanonicalLogicVec(
    svLogicVecVal* dst, const T& sv) {
  WriteCanonicalLogicVec(dst, sv.Load().Read(), T::kWidth);
}

// A DPI boundary buffer: a canonical vector the foreign side reads and writes
// by pointer. It is an ABI temporary, not an SV value -- it exists only inside
// one foreign-call lowering window. It sizes itself from the SV value it is
// constructed from, which fills it, and exposes a writable group pointer for
// the foreign call and the copy-back read.
class DpiBitBuffer {
 public:
  DpiBitBuffer(ConstPlanes sv, std::uint64_t width);
  template <IntegralValue T>
  explicit DpiBitBuffer(const T& sv)
      : DpiBitBuffer(sv.Load().Read(), T::kWidth) {
  }
  [[nodiscard]] auto Data() -> svBitVecVal*;

 private:
  std::vector<svBitVecVal> groups_;
};

class DpiLogicBuffer {
 public:
  DpiLogicBuffer(ConstPlanes sv, std::uint64_t width);
  template <IntegralValue T>
  explicit DpiLogicBuffer(const T& sv)
      : DpiLogicBuffer(sv.Load().Read(), T::kWidth) {
  }
  [[nodiscard]] auto Data() -> svLogicVecVal*;

 private:
  std::vector<svLogicVecVal> groups_;
};

}  // namespace lyra::value
