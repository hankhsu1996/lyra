#pragma once

#include <cstdint>
#include <vector>

#include "lyra/value/packed_array.hpp"

// The canonical DPI-C representation of packed values (LRM Annex H.10.1.2): the
// C ABI a packed SV value crosses the foreign boundary as. The layout is fixed
// by the standard and matches svdpi.h, so an emitted `extern "C"` declaration
// and a user's svdpi.h agree at the ABI level. A `svBitVecVal` chunk is 32 bits
// of a 2-state value; a `svLogicVecVal` chunk is 32 bits of a 4-state value,
// with `aval` the value plane and `bval` the unknown (X/Z) plane -- exactly
// Lyra's internal two-plane representation, split into 32-bit chunks.
#ifndef LYRA_SV_CANONICAL_DEFINED
#define LYRA_SV_CANONICAL_DEFINED
using svBitVecVal = std::uint32_t;
struct svLogicVecVal {
  std::uint32_t aval;
  std::uint32_t bval;
};
#endif

namespace lyra::value {

// A 1-bit 4-state value's `svLogic` scalar encoding (LRM Annex H.10.1.1):
// `value | unknown << 1`, i.e. sv_0=0, sv_1=1, sv_z=2, sv_x=3.
[[nodiscard]] auto ToSvLogic(const PackedArray& sv) -> unsigned char;

// Builds a 1-bit 4-state value from its `svLogic` scalar encoding, in the
// prototype's shape.
[[nodiscard]] auto FromSvLogic(
    unsigned char encoded, const PackedArray& prototype) -> PackedArray;

// A DPI boundary buffer: a canonical `ceil(width / 32)`-chunk vector the
// foreign side reads and writes by pointer. It is an ABI temporary, not an SV
// value -- it exists only inside one foreign-call lowering window. It sizes
// itself from the SV value it is constructed from (which fills it, a plane
// reshape with no per-bit transcode) and exposes a writable chunk pointer for
// the foreign call and the copy-back read; the width never has to be named
// separately. `Bit` carries the value plane only (2-state); `Logic` carries
// both planes (4-state, `aval` the value plane, `bval` the unknown plane).
class DpiBitBuffer {
 public:
  explicit DpiBitBuffer(const PackedArray& sv);
  [[nodiscard]] auto Data() -> svBitVecVal*;

 private:
  std::vector<svBitVecVal> chunks_;
};

class DpiLogicBuffer {
 public:
  explicit DpiLogicBuffer(const PackedArray& sv);
  [[nodiscard]] auto Data() -> svLogicVecVal*;

 private:
  std::vector<svLogicVecVal> chunks_;
};

// Build an SV value from a canonical buffer in the prototype's shape (width,
// signedness). `Bit` reads a 2-state buffer (unknown plane all zero); `Logic`
// reads both planes.
[[nodiscard]] auto ReadCanonicalBitVec(
    const svBitVecVal* src, const PackedArray& prototype) -> PackedArray;
[[nodiscard]] auto ReadCanonicalLogicVec(
    const svLogicVecVal* src, const PackedArray& prototype) -> PackedArray;

}  // namespace lyra::value
