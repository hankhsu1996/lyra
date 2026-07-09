#pragma once

#include <cstdint>

namespace lyra::support {

// The C ABI category an SV type crosses the DPI-C boundary as (LRM 35.5.6).
// One value per distinct foreign-carrier shape, shared by HIR, MIR, and LIR so
// no layer re-derives the classification from a type's properties. The SV type
// is classified into this category once at AST-to-HIR, where slang types are
// available; every layer below reads the category.
//
// The set covers 2-state integral scalars up to one machine word, real, string,
// and void; four-state, wide packed, chandle, and open-array carriers are not
// yet represented.
enum class DpiAbiClass : std::uint8_t {
  kVoid,
  kBit,
  kByte,
  kShortInt,
  kInt,
  kLongInt,
  kReal,
  kString,
};

}  // namespace lyra::support
