#pragma once

#include <cstdint>
#include <string_view>

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

// The SV keyword an ABI category corresponds to. The shared human-readable
// spelling every HIR and MIR dump names the category by, so the two dumps
// agree without each restating the mapping.
[[nodiscard]] constexpr auto DpiAbiClassName(DpiAbiClass abi)
    -> std::string_view {
  switch (abi) {
    case DpiAbiClass::kVoid:
      return "void";
    case DpiAbiClass::kBit:
      return "bit";
    case DpiAbiClass::kByte:
      return "byte";
    case DpiAbiClass::kShortInt:
      return "shortint";
    case DpiAbiClass::kInt:
      return "int";
    case DpiAbiClass::kLongInt:
      return "longint";
    case DpiAbiClass::kReal:
      return "real";
    case DpiAbiClass::kString:
      return "string";
  }
  return "unknown";
}

}  // namespace lyra::support
