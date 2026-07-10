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
// chandle, and void; four-state, wide packed, and open-array carriers are not
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
  kChandle,
};

// The direction of a DPI-C formal argument (LRM 35.5.1.2). `ref` is illegal in
// import declarations, so the set is exactly input / output / inout. The
// direction decides the boundary plumbing -- input crosses by value, output and
// inout cross by pointer with a copy back -- not the carrier class, which is
// direction-independent. Shared by HIR and MIR so no layer re-derives it.
enum class DpiDirection : std::uint8_t {
  kInput,
  kOutput,
  kInout,
};

// Whether the direction writes the actual back after the call: output and inout
// copy the foreign-written carrier back into the SV actual, input does not.
[[nodiscard]] constexpr auto DpiDirectionWritesBack(DpiDirection dir) -> bool {
  return dir != DpiDirection::kInput;
}

// The SV keyword a direction corresponds to, for dumps.
[[nodiscard]] constexpr auto DpiDirectionName(DpiDirection dir)
    -> std::string_view {
  switch (dir) {
    case DpiDirection::kInput:
      return "input";
    case DpiDirection::kOutput:
      return "output";
    case DpiDirection::kInout:
      return "inout";
  }
  return "unknown";
}

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
    case DpiAbiClass::kChandle:
      return "chandle";
  }
  return "unknown";
}

}  // namespace lyra::support
