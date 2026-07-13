#pragma once

#include <cstdint>
#include <string>
#include <string_view>
#include <variant>

namespace lyra::support {

// The C ABI category a scalar SV value crosses the DPI-C boundary as, by value
// (LRM 35.5.6, Table H.1). One value per distinct by-value carrier shape. A
// packed vector is not here -- it crosses by pointer as a canonical buffer and
// is a `VectorCarrier`. The SV type is classified once at AST-to-HIR, where
// slang types are available; every layer below reads the category.
enum class DpiScalarAbi : std::uint8_t {
  kVoid,
  kBitScalar,
  kLogicScalar,
  kByte,
  kShortInt,
  kInt,
  kLongInt,
  kReal,
  kString,
  kChandle,
};

// A by-value scalar carrier: the SV value crosses in a C register as its scalar
// C type (`int`, `double`, `const char*`, `void*`, `unsigned char`).
struct ScalarCarrier {
  DpiScalarAbi abi = DpiScalarAbi::kVoid;

  auto operator==(const ScalarCarrier&) const -> bool = default;
};

// A canonical packed-vector carrier (LRM 35.5.6, Annex H.10.1.2): the SV value
// crosses by pointer to a buffer of 32-bit chunks -- `svBitVecVal` (2-state,
// value plane only) or `svLogicVecVal` (4-state, `{aval, bval}` planes). Only
// the chunk kind (`four_state`) lives here.
//
// `VectorCarrier` deliberately does not store the width. Width is a property of
// the formal's SV type, not of the ABI family: the C spelling is `svBitVecVal*`
// / `svLogicVecVal*` regardless of width, and the buffer extent
// (`ceil(width / 32)` chunks) is derived at the call site from the seed and
// prototype values, which come from that SV type. Storing a width here would
// duplicate what the SV type already fixes. A future duplicate-signature check
// (LRM 35.5.4) compares the full SV type, direction, and return type, not this
// ABI-family projection.
struct VectorCarrier {
  bool four_state = false;

  auto operator==(const VectorCarrier&) const -> bool = default;
};

// The C ABI carrier an SV value crosses the DPI-C boundary as. Exactly one of a
// by-value scalar or a by-pointer canonical vector -- the two are different
// families (register value vs boundary buffer), so the variant carries each
// family's own payload. It is the ABI-family projection of a formal, not the
// whole formal: the SV type shape (width, signedness) stays on the formal's SV
// type, carried alongside this, and this never duplicates it. Shared by HIR and
// MIR so no layer re-derives the classification. Kept in `support` as pure data
// (no HIR / MIR / slang types) so it stays IR-agnostic.
using DpiCarrier = std::variant<ScalarCarrier, VectorCarrier>;

// The direction of a DPI-C formal argument (LRM 35.5.1.2). `ref` is illegal in
// import declarations, so the set is exactly input / output / inout. The
// direction decides the boundary plumbing -- input crosses by value, output and
// inout cross by pointer with a copy back -- not the carrier, which is
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

// The SV keyword a scalar ABI category corresponds to. The shared
// human-readable spelling every HIR and MIR dump names the category by, so the
// two dumps agree without each restating the mapping.
[[nodiscard]] constexpr auto DpiScalarAbiName(DpiScalarAbi abi)
    -> std::string_view {
  switch (abi) {
    case DpiScalarAbi::kVoid:
      return "void";
    case DpiScalarAbi::kBitScalar:
      return "bit";
    case DpiScalarAbi::kLogicScalar:
      return "logic";
    case DpiScalarAbi::kByte:
      return "byte";
    case DpiScalarAbi::kShortInt:
      return "shortint";
    case DpiScalarAbi::kInt:
      return "int";
    case DpiScalarAbi::kLongInt:
      return "longint";
    case DpiScalarAbi::kReal:
      return "real";
    case DpiScalarAbi::kString:
      return "string";
    case DpiScalarAbi::kChandle:
      return "chandle";
  }
  return "unknown";
}

// A human-readable spelling of a carrier, for HIR and MIR dumps: a scalar names
// its ABI category, a vector names its chunk kind and declared width.
[[nodiscard]] inline auto DpiCarrierName(const DpiCarrier& carrier)
    -> std::string {
  if (const auto* scalar = std::get_if<ScalarCarrier>(&carrier)) {
    return std::string{DpiScalarAbiName(scalar->abi)};
  }
  const auto& vec = std::get<VectorCarrier>(carrier);
  return vec.four_state ? "logicvec" : "bitvec";
}

}  // namespace lyra::support
