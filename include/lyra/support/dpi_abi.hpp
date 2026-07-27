#pragma once

#include <cstdint>
#include <format>
#include <optional>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

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

// One declared unpacked dimension of an open-array formal, `[left:right]`.
// Spelled here rather than reused from an IR layer so the carrier stays
// IR-agnostic.
struct DpiRange {
  std::int64_t left = 0;
  std::int64_t right = 0;

  auto operator==(const DpiRange&) const -> bool = default;
};

// A by-handle open-array carrier (LRM 35.5.6.1, Annex H.8.6): the formal leaves
// at least one dimension unsized, so the actual's extent is fixed per call and
// the value crosses as a handle to a canonical image of the whole array.
//
// Only what the formal's SV type does not already fix lives here. `unpacked`
// runs outermost-first, one entry per unpacked dimension, holding the range the
// declaration fixes or nothing where the dimension is unsized and the actual
// supplies it at the call (LRM Annex H.7.6). `packed_unsized` says the same for
// the sole packed dimension, whose width then comes from the actual; a sized
// one takes its width from the element type. The element type itself, its
// width, and its state domain stay on the formal's SV type and are read from
// there.
//
// `element_crosses_as_canonical_vector` is the element's own ABI family: it
// says an individual value of the element type crosses in the same canonical
// form the array image holds it in, which is what decides whether the foreign
// side may take the address of the whole array or of one element (LRM Annex
// H.12.4).
struct OpenArrayCarrier {
  bool packed_unsized = false;
  bool element_crosses_as_canonical_vector = false;
  std::vector<std::optional<DpiRange>> unpacked;

  auto operator==(const OpenArrayCarrier&) const -> bool = default;
};

// The C ABI carrier an SV value crosses the DPI-C boundary as: a by-value
// scalar, a by-pointer canonical vector, or a by-handle open array. The three
// are different families (register value, boundary buffer, boundary image), so
// the variant carries each family's own payload. It is the ABI-family
// projection of a formal, not the whole formal: the SV type shape (width,
// signedness) stays on the formal's SV type, carried alongside this, and this
// never duplicates it. Shared by HIR and MIR so no layer re-derives the
// classification. Kept in `support` as pure data (no HIR / MIR / slang types)
// so it stays IR-agnostic.
using DpiCarrier = std::variant<ScalarCarrier, VectorCarrier, OpenArrayCarrier>;

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

// Whether the category is one of the 2- and 4-state scalar and packed types --
// the ones an open array holds in canonical form (LRM Annex H.7.3). The
// remainder (`real`, `string`, `chandle`, and `void`, which is not an argument
// at all) are the C-compatible-representation half of that rule.
[[nodiscard]] constexpr auto DpiScalarAbiIsIntegral(DpiScalarAbi abi) -> bool {
  switch (abi) {
    case DpiScalarAbi::kBitScalar:
    case DpiScalarAbi::kLogicScalar:
    case DpiScalarAbi::kByte:
    case DpiScalarAbi::kShortInt:
    case DpiScalarAbi::kInt:
    case DpiScalarAbi::kLongInt:
      return true;
    case DpiScalarAbi::kVoid:
    case DpiScalarAbi::kReal:
    case DpiScalarAbi::kString:
    case DpiScalarAbi::kChandle:
      return false;
  }
  return false;
}

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
// its ABI category, a vector its chunk kind, an open array its per-dimension
// shape with `[]` for each dimension the actual sizes.
[[nodiscard]] inline auto DpiCarrierName(const DpiCarrier& carrier)
    -> std::string {
  if (const auto* scalar = std::get_if<ScalarCarrier>(&carrier)) {
    return std::string{DpiScalarAbiName(scalar->abi)};
  }
  if (const auto* vec = std::get_if<VectorCarrier>(&carrier)) {
    return vec->four_state ? "logicvec" : "bitvec";
  }
  const auto& open = std::get<OpenArrayCarrier>(carrier);
  std::string out = "openarray";
  out += open.packed_unsized ? "[]" : "";
  for (const std::optional<DpiRange>& dim : open.unpacked) {
    out += dim.has_value() ? std::format("[{}:{}]", dim->left, dim->right)
                           : std::string{"[]"};
  }
  return out;
}

}  // namespace lyra::support
