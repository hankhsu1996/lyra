#pragma once

// Common DPI ABI type classification shared across HIR, MIR, and LLVM layers.

#include <cstdint>

namespace lyra {

// Normalized DPI ABI type classification.
// Each value maps to exactly one C ABI type at the LLVM boundary.
// This enum is the single source of truth for DPI type decisions;
// all layers (HIR, MIR, LLVM) key on this, not on raw TypeId properties.
//
// kInvalid is a sentinel for unnormalized fields. AST-to-HIR normalization
// must overwrite it before storing any record.
//
// kVoid is valid only as a return type, never as a parameter type.
enum class DpiAbiTypeClass : uint8_t {
  kInvalid,
  kBit,
  kByte,
  kShortInt,
  kInt,
  kLongInt,
  kReal,
  kShortReal,
  kString,
  kChandle,
  kVoid,

  // D3a: 4-state types
  kLogicScalar,     // 1-bit 4-state (svLogic = uint8_t)
  kLogicVecNarrow,  // 2..64-bit 4-state packed (svLogicVecVal[1..2])

  // D3b: wide packed types (>64-bit)
  kBitVecWide,    // >64-bit 2-state packed (svBitVecVal[3..])
  kLogicVecWide,  // >64-bit 4-state packed (svLogicVecVal[3..])
};

// True if the type class has been normalized (not the sentinel).
[[nodiscard]] inline auto IsNormalizedDpiType(DpiAbiTypeClass t) -> bool {
  return t != DpiAbiTypeClass::kInvalid;
}

// True if the ABI type class is valid for a DPI parameter.
// kVoid is a return-only classification and must not appear on parameters.
[[nodiscard]] inline auto IsValidDpiParamType(DpiAbiTypeClass t) -> bool {
  return IsNormalizedDpiType(t) && t != DpiAbiTypeClass::kVoid;
}

// True if the ABI type class is a 4-state scalar (svLogic).
[[nodiscard]] inline auto IsFourStateScalarDpiType(DpiAbiTypeClass t) -> bool {
  return t == DpiAbiTypeClass::kLogicScalar;
}

// True if the ABI type class is a 4-state packed vector (narrow or wide).
// These use svLogicVecVal* transport and require by-pointer passing for input.
[[nodiscard]] inline auto IsFourStateVecDpiType(DpiAbiTypeClass t) -> bool {
  return t == DpiAbiTypeClass::kLogicVecNarrow ||
         t == DpiAbiTypeClass::kLogicVecWide;
}

// True if the ABI type class is a 4-state DPI type (scalar or vector).
[[nodiscard]] inline auto IsFourStateDpiType(DpiAbiTypeClass t) -> bool {
  return IsFourStateScalarDpiType(t) || IsFourStateVecDpiType(t);
}

// True if the ABI type class is a 2-state wide packed vector (svBitVecVal*).
[[nodiscard]] inline auto IsTwoStateWideVecDpiType(DpiAbiTypeClass t) -> bool {
  return t == DpiAbiTypeClass::kBitVecWide;
}

// True for any packed vector requiring multiword by-pointer transport.
[[nodiscard]] inline auto IsPackedVecDpiType(DpiAbiTypeClass t) -> bool {
  return IsFourStateVecDpiType(t) || IsTwoStateWideVecDpiType(t);
}

// True for wide packed types (>64-bit, both 2-state and 4-state).
[[nodiscard]] inline auto IsWidePackedDpiType(DpiAbiTypeClass t) -> bool {
  return t == DpiAbiTypeClass::kBitVecWide ||
         t == DpiAbiTypeClass::kLogicVecWide;
}

// True if the ABI type class is valid for a DPI return type.
// Packed vector returns require indirect return modeling, not yet supported.
[[nodiscard]] inline auto IsValidDpiReturnType(DpiAbiTypeClass t) -> bool {
  return IsNormalizedDpiType(t) && !IsPackedVecDpiType(t);
}

}  // namespace lyra
