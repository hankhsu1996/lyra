#pragma once

// Common DPI ABI type classification shared across HIR, MIR, and LLVM layers.

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
enum class DpiAbiTypeClass {
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

// True if the ABI type class is valid for a DPI return type.
// kLogicVecNarrow return requires indirect return modeling (D3b scope).
[[nodiscard]] inline auto IsValidDpiReturnType(DpiAbiTypeClass t) -> bool {
  return IsNormalizedDpiType(t) && t != DpiAbiTypeClass::kLogicVecNarrow;
}

// True if the ABI type class is a 4-state DPI type (D3a).
[[nodiscard]] inline auto IsFourStateDpiType(DpiAbiTypeClass t) -> bool {
  return t == DpiAbiTypeClass::kLogicScalar ||
         t == DpiAbiTypeClass::kLogicVecNarrow;
}

// True if the ABI type class is a 4-state vector requiring by-pointer passing
// even for input direction (const svLogicVecVal* per IEEE 1800-2023).
[[nodiscard]] inline auto IsFourStateVecDpiType(DpiAbiTypeClass t) -> bool {
  return t == DpiAbiTypeClass::kLogicVecNarrow;
}

}  // namespace lyra
