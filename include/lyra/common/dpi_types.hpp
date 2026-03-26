#pragma once

// Common DPI ABI type classification shared across HIR, MIR, and LLVM layers.
// D1-only. Broader DPI surfaces should extend deliberately.

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
  kVoid,
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
// D1 allows all normalized values including kVoid on returns.
[[nodiscard]] inline auto IsValidDpiReturnType(DpiAbiTypeClass t) -> bool {
  return IsNormalizedDpiType(t);
}

}  // namespace lyra
