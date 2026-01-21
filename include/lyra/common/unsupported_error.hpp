#pragma once

#include <cstdint>
#include <exception>
#include <string>

namespace lyra::common {

// Layer where the unsupported feature was encountered.
// Ordered from frontend to backend.
enum class UnsupportedLayer {
  kAstToHir,   // AST -> HIR lowering
  kHirToMir,   // HIR -> MIR lowering
  kMirToLlvm,  // MIR -> LLVM lowering
  kExecution,  // Runtime execution
};

// Category of what is unsupported.
enum class UnsupportedKind {
  kType,       // Unsupported type (e.g., wide integers, 4-state)
  kOperation,  // Unsupported operation (e.g., binary op variant)
  kFeature,    // Unsupported language feature (e.g., packed structs)
};

// Opaque identifier for tracing errors back to source locations.
// Allocated by OriginMap, resolved against HIR/MIR artifacts.
struct OriginId {
  uint32_t value;

  static constexpr auto Invalid() -> OriginId {
    return {UINT32_MAX};
  }
  [[nodiscard]] auto IsValid() const -> bool {
    return value != UINT32_MAX;
  }

  auto operator==(const OriginId&) const -> bool = default;
};

// Structured error for valid SystemVerilog that is not yet implemented
// in a particular backend. Distinct from:
// - DiagnosticException: invalid SV code (AST->HIR only)
// - InternalError: compiler bug / invariant violation
struct UnsupportedError {
  UnsupportedLayer layer;
  UnsupportedKind kind;
  OriginId origin;
  std::string detail;
};

// Exception wrapper for UnsupportedError.
// Allows error propagation through call stacks that use exceptions.
// Throw directly: throw UnsupportedErrorException(layer, kind, origin, detail);
class UnsupportedErrorException final : public std::exception {
 public:
  explicit UnsupportedErrorException(UnsupportedError error);

  UnsupportedErrorException(
      UnsupportedLayer layer, UnsupportedKind kind, OriginId origin,
      std::string detail);

  [[nodiscard]] auto GetError() const -> const UnsupportedError& {
    return error_;
  }

  [[nodiscard]] auto what() const noexcept -> const char* override {
    return error_.detail.c_str();
  }

 private:
  UnsupportedError error_;
};

// String conversion for diagnostic display
auto ToString(UnsupportedLayer layer) -> const char*;
auto ToString(UnsupportedKind kind) -> const char*;

}  // namespace lyra::common
