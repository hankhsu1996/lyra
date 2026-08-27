#pragma once

#include <cstdint>

#include "lyra/base/internal_error.hpp"

namespace lyra::hir {

// LRM 13.5 argument directions. On a subroutine declaration a formal is also a
// local variable of the body; on a cross-unit call reference the same direction
// classifies how each actual is marshalled at the boundary.
enum class ParamDirection : std::uint8_t {
  kInput,
  kOutput,
  kInOut,
  kRef,
  kConstRef,
};

// LRM 13.5 data flow at the call boundary: whether the callee hands a value
// back to the actual when it completes. `output` and `inout` do, so each takes
// a component of the completion the caller writes into the actual (`inout` also
// passes its incoming value in). `input` only passes a value in, and `ref` /
// `const ref` alias the actual's own storage, which the callee has already
// written -- so neither hands anything back.
[[nodiscard]] constexpr auto RequiresWriteback(ParamDirection direction)
    -> bool {
  switch (direction) {
    case ParamDirection::kOutput:
    case ParamDirection::kInOut:
      return true;
    case ParamDirection::kInput:
    case ParamDirection::kRef:
    case ParamDirection::kConstRef:
      return false;
  }
  throw InternalError("hir::RequiresWriteback: unknown ParamDirection");
}

}  // namespace lyra::hir
