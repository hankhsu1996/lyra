#pragma once

#include <cstdint>
#include <optional>

#include "lyra/base/internal_error.hpp"
#include "lyra/hir/param_direction.hpp"

namespace lyra::lowering::hir_to_mir {

// How a value crosses a call boundary (LRM 13.5): by value, as a copy the other
// side owns, or by reference, as the storage itself, which the other side may
// write through or only read. The source states a direction on each formal and
// MIR states a type on each parameter and result; this is what the one is
// translated into the other through, read off the direction and which side of
// the boundary the value stands on.
enum class Passing : std::uint8_t {
  kByValue,
  kByReference,
  kByReadOnlyReference,
};

// How a formal of `direction` is passed in, or nothing for an `output`, which
// takes nothing in and whose variable's final value rides the completion (LRM
// 13.5); an `inout` is passed its value and rides it back the same way.
[[nodiscard]] constexpr auto ParamPassingOf(hir::ParamDirection direction)
    -> std::optional<Passing> {
  switch (direction) {
    case hir::ParamDirection::kOutput:
      return std::nullopt;
    case hir::ParamDirection::kInput:
    case hir::ParamDirection::kInOut:
      return Passing::kByValue;
    case hir::ParamDirection::kRef:
      return Passing::kByReference;
    case hir::ParamDirection::kConstRef:
      return Passing::kByReadOnlyReference;
  }
  throw InternalError("ParamPassingOf: unknown ParamDirection");
}

// How a result standing as the actual of a formal of `direction` is passed
// back: the value for an `input`, and for every direction that binds a place
// the place itself (LRM 13.5.1, 13.5.2), which an `output` or `inout` call then
// writes back into, read-only where the formal is a `const ref`.
[[nodiscard]] constexpr auto ResultPassingOf(hir::ParamDirection direction)
    -> Passing {
  switch (direction) {
    case hir::ParamDirection::kInput:
      return Passing::kByValue;
    case hir::ParamDirection::kOutput:
    case hir::ParamDirection::kInOut:
    case hir::ParamDirection::kRef:
      return Passing::kByReference;
    case hir::ParamDirection::kConstRef:
      return Passing::kByReadOnlyReference;
  }
  throw InternalError("ResultPassingOf: unknown ParamDirection");
}

}  // namespace lyra::lowering::hir_to_mir
