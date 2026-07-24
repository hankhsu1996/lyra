#pragma once

#include <cstdint>

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

// LRM 13.5 data flow at the call boundary. `output` and `inout` carry a value
// back to the actual at return, so a call desugars them into a temp with a
// copy-out assignment after the call (`inout` also copies in before). `input`
// passes a value and `ref` / `const ref` alias the actual directly, so neither
// writes back. Only the writeback directions force the call into statement
// position, where the copy-out can be sequenced.
[[nodiscard]] constexpr auto RequiresWriteback(ParamDirection direction)
    -> bool {
  return direction == ParamDirection::kOutput ||
         direction == ParamDirection::kInOut;
}

}  // namespace lyra::hir
