#pragma once

#include <cstdint>

namespace lyra::hir {

// LRM 11.4.2: `++a`, `a++`, `--a`, `a--`. The four forms differ on which
// side of the operand the operator sits (pre / post -- affects the yielded
// value) and on the direction of the side effect (inc / dec).
enum class IncDecOp : std::uint8_t {
  kPreInc,
  kPostInc,
  kPreDec,
  kPostDec,
};

}  // namespace lyra::hir
