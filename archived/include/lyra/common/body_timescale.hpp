#pragma once

#include <cstdint>

namespace lyra::common {

// Per-body timescale metadata as power-of-10 exponents.
// Produced during AST->HIR lowering from slang scope timescale.
// Consumed by LLVM backend for BodyRealizationDesc emission.
// Keyed by ModuleBodyId::value (see body_id field).
struct BodyTimeScale {
  uint32_t body_id;
  int8_t unit_power;
  int8_t precision_power;
};

}  // namespace lyra::common
