#pragma once

#include <cstdint>

namespace lyra::mir {

// LRM 11.4.2 mirror of hir::IncDecOp.
enum class IncDecOp : std::uint8_t {
  kPreInc,
  kPostInc,
  kPreDec,
  kPostDec,
};

}  // namespace lyra::mir
