#pragma once

#include <cstdint>

namespace lyra::hir {

enum class ConversionKind : std::uint8_t {
  kImplicit,
  kPropagated,
  kStreamingConcat,
  kExplicit,
  kBitstreamCast,
  // `const'(e)` (LRM 6.24.1): e's value, whose sampled value is its current
  // value (LRM 16.5.1), and which a procedural concurrent assertion saves where
  // it is reached (LRM 16.14.6.1).
  kConstCast,
};

}  // namespace lyra::hir
