#pragma once

#include <cstdint>

namespace lyra::hir {

enum class ConversionKind : std::uint8_t {
  kImplicit,
  kPropagated,
  kStreamingConcat,
  kExplicit,
  kBitstreamCast,
};

}  // namespace lyra::hir
