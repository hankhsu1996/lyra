#include "lyra/runtime/format_spec_abi.hpp"

#include <cstdint>
#include <optional>

#include "lyra/common/format.hpp"
#include "lyra/semantic/format.hpp"

namespace lyra::runtime {

auto EncodeFormatSpec(const semantic::FormatSpec& spec) -> LyraFormatSpec {
  uint8_t flags = 0;
  if (spec.zero_pad) {
    flags |= kFormatFlagZeroPad;
  }
  if (spec.left_align) {
    flags |= kFormatFlagLeftAlign;
  }
  return LyraFormatSpec{
      .kind = static_cast<int32_t>(spec.kind),
      .width = spec.width.value_or(-1),
      .precision = spec.precision.value_or(-1),
      .flags = flags,
      .reserved = {}};
}

auto DecodeFormatSpec(const LyraFormatSpec& abi) -> semantic::FormatSpec {
  return semantic::FormatSpec{
      .kind = static_cast<FormatKind>(abi.kind),
      .width = abi.width >= 0 ? std::optional(abi.width) : std::nullopt,
      .precision =
          abi.precision >= 0 ? std::optional(abi.precision) : std::nullopt,
      .zero_pad = (abi.flags & kFormatFlagZeroPad) != 0,
      .left_align = (abi.flags & kFormatFlagLeftAlign) != 0,
  };
}

}  // namespace lyra::runtime
