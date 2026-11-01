#pragma once

#include <cstdint>

#include "lyra/base/internal_error.hpp"

namespace lyra::support {

// The two IEEE 754 formats a floating-point value takes, binary32 and
// binary64, as rustc's `FloatTy` is the one set its float types carry: a
// machine float comes in both, and so does an SV floating-point value, binary32
// for a `shortreal` and binary64 for a `real` or a `realtime` (LRM 6.12). Every
// layer naming a float, and the library laying one out, names it by this.
enum class FloatWidth : std::uint8_t { k32, k64 };

// The width as a number, for a reader of the width -- the bytes a float of it
// takes, a target parameterizing its float by a count -- and not for composing
// a type name.
constexpr auto BitsOf(FloatWidth width) -> std::uint32_t {
  switch (width) {
    case FloatWidth::k32:
      return 32;
    case FloatWidth::k64:
      return 64;
  }
  throw InternalError("support: unknown float width");
}

}  // namespace lyra::support
