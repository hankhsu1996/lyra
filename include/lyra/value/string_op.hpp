#pragma once

#include <cstdint>
#include <string>
#include <utility>

#include "lyra/value/string.hpp"

namespace lyra::value {

// LRM 11.4.12.2: when at least one inner operand is string-typed or the
// multiplier is non-constant, `{multiplier{...}}` shall yield M concatenated
// copies of the inner concatenation. A multiplier of zero yields the empty
// string. The multiplier is unsigned in SystemVerilog; we still accept signed
// here and treat negative as zero for defence-in-depth.
inline auto ReplicateString(const String& operand, std::int64_t count)
    -> String {
  if (count <= 0) return {};
  std::string out;
  out.reserve(operand.View().size() * static_cast<std::size_t>(count));
  for (std::int64_t i = 0; i < count; ++i) {
    out.append(operand.View());
  }
  return String{std::move(out)};
}

}  // namespace lyra::value
