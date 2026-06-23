#pragma once

#include <initializer_list>
#include <variant>

#include "lyra/value/packed_array.hpp"
#include "lyra/value/string.hpp"

namespace lyra::value {

// LRM 21.3.4.3 output destination for a single parsed value.
using ScanTarget = std::variant<PackedArray*, String*>;

namespace detail {

[[nodiscard]] auto ScanImpl(
    const String& input, const String& format, PackedArray& consumed,
    std::initializer_list<ScanTarget> targets) -> PackedArray;

}  // namespace detail

inline auto MakeScanTarget(PackedArray& v) -> ScanTarget {
  return ScanTarget{&v};
}
inline auto MakeScanTarget(String& v) -> ScanTarget {
  return ScanTarget{&v};
}

// LRM 21.3.4.3 `$sscanf` / `$fscanf` parser. Reads `input` under
// `format`, writes one value per target, returns the matched-conversion
// count. `consumed` reports how many bytes of `input` the parser
// advanced past, so a streaming caller can rewind the unconsumed tail.
template <typename... Targets>
auto Scan(
    const String& input, const String& format, PackedArray& consumed,
    Targets&... targets) -> PackedArray {
  return detail::ScanImpl(
      input, format, consumed, {MakeScanTarget(targets)...});
}

}  // namespace lyra::value
