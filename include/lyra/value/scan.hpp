#pragma once

#include <cstdint>
#include <initializer_list>
#include <variant>

#include "lyra/value/packed_array.hpp"
#include "lyra/value/string.hpp"

namespace lyra::value {

// LRM 21.3.4.3 output destination for a single parsed value.
using ScanTarget = std::variant<PackedArray*, String*>;

namespace detail {

// LRM 21.3.4.3(a): a null character counts as white space -- and so separates
// input fields -- under `$sscanf` alone. Every other scan source delimits on
// the ASCII white-space set only, and a null character is ordinary input.
enum class NullByte : std::uint8_t { kOrdinary, kWhiteSpace };

[[nodiscard]] auto ScanImpl(
    const String& input, const String& format, NullByte null_byte,
    PackedArray& consumed, std::initializer_list<ScanTarget> targets)
    -> PackedArray;

}  // namespace detail

inline auto MakeScanTarget(PackedArray& v) -> ScanTarget {
  return ScanTarget{&v};
}
inline auto MakeScanTarget(String& v) -> ScanTarget {
  return ScanTarget{&v};
}

// LRM 21.3.4.3 `$sscanf`. Reads `input` under `format`, writes one value per
// target, and returns the matched-conversion count. `consumed` reports how
// many bytes of `input` the parser advanced past, so a streaming caller can
// rewind the unconsumed tail.
template <typename... Targets>
auto ScanString(
    const String& input, const String& format, PackedArray& consumed,
    Targets&... targets) -> PackedArray {
  return detail::ScanImpl(
      input, format, detail::NullByte::kWhiteSpace, consumed,
      {MakeScanTarget(targets)...});
}

// LRM 21.3.4.3 `$fscanf`, over bytes already read from the descriptor. Same
// parse as the string form, except that a null character is ordinary input
// rather than a field separator (LRM 21.3.4.3(a)).
template <typename... Targets>
auto ScanFile(
    const String& input, const String& format, PackedArray& consumed,
    Targets&... targets) -> PackedArray {
  return detail::ScanImpl(
      input, format, detail::NullByte::kOrdinary, consumed,
      {MakeScanTarget(targets)...});
}

}  // namespace lyra::value
