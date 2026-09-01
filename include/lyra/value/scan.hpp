#pragma once

#include <array>
#include <cstddef>
#include <cstdint>
#include <span>
#include <utility>
#include <variant>

#include "lyra/value/packed_array.hpp"
#include "lyra/value/string.hpp"
#include "lyra/value/tuple.hpp"

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
    PackedArray& consumed, std::span<const ScanTarget> targets) -> PackedArray;

inline auto MakeScanTarget(PackedArray& v) -> ScanTarget {
  return ScanTarget{&v};
}
inline auto MakeScanTarget(String& v) -> ScanTarget {
  return ScanTarget{&v};
}

// The completion both scan forms hand back, parsed in place: the count leads,
// then how far the parse advanced, then one value per conversion. Each starts
// as the prototype the caller supplied, so a conversion that never ran carries
// that prototype back and the caller's own destination stays as it was.
template <typename... Targets>
auto ScanInto(
    const String& input, const String& format, NullByte null_byte,
    Tuple<Targets...> prototypes)
    -> Tuple<PackedArray, PackedArray, Targets...> {
  return [&]<std::size_t... I>(std::index_sequence<I...>) {
    Tuple<PackedArray, PackedArray, Targets...> completion{
        PackedArray::Integer(0), PackedArray::Int(0),
        std::move(prototypes).template Get<I>()...};
    const std::array<ScanTarget, sizeof...(Targets)> targets{
        MakeScanTarget(completion.template Get<I + 2>())...};
    completion.template Get<0>() = ScanImpl(
        input, format, null_byte, completion.template Get<1>(), targets);
    return completion;
  }(std::index_sequence_for<Targets...>{});
}

}  // namespace detail

// LRM 21.3.4.3 `$sscanf`. Reads `input` under `format` and completes with the
// matched-conversion count, how many bytes of `input` the parser advanced past
// -- which is what lets a streaming caller rewind the unconsumed tail -- and
// one value per conversion. A prototype states the shape its conversion parses
// into, which nothing else on the call states.
template <typename... Targets>
auto ScanString(
    const String& input, const String& format, Tuple<Targets...> prototypes)
    -> Tuple<PackedArray, PackedArray, Targets...> {
  return detail::ScanInto(
      input, format, detail::NullByte::kWhiteSpace, std::move(prototypes));
}

// LRM 21.3.4.3 `$fscanf`, over bytes already read from the descriptor. Same
// parse as the string form, except that a null character is ordinary input
// rather than a field separator (LRM 21.3.4.3(a)).
template <typename... Targets>
auto ScanFile(
    const String& input, const String& format, Tuple<Targets...> prototypes)
    -> Tuple<PackedArray, PackedArray, Targets...> {
  return detail::ScanInto(
      input, format, detail::NullByte::kOrdinary, std::move(prototypes));
}

}  // namespace lyra::value
