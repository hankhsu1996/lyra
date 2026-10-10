#pragma once

#include <array>
#include <cstddef>
#include <cstdint>
#include <span>
#include <tuple>
#include <utility>
#include <variant>

#include "lyra/value/integral.hpp"
#include "lyra/value/string.hpp"
#include "lyra/value/tuple.hpp"

namespace lyra::value {

// LRM 21.3.4.3 output destination for a single parsed value: the planes of an
// integral one, written in place at the width and signedness the view states,
// or a string.
using ScanTarget = std::variant<IntegralView, String*>;

// What a scan answers beside the values it parsed: how many conversions it
// matched, and how many bytes of its input it advanced past.
struct ScanCount {
  std::int64_t items = 0;
  std::int64_t consumed = 0;
};

namespace detail {

// LRM 21.3.4.3(a): a null character counts as white space -- and so separates
// input fields -- under `$sscanf` alone. Every other scan source delimits on
// the ASCII white-space set only, and a null character is ordinary input.
enum class NullByte : std::uint8_t { kOrdinary, kWhiteSpace };

[[nodiscard]] auto ScanImpl(
    const String& input, const String& format, NullByte null_byte,
    std::span<const ScanTarget> targets) -> ScanCount;

// Where one conversion's value lands while the scan runs, starting as the
// value the caller supplied, so a conversion that never ran answers with that
// value.
template <typename T>
class ScanSlot;

template <IntegralValue T>
class ScanSlot<T> {
 public:
  explicit ScanSlot(const T& prototype) : held_(prototype.Load()) {
  }
  [[nodiscard]] auto Target() -> ScanTarget {
    return held_.MutableView();
  }
  [[nodiscard]] auto Value() const -> T {
    return T::FromWords(held_);
  }

 private:
  typename T::Words held_;
};

template <>
class ScanSlot<String> {
 public:
  explicit ScanSlot(String prototype) : held_(std::move(prototype)) {
  }
  [[nodiscard]] auto Target() -> ScanTarget {
    return &held_;
  }
  [[nodiscard]] auto Value() const -> String {
    return held_;
  }

 private:
  String held_;
};

// The completion both scan forms hand back: the count leads, then how far the
// parse advanced, then one value per conversion.
template <typename... Targets>
auto ScanInto(
    const String& input, const String& format, NullByte null_byte,
    Tuple<Targets...> prototypes) -> Tuple<Integer, Int, Targets...> {
  return [&]<std::size_t... I>(std::index_sequence<I...>) {
    std::tuple<ScanSlot<Targets>...> slots{
        ScanSlot<Targets>(std::move(prototypes).template Component<I>())...};
    const std::array<ScanTarget, sizeof...(Targets)> targets{
        std::get<I>(slots).Target()...};
    const ScanCount count = ScanImpl(input, format, null_byte, targets);
    return Tuple<Integer, Int, Targets...>{
        Integer::FromInt(count.items), Int::FromInt(count.consumed),
        std::get<I>(slots).Value()...};
  }(std::index_sequence_for<Targets...>{});
}

}  // namespace detail

// LRM 21.3.4.3 `$sscanf`. Reads `input` under `format` and completes with the
// matched-conversion count, how many bytes of `input` the parser advanced past
// -- which is what lets a streaming caller rewind the unconsumed tail -- and
// one value per conversion. Each of `prototypes` is what its conversion answers
// with where the scan stopped before reaching it.
template <typename... Targets>
auto ScanString(
    const String& input, const String& format, Tuple<Targets...> prototypes)
    -> Tuple<Integer, Int, Targets...> {
  return detail::ScanInto(
      input, format, detail::NullByte::kWhiteSpace, std::move(prototypes));
}

// LRM 21.3.4.3 `$fscanf`, over bytes already read from the descriptor. Same
// parse as the string form, except that a null character is ordinary input
// rather than a field separator (LRM 21.3.4.3(a)).
template <typename... Targets>
auto ScanFile(
    const String& input, const String& format, Tuple<Targets...> prototypes)
    -> Tuple<Integer, Int, Targets...> {
  return detail::ScanInto(
      input, format, detail::NullByte::kOrdinary, std::move(prototypes));
}

}  // namespace lyra::value
