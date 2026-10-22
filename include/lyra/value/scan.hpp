#pragma once

#include <array>
#include <cstddef>
#include <cstdint>
#include <span>
#include <string_view>
#include <utility>
#include <variant>

#include "lyra/value/integral.hpp"
#include "lyra/value/string.hpp"
#include "lyra/value/tuple.hpp"

namespace lyra::value {

// LRM 21.3.4.3 output destination for a single parsed value: an integral one
// written in place at the width its view states, or a string.
using ScanTarget = std::variant<IntegralView, String*>;

// LRM 21.3.4.3(a): a null character counts as white space -- and so separates
// input fields -- under `$sscanf` alone. Every other scan source delimits on
// the ASCII white-space set only, and a null character is ordinary input.
enum class NullByte : std::uint8_t { kOrdinary, kWhiteSpace };

// The bytes a scan reads, taken one at a time from where the input stands. A
// scan looks at most one byte past what it has accepted and ungets at most the
// one it consumed last, so whatever it does not accept is still the input's
// next byte when the scan returns (LRM 21.3.4.3, "the offending input character
// is left unread"). Which bytes separate input fields is the scan form's, and
// each form has an input of its own, so the input answers it.
class ScanInput {
 public:
  // What `Peek` and `Consume` answer where the input has no more bytes.
  static constexpr int kNoByte = -1;

  explicit ScanInput(NullByte null_byte) : null_byte_(null_byte) {
  }
  ScanInput(const ScanInput&) = delete;
  auto operator=(const ScanInput&) -> ScanInput& = delete;
  ScanInput(ScanInput&&) = delete;
  auto operator=(ScanInput&&) -> ScanInput& = delete;
  virtual ~ScanInput() = default;

  virtual auto Peek() -> int = 0;
  virtual auto Consume() -> int = 0;
  virtual void Unget(int byte) = 0;

  [[nodiscard]] auto IsWhitespace(int byte) const -> bool;

 private:
  NullByte null_byte_;
};

// A string read from its first byte, as `$sscanf` reads its source.
class StringScanInput final : public ScanInput {
 public:
  explicit StringScanInput(std::string_view text)
      : ScanInput(NullByte::kWhiteSpace), text_(text) {
  }

  auto Peek() -> int override;
  auto Consume() -> int override;
  void Unget(int byte) override;

 private:
  std::string_view text_;
  std::size_t next_ = 0;
};

namespace detail {

// How many conversions matched, or -1 where the input ended before the first
// one (LRM 21.3.4.3).
[[nodiscard]] auto ScanImpl(
    ScanInput& input, const String& format, std::span<const ScanTarget> targets)
    -> std::int64_t;

// Where one conversion's value lands while the scan runs, starting as the
// value the caller supplied, so a conversion that never ran answers with that
// value.
template <typename T>
class ScanSlot;

template <IntegralValue T>
class ScanSlot<T> {
 public:
  explicit ScanSlot(const T& prototype) : held_(kShapeOf<T>) {
    Convert(held_.MutableView(), prototype.View());
  }
  [[nodiscard]] auto Target() -> ScanTarget {
    return held_.MutableView();
  }
  [[nodiscard]] auto Value() const -> T {
    T out = T::Zero();
    Convert(out.MutableView(), held_.View());
    return out;
  }

 private:
  WrittenWords held_;
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

}  // namespace detail

// LRM 21.3.4.3. Reads `input` under `format` and completes with the
// matched-conversion count and one value per conversion. Each of `prototypes`
// is what its conversion answers with where the scan stopped before reaching
// it.
template <typename... Targets>
auto Scan(ScanInput& input, const String& format, Tuple<Targets...> prototypes)
    -> Tuple<Integer, Targets...> {
  return [&]<std::size_t... I>(std::index_sequence<I...>) {
    Tuple<detail::ScanSlot<Targets>...> slots{detail::ScanSlot<Targets>(
        std::move(prototypes).template Component<I>())...};
    const std::array<ScanTarget, sizeof...(Targets)> targets{
        slots.template Component<I>().Target()...};
    const std::int64_t items = detail::ScanImpl(input, format, targets);
    return Tuple<Integer, Targets...>{
        Integer::FromInt(items), slots.template Component<I>().Value()...};
  }(std::index_sequence_for<Targets...>{});
}

// LRM 21.3.4.3 `$sscanf`, a scan of a string.
template <typename... Targets>
auto ScanString(
    const String& input, const String& format, Tuple<Targets...> prototypes)
    -> Tuple<Integer, Targets...> {
  StringScanInput source(input.View());
  return Scan(source, format, std::move(prototypes));
}

}  // namespace lyra::value
