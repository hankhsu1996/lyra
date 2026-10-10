#pragma once

#include <concepts>
#include <cstddef>
#include <cstdint>
#include <memory>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_value_type.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/reduction.hpp"
#include "lyra/value/value_type.hpp"

namespace lyra::value {

template <typename Host>
class RealValue;
class Chandle;

namespace detail {

[[noreturn]] inline void LacksOperation() {
  throw InternalError(
      "value type: an operation is asked of a type the language does not "
      "define it for -- please report this as a bug");
}

// LRM 6.24.3: a real and a chandle are the kinds that hold no stream of bits,
// so a streaming conversion of one is a program the front end rejected. Every
// other kind is one a program may stream.
template <typename T>
inline constexpr bool kIsBitStreamType = true;
template <typename Host>
inline constexpr bool kIsBitStreamType<RealValue<Host>> = false;
template <>
inline constexpr bool kIsBitStreamType<Chandle> = false;

// A value of `T` holds parts ordered by position, each a value of a type the
// value states and handed out where it lies.
template <typename T>
concept HoldsParts = requires(const T& c, T& m, std::size_t position) {
  { c.Count() } -> std::same_as<std::size_t>;
  { c.ElementType() } -> std::same_as<const ValueType&>;
  { c.ElementAt(position) } -> std::same_as<const void*>;
  { m.ElementAt(position) } -> std::same_as<void*>;
};

// A value of `T` written into a stream of bits and built back out of one, the
// stream being planes whose width the caller states (LRM 6.24.3).
template <typename T>
concept StreamsThroughPlanes = requires(
    const T& value, Planes written, ConstPlanes read, std::uint64_t at) {
  { value.WriteToStream(written, at, at) } -> std::same_as<std::uint64_t>;
  {
    value.ReadFromStream(read, at, at)
  } -> std::same_as<std::pair<T, std::uint64_t>>;
};

// A question about a value's stream of bits the language defines for a kind
// and Lyra does not yet answer, which a legal program can ask.
[[noreturn]] inline void StreamNotYetSupported() {
  throw SimulationError(
      "reading this value as a stream of bits, or building one from it, is "
      "not yet supported on this backend; please open an issue asking for "
      "support");
}

// The parts of a value of `T`, each answered with what `T` itself does.
template <HoldsParts T>
class PartsOf final : public PartsByPosition {
 public:
  constexpr PartsOf() = default;

  [[nodiscard]] auto Count(const void* value) const -> std::size_t override {
    return static_cast<const T*>(value)->Count();
  }
  [[nodiscard]] auto Type(const void* value) const
      -> const ValueType& override {
    return static_cast<const T*>(value)->ElementType();
  }
  [[nodiscard]] auto At(const void* value, std::size_t position) const -> const
      void* override {
    return static_cast<const T*>(value)->ElementAt(position);
  }
  [[nodiscard]] auto RefAt(void* value, std::size_t position) const
      -> void* override {
    return static_cast<T*>(value)->ElementAt(position);
  }
};

}  // namespace detail

// The type `T` is, answering each operation with what `T` itself does: C++
// states a type's operations as its own members, so the table is generated
// from them rather than written.
template <LyraValue T>
class ValueTypeOf final : public ValueType {
 public:
  constexpr ValueTypeOf()
      : ValueType(
            static_cast<std::uint32_t>(sizeof(T)),
            static_cast<std::uint32_t>(alignof(T))) {
  }

  // Defined beside the library's instances of this class, which are the only
  // ones: the order of an index type is stated with the associative array.
  [[nodiscard]] auto OrderBefore(const void* lhs, const void* rhs) const
      -> bool override;
  [[nodiscard]] auto IsTrue(const void* value) const -> bool override {
    if constexpr (requires { Of(value).IsTruthy(); }) {
      return Of(value).IsTruthy();
    } else {
      detail::LacksOperation();
    }
  }
  void Reduce(Reduction reduction, const void* lhs, const void* rhs, void* out)
      const override {
    std::construct_at(
        static_cast<T*>(out), Reduced(reduction, Of(lhs), Of(rhs)));
  }

  [[nodiscard]] auto Parts() const -> const PartsByPosition* override {
    if constexpr (detail::HoldsParts<T>) {
      static constinit const detail::PartsOf<T> kParts;
      return &kParts;
    } else {
      return nullptr;
    }
  }
  [[nodiscard]] auto AsIntegral() const -> const IntegralValueType* override {
    return nullptr;
  }

  void Copy(const void* value, void* out) const override {
    std::construct_at(static_cast<T*>(out), Of(value));
  }
  void Move(void* value, void* out) const noexcept override {
    std::construct_at(static_cast<T*>(out), std::move(*static_cast<T*>(value)));
  }
  void Destroy(void* value) const noexcept override {
    std::destroy_at(static_cast<T*>(value));
  }
  void Assign(void* storage, const void* value) const override {
    *static_cast<T*>(storage) = Of(value);
  }

  [[nodiscard]] auto Equal(const void* lhs, const void* rhs) const
      -> FourStateBit override {
    return AnswerScalar(Of(lhs) == Of(rhs));
  }
  [[nodiscard]] auto CaseEqual(const void* lhs, const void* rhs) const
      -> bool override {
    if constexpr (CaseEqualComparable<T>) {
      return Of(lhs).CaseEqual(Of(rhs)).IsTruthy();
    } else {
      detail::LacksOperation();
    }
  }
  [[nodiscard]] auto BitIdentical(const void* lhs, const void* rhs) const
      -> bool override {
    return Of(lhs).IsBitIdentical(Of(rhs));
  }
  [[nodiscard]] auto HasUnknown(const void* value) const -> bool override {
    return Of(value).HasUnknown();
  }

  void BitstreamWidth(const void* value, void* out) const override {
    if constexpr (requires { Of(value).BitstreamWidth(); }) {
      std::construct_at(static_cast<Int*>(out), Of(value).BitstreamWidth());
    } else if constexpr (detail::kIsBitStreamType<T>) {
      detail::StreamNotYetSupported();
    } else {
      detail::LacksOperation();
    }
  }
  void CountBits(
      const void* value, const ConstPlanes& control_bits,
      std::uint64_t control_width, void* out) const override {
    const ConstIntegralView control{
        .planes = control_bits,
        .width = control_width,
        .signedness = Signedness::kUnsigned};
    if constexpr (requires { Of(value).CountBits(control); }) {
      std::construct_at(static_cast<Int*>(out), Of(value).CountBits(control));
    } else if constexpr (detail::kIsBitStreamType<T>) {
      detail::StreamNotYetSupported();
    } else {
      detail::LacksOperation();
    }
  }
  auto WriteToStream(
      const void* value, const Planes& stream, std::uint64_t stream_width,
      std::uint64_t filled) const -> std::uint64_t override {
    if constexpr (detail::StreamsThroughPlanes<T>) {
      return Of(value).WriteToStream(stream, stream_width, filled);
    } else if constexpr (detail::kIsBitStreamType<T>) {
      detail::StreamNotYetSupported();
    } else {
      detail::LacksOperation();
    }
  }
  auto ReadFromStream(
      const ConstPlanes& stream, std::uint64_t stream_width,
      std::uint64_t taken, const void* prototype, void* out) const
      -> std::uint64_t override {
    if constexpr (detail::StreamsThroughPlanes<T>) {
      auto [read, after] =
          Of(prototype).ReadFromStream(stream, stream_width, taken);
      std::construct_at(static_cast<T*>(out), std::move(read));
      return after;
    } else if constexpr (detail::kIsBitStreamType<T>) {
      detail::StreamNotYetSupported();
    } else {
      detail::LacksOperation();
    }
  }

  void ResolveTriState(
      const void* lhs, const void* rhs, void* out) const override {
    if constexpr (NetResolvable<T>) {
      std::construct_at(static_cast<T*>(out), Of(lhs).ResolveTriState(Of(rhs)));
    } else {
      detail::LacksOperation();
    }
  }
  void ResolveWiredAnd(
      const void* lhs, const void* rhs, void* out) const override {
    if constexpr (NetResolvable<T>) {
      std::construct_at(static_cast<T*>(out), Of(lhs).ResolveWiredAnd(Of(rhs)));
    } else {
      detail::LacksOperation();
    }
  }
  void ResolveWiredOr(
      const void* lhs, const void* rhs, void* out) const override {
    if constexpr (NetResolvable<T>) {
      std::construct_at(static_cast<T*>(out), Of(lhs).ResolveWiredOr(Of(rhs)));
    } else {
      detail::LacksOperation();
    }
  }
  void Dominating(
      const void* stronger, const void* weaker, void* out) const override {
    if constexpr (NetResolvable<T>) {
      std::construct_at(
          static_cast<T*>(out), Of(stronger).Dominating(Of(weaker)));
    } else {
      detail::LacksOperation();
    }
  }
  void FilledLike(
      const void* prototype, const void* fill, void* out) const override {
    if constexpr (NetResolvable<T>) {
      std::construct_at(
          static_cast<T*>(out),
          FilledAs(Of(prototype), *static_cast<const Logic*>(fill)));
    } else {
      detail::LacksOperation();
    }
  }

 private:
  [[nodiscard]] static auto Of(const void* value) -> const T& {
    return *static_cast<const T*>(value);
  }
};

}  // namespace lyra::value
