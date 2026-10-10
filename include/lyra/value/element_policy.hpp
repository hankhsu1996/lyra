#pragma once

#include <concepts>
#include <cstddef>
#include <cstdint>
#include <memory>
#include <new>
#include <utility>

#include "lyra/value/concepts.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"

namespace lyra::value {

// What a container's own algorithms ask of one of its elements, each element
// passed as its address. A container's algorithms are written once over this
// and compiled with the element's C++ type, where every answer is that type's
// own, and in the library, where the answers come from the element type's
// table.
//
// The element default (LRM Table 7-1) is the policy's to hold, since it is a
// property of the element type: what a read naming no element answers with and
// what a new element starts as.
//
// A comparison of two elements answers 0, 1 or x as a scalar, which a container
// folds over its elements and whoever knows the type the comparison has holds
// as a value of it. A stream of bits (LRM 6.24.3) is planes whose width the
// caller states, which a policy writes one element's bits into and reads one
// element back out of.
template <typename P>
concept ElementPolicy =
    std::copyable<P> &&
    requires(const P& p, void* out, const void* in, void* from) {
      { p.Size() } -> std::same_as<std::size_t>;
      { p.Align() } -> std::same_as<std::size_t>;
      { p.Default() } -> std::same_as<const void*>;
      p.Copy(in, out);
      p.Move(from, out);
      p.Destroy(from);
      p.Assign(from, in);
      { p.Agree(in, in) } -> std::same_as<bool>;
      { p.BitIdentical(in, in) } -> std::same_as<bool>;
      { p.HasUnknown(in) } -> std::same_as<bool>;
    };

// The element is the C++ type `T`.
template <LyraValue T>
class StaticElem {
 public:
  // What comparing two elements answers (LRM 11.4.5): one bit, unknown where
  // the element can hold x or z.
  using Equality =
      decltype(std::declval<const T&>() == std::declval<const T&>());

  StaticElem() = default;
  explicit StaticElem(T element_default)
      : default_(std::move(element_default)) {
  }

  [[nodiscard]] static constexpr auto Size() -> std::size_t {
    return sizeof(T);
  }
  [[nodiscard]] static constexpr auto Align() -> std::size_t {
    return alignof(T);
  }

  [[nodiscard]] auto Default() const -> const void* {
    return &default_;
  }
  [[nodiscard]] auto DefaultValue() const -> const T& {
    return default_;
  }

  static void Copy(const void* value, void* out) {
    std::construct_at(static_cast<T*>(out), Of(value));
  }
  static void Move(void* value, void* out) {
    std::construct_at(static_cast<T*>(out), std::move(*static_cast<T*>(value)));
  }
  static void Destroy(void* value) {
    std::destroy_at(static_cast<T*>(value));
  }
  static void Assign(void* storage, const void* value) {
    *static_cast<T*>(storage) = Of(value);
  }

  [[nodiscard]] static auto Equal(const void* lhs, const void* rhs)
      -> FourStateBit {
    return AnswerScalar(Of(lhs) == Of(rhs));
  }
  // LRM 11.4.5 `===`, which is never unknown.
  [[nodiscard]] static auto CaseEqual(const void* lhs, const void* rhs)
      -> bool {
    return Of(lhs).CaseEqual(Of(rhs)).IsTruthy();
  }
  [[nodiscard]] static auto CaseAnswer(bool holds) -> Bit {
    return Bit::FromBool(holds);
  }
  // LRM 11.4.11: whether the two arms of an ambiguous conditional agree on an
  // element, which only an equality known to hold says.
  [[nodiscard]] static auto Agree(const void* lhs, const void* rhs) -> bool {
    return Holds(Of(lhs) == Of(rhs));
  }
  [[nodiscard]] static auto BitIdentical(const void* lhs, const void* rhs)
      -> bool {
    return Of(lhs).IsBitIdentical(Of(rhs));
  }
  [[nodiscard]] static auto HasUnknown(const void* value) -> bool {
    return Of(value).HasUnknown();
  }

  // LRM 20.6.2 `$bits` / 20.9 `$countbits` of one element.
  [[nodiscard]] static auto BitstreamWidth(const void* value) -> std::int64_t {
    return Of(value).BitstreamWidth().ToInt64();
  }
  template <IntegralValue Control>
  [[nodiscard]] static auto CountBits(
      const void* value, const Control& control_bits) -> std::int64_t {
    return Of(value).CountBits(control_bits).ToInt64();
  }

  // What an array asks of its elements as a whole value of its own: as a net
  // (LRM 6.6, 28.12.1, 6.7.1) and as a stream of bits (LRM 6.24.3). Each
  // answers in `out`.
  static void Resolve(
      NetResolution fold, const void* lhs, const void* rhs, void* out) {
    Build(out, lyra::value::Resolve(fold, Of(lhs), Of(rhs)));
  }
  static void Dominating(const void* stronger, const void* weaker, void* out) {
    Build(out, Of(stronger).Dominating(Of(weaker)));
  }
  template <IntegralValue Fill>
  static void FilledLike(const void* prototype, const Fill& fill, void* out) {
    Build(out, FilledAs(Of(prototype), fill));
  }

  // The element written into a stream below its `filled` most significant
  // positions, answering how many are filled after it. A structure states its
  // stream as a value of its own, and an array writes its elements in turn.
  static auto WriteToStream(
      const void* value, Planes stream, std::uint64_t stream_width,
      std::uint64_t filled) -> std::uint64_t {
    if constexpr (IntegralValue<T>) {
      return lyra::value::WriteToStream(
          Of(value), stream, stream_width, filled);
    } else if constexpr (requires {
                           Of(value).WriteToStream(
                               stream, stream_width, filled);
                         }) {
      return Of(value).WriteToStream(stream, stream_width, filled);
    } else {
      return lyra::value::WriteToStream(
          Of(value).ToBitstream(), stream, stream_width, filled);
    }
  }

  // The inverse: the element of `prototype`'s shape a stream holds below its
  // `taken` most significant positions, built in `out`. Answers how many are
  // taken after it.
  static auto ReadFromStream(
      ConstPlanes stream, std::uint64_t stream_width, std::uint64_t taken,
      const void* prototype, void* out) -> std::uint64_t {
    if constexpr (IntegralValue<T>) {
      Build(out, lyra::value::ReadFromStream<T>(stream, stream_width, taken));
      return taken + T::kWidth;
    } else if constexpr (requires {
                           Of(prototype).ReadFromStream(
                               stream, stream_width, taken);
                         }) {
      auto [element, after] =
          Of(prototype).ReadFromStream(stream, stream_width, taken);
      Build(out, std::move(element));
      return after;
    } else {
      using Bits = decltype(Of(prototype).ToBitstream());
      Build(
          out,
          T::FromBitstream(
              lyra::value::ReadFromStream<Bits>(stream, stream_width, taken),
              Of(prototype)));
      return taken + Bits::kWidth;
    }
  }

 private:
  [[nodiscard]] static auto Of(const void* value) -> const T& {
    return *static_cast<const T*>(value);
  }
  static void Build(void* out, T value) {
    std::construct_at(static_cast<T*>(out), std::move(value));
  }

  T default_{};
};

static_assert(ElementPolicy<StaticElem<Int>>);

// Storage for one element of `elem`'s type, holding none yet.
template <ElementPolicy Elem>
[[nodiscard]] auto AllocateElement(const Elem& elem) -> void* {
  return ::operator new(elem.Size(), std::align_val_t{elem.Align()});
}

// Storage of its own holding a copy of `value`.
template <ElementPolicy Elem>
[[nodiscard]] auto CopyElement(const Elem& elem, const void* value) -> void* {
  void* slot = AllocateElement(elem);
  elem.Copy(value, slot);
  return slot;
}

// Ends the element at `slot` and gives its storage back.
template <ElementPolicy Elem>
void FreeElement(const Elem& elem, void* slot) {
  elem.Destroy(slot);
  ::operator delete(slot, std::align_val_t{elem.Align()});
}

// Where an invalid-index write lands (LRM 7.4.5): storage a container keeps
// outside its elements, made on first use, holding the element default again
// each time it is handed out so a discarded write never shows through a later
// access.
template <ElementPolicy Elem>
[[nodiscard]] auto DiscardTarget(const Elem& elem, void*& slot) -> void* {
  if (slot == nullptr) {
    slot = CopyElement(elem, elem.Default());
  } else {
    elem.Assign(slot, elem.Default());
  }
  return slot;
}

}  // namespace lyra::value
