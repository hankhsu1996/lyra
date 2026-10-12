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
// The element default (LRM Table 7-1) is the policy's to answer, since it is a
// property of the element type: what a read naming no element answers with and
// what a new element starts as. So is where a write naming no element lands
// (LRM 7.4.5): storage no read reaches, holding the default again each time it
// is handed out so a discarded write never shows through a later one.
//
// A comparison of two elements answers 0, 1 or x as a scalar, which a container
// folds over its elements and whoever knows the type the comparison has holds
// as a value of it. A stream of bits (LRM 6.24.3) is an integral value, which
// a policy writes one element's bits into and reads one element back out of.
template <typename P>
concept ElementPolicy =
    std::copyable<P> &&
    requires(const P& p, void* out, const void* in, void* from) {
      { p.Size() } -> std::same_as<std::size_t>;
      { p.Align() } -> std::same_as<std::size_t>;
      { p.Default() } -> std::same_as<const void*>;
      { p.DiscardedWrite() } -> std::same_as<void*>;
      p.Copy(in, out);
      p.Move(from, out);
      p.Destroy(from);
      p.Assign(from, in);
      { p.Agree(in, in) } -> std::same_as<bool>;
      { p.BitIdentical(in, in) } -> std::same_as<bool>;
      { p.HasUnknown(in) } -> std::same_as<bool>;
    };

namespace detail {

// The two below are of the element type alone, so every array of one element
// type shares them.
template <typename T>
[[nodiscard]] auto DefaultOf() -> const T& {
  static const T element_default{};
  return element_default;
}

template <typename T>
[[nodiscard]] auto DiscardedWriteOf() -> T& {
  static T discarded;
  discarded = DefaultOf<T>();
  return discarded;
}

}  // namespace detail

// The element is the C++ type `T`, whose default is what its own default
// construction builds.
template <LyraValue T>
class StaticElem {
 public:
  // What comparing two elements answers (LRM 11.4.5): one bit, unknown where
  // the element can hold x or z.
  using Equality =
      decltype(std::declval<const T&>() == std::declval<const T&>());

  [[nodiscard]] static constexpr auto Size() -> std::size_t {
    return sizeof(T);
  }
  [[nodiscard]] static constexpr auto Align() -> std::size_t {
    return alignof(T);
  }

  [[nodiscard]] static auto Default() -> const void* {
    return &detail::DefaultOf<T>();
  }
  [[nodiscard]] static auto DiscardedWrite() -> void* {
    return &detail::DiscardedWriteOf<T>();
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
      const void* value, const IntegralView& stream, std::uint64_t filled)
      -> std::uint64_t {
    if constexpr (IntegralValue<T>) {
      return lyra::value::WriteToStream(Of(value), stream, filled);
    } else if constexpr (requires {
                           Of(value).WriteToStream(stream, filled);
                         }) {
      return Of(value).WriteToStream(stream, filled);
    } else {
      return lyra::value::WriteToStream(
          Of(value).ToBitstream(), stream, filled);
    }
  }

  // The inverse: the element a stream holds below its `taken` most significant
  // positions, built in `out`, `T` stating how wide it is. Answers how many
  // are taken after it.
  static auto ReadFromStream(
      const ConstIntegralView& stream, std::uint64_t taken, void* out)
      -> std::uint64_t {
    if constexpr (IntegralValue<T>) {
      Build(out, lyra::value::ReadFromStream<T>(stream, taken));
      return taken + T::kWidth;
    } else if constexpr (requires { T::ReadFromStream(stream, taken); }) {
      auto [element, after] = T::ReadFromStream(stream, taken);
      Build(out, std::move(element));
      return after;
    } else {
      using Bits = decltype(std::declval<const T&>().ToBitstream());
      Build(
          out,
          T::FromBitstream(lyra::value::ReadFromStream<Bits>(stream, taken)));
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
};

static_assert(ElementPolicy<StaticElem<Int>>);

// Storage for one value of a type that states its size and alignment, given
// back unless it is released: building the value may fail, and the storage is
// then nobody's.
class StorageFor {
 public:
  template <typename Type>
    requires requires(const Type& type) {
      { type.Size() } -> std::same_as<std::size_t>;
      { type.Align() } -> std::same_as<std::size_t>;
    }
  explicit StorageFor(const Type& type)
      : align_(std::align_val_t{type.Align()}),
        storage_(::operator new(type.Size(), align_)) {
  }
  StorageFor(const StorageFor&) = delete;
  auto operator=(const StorageFor&) -> StorageFor& = delete;
  StorageFor(StorageFor&&) = delete;
  auto operator=(StorageFor&&) -> StorageFor& = delete;
  ~StorageFor() {
    if (storage_ != nullptr) {
      ::operator delete(storage_, align_);
    }
  }

  [[nodiscard]] auto Get() const -> void* {
    return storage_;
  }
  [[nodiscard]] auto Release() -> void* {
    return std::exchange(storage_, nullptr);
  }

 private:
  std::align_val_t align_;
  void* storage_;
};

// Storage of its own holding a copy of `value`.
template <ElementPolicy Elem>
[[nodiscard]] auto CopyElement(const Elem& elem, const void* value) -> void* {
  StorageFor storage(elem);
  elem.Copy(value, storage.Get());
  return storage.Release();
}

// Ends the element at `slot` and gives its storage back.
template <ElementPolicy Elem>
void FreeElement(const Elem& elem, void* slot) {
  elem.Destroy(slot);
  ::operator delete(slot, std::align_val_t{elem.Align()});
}

}  // namespace lyra::value
