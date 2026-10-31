#pragma once

#include <algorithm>
#include <concepts>
#include <cstddef>
#include <cstdint>
#include <memory>
#include <new>
#include <span>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/value/bit_vec.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/container_lease.hpp"
#include "lyra/value/frame_bound.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/layout.hpp"
#include "lyra/value/small_box.hpp"

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
// Comparing two elements and streaming one (LRM 11.4.5, 6.24.3) are not the
// policy's: an algorithm doing either is handed them by whoever asks, since a
// container holds its element type only for what its storage needs.
//
// A policy also round-trips through an opaque address, as LLVM's
// `PointerIntPair` does through `getOpaqueValue`, so storage a container
// detached can be ended after the container is gone: whatever the address
// names lasts as long as the program.
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
      { p.BitIdentical(in, in) } -> std::same_as<bool>;
      { p.OpaqueValue() } -> std::same_as<const void*>;
      { P::FromOpaqueValue(in) } -> std::same_as<P>;
    };

// A type whose default is the one its declaration states (LRM 7.2.2, Table
// 7-1) rather than what building one from nothing makes: a structure or a
// union the source declared, and an array of either. C++ builds that default
// only from the function stating it, since a constructor handing on to another
// builds a copy first.
template <typename T>
concept HasDeclaredDefault = requires { T::DeclaredDefault(); };

namespace detail {

// The two below are of the element type alone, so every array of one element
// type shares them. A type with a declared default is built at it where it is
// kept; every other type's is what its own default construction builds.
template <typename T>
[[nodiscard]] auto DefaultOf() -> const T& {
  static const T element_default = [] {
    if constexpr (HasDeclaredDefault<T>) {
      return T::DeclaredDefault();
    } else {
      return T{};
    }
  }();
  return element_default;
}

template <typename T>
[[nodiscard]] auto DiscardedWriteOf() -> T& {
  static T discarded;
  discarded = DefaultOf<T>();
  return discarded;
}

}  // namespace detail

// The element is the C++ type `T`.
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

  // The type is the whole of this policy, so its opaque value is null.
  [[nodiscard]] static auto OpaqueValue() -> const void* {
    return nullptr;
  }
  [[nodiscard]] static auto FromOpaqueValue(const void* opaque) -> StaticElem {
    if (opaque != nullptr) {
      throw InternalError(
          "an element policy over a C++ type is rebuilt from an opaque value "
          "it never gave -- please report this as a bug");
    }
    return {};
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

  // The element written into a stream below its `filled` most significant
  // positions, answering how many are filled after it. A structure states its
  // stream as a value of its own -- a run of bits where a part's width is the
  // running program's -- and an array writes its elements in turn.
  static auto WriteToStream(
      const void* value, const IntegralView& stream, std::uint64_t filled)
      -> std::uint64_t {
    if constexpr (IntegralValue<T>) {
      return lyra::value::WriteToStream(Of(value), stream, filled);
    } else if constexpr (requires {
                           Of(value).WriteToStream(stream, filled);
                         }) {
      return Of(value).WriteToStream(stream, filled);
    } else if constexpr (requires { Of(value).ToBitVec(); }) {
      const BitVec run = Of(value).ToBitVec();
      Insert(
          stream, run.View(),
          static_cast<std::int64_t>(stream.width - filled - run.View().width));
      return filled + run.View().width;
    } else {
      return lyra::value::WriteToStream(
          Of(value).ToBitstream(), stream, filled);
    }
  }

  // The inverse: the element a stream holds below its `taken` most significant
  // positions, read into the element lying at `out`, which it replaces. An
  // element whose type fixes its width takes that many positions, and one
  // whose width the running program decides takes every position before
  // `until`. Answers how many are taken after it.
  static auto ReadFromStream(
      const ConstIntegralView& stream, std::uint64_t taken, std::uint64_t until,
      void* out) -> std::uint64_t {
    T& element = *static_cast<T*>(out);
    if constexpr (IntegralValue<T>) {
      return lyra::value::ReadFromStream(stream, taken, element);
    } else if constexpr (requires {
                           element.ReadFromStream(stream, taken, until);
                         }) {
      return element.ReadFromStream(stream, taken, until);
    } else if constexpr (requires(const BitVec& run) { T::FromBitVec(run); }) {
      const BitVec run = BitVec::Of(stream, taken, until);
      RebuildFrom(element, [&] { return T::FromBitVec(run); });
      return until;
    } else {
      // A structure's stream is as wide as the structure, so its bits are kept
      // as a frame keeps a value aside, and the structure they make is built
      // where the element lies. Nothing on the way can fail: storage that
      // cannot be had ends the run.
      using Bits = decltype(std::declval<const T&>().ToBitstream());
      SmallBox<Bits> bits;
      const std::uint64_t after =
          lyra::value::ReadFromStream(stream, taken, *bits);
      RebuildFrom(element, [&] { return T::FromBitstream(*bits); });
      return after;
    }
  }

  // How many positions an element takes where every part of it whose width
  // the running program decides is empty, which is what each of those after
  // the first such part takes (LRM 11.4.14.4).
  [[nodiscard]] static auto EmptyStreamWidth() -> std::uint64_t {
    return static_cast<std::uint64_t>(BitstreamWidth(Default()));
  }

 private:
  [[nodiscard]] static auto Of(const void* value) -> const T& {
    return *static_cast<const T*>(value);
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

namespace detail {

// How an element a container keeps in storage of its own is laid out: the
// element at the start, and after it the count of leases on it.
struct SlotShape {
  std::size_t element_size;
  std::size_t element_align;

  [[nodiscard]] auto CountAt() const -> std::size_t {
    return AlignTo(element_size, alignof(LeaseCount));
  }
  [[nodiscard]] auto Size() const -> std::size_t {
    return CountAt() + sizeof(LeaseCount);
  }
  [[nodiscard]] auto Align() const -> std::size_t {
    return std::max(element_align, alignof(LeaseCount));
  }
  [[nodiscard]] auto CountOf(void* slot) const -> LeaseCount& {
    return CountLyingAt(
        std::span(static_cast<std::byte*>(slot), Size())
            .subspan(CountAt())
            .data());
  }

 private:
  [[nodiscard]] static auto CountLyingAt(void* at) -> LeaseCount& {
    return *static_cast<LeaseCount*>(at);
  }
};

template <ElementPolicy Elem>
[[nodiscard]] auto ShapeOf(const Elem& elem) -> SlotShape {
  return SlotShape{.element_size = elem.Size(), .element_align = elem.Align()};
}

// Gives back the storage of the element at `slot`, which holds no value.
inline void DeleteSlot(const SlotShape& shape, void* slot) {
  std::destroy_at(&shape.CountOf(slot));
  ::operator delete(slot, std::align_val_t{shape.Align()});
}

// Ends an element its container detached while a call leased it.
template <ElementPolicy Elem>
void EndDetachedSlot(void* slot, const void* element) {
  const Elem elem = Elem::FromOpaqueValue(element);
  elem.Destroy(slot);
  DeleteSlot(ShapeOf(elem), slot);
}

}  // namespace detail

// Storage of its own holding a copy of `value`, which a call may lease.
template <ElementPolicy Elem>
[[nodiscard]] auto CopyElement(const Elem& elem, const void* value) -> void* {
  const detail::SlotShape shape = detail::ShapeOf(elem);
  StorageFor slot(shape);
  std::construct_at(&shape.CountOf(slot.Get()));
  elem.Copy(value, slot.Get());
  return slot.Release();
}

// The container lets the element at `slot` go: it is ended and its storage
// given back, unless a call leases it, which detaches it and leaves ending it
// to the last lease (LRM 13.5.2).
template <ElementPolicy Elem>
void FreeElement(const Elem& elem, void* slot) {
  const detail::SlotShape shape = detail::ShapeOf(elem);
  LeaseCount& count = shape.CountOf(slot);
  if (count.held > 0) {
    count.detached = true;
    return;
  }
  elem.Destroy(slot);
  detail::DeleteSlot(shape, slot);
}

// The value of the element at `slot`, built in `out`, as the container lets
// the element go: moved out where nothing leases it, copied where a call does,
// since that call goes on reading it.
template <ElementPolicy Elem>
void TakeElement(const Elem& elem, void* slot, void* out) {
  if (detail::ShapeOf(elem).CountOf(slot).held > 0) {
    elem.Copy(slot, out);
  } else {
    elem.Move(slot, out);
  }
}

// A lease a call takes on the element at `slot`, which a container keeps in
// storage of its own.
template <ElementPolicy Elem>
[[nodiscard]] auto LeaseElement(const Elem& elem, void* slot)
    -> ContainerLease {
  return ContainerLease(
      detail::ShapeOf(elem).CountOf(slot), slot, &detail::EndDetachedSlot<Elem>,
      elem.OpaqueValue());
}

}  // namespace lyra::value
