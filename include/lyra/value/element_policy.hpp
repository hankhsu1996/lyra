#pragma once

#include <concepts>
#include <cstddef>
#include <memory>
#include <new>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/value/any_value.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/net_resolution.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/value_type.hpp"

namespace lyra::value {

// What a container's own algorithms ask of one of its elements, each element
// passed as its address. A container is written once over this, and compiled
// twice: with the element's C++ type, where every answer is that type's own,
// and in the library, where the answers come from the element type's table.
//
// The element default (LRM Table 7-1) is the policy's to hold, since it is a
// property of the element type: what a read naming no element answers with and
// what a new element starts as.
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
      { p.Equal(in, in) } -> std::same_as<PackedArray>;
      { p.CaseEqual(in, in) } -> std::same_as<PackedArray>;
      { p.BitIdentical(in, in) } -> std::same_as<bool>;
      { p.HasUnknown(in) } -> std::same_as<bool>;
      { p.BitstreamWidth(in) } -> std::same_as<PackedArray>;
      {
        p.CountBits(in, std::declval<const PackedArray&>())
      } -> std::same_as<PackedArray>;
    };

// The element is the C++ type `T`. A `T` such as a packed value carries its
// width in the value rather than in the type, so a `T` cannot be built with
// the shape its declaration gives it, and the default arrives as a value of
// `T`, carrying that shape.
template <LyraValue T>
class StaticElem {
 public:
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
      -> PackedArray {
    return Of(lhs) == Of(rhs);
  }
  [[nodiscard]] static auto CaseEqual(const void* lhs, const void* rhs)
      -> PackedArray {
    return Of(lhs).CaseEqual(Of(rhs));
  }
  [[nodiscard]] static auto BitIdentical(const void* lhs, const void* rhs)
      -> bool {
    return Of(lhs).IsBitIdentical(Of(rhs));
  }
  [[nodiscard]] static auto HasUnknown(const void* value) -> bool {
    return Of(value).HasUnknown();
  }
  [[nodiscard]] static auto BitstreamWidth(const void* value) -> PackedArray {
    return Of(value).BitstreamWidth();
  }
  [[nodiscard]] static auto CountBits(
      const void* value, const PackedArray& control_bits) -> PackedArray {
    return Of(value).CountBits(control_bits);
  }

  // What an array asks of its elements as a whole value of its own: as a net
  // (LRM 6.6, 28.12.1, 6.7.1) and as a stream of bits (LRM 6.24.3). Each
  // answers in `out`.
  static void Resolve(
      NetResolution fold, const void* lhs, const void* rhs, void* out) {
    Build(out, ResolvedUnder(fold, Of(lhs), Of(rhs)));
  }
  static void Dominating(const void* stronger, const void* weaker, void* out) {
    Build(out, Of(stronger).Dominating(Of(weaker)));
  }
  static void FilledLike(
      const void* prototype, const PackedArray& fill, void* out) {
    Build(out, T::FilledLike(Of(prototype), fill));
  }
  [[nodiscard]] static auto ToBitstream(const void* value) -> PackedArray {
    return Of(value).ToBitstream();
  }
  static void FromBitstream(
      const PackedArray& bits, const void* prototype, void* out) {
    Build(out, T::FromBitstream(bits, Of(prototype)));
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

// The element is of a type the library was compiled without, answered by its
// type's table. As for a C++ element type, the default arrives as a value of
// the type, at `element_default`.
class WitnessedElem {
 public:
  WitnessedElem(const ValueType& type, const void* element_default)
      : type_(&type), default_(AnyValue::CopyOf(type, element_default)) {
  }

  [[nodiscard]] auto Type() const -> const ValueType& {
    return *type_;
  }

  [[nodiscard]] auto Size() const -> std::size_t {
    return type_->Size();
  }
  [[nodiscard]] auto Align() const -> std::size_t {
    return type_->Align();
  }

  [[nodiscard]] auto Default() const -> const void* {
    return default_.Bytes();
  }

  void Copy(const void* value, void* out) const {
    type_->Copy(value, out);
  }
  void Move(void* value, void* out) const {
    type_->Move(value, out);
  }
  void Destroy(void* value) const {
    type_->Destroy(value);
  }
  void Assign(void* storage, const void* value) const {
    type_->Assign(storage, value);
  }

  [[nodiscard]] auto Equal(const void* lhs, const void* rhs) const
      -> PackedArray {
    return Answered([&](void* out) { type_->Equal(lhs, rhs, out); });
  }
  [[nodiscard]] auto CaseEqual(const void* lhs, const void* rhs) const
      -> PackedArray {
    return Answered([&](void* out) { type_->CaseEqual(lhs, rhs, out); });
  }
  [[nodiscard]] auto BitIdentical(const void* lhs, const void* rhs) const
      -> bool {
    return type_->BitIdentical(lhs, rhs);
  }
  [[nodiscard]] auto HasUnknown(const void* value) const -> bool {
    return type_->HasUnknown(value);
  }
  [[nodiscard]] auto BitstreamWidth(const void* value) const -> PackedArray {
    return Answered([&](void* out) { type_->BitstreamWidth(value, out); });
  }
  [[nodiscard]] auto CountBits(
      const void* value, const PackedArray& control_bits) const -> PackedArray {
    return Answered(
        [&](void* out) { type_->CountBits(value, &control_bits, out); });
  }

  void Resolve(
      NetResolution fold, const void* lhs, const void* rhs, void* out) const {
    switch (fold) {
      case NetResolution::kTriState:
        type_->ResolveTriState(lhs, rhs, out);
        return;
      case NetResolution::kWiredAnd:
        type_->ResolveWiredAnd(lhs, rhs, out);
        return;
      case NetResolution::kWiredOr:
        type_->ResolveWiredOr(lhs, rhs, out);
        return;
    }
    throw InternalError("WitnessedElem: unknown net resolution");
  }
  void Dominating(const void* stronger, const void* weaker, void* out) const {
    type_->Dominating(stronger, weaker, out);
  }
  void FilledLike(
      const void* prototype, const PackedArray& fill, void* out) const {
    type_->FilledLike(prototype, &fill, out);
  }
  [[nodiscard]] auto ToBitstream(const void* value) const -> PackedArray {
    return Answered([&](void* out) { type_->ToBitstream(value, out); });
  }
  void FromBitstream(
      const PackedArray& bits, const void* prototype, void* out) const {
    type_->FromBitstream(&bits, prototype, out);
  }

 private:
  template <typename Build>
  [[nodiscard]] static auto Answered(Build build) -> PackedArray {
    return TakeBuilt<PackedArray>(build);
  }

  const ValueType* type_;
  AnyValue default_;
};

static_assert(ElementPolicy<StaticElem<PackedArray>>);
static_assert(ElementPolicy<WitnessedElem>);

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
