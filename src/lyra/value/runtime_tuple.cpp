#include "lyra/value/runtime_tuple.hpp"

#include <array>
#include <bit>
#include <concepts>
#include <cstddef>
#include <cstdint>
#include <memory>
#include <new>
#include <span>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/support/value_domain.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/runtime_value.hpp"
#include "lyra/value/value_type.hpp"

namespace lyra::value {

namespace {

using support::ValueDomain;

// The domain a runtime value's alternative realizes. A component's domain is
// what its tuple type states, and a value built for it has to be of that
// domain.
template <typename T>
constexpr auto DomainOf() -> ValueDomain {
  if constexpr (std::same_as<T, PackedArray>) {
    return ValueDomain::kPacked;
  } else if constexpr (std::same_as<T, String>) {
    return ValueDomain::kString;
  } else if constexpr (std::same_as<T, Real>) {
    return ValueDomain::kReal;
  } else if constexpr (std::same_as<T, ShortReal>) {
    return ValueDomain::kShortReal;
  } else if constexpr (std::same_as<T, Chandle>) {
    return ValueDomain::kChandle;
  } else if constexpr (std::same_as<T, Empty>) {
    return ValueDomain::kEmpty;
  } else if constexpr (std::same_as<T, RuntimeTuple>) {
    return ValueDomain::kTuple;
  } else if constexpr (std::same_as<T, RuntimeUnion>) {
    return ValueDomain::kUnion;
  } else if constexpr (std::same_as<T, RuntimeTaggedUnion>) {
    return ValueDomain::kTaggedUnion;
  } else if constexpr (std::same_as<T, RuntimeDynamicArray>) {
    return ValueDomain::kDynArray;
  } else if constexpr (std::same_as<T, RuntimeUnpackedArray>) {
    return ValueDomain::kUnpackedArray;
  } else if constexpr (std::same_as<T, RuntimeQueue>) {
    return ValueDomain::kQueue;
  } else if constexpr (std::same_as<T, RuntimeAssociativeArray>) {
    return ValueDomain::kAssocArray;
  } else {
    static_assert(std::same_as<T, ObjectRef>);
    return ValueDomain::kManagedRef;
  }
}

// Where a component lies, `offset` bytes into its tuple.
auto At(void* tuple, std::uint32_t offset) -> void* {
  return std::bit_cast<void*>(std::bit_cast<std::uintptr_t>(tuple) + offset);
}

// A copy of the value of `domain` laid out at `at`.
auto ValueAt(ValueDomain domain, const void* at) -> RuntimeValue {
  const auto copy = [at]<typename T>(std::type_identity<T>) {
    return RuntimeValue{*static_cast<const T*>(at)};
  };
  switch (domain) {
    case ValueDomain::kPacked:
      return copy(std::type_identity<PackedArray>{});
    case ValueDomain::kString:
      return copy(std::type_identity<String>{});
    case ValueDomain::kReal:
      return copy(std::type_identity<Real>{});
    case ValueDomain::kShortReal:
      return copy(std::type_identity<ShortReal>{});
    case ValueDomain::kChandle:
      return copy(std::type_identity<Chandle>{});
    case ValueDomain::kEmpty:
      return copy(std::type_identity<Empty>{});
    case ValueDomain::kTuple:
      return RuntimeValue{RuntimeTuple::CopyOf(at)};
    case ValueDomain::kUnion:
      return copy(std::type_identity<RuntimeUnion>{});
    case ValueDomain::kTaggedUnion:
      return copy(std::type_identity<RuntimeTaggedUnion>{});
    case ValueDomain::kDynArray:
      return copy(std::type_identity<RuntimeDynamicArray>{});
    case ValueDomain::kUnpackedArray:
      return copy(std::type_identity<RuntimeUnpackedArray>{});
    case ValueDomain::kQueue:
      return copy(std::type_identity<RuntimeQueue>{});
    case ValueDomain::kAssocArray:
      return copy(std::type_identity<RuntimeAssociativeArray>{});
    case ValueDomain::kManagedRef:
      return copy(std::type_identity<ObjectRef>{});
  }
  throw InternalError("RuntimeTuple: unknown value domain");
}

auto TypeAt(const void* laid_out) noexcept -> const TupleType& {
  return **static_cast<const TupleType* const*>(laid_out);
}

// A value of type `T` that `build` constructs in storage handed to it, taken
// out of that storage.
template <typename T, typename Build>
auto Answered(Build build) -> T {
  alignas(T) std::array<std::byte, sizeof(T)> storage{};
  build(static_cast<void*>(storage.data()));
  T* built = std::launder(std::bit_cast<T*>(storage.data()));
  T answer = std::move(*built);
  std::destroy_at(built);
  return answer;
}

}  // namespace

void RuntimeTuple::End::operator()(void* laid_out) const noexcept {
  const TupleType& type = TypeAt(laid_out);
  type.Destroy(laid_out);
  Deallocate{std::align_val_t{type.Align()}}(laid_out);
}

RuntimeTuple::RuntimeTuple(const RuntimeTuple& other) {
  if (other.bytes_ != nullptr) {
    *this = CopyOf(other.Bytes());
  }
}

auto RuntimeTuple::operator=(const RuntimeTuple& other) -> RuntimeTuple& {
  if (this == &other) {
    return *this;
  }
  // Two tuples of one type assign component by component, into the storage
  // this already owns.
  if (bytes_ != nullptr && other.bytes_ != nullptr &&
      &Type() == &other.Type()) {
    Type().Assign(Bytes(), other.Bytes());
    return *this;
  }
  RuntimeTuple copy(other);
  return *this = std::move(copy);
}

auto RuntimeTuple::CopyOf(const void* laid_out) -> RuntimeTuple {
  const TupleType& type = TypeAt(laid_out);
  return Built(type, [&](void* out) { type.Copy(laid_out, out); });
}

auto RuntimeTuple::MovedFrom(void* laid_out) -> RuntimeTuple {
  const TupleType& type = TypeAt(laid_out);
  return Built(type, [&](void* out) { type.Move(laid_out, out); });
}

auto RuntimeTuple::LayOut(void* out, std::vector<RuntimeValue> components)
    -> void* {
  const std::span<const TupleComponent> stated = TypeAt(out).Components();
  if (components.size() != stated.size()) {
    throw InternalError(
        "RuntimeTuple: a tuple is built from a component count its type "
        "does not have");
  }
  for (std::size_t i = 0; i < components.size(); ++i) {
    void* at = At(out, stated[i].offset);
    std::visit(
        [&]<typename T>(T& value) {
          if (DomainOf<T>() != stated[i].domain) {
            throw InternalError(
                "RuntimeTuple: a component is built from a value of a domain "
                "its tuple type does not state");
          }
          if constexpr (std::same_as<T, RuntimeTuple>) {
            std::move(value).MoveInto(at);
          } else {
            std::construct_at(static_cast<T*>(at), std::move(value));
          }
        },
        components[i].value);
  }
  return out;
}

auto RuntimeTuple::Bytes() const -> const void* {
  return bytes_.get();
}

auto RuntimeTuple::Bytes() -> void* {
  return bytes_.get();
}

auto RuntimeTuple::ComponentAt(void* laid_out, std::size_t index) -> void* {
  const std::span<const TupleComponent> stated = TypeAt(laid_out).Components();
  if (index >= stated.size()) {
    throw InternalError("RuntimeTuple::ComponentAt: index out of range");
  }
  return At(laid_out, stated[index].offset);
}

auto RuntimeTuple::BitIdentical(const void* lhs, const void* rhs) -> bool {
  return TypeAt(lhs).BitIdentical(lhs, rhs);
}

void RuntimeTuple::AssignAt(void* storage, const void* value) {
  TypeAt(storage).Assign(storage, value);
}

auto RuntimeTuple::CopyInto(void* out) const -> void* {
  Type().Copy(Bytes(), out);
  return out;
}

auto RuntimeTuple::MoveInto(void* out) && -> void* {
  Type().Move(Bytes(), out);
  bytes_.reset();
  return out;
}

auto RuntimeTuple::Type() const -> const TupleType& {
  if (bytes_ == nullptr) {
    throw InternalError(
        "RuntimeTuple: a tuple is read from storage that holds none");
  }
  return TypeAt(Bytes());
}

auto RuntimeTuple::RawSize() const -> std::size_t {
  return Type().Components().size();
}

auto RuntimeTuple::Component(std::size_t index) const -> RuntimeValue {
  const std::span<const TupleComponent> stated = Type().Components();
  if (index >= stated.size()) {
    throw InternalError("RuntimeTuple::Component: index out of range");
  }
  return ValueAt(stated[index].domain, At(bytes_.get(), stated[index].offset));
}

auto RuntimeTuple::operator==(const RuntimeTuple& other) const -> PackedArray {
  return Answered<PackedArray>(
      [&](void* out) { Type().Equal(Bytes(), other.Bytes(), out); });
}

auto RuntimeTuple::operator!=(const RuntimeTuple& other) const -> PackedArray {
  return !(*this == other);
}

auto RuntimeTuple::CaseEqual(const RuntimeTuple& other) const -> PackedArray {
  return Answered<PackedArray>(
      [&](void* out) { Type().CaseEqual(Bytes(), other.Bytes(), out); });
}

auto RuntimeTuple::ResolveTriState(const RuntimeTuple& other) const
    -> RuntimeTuple {
  return FoldedBy(&ValueType::ResolveTriState, other);
}

auto RuntimeTuple::ResolveWiredAnd(const RuntimeTuple& other) const
    -> RuntimeTuple {
  return FoldedBy(&ValueType::ResolveWiredAnd, other);
}

auto RuntimeTuple::ResolveWiredOr(const RuntimeTuple& other) const
    -> RuntimeTuple {
  return FoldedBy(&ValueType::ResolveWiredOr, other);
}

auto RuntimeTuple::FoldedBy(Fold fold, const RuntimeTuple& other) const
    -> RuntimeTuple {
  return Built(
      Type(), [&](void* out) { (Type().*fold)(Bytes(), other.Bytes(), out); });
}

auto RuntimeTuple::Dominating(const RuntimeTuple& weaker) const
    -> RuntimeTuple {
  return Built(Type(), [&](void* out) {
    Type().Dominating(Bytes(), weaker.Bytes(), out);
  });
}

auto RuntimeTuple::FilledLike(
    const RuntimeTuple& prototype, const PackedArray& fill) -> RuntimeTuple {
  return Built(prototype.Type(), [&](void* out) {
    prototype.Type().FilledLike(prototype.Bytes(), &fill, out);
  });
}

auto RuntimeTuple::IsBitIdentical(const RuntimeTuple& other) const -> bool {
  return Type().BitIdentical(Bytes(), other.Bytes());
}

auto RuntimeTuple::HasUnknown() const -> bool {
  return Type().HasUnknown(Bytes());
}

auto RuntimeTuple::IsUnknown() const -> PackedArray {
  return PackedArray::Bit(HasUnknown());
}

auto RuntimeTuple::BitstreamWidth() const -> PackedArray {
  return Answered<PackedArray>(
      [&](void* out) { Type().BitstreamWidth(Bytes(), out); });
}

auto RuntimeTuple::CountBits(const PackedArray& control_bits) const
    -> PackedArray {
  return Answered<PackedArray>(
      [&](void* out) { Type().CountBits(Bytes(), &control_bits, out); });
}

auto RuntimeTuple::ToBitstream() const -> PackedArray {
  return Answered<PackedArray>(
      [&](void* out) { Type().ToBitstream(Bytes(), out); });
}

auto RuntimeTuple::FromBitstream(
    const PackedArray& bits, const RuntimeTuple& prototype) -> RuntimeTuple {
  return Built(prototype.Type(), [&](void* out) {
    prototype.Type().FromBitstream(&bits, prototype.Bytes(), out);
  });
}

}  // namespace lyra::value
