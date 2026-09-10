#include "lyra/mir/type.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <type_traits>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"

namespace lyra::mir {

auto PackedRange::ElementCount() const -> std::uint64_t {
  const std::int64_t span = (left >= right) ? (left - right) : (right - left);
  return static_cast<std::uint64_t>(span) + 1U;
}

auto PackedRange::IsAscending() const -> bool {
  return left <= right;
}

auto PackedRange::Contains(std::int64_t index) const -> bool {
  const std::int64_t lo = std::min(left, right);
  const std::int64_t hi = std::max(left, right);
  return index >= lo && index <= hi;
}

auto PackedArrayType::BitWidth() const -> std::uint64_t {
  if (dims.empty()) {
    return 1U;
  }
  std::uint64_t width = 1U;
  for (const auto& dim : dims) {
    width *= dim.ElementCount();
  }
  return width;
}

auto BitsOf(MachineIntWidth width) -> std::uint32_t {
  switch (width) {
    case MachineIntWidth::k8:
      return 8;
    case MachineIntWidth::k16:
      return 16;
    case MachineIntWidth::k32:
      return 32;
    case MachineIntWidth::k64:
      return 64;
  }
  throw InternalError("mir: unknown MachineIntWidth");
}

auto BitsOf(MachineFloatWidth width) -> std::uint32_t {
  switch (width) {
    case MachineFloatWidth::k32:
      return 32;
    case MachineFloatWidth::k64:
      return 64;
  }
  throw InternalError("mir: unknown MachineFloatWidth");
}

namespace {

void HashCombine(std::size_t& seed, std::size_t value) {
  seed ^= value + 0x9e3779b97f4a7c15ULL + (seed << 6) + (seed >> 2);
}

template <typename T>
void HashField(std::size_t& seed, const T& value) {
  HashCombine(seed, std::hash<T>{}(value));
}

void HashId(std::size_t& seed, TypeId id) {
  HashCombine(seed, std::hash<std::uint32_t>{}(id.value));
}

// Hashes a packed array by the three attributes that decide what it means --
// four-stateness, signedness, and the dimensions its width comes from.
void HashPackedShape(std::size_t& seed, const PackedArrayType& packed) {
  HashCombine(seed, std::hash<int>{}(static_cast<int>(packed.state_kind)));
  HashCombine(seed, std::hash<int>{}(static_cast<int>(packed.signedness)));
  for (const PackedRange& dim : packed.dims) {
    HashCombine(seed, std::hash<std::int64_t>{}(dim.left));
    HashCombine(seed, std::hash<std::int64_t>{}(dim.right));
  }
}

}  // namespace

auto Type::Hash::operator()(const Type& type) const -> std::size_t {
  std::size_t seed = std::hash<std::size_t>{}(type.data_.index());
  type.Visit([&](const auto& t) {
    using T = std::decay_t<decltype(t)>;
    if constexpr (std::is_same_v<T, PackedArrayType>) {
      HashPackedShape(seed, t);
    } else if constexpr (std::is_same_v<T, EnumType>) {
      HashPackedShape(seed, t.base);
      for (const EnumMember& m : t.members) {
        HashField(seed, m.name);
        HashField(seed, m.value);
      }
    } else if constexpr (std::is_same_v<T, UnpackedArrayType>) {
      HashId(seed, t.element_type);
      HashField(seed, t.dim.left);
      HashField(seed, t.dim.right);
    } else if constexpr (std::is_same_v<T, DynamicArrayType>) {
      HashId(seed, t.element_type);
    } else if constexpr (std::is_same_v<T, QueueType>) {
      HashId(seed, t.element_type);
      if (t.max_bound) {
        HashField(seed, *t.max_bound);
      }
    } else if constexpr (std::is_same_v<T, AssociativeArrayType>) {
      HashId(seed, t.element_type);
      HashId(seed, t.key_type);
    } else if constexpr (std::is_same_v<T, ObjectType>) {
      HashField(seed, t.class_id.value);
    } else if constexpr (std::is_same_v<T, ExternalUnitObjectType>) {
      HashField(seed, t.object.value);
    } else if constexpr (std::is_same_v<T, CrossUnitClassType>) {
      HashField(seed, t.unit_name);
      HashField(seed, t.class_name);
    } else if constexpr (std::is_same_v<T, RuntimeClassType>) {
      HashField(seed, t.symbol);
    } else if constexpr (std::is_same_v<T, MachineIntType>) {
      HashCombine(seed, std::hash<int>{}(static_cast<int>(t.width)));
      HashCombine(seed, std::hash<int>{}(static_cast<int>(t.signedness)));
    } else if constexpr (std::is_same_v<T, MachineFloatType>) {
      HashCombine(seed, std::hash<int>{}(static_cast<int>(t.width)));
    } else if constexpr (std::is_same_v<T, MachineArrayType>) {
      HashId(seed, t.element);
      HashField(seed, t.size);
    } else if constexpr (std::is_same_v<T, MachineFunctionType>) {
      for (TypeId param : t.params) {
        HashId(seed, param);
      }
      HashId(seed, t.result);
    } else if constexpr (std::is_same_v<T, RuntimeLibraryType>) {
      HashCombine(seed, std::hash<int>{}(static_cast<int>(t.kind)));
    } else if constexpr (std::is_same_v<T, CoroutineType>) {
      HashId(seed, t.payload);
    } else if constexpr (std::is_same_v<T, ClosureType>) {
      HashField(seed, t.closure_id.value);
    } else if constexpr (std::is_same_v<T, StructType>) {
      HashField(seed, t.struct_id.value);
    } else if constexpr (std::is_same_v<T, RefType>) {
      HashId(seed, t.pointee);
      HashCombine(seed, std::hash<int>{}(static_cast<int>(t.mutability)));
    } else if constexpr (std::is_same_v<T, PointerType>) {
      HashId(seed, t.pointee);
      HashCombine(seed, std::hash<int>{}(static_cast<int>(t.ownership)));
      HashCombine(seed, std::hash<int>{}(static_cast<int>(t.mutability)));
    } else if constexpr (std::is_same_v<T, ManagedRefType>) {
      HashId(seed, t.pointee);
    } else if constexpr (std::is_same_v<T, VectorType>) {
      HashId(seed, t.element);
    } else if constexpr (std::is_same_v<T, TupleType>) {
      for (TypeId element : t.elements) {
        HashId(seed, element);
      }
    } else if constexpr (std::is_same_v<T, UnionType>) {
      for (TypeId element : t.elements) {
        HashId(seed, element);
      }
    } else if constexpr (std::is_same_v<T, TaggedUnionType>) {
      for (TypeId element : t.elements) {
        HashId(seed, element);
      }
    } else if constexpr (std::is_same_v<T, ObservableType>) {
      HashId(seed, t.value);
    } else if constexpr (std::is_same_v<T, ResolvedType>) {
      HashId(seed, t.value);
    } else if constexpr (std::is_same_v<T, DriverType>) {
      HashId(seed, t.value);
    } else if constexpr (std::is_same_v<T, SampledHistoryType>) {
      HashId(seed, t.value);
    }
    // The remaining variants are parameter-less; the variant index above
    // is their whole identity.
  });
  return seed;
}

auto Type::IsIntegralPacked() const -> bool {
  return Is<PackedArrayType>() || Is<EnumType>();
}

auto Type::PackedShape() const -> const PackedArrayType& {
  if (const auto* packed = As<PackedArrayType>()) {
    return *packed;
  }
  if (const auto* enumeration = As<EnumType>()) {
    return enumeration->base;
  }
  throw InternalError("mir: type has no packed shape; it is not integral");
}

auto Type::IsRealFamily() const -> bool {
  return Is<RealType>() || Is<ShortRealType>() || Is<RealTimeType>();
}

auto Type::IsAliasHandle() const -> bool {
  return Is<RuntimeEffectsType>() || Is<FilesType>() || Is<DiagnosticType>();
}

auto Type::IsCapabilityWrapper() const -> bool {
  return Is<ObservableType>() || Is<RefType>() || Is<ResolvedType>() ||
         Is<DriverType>();
}

auto Type::WrappedValueType() const -> TypeId {
  if (const auto* observable = As<ObservableType>()) {
    return observable->value;
  }
  if (const auto* reference = As<RefType>()) {
    return reference->pointee;
  }
  if (const auto* resolved = As<ResolvedType>()) {
    return resolved->value;
  }
  if (const auto* driver = As<DriverType>()) {
    return driver->value;
  }
  throw InternalError("mir: type is not a capability wrapper");
}

auto Type::HeldValueTypes() const -> std::vector<TypeId> {
  using Held = std::vector<TypeId>;
  return Visit(
      Overloaded{
          // An integral value is one vector of bits, and an enumeration is that
          // vector under a set of names. Both are indivisible, as is every
          // other value that is a single quantity, a single token, or nothing.
          [](const PackedArrayType&) -> Held { return {}; },
          [](const EnumType&) -> Held { return {}; },
          [](const WildcardIndexType&) -> Held { return {}; },
          [](const StringType&) -> Held { return {}; },
          [](const MachineCStringType&) -> Held { return {}; },
          [](const MachineBoolType&) -> Held { return {}; },
          [](const MachineIntType&) -> Held { return {}; },
          [](const MachineFloatType&) -> Held { return {}; },
          [](const RealType&) -> Held { return {}; },
          [](const ShortRealType&) -> Held { return {}; },
          [](const RealTimeType&) -> Held { return {}; },
          [](const ChandleType&) -> Held { return {}; },
          [](const EventType&) -> Held { return {}; },
          [](const EmptyType&) -> Held { return {}; },
          [](const VoidType&) -> Held { return {}; },

          // A container holds its elements, and a keyed one holds its keys
          // beside them.
          [](const UnpackedArrayType& t) -> Held { return {t.element_type}; },
          [](const DynamicArrayType& t) -> Held { return {t.element_type}; },
          [](const QueueType& t) -> Held { return {t.element_type}; },
          [](const AssociativeArrayType& t) -> Held {
            return {t.key_type, t.element_type};
          },
          [](const MachineArrayType& t) -> Held { return {t.element}; },
          [](const VectorType& t) -> Held { return {t.element}; },

          // A product holds every component at once; a union and a tagged sum
          // hold one at a time, which is still one of these.
          [](const TupleType& t) -> Held { return t.elements; },
          [](const UnionType& t) -> Held { return t.elements; },
          [](const TaggedUnionType& t) -> Held { return t.elements; },

          // A cell holds the value it keeps, however it publishes a change to
          // it and however far back it remembers.
          [](const ObservableType& t) -> Held { return {t.value}; },
          [](const ResolvedType& t) -> Held { return {t.value}; },
          [](const DriverType& t) -> Held { return {t.value}; },
          [](const SampledHistoryType& t) -> Held { return {t.value}; },

          // What a concurrent assertion has in flight is member storage like
          // those, and holds no value of the design at all: the words in it
          // mean something only to the transition that reads them.
          [](const EvaluationAttemptsType&) -> Held { return {}; },

          // These refer to a value living elsewhere rather than holding one:
          // copying the referring value copies no part of what it reaches. A
          // coroutine's payload is what awaiting it produces, not something it
          // carries, and a code address names a signature it is not made of.
          [](const RefType&) -> Held { return {}; },
          [](const PointerType&) -> Held { return {}; },
          [](const ManagedRefType&) -> Held { return {}; },
          [](const CoroutineType&) -> Held { return {}; },
          [](const MachineFunctionType&) -> Held { return {}; },

          // A nominal type names a declaration, and the declaration is what
          // lists the members; the type states none, so a walk that must reach
          // them goes to the registry the id resolves in.
          [](const ObjectType&) -> Held { return {}; },
          [](const ExternalUnitObjectType&) -> Held { return {}; },
          [](const CrossUnitClassType&) -> Held { return {}; },
          [](const RuntimeClassType&) -> Held { return {}; },
          [](const StructType&) -> Held { return {}; },
          [](const ClosureType&) -> Held { return {}; },

          // A handle to a runtime facility, and an inert payload the library
          // owns the shape of. Neither is composed of values this layer names.
          [](const RuntimeEffectsType&) -> Held { return {}; },
          [](const FilesType&) -> Held { return {}; },
          [](const DiagnosticType&) -> Held { return {}; },
          [](const RuntimeLibraryType&) -> Held { return {}; }});
}

}  // namespace lyra::mir
