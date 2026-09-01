#include "lyra/mir/type.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <type_traits>

#include "lyra/base/internal_error.hpp"

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
      HashField(seed, t.bit_width);
      HashCombine(seed, std::hash<int>{}(static_cast<int>(t.signedness)));
    } else if constexpr (std::is_same_v<T, MachineFloatType>) {
      HashField(seed, t.bit_width);
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
      HashField(seed, t.resolution);
    } else if constexpr (std::is_same_v<T, DriverType>) {
      HashId(seed, t.value);
      HashField(seed, t.resolution);
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

}  // namespace lyra::mir
