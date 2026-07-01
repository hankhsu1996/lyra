#include "lyra/mir/type_interner.hpp"

#include <cstddef>
#include <cstdint>
#include <functional>
#include <type_traits>
#include <utility>
#include <variant>

#include "lyra/mir/type.hpp"

namespace lyra::mir {

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

// Hashes a packed array's semantic shape, excluding its slang syntactic-origin
// marker: the marker carries no semantic content, so two arrays differing only
// there must hash equal to share one canonical id.
void HashPackedShape(std::size_t& seed, const PackedArrayType& packed) {
  HashCombine(seed, std::hash<int>{}(static_cast<int>(packed.atom)));
  HashCombine(seed, std::hash<int>{}(static_cast<int>(packed.signedness)));
  for (const PackedRange& dim : packed.dims) {
    HashCombine(seed, std::hash<std::int64_t>{}(dim.left));
    HashCombine(seed, std::hash<std::int64_t>{}(dim.right));
  }
}

auto PackedShapeEqual(const PackedArrayType& a, const PackedArrayType& b)
    -> bool {
  return a.atom == b.atom && a.signedness == b.signedness && a.dims == b.dims;
}

}  // namespace

auto SemanticTypeHash::operator()(const TypeData& data) const -> std::size_t {
  std::size_t seed = std::hash<std::size_t>{}(data.index());
  std::visit(
      [&](const auto& t) {
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
          HashField(seed, t.size);
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
          HashField(seed, t.unit_name);
        } else if constexpr (std::is_same_v<T, MachineIntType>) {
          HashField(seed, t.bit_width);
          HashCombine(seed, std::hash<int>{}(static_cast<int>(t.signedness)));
        } else if constexpr (std::is_same_v<T, RuntimeLibraryType>) {
          HashCombine(seed, std::hash<int>{}(static_cast<int>(t.kind)));
        } else if constexpr (std::is_same_v<T, CoroutineType>) {
          HashId(seed, t.payload);
        } else if constexpr (std::is_same_v<T, ClosureRecordType>) {
          HashField(seed, t.record_id.value);
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
        } else if constexpr (std::is_same_v<T, ObservableType>) {
          HashId(seed, t.value);
        } else if constexpr (std::is_same_v<T, ExternalRefType>) {
          HashId(seed, t.element);
        } else if constexpr (std::is_same_v<T, ResolvedType>) {
          HashId(seed, t.value);
        } else if constexpr (std::is_same_v<T, DriverType>) {
          HashId(seed, t.value);
        }
        // The remaining variants are parameter-less; the variant index above
        // is their whole identity.
      },
      data);
  return seed;
}

auto SemanticTypeEqual::operator()(const TypeData& a, const TypeData& b) const
    -> bool {
  if (a.index() != b.index()) {
    return false;
  }
  return std::visit(
      [&](const auto& lhs) -> bool {
        using T = std::decay_t<decltype(lhs)>;
        const auto& rhs = std::get<T>(b);
        // The two integral variants drop the non-semantic syntactic-origin
        // marker; every other variant's own equality already compares exactly
        // its semantic fields.
        if constexpr (std::is_same_v<T, PackedArrayType>) {
          return PackedShapeEqual(lhs, rhs);
        } else if constexpr (std::is_same_v<T, EnumType>) {
          return PackedShapeEqual(lhs.base, rhs.base) &&
                 lhs.members == rhs.members;
        } else {
          return lhs == rhs;
        }
      },
      a);
}

auto TypeInterner::Intern(TypeData data) -> TypeId {
  if (const auto it = index_.find(data); it != index_.end()) {
    return it->second;
  }
  const TypeId id = storage_.Add(Type{.data = data});
  index_.emplace(std::move(data), id);
  return id;
}

}  // namespace lyra::mir
