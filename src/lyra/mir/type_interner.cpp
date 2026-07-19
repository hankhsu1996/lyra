#include "lyra/mir/type_interner.hpp"

#include <cstddef>
#include <cstdint>
#include <functional>
#include <type_traits>
#include <utility>
#include <variant>

#include "lyra/base/internal_error.hpp"
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
          HashField(seed, t.unit_name);
        } else if constexpr (std::is_same_v<T, ExternalClassType>) {
          HashField(seed, t.qualified_name);
        } else if constexpr (std::is_same_v<T, MachineIntType>) {
          HashField(seed, t.bit_width);
          HashCombine(seed, std::hash<int>{}(static_cast<int>(t.signedness)));
        } else if constexpr (std::is_same_v<T, MachineFloatType>) {
          HashField(seed, t.bit_width);
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
        } else if constexpr (std::is_same_v<T, ObservableType>) {
          HashId(seed, t.value);
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

auto TypeInterner::ObservableCellOf(TypeId value_type) -> TypeId {
  // Exhaustive over TypeKind with no default: a MIR type added later fails to
  // compile here until it is classified, rather than silently defaulting to
  // non-observable -- which would leave a new value type's writes firing no
  // subscribers, a bug no test would obviously catch.
  switch (Get(value_type).Kind()) {
    // A SystemVerilog value-storage data object (LRM 6.5): a variable of one of
    // these is an observable signal, so it is wrapped in the cell that fires
    // subscribers on change.
    case TypeKind::kPackedArray:
    case TypeKind::kEnum:
    case TypeKind::kUnpackedArray:
    case TypeKind::kDynamicArray:
    case TypeKind::kQueue:
    case TypeKind::kAssociativeArray:
    case TypeKind::kString:
    case TypeKind::kReal:
    case TypeKind::kShortReal:
    case TypeKind::kRealTime:
    case TypeKind::kTuple:
    case TypeKind::kUnion:
      return Intern(ObservableType{.value = value_type});
    // Not value storage, so its own declaration shape is its storage and it is
    // not wrapped: a handle or container (pointer, managed / borrowed
    // reference, vector, chandle), an object (a class instance or an
    // instantiated child), a named event (LRM 15 -- it carries its own
    // subscribe mechanism), a runtime facade (services, files, diagnostics, a
    // runtime-library type), a coroutine result, a machine primitive (a machine
    // int / float / C string used at an ABI boundary), a compiler-generated
    // promoted scope struct or closure, an internal index, `void`, and the
    // observable / net-cell wrappers themselves, which are already storage
    // cells.
    case TypeKind::kWildcardIndex:
    case TypeKind::kMachineCString:
    case TypeKind::kMachineInt:
    case TypeKind::kMachineFloat:
    case TypeKind::kEvent:
    case TypeKind::kChandle:
    case TypeKind::kVoid:
    case TypeKind::kObject:
    case TypeKind::kExternalUnitObject:
    case TypeKind::kExternalClass:
    case TypeKind::kServices:
    case TypeKind::kFiles:
    case TypeKind::kDiagnostic:
    case TypeKind::kRuntimeLibrary:
    case TypeKind::kCoroutine:
    case TypeKind::kReference:
    case TypeKind::kPointer:
    case TypeKind::kManagedRef:
    case TypeKind::kVector:
    case TypeKind::kObservable:
    case TypeKind::kResolved:
    case TypeKind::kDriver:
    case TypeKind::kStruct:
    case TypeKind::kClosure:
      return value_type;
  }
  throw InternalError("TypeInterner::ObservableCellOf: unhandled TypeKind");
}

}  // namespace lyra::mir
