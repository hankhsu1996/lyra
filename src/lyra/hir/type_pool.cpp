#include "lyra/hir/type_pool.hpp"

#include <cstddef>
#include <functional>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/overloaded.hpp"
#include "lyra/hir/class_ref.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/type_id.hpp"

namespace lyra::hir {

namespace {

template <typename T>
auto Combine(std::size_t seed, const T& value) -> std::size_t {
  // The mixing constant and shifts are Boost's `hash_combine`, which spreads
  // the low-entropy inputs here -- small ids, enum tags, container sizes --
  // across the whole word.
  return seed ^
         (std::hash<T>{}(value) + 0x9e3779b9 + (seed << 6) + (seed >> 2));
}

auto HashTypeId(std::size_t seed, TypeId id) -> std::size_t {
  return Combine(seed, id.value);
}

auto HashClassRef(std::size_t seed, const ClassRef& ref) -> std::size_t {
  return std::visit(
      Overloaded{
          [seed](const LocalClassRef& local) {
            return Combine(seed, local.class_id.value);
          },
          [seed](const ExternalClassRef& external) {
            return Combine(
                Combine(seed, external.unit_name), external.class_name);
          }},
      ref);
}

// The identities a field list names. Field names are left to equality: two
// aggregates whose members have the same types in the same order are rare
// enough that separating them by name buys nothing.
template <typename Field>
auto HashFields(std::size_t seed, const std::vector<Field>& fields)
    -> std::size_t {
  std::size_t hash = Combine(seed, fields.size());
  for (const Field& field : fields) {
    hash = HashTypeId(hash, field.type);
  }
  return hash;
}

}  // namespace

auto TypeDataHash::operator()(const TypeData& data) const -> std::size_t {
  const std::size_t seed = Combine(std::size_t{0}, data.index());
  return std::visit(
      Overloaded{
          [seed](const ScalarBitType& t) {
            return Combine(seed, static_cast<std::size_t>(t.atom));
          },
          [seed](const PackedArrayType& t) {
            return Combine(
                Combine(
                    Combine(HashTypeId(seed, t.element_type), t.dim.left),
                    t.dim.right),
                static_cast<std::size_t>(t.form));
          },
          [seed](const PackedStructType& t) {
            return HashFields(seed, t.fields);
          },
          [seed](const PackedUnionType& t) {
            return HashFields(seed, t.fields);
          },
          [seed](const EnumType& t) {
            return Combine(HashTypeId(seed, t.base_type), t.members.size());
          },
          [seed](const UnpackedStructType& t) {
            return HashFields(seed, t.fields);
          },
          [seed](const UnpackedUnionType& t) {
            return HashFields(seed, t.fields);
          },
          [seed](const UnpackedArrayType& t) {
            return Combine(
                Combine(HashTypeId(seed, t.element_type), t.dim.left),
                t.dim.right);
          },
          [seed](const DynamicArrayType& t) {
            return HashTypeId(seed, t.element_type);
          },
          [seed](const QueueType& t) {
            return Combine(
                HashTypeId(seed, t.element_type), t.max_bound.value_or(0));
          },
          [seed](const AssociativeArrayType& t) {
            return HashTypeId(HashTypeId(seed, t.element_type), t.key_type);
          },
          [seed](const ClassHandleType& t) {
            return HashClassRef(seed, t.class_ref);
          },
          [seed](const ImportedClassHandleType& t) {
            return Combine(seed, static_cast<std::size_t>(t.klass));
          },
          // A type carrying nothing beyond being itself is separated by its arm
          // alone, which the seed already holds.
          [seed](const WildcardIndexType&) { return seed; },
          [seed](const StringType&) { return seed; },
          [seed](const EventType&) { return seed; },
          [seed](const RealType&) { return seed; },
          [seed](const ShortRealType&) { return seed; },
          [seed](const RealTimeType&) { return seed; },
          [seed](const ChandleType&) { return seed; },
          [seed](const NullType&) { return seed; },
          [seed](const VoidType&) { return seed; }},
      data);
}

auto TypePool::Intern(TypeData data) -> TypeId {
  if (const auto it = interned_.find(data); it != interned_.end()) {
    return it->second;
  }
  const TypeId id = types_.Add(Type{.data = data});
  interned_.emplace(std::move(data), id);
  return id;
}

}  // namespace lyra::hir
