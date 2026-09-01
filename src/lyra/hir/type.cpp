#include "lyra/hir/type.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/hir/class_ref.hpp"
#include "lyra/hir/type_id.hpp"

namespace lyra::hir {

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

// A packed array's elements read most significant first from the dimension's
// left end (LRM 7.4.1), so the element the dimension names first is offset
// zero however the range was declared -- which is the order a concatenation of
// those elements takes them in, and the reverse of index order whenever the
// dimension descends.
auto PackedRange::LinearOffset(std::int64_t index) const -> std::uint64_t {
  if (!Contains(index)) {
    throw InternalError("PackedRange::LinearOffset: index out of range");
  }
  if (left >= right) {
    return static_cast<std::uint64_t>(left - index);
  }
  return static_cast<std::uint64_t>(index - left);
}

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

auto Type::Hash::operator()(const Type& type) const -> std::size_t {
  const std::size_t seed = Combine(std::size_t{0}, type.data_.index());
  return type.Visit(
      Overloaded{
          [seed](const ScalarBitType& t) {
            return Combine(seed, static_cast<std::size_t>(t.atom));
          },
          [seed](const PackedArrayType& t) {
            return Combine(
                Combine(HashTypeId(seed, t.element_type), t.dim.left),
                t.dim.right);
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
          [seed](const UnitObjectType& t) {
            return Combine(seed, t.unit_name);
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
          [seed](const VoidType&) { return seed; }});
}

auto Type::IsBitVector() const -> bool {
  return Is<ScalarBitType>() || Is<PackedArrayType>();
}

auto Type::IsValueChangeObservable() const -> bool {
  // One arm per HIR type and no catch-all, so a type added later fails to
  // compile here until it says which side it is on.
  return Visit(
      Overloaded{
          [](const ScalarBitType&) { return true; },
          [](const PackedArrayType&) { return true; },
          [](const PackedStructType&) { return true; },
          [](const PackedUnionType&) { return true; },
          [](const EnumType&) { return true; },
          [](const UnpackedStructType&) { return true; },
          [](const UnpackedUnionType&) { return true; },
          [](const UnpackedArrayType&) { return true; },
          [](const DynamicArrayType&) { return true; },
          [](const QueueType&) { return true; },
          [](const AssociativeArrayType&) { return true; },
          [](const StringType&) { return true; },
          [](const RealType&) { return true; },
          [](const ShortRealType&) { return true; },
          [](const RealTimeType&) { return true; },
          [](const WildcardIndexType&) { return false; },
          [](const EventType&) { return false; },
          [](const ChandleType&) { return false; },
          [](const ClassHandleType&) { return false; },
          [](const ImportedClassHandleType&) { return false; },
          [](const UnitObjectType&) { return false; },
          [](const NullType&) { return false; },
          [](const VoidType&) { return false; },
      });
}

}  // namespace lyra::hir
