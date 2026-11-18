#include "lyra/hir/typespec.hpp"

#include <cstddef>
#include <variant>
#include <vector>

#include "lyra/base/hash.hpp"
#include "lyra/base/overloaded.hpp"

namespace lyra::hir {

namespace {

void HashMembers(std::size_t& seed, const std::vector<TypespecId>& members) {
  for (const TypespecId member : members) {
    base::HashField(seed, member);
  }
}

}  // namespace

auto Typespec::Hash::operator()(const Typespec& typespec) const -> std::size_t {
  std::size_t seed = typespec.data.index();
  std::visit(
      Overloaded{
          [&](const IntegerTypespec& integer) {
            base::HashField(seed, static_cast<int>(integer.keyword));
            base::HashField(seed, integer.is_signed);
          },
          [&](const BitTypespec& bit) {
            base::HashField(seed, static_cast<int>(bit.keyword));
            base::HashField(seed, bit.is_signed);
          },
          [&](const PackedArrayTypespec& array) {
            base::HashField(seed, array.range.left);
            base::HashField(seed, array.range.right);
            base::HashField(seed, array.element);
            base::HashField(seed, array.is_signed);
          },
          [&](const PackedStructTypespec& aggregate) {
            base::HashField(seed, aggregate.type);
            base::HashField(seed, aggregate.is_signed);
            HashMembers(seed, aggregate.members);
          },
          [&](const PackedUnionTypespec& aggregate) {
            base::HashField(seed, aggregate.type);
            base::HashField(seed, aggregate.is_signed);
            HashMembers(seed, aggregate.members);
            base::HashField(seed, aggregate.soft);
          },
          [&](const EnumTypespec& enumeration) {
            base::HashField(seed, enumeration.base);
            base::HashField(seed, enumeration.enumeration);
          },
          [&](const PlainTypespec& plain) {
            base::HashField(seed, static_cast<int>(plain.kind));
          },
          [&](const ClassTypespec& cls) { base::HashField(seed, cls.name); },
          [&](const UnpackedAggregateTypespec& aggregate) {
            base::HashField(seed, aggregate.type);
            HashMembers(seed, aggregate.members);
          },
          [&](const FixedArrayTypespec& array) {
            base::HashField(seed, array.range.left);
            base::HashField(seed, array.range.right);
            base::HashField(seed, array.element);
          },
          [&](const DynamicArrayTypespec& array) {
            base::HashField(seed, array.element);
          },
          [&](const QueueTypespec& queue) {
            base::HashField(seed, queue.element);
          },
          [&](const AssociativeArrayTypespec& array) {
            base::HashField(seed, array.element);
            if (array.index.has_value()) {
              base::HashField(seed, *array.index);
            }
          },
          [&](const TypedefTypespec& named) {
            base::HashField(seed, named.name);
            base::HashField(seed, named.names);
            base::HashField(seed, named.aliases);
          }},
      typespec.data);
  return seed;
}

}  // namespace lyra::hir
