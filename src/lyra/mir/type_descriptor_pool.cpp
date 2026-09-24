#include "lyra/mir/type_descriptor_pool.hpp"

#include <cstddef>
#include <variant>

#include "lyra/base/hash.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::mir {

auto TypeDescriptionHash::operator()(const TypeDescription& description) const
    -> std::size_t {
  std::size_t seed = 0;
  base::HashField(seed, description.index());
  std::visit(
      Overloaded{
          [&seed](const PackedArrayType& packed) {
            HashPackedShape(seed, packed);
          },
          [&seed](const UnpackedRange& range) {
            base::HashField(seed, range.left);
            base::HashField(seed, range.right);
          },
          [&seed](const EnumType& enumeration) {
            HashEnumeration(seed, enumeration);
          }},
      description);
  return seed;
}

}  // namespace lyra::mir
