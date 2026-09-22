#include "lyra/mir/type_descriptor_pool.hpp"

#include <cstddef>
#include <cstdint>
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
            base::HashField(
                seed, static_cast<std::uint64_t>(packed.state_kind));
            base::HashField(
                seed, static_cast<std::uint64_t>(packed.signedness));
            base::HashField(seed, packed.dims.size());
            for (const PackedRange& dim : packed.dims) {
              base::HashField(seed, dim.left);
              base::HashField(seed, dim.right);
            }
          },
          [&seed](const UnpackedRange& range) {
            base::HashField(seed, range.left);
            base::HashField(seed, range.right);
          }},
      description);
  return seed;
}

}  // namespace lyra::mir
