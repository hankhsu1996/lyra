#include "lyra/mir/integral_constant.hpp"

#include <cstddef>
#include <cstdint>
#include <vector>

#include "lyra/base/hash.hpp"

namespace lyra::mir {

namespace {

// A plane's length is folded in beside its words: two planes that share a
// prefix and differ in length are different bits, and without the length they
// would land on one hash.
void HashWords(std::size_t& seed, const std::vector<std::uint64_t>& words) {
  base::HashField(seed, words.size());
  for (const std::uint64_t word : words) {
    base::HashField(seed, word);
  }
}

}  // namespace

auto IntegralConstantDecl::Hash::operator()(
    const IntegralConstantDecl& decl) const -> std::size_t {
  std::size_t seed = 0;
  base::HashField(seed, decl.type.value);
  HashWords(seed, decl.value.value_words);
  HashWords(seed, decl.value.state_words);
  return seed;
}

}  // namespace lyra::mir
