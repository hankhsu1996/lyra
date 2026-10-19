#include "lyra/value/tuple.hpp"

#include <array>
#include <cstddef>
#include <gtest/gtest.h>
#include <utility>

namespace lyra::value {
namespace {

// A structure may declare more members than a compiler lets one expression
// range over (LRM 7.2 bounds no count), so a product of that many components is
// built from that many values like any other.
template <std::size_t>
using IntAt = int;

template <typename Positions>
struct ProductOfPositions;

template <std::size_t... I>
struct ProductOfPositions<std::index_sequence<I...>> {
  static auto Build() {
    return Tuple<IntAt<I>...>(static_cast<int>(I)...);
  }

  // Every component read, as emitted code reaches each member, into a list
  // rather than by one expression over all of them.
  static auto ReadEvery(const Tuple<IntAt<I>...>& product)
      -> std::array<int, sizeof...(I)> {
    return {product.template Component<I>()...};
  }
};

// One past the deepest nesting either compiler allows by default, clang's
// 1,024 instantiations.
constexpr std::size_t kMoreThanAnyNesting = 1025;

TEST(Tuple, BuildsAProductOfMoreComponentsThanAnExpressionMayRangeOver) {
  using Product =
      ProductOfPositions<std::make_index_sequence<kMoreThanAnyNesting>>;
  auto product = Product::Build();
  const std::array<int, kMoreThanAnyNesting> read = Product::ReadEvery(product);
  for (std::size_t i = 0; i < read.size(); ++i) {
    EXPECT_EQ(read.at(i), static_cast<int>(i));
  }
  product.Component<kMoreThanAnyNesting - 1>() = 5;
  EXPECT_EQ(std::move(product).Component<kMoreThanAnyNesting - 1>(), 5);
}

// Building from one value that is itself the product is a copy of it, never
// the product taken as its one component's value.
TEST(Tuple, CopiesAProductOfOneComponent) {
  Tuple<int> product(7);
  const Tuple<int> copy(product);
  EXPECT_EQ(copy.Component<0>(), 7);
}

}  // namespace
}  // namespace lyra::value
