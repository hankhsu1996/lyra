#include "lyra/value/tuple.hpp"

#include <array>
#include <cstddef>
#include <gtest/gtest.h>
#include <type_traits>
#include <utility>

#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"

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

// A product answers each operation on its whole value as its components do
// (LRM 11.4.5, 20.9), so the product of no components, which holds nothing, is
// equal, case-equal and bit-identical to another and holds no unknown, and a
// product holding an unknown compares unknown where no component is unequal.
TEST(Tuple, AnswersEachWholeValueOperationAsItsComponentsDo) {
  const Tuple<> none;
  const Tuple<> other;
  static_assert(std::is_same_v<decltype(none == other), Bit>);
  EXPECT_EQ((none == other).Lsb(), FourStateBit::kOne);
  EXPECT_EQ((none != other).Lsb(), FourStateBit::kZero);
  EXPECT_TRUE(none.CaseEqual(other).IsTruthy());
  EXPECT_TRUE(none.IsBitIdentical(other));
  EXPECT_FALSE(none.HasUnknown());

  using Pair = Tuple<Int, Logic>;
  const Pair known(Int::FromInt(3), Logic::Filled(FourStateBit::kOne));
  const Pair unknown(Int::FromInt(3), Logic::Filled(FourStateBit::kUnknown));
  const Pair unequal(Int::FromInt(4), Logic::Filled(FourStateBit::kUnknown));
  static_assert(std::is_same_v<decltype(known == unknown), Logic>);
  EXPECT_EQ((known == known).Lsb(), FourStateBit::kOne);
  EXPECT_EQ((known == unknown).Lsb(), FourStateBit::kUnknown);
  EXPECT_EQ((known == unequal).Lsb(), FourStateBit::kZero);
  EXPECT_TRUE(unknown.CaseEqual(unknown).IsTruthy());
  EXPECT_FALSE(known.CaseEqual(unknown).IsTruthy());
  EXPECT_TRUE(unknown.IsBitIdentical(unknown));
  EXPECT_FALSE(known.IsBitIdentical(unknown));
  EXPECT_FALSE(known.HasUnknown());
  EXPECT_TRUE(unknown.HasUnknown());
}

}  // namespace
}  // namespace lyra::value
