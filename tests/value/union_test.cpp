#include "lyra/value/union.hpp"

#include <array>
#include <cstddef>
#include <cstdint>
#include <gtest/gtest.h>
#include <string>
#include <type_traits>
#include <utility>

#include "lyra/value/tagged_union.hpp"

namespace lyra::value {
namespace {

// A union may declare more members than a compiler lets a template recurse
// (LRM 7.3 bounds no count): one past the deepest nesting either compiler
// allows by default, clang's 1,024 instantiations.
constexpr std::size_t kMoreThanAnyNesting = 1025;
constexpr std::size_t kLast = kMoreThanAnyNesting - 1;

template <std::size_t>
using IntAt = int;

template <typename Positions>
struct UnionsOfPositions;

// Every member an `int` but the last, which owns storage, so the tagged union
// copies through the member its tag names.
template <std::size_t... I>
struct UnionsOfPositions<std::index_sequence<I...>> {
  using Untagged = Union<IntAt<I>..., int>;
  using Tagged = TaggedUnion<IntAt<I>..., std::string>;

  // Every member but the last read, as emitted code reaches each member, into
  // a list rather than by one expression over all of them. Each reads the
  // bits the last one wrote (LRM 7.3).
  static auto ReadEvery(const Untagged& held) -> std::array<int, sizeof...(I)> {
    return {held.template Component<I>()...};
  }

  // Whether every member but the last is the type it was declared as, which
  // names each member's type as an access to it does; a tagged union's member
  // the tag does not name is not read.
  static constexpr std::array<bool, sizeof...(I)> kEachTaggedMemberNamed{
      std::is_same_v<typename Tagged::template Member<I>, int>...};
};

using Unions = UnionsOfPositions<std::make_index_sequence<kLast>>;

TEST(Union, HoldsMoreMembersThanATemplateMayRecurseOver) {
  Unions::Untagged held(std::in_place_index<kLast>, 7);
  EXPECT_EQ(held.Component<kLast>(), 7);
  for (const int read : Unions::ReadEvery(held)) {
    EXPECT_EQ(read, 7);
  }
}

TEST(TaggedUnion, HoldsMoreMembersThanATemplateMayRecurseOver) {
  const Unions::Tagged held(
      std::in_place_index<kLast>, std::string("the last member"));
  const Unions::Tagged copy(held);
  EXPECT_EQ(copy.Discriminant(), static_cast<std::int64_t>(kLast));
  EXPECT_EQ(copy.Component<kLast>(), "the last member");
  for (const bool named : Unions::kEachTaggedMemberNamed) {
    EXPECT_TRUE(named);
  }
}

}  // namespace
}  // namespace lyra::value
