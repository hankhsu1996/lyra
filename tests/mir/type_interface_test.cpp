#include "lyra/mir/type_interface.hpp"

#include <algorithm>
#include <gtest/gtest.h>
#include <optional>

#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/library_decl.hpp"
#include "lyra/support/library_trait.hpp"
#include "lyra/support/value_operation.hpp"
#include "lyra/support/value_step.hpp"

namespace lyra::mir {
namespace {

// A run-time-sized container implements every trait of the library its
// declaration lists, its stream only where its elements stream (LRM 6.24.3),
// so its implementation's items are exactly those traits' requirements; a
// type whose declaration lists none implements none.
TEST(MirImplTest, AContainerImplementsTheTraitsItsElementsAllow) {
  CompilationUnit unit;
  unit.name = "U";
  const TypeId of_ints =
      QueueOf(unit.types, unit.builtins.int_type, support::UnboundedQueue{});
  const TypeId of_reals = DynamicArrayOf(unit.types, unit.builtins.real);
  const auto implements = [&](TypeId type, const support::LibraryTrait& trait) {
    const std::optional<Impl> impl = ImplOf(unit, type);
    return impl.has_value() && std::ranges::contains(impl->traits, &trait);
  };
  for (const TypeId type : {of_ints, of_reals}) {
    EXPECT_TRUE(implements(type, support::kValueLife));
    EXPECT_TRUE(implements(type, support::kChangeTest));
    EXPECT_TRUE(implements(type, support::kElementAccess));
    EXPECT_TRUE(implements(type, support::kResize));
    EXPECT_TRUE(implements(type, support::kWholeValueQuestions));
  }
  EXPECT_TRUE(implements(of_ints, support::kStream));
  EXPECT_FALSE(implements(of_reals, support::kStream));

  const auto has_item = [&](TypeId type, support::ValueOperation item) {
    return std::ranges::contains(ImplOf(unit, type)->items, item);
  };
  EXPECT_TRUE(has_item(of_reals, support::ValueStep::kCopy));
  EXPECT_TRUE(has_item(of_ints, support::StreamItem::kToBitVec));
  EXPECT_FALSE(has_item(of_reals, support::StreamItem::kToBitVec));

  EXPECT_FALSE(ImplOf(unit, unit.builtins.int_type).has_value());
  EXPECT_FALSE(ImplOf(unit, unit.builtins.string).has_value());
}

// The library asks each item of a value it holds apart from the rest, so a
// fixed-size array has as items each operation of its interface some trait
// requires, whether or not it has the rest of that trait: an array of
// associative arrays streams as a run of bits it cannot be read back from
// (LRM 6.24.3), and still answers how wide that run is. Its steps are its
// elements', so it states none, and the merge under an ambiguous condition is
// no trait's.
TEST(MirImplTest, AFixedSizeArrayHasEachItemItsInterfaceDeclares) {
  CompilationUnit unit;
  unit.name = "U";
  const auto array_of = [&](TypeId element) {
    return unit.types.Intern(
        Type{UnpackedArrayType{.element_type = element, .count = 2}});
  };
  const TypeId of_ints = array_of(unit.builtins.int_type);
  const TypeId of_maps = array_of(AssociativeArrayOf(
      unit.types, unit.builtins.int_type, unit.builtins.int_type));
  const auto has_item = [&](TypeId type, support::ValueOperation item) {
    return std::ranges::contains(ImplOf(unit, type)->items, item);
  };
  const auto implements = [&](TypeId type, const support::LibraryTrait& trait) {
    return std::ranges::contains(ImplOf(unit, type)->traits, &trait);
  };

  for (const TypeId type : {of_ints, of_maps}) {
    EXPECT_TRUE(has_item(type, support::DefaultItem::kDeclaredDefault));
    EXPECT_TRUE(has_item(type, support::ChangeTestItem::kBitIdentical));
    EXPECT_TRUE(has_item(type, support::StreamItem::kBitstreamWidth));
    EXPECT_FALSE(has_item(type, support::ValueStep::kCopy));
    EXPECT_FALSE(has_item(type, support::UnknownItem::kMergeConditional));
    EXPECT_FALSE(implements(type, support::kValueLife));
    EXPECT_TRUE(implements(type, support::kWholeValueQuestions));
  }
  EXPECT_TRUE(has_item(of_ints, support::StreamItem::kToBitstream));
  EXPECT_TRUE(has_item(of_ints, support::PositionsItem::kWritePositions));
  EXPECT_TRUE(implements(of_ints, support::kFixedStream));
  EXPECT_TRUE(has_item(of_maps, support::StreamItem::kToBitVec));
  EXPECT_FALSE(has_item(of_maps, support::StreamItem::kFromBitVec));
  EXPECT_FALSE(implements(of_maps, support::kStream));
}

}  // namespace
}  // namespace lyra::mir
