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
    return std::ranges::contains(ImplOf(unit, type)->Items(), item);
  };
  EXPECT_TRUE(has_item(of_reals, support::ValueStep::kCopy));
  EXPECT_TRUE(has_item(of_ints, support::StreamItem::kToBitVec));
  EXPECT_FALSE(has_item(of_reals, support::StreamItem::kToBitVec));

  EXPECT_FALSE(ImplOf(unit, unit.builtins.int_type).has_value());
  EXPECT_FALSE(ImplOf(unit, unit.builtins.string).has_value());
}

}  // namespace
}  // namespace lyra::mir
