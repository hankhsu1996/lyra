#include "lyra/mir/source_location_pool.hpp"

#include <gtest/gtest.h>

#include "lyra/diag/source_manager.hpp"
#include "lyra/mir/source_location_id.hpp"

namespace lyra::mir {
namespace {

// A location is what it says, so two operations written at one place are one
// constant, and any difference in what is said is another.
TEST(MirSourceLocationPoolTest, ALocationIsWhatItSays) {
  const SourceLocationPool pool;
  const diag::SourcePosition at{.file = "a.sv", .line = 1, .column = 2};
  const SourceLocationId first = pool.Intern(at);
  const SourceLocationId again = pool.Intern(at);
  EXPECT_EQ(first, again);
  EXPECT_NE(first, pool.Intern({.file = "b.sv", .line = 1, .column = 2}));
  EXPECT_NE(first, pool.Intern({.file = "a.sv", .line = 3, .column = 2}));
  EXPECT_NE(first, pool.Intern({.file = "a.sv", .line = 1, .column = 4}));
  EXPECT_EQ(pool.size(), 4U);
}

}  // namespace
}  // namespace lyra::mir
