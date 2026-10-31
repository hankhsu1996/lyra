#include "lyra/abi/records.hpp"

#include <cstdint>
#include <gtest/gtest.h>

namespace lyra::abi {
namespace {

using Phase = void (*)();

struct Inner {
  std::int8_t first = 0;
  std::int8_t second = 0;
};

// Two members of one type, and a structure nested first, which lies where its
// own first member does.
struct Outer {
  Inner nested;
  Phase before = nullptr;
  Phase after = nullptr;
};

template <typename Visit>
void ForEachField(const Inner& record, const Visit& visit) {
  const auto& [first, second] = record;
  visit(first);
  visit(second);
}

template <typename Visit>
void ForEachField(const Outer& record, const Visit& visit) {
  const auto& [nested, before, after] = record;
  visit(nested);
  visit(before);
  visit(after);
}

// A member is found where it lies among its own record's members, so two of
// one type are told apart, and a nested structure is its record's member
// rather than its own first member's.
TEST(RecordsTest, AMemberIsFoundWhereItLiesAmongItsRecordsMembers) {
  EXPECT_EQ(PositionOf(&Outer::nested), 0U);
  EXPECT_EQ(PositionOf(&Outer::before), 1U);
  EXPECT_EQ(PositionOf(&Outer::after), 2U);
  EXPECT_EQ(PositionOf(&Inner::first), 0U);
  EXPECT_EQ(PositionOf(&Inner::second), 1U);
  EXPECT_EQ(PositionOf(&ScopeInfo::resolve), 2U);
  EXPECT_EQ(PositionOf(&ScopeInfo::initialize), 3U);
}

}  // namespace
}  // namespace lyra::abi
