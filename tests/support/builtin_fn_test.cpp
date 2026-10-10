#include "lyra/support/builtin_fn.hpp"

#include <cstdint>
#include <gtest/gtest.h>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/support/integral_operation.hpp"

namespace lyra::support {
namespace {

// An entry holds a fixed number of operand readings, and one stating more is
// refused where it is written rather than laid out past what an entry holds.
TEST(OperandReadingsTest, RefusesMoreReadingsThanAnEntryHolds) {
  using enum OperandReading;
  const OperandReadings most = {kHeld, kBits,     kNumber,
                                kHeld, kPosition, kMachine};
  EXPECT_EQ(most.All().size(), 6U);
  EXPECT_THROW(
      (OperandReadings{
          kHeld, kBits, kNumber, kHeld, kPosition, kMachine, kMachine}),
      InternalError);
}

// Every entry, found by asking for each one's declaration until the set
// refuses a value.
auto EveryEntry() -> std::vector<BuiltinFn> {
  std::vector<BuiltinFn> all;
  for (std::uint32_t i = 0;; ++i) {
    const auto fn = static_cast<BuiltinFn>(i);
    try {
      if (RuntimeEntryOf(fn).name.empty()) {
        return all;
      }
    } catch (const InternalError&) {
      return all;
    }
    all.push_back(fn);
  }
}

// An entry is over values of one kind, so it has one declaration: an operation
// over integral values states neither readings of its own nor another entry
// to stand in for it, and the entry a source-level one names for integral
// values is such an operation.
TEST(RuntimeEntryTest, AnEntryOverIntegralValuesIsThatOperationAlone) {
  const std::vector<BuiltinFn> entries = EveryEntry();
  ASSERT_GT(entries.size(), 300U);
  for (const BuiltinFn fn : entries) {
    const RuntimeEntry entry = RuntimeEntryOf(fn);
    if (entry.integral.has_value()) {
      EXPECT_TRUE(entry.operands.All().empty()) << entry.name;
      EXPECT_FALSE(entry.over_integral_values.has_value()) << entry.name;
      EXPECT_EQ(entry.name, IntegralOperationOf(*entry.integral).name);
    }
    if (entry.over_integral_values.has_value()) {
      EXPECT_TRUE(
          RuntimeEntryOf(*entry.over_integral_values).integral.has_value())
          << entry.name;
    }
  }
}

}  // namespace
}  // namespace lyra::support
