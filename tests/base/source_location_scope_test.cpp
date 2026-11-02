#include <gtest/gtest.h>
#include <string>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/base/source_location.hpp"

namespace {

using lyra::InternalError;
using lyra::SimulationError;
using lyra::base::CurrentSourceLocation;
using lyra::base::kNoSourceLocation;
using lyra::base::SourceLocation;
using lyra::base::SourceLocationScope;

constexpr SourceLocation::Impl kOuterImpl{
    .file = "outer.sv", .line = 1, .column = 2};
constexpr SourceLocation::Impl kInnerImpl{
    .file = "inner.sv", .line = 3, .column = 4};
constexpr SourceLocation kOuter{&kOuterImpl};
constexpr SourceLocation kInner{&kInnerImpl};

auto RaisedPlace() -> SourceLocation {
  SourceLocation at;
  try {
    throw SimulationError("a failure of the design");
  } catch (const SimulationError& error) {
    at = error.At();
  }
  return at;
}

TEST(SourceLocationTest, TheDefaultValueIsNoPlace) {
  EXPECT_FALSE(SourceLocation{}.IsValid());
  EXPECT_TRUE(kOuter.IsValid());
  EXPECT_STREQ(kOuter.File(), "outer.sv");
  EXPECT_EQ(kOuter.Line(), 1U);
  EXPECT_EQ(kOuter.Column(), 2U);
  EXPECT_EQ(sizeof(SourceLocation), sizeof(void*));
}

TEST(SourceLocationScopeTest, AScopeBindsForItsExtent) {
  EXPECT_FALSE(CurrentSourceLocation().IsValid());
  {
    const SourceLocationScope bound(kOuter);
    EXPECT_EQ(CurrentSourceLocation(), kOuter);
  }
  EXPECT_FALSE(CurrentSourceLocation().IsValid());
}

TEST(SourceLocationScopeTest, ANestedScopeRestoresTheOuterPlace) {
  const SourceLocationScope outer(kOuter);
  {
    const SourceLocationScope inner(kInner);
    EXPECT_EQ(CurrentSourceLocation(), kInner);
  }
  EXPECT_EQ(CurrentSourceLocation(), kOuter);
}

TEST(SourceLocationScopeTest, AScopeHandedNoPlaceLeavesTheOuterOneBound) {
  const SourceLocationScope outer(kOuter);
  {
    const SourceLocationScope handed_none{SourceLocation{}};
    EXPECT_EQ(CurrentSourceLocation(), kOuter);
    {
      const SourceLocationScope inner(kInner);
      EXPECT_EQ(CurrentSourceLocation(), kInner);
    }
    EXPECT_EQ(CurrentSourceLocation(), kOuter);
  }
  EXPECT_EQ(CurrentSourceLocation(), kOuter);
}

TEST(SourceLocationScopeTest, AScopeRestoresWhenItIsUnwoundOutOf) {
  try {
    const SourceLocationScope bound(kOuter);
    throw SimulationError("a failure of the design");
  } catch (const SimulationError& error) {
    EXPECT_EQ(error.At(), kOuter);
  }
  EXPECT_FALSE(CurrentSourceLocation().IsValid());
}

TEST(SimulationErrorTest, ARaiseKeepsThePlaceBoundWhereItWasRaised) {
  const SourceLocationScope outer(kOuter);
  EXPECT_EQ(RaisedPlace(), kOuter);
  const SourceLocationScope inner(kInner);
  EXPECT_EQ(RaisedPlace(), kInner);
}

TEST(SimulationErrorTest, ARaiseHandedNoPlaceIsAtTheOneBound) {
  const SourceLocationScope outer(kOuter);
  const SourceLocationScope handed_none{SourceLocation{}};
  EXPECT_EQ(RaisedPlace(), kOuter);
}

TEST(SimulationErrorTest, ARaiseUnderNoBindingIsAFailureOfTheTool) {
  try {
    throw SimulationError("a failure of the design");
  } catch (const InternalError& error) {
    EXPECT_NE(
        std::string(error.what()).find("a failure of the design"),
        std::string::npos);
    return;
  } catch (const SimulationError&) {
    FAIL() << "a raise under no binding was taken for the design's";
  }
  FAIL() << "nothing was raised";
}

TEST(SimulationErrorTest, ARaiseStatingNoLocationReadsNoPlaceWhateverIsBound) {
  const auto raised = [] {
    SourceLocation at;
    try {
      throw SimulationError(kNoSourceLocation, "a failure of the run");
    } catch (const SimulationError& error) {
      at = error.At();
    }
    return at;
  };
  EXPECT_FALSE(raised().IsValid());
  const SourceLocationScope bound(kOuter);
  EXPECT_FALSE(raised().IsValid());
}

}  // namespace
