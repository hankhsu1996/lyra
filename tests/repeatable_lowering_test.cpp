// Lowering one unchanged design twice states one thing. The written program
// repeating does not show it: nothing below the first semantic form reads where
// that form put each of its parts, so two lowerings that differ only there
// write the same text. What does read it is the comparison that decides whether
// two copies of one body are one compiled artifact, which holds two scopes
// equal position for position -- so a form that moves on its own costs sharing
// while every program stays right, and nothing a design simulates or a build
// writes reports it.
//
// What is compared is each unit's own form, by the equality sharing is decided
// with, in the order the design lists its units.
//
// The two lowerings also differ in which instances they read. The first reads
// the bodies the front end elaborated and names every other instance where it
// is written, as a build does. The second reads every instance's body and
// holds each to the unit it shares, so a design where an instance left unread
// would have lowered apart from its unit stops there, and the two are then
// held to having found the same units stating the same things.

#include <cstddef>
#include <gtest/gtest.h>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include <fmt/format.h>

#include "lyra/compiler/compile.hpp"
#include "lyra/compiler/lower_design.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/sink.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/dump.hpp"
#include "tests/compiled_twice.hpp"
#include "tests/framework/conformance_case.hpp"

namespace {

using lyra::support::BodiesRead;
using lyra::test::Compilation;
using lyra::test::ConformanceCase;
using lyra::test::Elaborate;
using lyra::test::FirstDifference;
using lyra::test::kWide;
using lyra::test::ScratchDirectory;

using LoweredUnits = std::vector<lyra::hir::CompilationUnit>;

// Every unit of this compilation's design, reading the instances `bodies_read`
// says and lowering `width` units at once. Nothing where the design did not
// lower, which is what a design this path refuses comes to and is not this
// test's subject.
auto Lower(Compilation& compilation, std::size_t width, BodiesRead bodies_read)
    -> std::optional<LoweredUnits> {
  if (!compilation.front.elaborated.has_value()) {
    return std::nullopt;
  }

  lyra::diag::DiagnosticSink sink;
  auto design = lyra::compiler::DeclareUnits(
      std::move(compilation.front.elaborated->compilation),
      lyra::compiler::LoweringPolicy{
          .assertions = lyra::support::AssertionPolicy::kCheck,
          .bodies_read = bodies_read},
      sink);
  if (!design.has_value()) {
    return std::nullopt;
  }

  LoweredUnits units;
  lyra::compiler::LowerToHir(
      *design, sink, width,
      [](lyra::hir::CompilationUnit unit)
          -> lyra::diag::Result<lyra::hir::CompilationUnit> { return unit; },
      [&units](lyra::hir::CompilationUnit unit) {
        units.push_back(std::move(unit));
      });
  if (sink.HasErrors()) {
    return std::nullopt;
  }
  return units;
}

auto Describe(const LoweredUnits& first, const LoweredUnits& second)
    -> std::string {
  if (first.size() != second.size()) {
    return fmt::format(
        "the first lowering has {} units and the second {}", first.size(),
        second.size());
  }
  for (std::size_t at = 0; at < first.size(); ++at) {
    if (first[at] == second[at]) {
      continue;
    }
    return fmt::format(
        "unit '{}' differs at {}", first[at].name,
        FirstDifference(
            lyra::hir::DumpHir(first[at]), lyra::hir::DumpHir(second[at])));
  }
  return {};
}

class RepeatableLoweringTest : public testing::Test {
 public:
  explicit RepeatableLoweringTest(const ConformanceCase& test_case)
      : case_(&test_case) {
  }

  void TestBody() override {
    Compilation first = Elaborate(*case_);
    Compilation second = Elaborate(*case_);

    const std::optional<LoweredUnits> first_units =
        Lower(first, 1, BodiesRead::kElaborated);
    const std::optional<LoweredUnits> second_units =
        Lower(second, kWide, BodiesRead::kEveryInstance);
    if (!first_units.has_value() && !second_units.has_value()) {
      GTEST_SKIP() << "this path does not lower '" << case_->id << "'";
    }
    ASSERT_TRUE(first_units.has_value() && second_units.has_value())
        << "'" << case_->id << "' lowered one time and not the other";

    const std::string difference = Describe(*first_units, *second_units);
    EXPECT_TRUE(difference.empty())
        << "lowering '" << case_->id
        << "' twice stated two things: " << difference;
  }

 private:
  const ConformanceCase* case_;
};

// The design written to show an ordering, held to the same claim and to one
// more: it has to lower. A corpus design that does not is a path's stated
// refusal and no concern of this check, but this one exists so that something
// here is known to be sensitive to an ordering, and a skip would leave that
// silently untrue.
TEST(RepeatableLowering, ADesignWrittenToShowAnOrderingStatesOneThing) {
  const ScratchDirectory scratch("wide-sensitivity-lowering");
  const ConformanceCase design =
      lyra::test::WriteWideSensitivityDesign(scratch.Under("source"));

  Compilation first = Elaborate(design);
  Compilation second = Elaborate(design);

  const std::optional<LoweredUnits> first_units =
      Lower(first, 1, BodiesRead::kElaborated);
  const std::optional<LoweredUnits> second_units =
      Lower(second, kWide, BodiesRead::kEveryInstance);
  ASSERT_TRUE(first_units.has_value() && second_units.has_value())
      << "the design written to exercise this check no longer lowers";

  const std::string difference = Describe(*first_units, *second_units);
  EXPECT_TRUE(difference.empty())
      << "lowering it twice stated two things: " << difference;
}

}  // namespace

auto main(int argc, char** argv) -> int {
  return lyra::test::RunOverCorpus(
      argc, argv, "a lowered form",
      [](const ConformanceCase& test_case) -> testing::Test* {
        // NOLINTNEXTLINE(cppcoreguidelines-owning-memory)
        return new RepeatableLoweringTest(test_case);
      });
}
