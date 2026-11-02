// Lowering one unchanged design twice states one thing. The written program
// repeating does not show it: nothing below the first semantic form reads where
// that form put each of its parts, so two lowerings that differ only there
// write the same text. What does read it is holding two lowerings of one body
// equal position for position, so a form that moves on its own is caught here,
// and nothing a design simulates or a build writes reports it elsewhere.
//
// What is compared is each lowering's own form, by that equality, in the
// order the design lists them. The two differ in how many units lower at once,
// so an order that depends on which unit finished first is caught too.

#include <cstddef>
#include <gtest/gtest.h>
#include <memory>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include <fmt/format.h>

#include "conformance/framework/case.hpp"
#include "lyra/compiler/compile.hpp"
#include "lyra/compiler/lower_design.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/sink.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/dump.hpp"
#include "lyra/hir/lowering.hpp"
#include "tests/compiled_twice.hpp"

namespace {

using conformance::framework::Case;
using lyra::test::Compilation;
using lyra::test::Elaborate;
using lyra::test::FirstDifference;
using lyra::test::kWide;
using lyra::test::ScratchDirectory;

using LoweredUnits = std::vector<lyra::hir::Lowering>;

// Every lowering of this compilation's design, lowering `width` at once.
// Nothing where the design did not lower, which is what a design this path
// refuses comes to and is not this test's subject.
auto Lower(Compilation& compilation, std::size_t width)
    -> std::optional<LoweredUnits> {
  if (!compilation.front.elaborated.has_value()) {
    return std::nullopt;
  }

  lyra::diag::DiagnosticSink sink;
  auto design = lyra::compiler::DeclareUnits(
      std::move(compilation.front.elaborated->compilation),
      lyra::compiler::LoweringPolicy{
          .assertions = lyra::support::AssertionPolicy::kCheck},
      sink, compilation.front.elaborated->diag_sources);
  if (!design.has_value()) {
    return std::nullopt;
  }

  LoweredUnits units;
  lyra::compiler::LowerToHir(
      *design, sink, width,
      [](lyra::hir::Lowering lowered)
          -> lyra::diag::Result<lyra::hir::Lowering> { return lowered; },
      [&units](lyra::hir::Lowering lowered) {
        units.push_back(std::move(lowered));
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
        "lowering '{}' differs at {}", first[at].name,
        FirstDifference(
            lyra::hir::DumpHir(first[at]), lyra::hir::DumpHir(second[at])));
  }
  return {};
}

class RepeatableLoweringTest : public testing::Test {
 public:
  explicit RepeatableLoweringTest(const Case& test_case) : case_(&test_case) {
  }

  void TestBody() override {
    Compilation first = Elaborate(*case_);
    Compilation second = Elaborate(*case_);

    const std::optional<LoweredUnits> first_units = Lower(first, 1);
    const std::optional<LoweredUnits> second_units = Lower(second, kWide);
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
  const Case* case_;
};

// The design written to show an ordering, held to the same claim and to one
// more: it has to lower. A corpus design that does not is a path's stated
// refusal and no concern of this check, but this one exists so that something
// here is known to be sensitive to an ordering, and a skip would leave that
// silently untrue.
TEST(RepeatableLowering, ADesignWrittenToShowAnOrderingStatesOneThing) {
  const ScratchDirectory scratch("wide-sensitivity-lowering");
  const Case design =
      lyra::test::WriteWideSensitivityDesign(scratch.Under("source"));

  Compilation first = Elaborate(design);
  Compilation second = Elaborate(design);

  const std::optional<LoweredUnits> first_units = Lower(first, 1);
  const std::optional<LoweredUnits> second_units = Lower(second, kWide);
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
      [](const Case& test_case) -> std::unique_ptr<testing::Test> {
        return std::make_unique<RepeatableLoweringTest>(test_case);
      });
}
