// A unit's fingerprint is a function of what a program compiled from it can
// tell, and of nothing else: not of the name it is compiled under, not of a
// name the program never reads, not of what another unit's body does. Each
// case here is held in both directions, because a fingerprint that leaves too
// much out passes the first and one that takes too much in passes the second.
// Cases comparing an edit build the design twice, so they also hold a
// fingerprint to coming out the same on every run.

#include "lyra/hir/fingerprint.hpp"

#include <filesystem>
#include <fstream>
#include <gtest/gtest.h>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include <slang/driver/Driver.h>

#include "lyra/compiler/compile.hpp"
#include "lyra/compiler/lower_design.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/sink.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/stable_hash/fingerprint.hpp"

namespace {

using lyra::hir::ByUnit;
using lyra::stable_hash::Fingerprint;

struct Fingerprints {
  ByUnit<Fingerprint> published;
  ByUnit<Fingerprint> bodies;
};

// The fingerprints of every unit of the design `source` states, reached
// through the same entries the command line uses. The file is named for the
// test, so two designs of one test are read from the same place.
auto FingerprintsOf(std::string_view source) -> std::optional<Fingerprints> {
  const std::filesystem::path path =
      std::filesystem::temp_directory_path() /
      std::filesystem::path{
          ::testing::UnitTest::GetInstance()->current_test_info()->name()}
          .concat(".sv");
  {
    std::ofstream out(path);
    out << source;
  }

  slang::driver::Driver driver;
  driver.addStandardArgs();
  const std::string path_text = path.string();
  const std::vector<const char*> args{"lyra", path_text.c_str()};
  EXPECT_TRUE(
      driver.parseCommandLine(static_cast<int>(args.size()), args.data()));

  lyra::compiler::FrontEndResult front = lyra::compiler::RunFrontEnd(driver);
  EXPECT_TRUE(front.elaborated.has_value())
      << "the front end rejected the probe design: " << front.diagnostics;
  if (!front.elaborated.has_value()) return std::nullopt;

  lyra::diag::DiagnosticSink sink;
  auto design = lyra::compiler::DeclareUnits(
      std::move(front.elaborated->compilation),
      lyra::compiler::LoweringPolicy{}, sink);
  EXPECT_TRUE(design.has_value()) << "the probe design did not declare";
  if (!design.has_value()) return std::nullopt;

  Fingerprints fingerprints{
      .published =
          lyra::hir::PublishedFingerprintsOf(design->units.Signatures()),
      .bodies = {}};
  lyra::compiler::LowerToHir(
      *design, sink, 1,
      [](lyra::hir::CompilationUnit unit)
          -> lyra::diag::Result<lyra::hir::CompilationUnit> { return unit; },
      [&](const lyra::hir::CompilationUnit& unit) {
        fingerprints.bodies.emplace(
            unit.name,
            lyra::hir::BodyFingerprintOf(
                unit, fingerprints.published, front.elaborated->diag_sources));
      });
  EXPECT_FALSE(sink.HasErrors()) << "the probe design did not lower";
  if (sink.HasErrors()) return std::nullopt;
  return fingerprints;
}

// The fingerprints of the units `definition` was compiled into, in the order
// of their names.
auto OfDefinition(const ByUnit<Fingerprint>& units, std::string_view definition)
    -> std::vector<Fingerprint> {
  std::vector<Fingerprint> found;
  for (const auto& [name, fingerprint] : units) {
    const std::string_view named = name;
    if (named == definition ||
        (named.starts_with(definition) &&
         named.substr(definition.size()).starts_with("__"))) {
      found.push_back(fingerprint);
    }
  }
  return found;
}

// Whether `definition` was compiled into two units, and the two state the same
// thing in what they publish and in what they lowered to.
void ExpectTwoUnitsThatAreOne(
    const Fingerprints& design, std::string_view definition, bool one) {
  const auto published = OfDefinition(design.published, definition);
  const auto bodies = OfDefinition(design.bodies, definition);
  ASSERT_EQ(published.size(), 2U) << definition;
  ASSERT_EQ(bodies.size(), 2U) << definition;
  EXPECT_EQ(bodies[0] == bodies[1], one) << definition;
  if (one) {
    EXPECT_EQ(published[0], published[1]) << definition;
  }
}

// `source` with its one `%` replaced by `text`.
auto With(std::string_view source, std::string_view text) -> std::string {
  std::string written(source);
  written.replace(written.find('%'), 1, text);
  return written;
}

// Whether `unit` lowered to the same thing in the design before an edit and in
// the design after it.
void ExpectBodyKept(
    std::string_view before, std::string_view after, std::string_view unit,
    bool kept) {
  const auto first = FingerprintsOf(before);
  const auto second = FingerprintsOf(after);
  ASSERT_TRUE(first.has_value());
  ASSERT_TRUE(second.has_value());
  const auto was = first->bodies.find(unit);
  const auto is = second->bodies.find(unit);
  ASSERT_NE(was, first->bodies.end()) << unit;
  ASSERT_NE(is, second->bodies.end()) << unit;
  EXPECT_EQ(was->second == is->second, kept) << unit;
}

// Each definition is compiled once per value of its parameter, because the
// value sizes a declaration. Halving it sizes the same declaration for 2 and
// for 3, so those two units lowered alike; 2 and 4 size two.
constexpr std::string_view kSizedByHalf = R"(
module Halved #(parameter int P = 2) (output int o);
  logic [P/2:0] held;
  initial o = $bits(held);
endmodule

module Top;
  int x, y;
  Halved #(.P(2)) a (.o(x));
  Halved #(.P(%)) b (.o(y));
endmodule
)";

TEST(Fingerprint, ApplicationsThatLowerAlikeAreOne) {
  const auto design = FingerprintsOf(With(kSizedByHalf, "3"));
  ASSERT_TRUE(design.has_value());
  ExpectTwoUnitsThatAreOne(*design, "Halved", true);
}

TEST(Fingerprint, ApplicationsThatLowerApartAreTwo) {
  const auto design = FingerprintsOf(With(kSizedByHalf, "4"));
  ASSERT_TRUE(design.has_value());
  ExpectTwoUnitsThatAreOne(*design, "Halved", false);
}

// A holder of each: it builds its child and reads its port, which is the
// child's published layout and nothing of its body. So two holders whose
// children publish alike are one, and two whose children do not are two.
constexpr std::string_view kHolder = R"(
module Halved #(parameter int P = 2) (output int o);
  logic [P/2:0] held;
  initial o = $bits(held);
endmodule

module Holder #(parameter int P = 2) (output int o);
  Halved #(.P(P)) inner (.o(o));
endmodule

module Top;
  int x, y;
  Holder #(.P(2)) a (.o(x));
  Holder #(.P(%)) b (.o(y));
endmodule
)";

TEST(Fingerprint, HoldersOfChildrenThatPublishAlikeAreOne) {
  const auto design = FingerprintsOf(With(kHolder, "3"));
  ASSERT_TRUE(design.has_value());
  ExpectTwoUnitsThatAreOne(*design, "Holder", true);
}

TEST(Fingerprint, HoldersOfChildrenThatPublishApartAreTwo) {
  const auto design = FingerprintsOf(With(kHolder, "4"));
  ASSERT_TRUE(design.has_value());
  ExpectTwoUnitsThatAreOne(*design, "Holder", false);
}

// A module calling a function of a package. The call names the function, and
// what it is handed and answers is what the package published; what the
// function does is the package's own. So a function answering another value
// changes the package and leaves its caller as it was, while one taking
// another argument changes both.
constexpr std::string_view kCaller = R"(
package pkg;
  function automatic int f(int x);
    return %;
  endfunction
endpackage

module Calls (output int o);
  initial o = pkg::f(2);
endmodule

module Top;
  int o;
  Calls calls (.o(o));
endmodule
)";

TEST(Fingerprint, ACallerIsNoneOfTheBodyItCalls) {
  ExpectBodyKept(With(kCaller, "x + 1"), With(kCaller, "x + 3"), "pkg", false);
  ExpectBodyKept(With(kCaller, "x + 1"), With(kCaller, "x + 3"), "Calls", true);
}

TEST(Fingerprint, ACallerIsWhatItCalls) {
  std::string narrower(kCaller);
  narrower.replace(narrower.find("int x"), 5, "byte x");
  ExpectBodyKept(
      With(kCaller, "x + 1"), With(narrower, "x + 1"), "Calls", false);
}

// A procedure's own variable is given another name. No run prints or compares
// that name, so the module is what it was. A block label is printed by `%m`
// (LRM 21.2.1.3), so the same rename on it makes another module.
constexpr std::string_view kRenamed = R"(
module Renames;
  logic [7:0] held;
  initial begin : %
    automatic logic [7:0] total;
    total = 8'd3;
    held = total;
    $display("%m");
  end
endmodule

module Top;
  Renames renames ();
endmodule
)";

TEST(Fingerprint, ALocalVariablesNameIsNoneOfIt) {
  const std::string before = With(kRenamed, "run");
  std::string after = before;
  for (std::size_t at = after.find("total"); at != std::string::npos;
       at = after.find("total", at)) {
    after.replace(at, 5, "count");
  }
  ExpectBodyKept(before, after, "Renames", true);
}

TEST(Fingerprint, ABlockLabelIsPartOfIt) {
  ExpectBodyKept(
      With(kRenamed, "run"), With(kRenamed, "go_"), "Renames", false);
}

}  // namespace
