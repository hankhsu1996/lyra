// The compiler failing through a fault of its own while it works on one unit
// is that unit's failure: it is reported with the unit it was working on, and
// the other units are still attempted, so one run accounts for every unit that
// breaks. Nothing in a design can make the compiler break an invariant on
// request, so the break is made here, in what each unit is handed to.

#include <cstddef>
#include <filesystem>
#include <fstream>
#include <gtest/gtest.h>
#include <string>
#include <string_view>
#include <vector>

#include <slang/driver/Driver.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/compiler/compile.hpp"
#include "lyra/compiler/lower_design.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/kind.hpp"
#include "lyra/diag/sink.hpp"
#include "lyra/hir/compilation_unit.hpp"

namespace {

constexpr std::string_view kTwoUnits = R"(
module Leaf(output int o);
  initial o = 1;
endmodule

module Top;
  int o;
  Leaf leaf (.o(o));
endmodule
)";

// Lowers `source` with every unit's own step breaking an invariant, on `width`
// threads, and answers with what was reported.
auto ReportedWhenEveryUnitBreaks(std::string_view source, std::size_t width)
    -> std::vector<lyra::diag::Diagnostic> {
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
  EXPECT_TRUE(front.elaborated.has_value()) << front.diagnostics;
  if (!front.elaborated.has_value()) return {};

  lyra::diag::DiagnosticSink sink;
  auto design = lyra::compiler::DeclareUnits(
      std::move(front.elaborated->compilation), front.elaborated->source_mapper,
      lyra::compiler::LoweringPolicy{}, sink);
  EXPECT_TRUE(design.has_value());
  if (!design.has_value()) return {};

  std::size_t consumed = 0;
  lyra::compiler::LowerToHir(
      *design, sink, width,
      [](const lyra::hir::CompilationUnit&) -> lyra::diag::Result<int> {
        throw lyra::InternalError("an invariant broke");
      },
      [&consumed](int) { ++consumed; });
  EXPECT_EQ(consumed, 0U);
  EXPECT_TRUE(sink.HasInternalErrors());
  return sink.Diagnostics();
}

auto NamesUnit(const lyra::diag::Diagnostic& report, std::string_view unit)
    -> bool {
  return report.primary.message.contains(
      std::string("in unit '") + std::string(unit));
}

void ExpectEachUnitReported(const std::vector<lyra::diag::Diagnostic>& all) {
  ASSERT_EQ(all.size(), 2U);
  for (const lyra::diag::Diagnostic& report : all) {
    EXPECT_EQ(report.primary.kind, lyra::diag::DiagKind::kInternalError);
    EXPECT_NE(
        report.primary.message.find("an invariant broke"), std::string::npos);
  }
  EXPECT_TRUE(NamesUnit(all[0], "Leaf") || NamesUnit(all[1], "Leaf"));
  EXPECT_TRUE(NamesUnit(all[0], "Top") || NamesUnit(all[1], "Top"));
}

TEST(UnitFailure, EveryUnitThatBreaksIsReportedWithItsUnit) {
  ExpectEachUnitReported(ReportedWhenEveryUnitBreaks(kTwoUnits, 1));
}

TEST(UnitFailure, AUnitBreakingOnAnotherThreadIsReportedTheSame) {
  ExpectEachUnitReported(ReportedWhenEveryUnitBreaks(kTwoUnits, 2));
}

}  // namespace
