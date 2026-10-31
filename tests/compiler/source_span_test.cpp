// What the compiler lowers a design to says, at every layer, which piece of the
// source each part of it was lowered from, including what a rewrite between
// the layers rebuilt. The reports a later layer makes stand on it, and nothing
// in a design shows it otherwise, so it is read off the lowered form here.

#include "lyra/diag/source_span.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <filesystem>
#include <format>
#include <fstream>
#include <gtest/gtest.h>
#include <optional>
#include <set>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include <slang/driver/Driver.h>

#include "lyra/compiler/compile.hpp"
#include "lyra/compiler/lower_design.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/sink.hpp"
#include "lyra/diag/source_manager.hpp"
#include "lyra/frontend/slang_source_manager.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/mir_transform/pass.hpp"

namespace {

// Two statements on lines 5 and 6, the first one whose operands a rewrite
// takes into temporaries before the call that reads them.
constexpr std::string_view kTwoStatements = R"(
module Top;
  int a, b;
  function automatic int f(int x); return x + 1; endfunction
  initial begin
    a = f(1) + f(2);
    b = a;
  end
endmodule
)";

// What `function` was lowered from: the lines its instructions and
// terminators start on, and each one of them that names no place.
struct LoweredFrom {
  std::set<std::uint32_t> lines;
  std::vector<std::string> unplaced;
};

auto LoweredFromOf(
    const lyra::lir::Function& function,
    const lyra::diag::SourceManager& sources) -> LoweredFrom {
  LoweredFrom from;
  const auto place = [&](lyra::diag::SourceSpan span, std::string what) {
    const auto position = sources.PositionOf(span);
    if (position.has_value()) {
      from.lines.insert(position->line);
    } else {
      from.unplaced.push_back(std::move(what));
    }
  };
  for (std::size_t b = 0; b < function.blocks.size(); ++b) {
    const lyra::lir::BasicBlock& block = function.blocks[b];
    for (std::size_t i = 0; i < block.instrs.size(); ++i) {
      place(
          block.instrs[i].span, std::format(
                                    "block {} instruction {} (kind {})", b, i,
                                    block.instrs[i].data.index()));
    }
    place(
        block.terminator.span,
        std::format(
            "block {} terminator (kind {})", b, block.terminator.data.index()));
  }
  return from;
}

TEST(
    CompilerSourceSpanTest, AProcedureIsLoweredFromTheLinesItsStatementsAreOn) {
  const std::filesystem::path path =
      std::filesystem::temp_directory_path() / "source_span_test.sv";
  {
    std::ofstream out(path);
    out << kTwoStatements;
  }
  slang::driver::Driver driver;
  driver.addStandardArgs();
  const std::string path_text = path.string();
  const std::vector<const char*> args{"lyra", path_text.c_str()};
  ASSERT_TRUE(
      driver.parseCommandLine(static_cast<int>(args.size()), args.data()));
  lyra::compiler::FrontEndResult front = lyra::compiler::RunFrontEnd(driver);
  ASSERT_TRUE(front.elaborated.has_value()) << front.diagnostics;
  const lyra::frontend::SlangSourceManager sources(driver.sourceManager);

  lyra::diag::DiagnosticSink sink;
  auto design = lyra::compiler::DeclareUnits(
      std::move(front.elaborated->compilation),
      lyra::compiler::LoweringPolicy{}, sink);
  ASSERT_TRUE(design.has_value());
  std::vector<lyra::lir::CompilationUnit> units;
  const auto executable = lyra::compiler::LowerToExecutable(
      *design, sources, lyra::mir_transform::PassOptions{}, sink, 1,
      [](lyra::lir::CompilationUnit unit)
          -> lyra::diag::Result<lyra::lir::CompilationUnit> { return unit; },
      [&](lyra::lir::CompilationUnit unit) {
        units.push_back(std::move(unit));
      });
  ASSERT_TRUE(executable.has_value());

  // The procedure is the one function some of whose code was lowered from the
  // first statement's line.
  std::optional<LoweredFrom> procedure;
  for (const lyra::lir::CompilationUnit& unit : units) {
    for (const lyra::lir::Function& function : unit.functions) {
      LoweredFrom from = LoweredFromOf(function, sources);
      if (from.lines.contains(5)) {
        procedure = std::move(from);
      }
    }
  }
  ASSERT_TRUE(procedure.has_value()) << "nothing was lowered from line 5";
  std::string unplaced;
  for (const std::string& what : procedure->unplaced) {
    unplaced += "\n  " + what;
  }
  EXPECT_TRUE(procedure->unplaced.empty()) << "named no place:" << unplaced;
  EXPECT_TRUE(procedure->lines.contains(6));
  EXPECT_TRUE(std::ranges::all_of(procedure->lines, [](std::uint32_t line) {
    return line >= 4 && line <= 7;
  }));
}

}  // namespace
