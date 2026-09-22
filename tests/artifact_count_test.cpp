// How many compiled artifacts a design turns into is a property no program it
// simulates can observe: a repeated structure compiled once and the same
// structure compiled once per repetition run identically and differ only in
// what was built. So nothing in the conformance corpus can state it, and the
// corpus should not try -- IEEE 1800 says nothing about artifact counts.
//
// It still has to be stated somewhere, because it is a first-class objective
// rather than an optimization anyone may drop: the count follows the number of
// distinct bodies, never the number of times one is repeated. What is asserted
// here is that count, read off the lowered design through the same entry the
// command line uses, in both directions -- one body where the repetitions agree
// and one per repetition where they do not, so a run that shared nothing and a
// run that shared what it must not are each a failure.

#include <filesystem>
#include <fstream>
#include <gtest/gtest.h>
#include <memory>
#include <string>
#include <string_view>
#include <vector>

#include <slang/driver/Driver.h>

#include "lyra/compiler/compile.hpp"
#include "lyra/diag/sink.hpp"
#include "lyra/hir/compilation_unit.hpp"

namespace {

// The blocks one loop generate of `source` was compiled into, for the loop the
// design's top declares. A shared loop holds one scope however many indices it
// counts out; an unshared one holds a scope per index.
auto CompiledBlocksOfFirstGenerate(std::string_view source) -> std::size_t {
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
  if (!front.elaborated.has_value()) return 0;

  lyra::diag::DiagnosticSink sink;
  auto design = lyra::compiler::LowerToHir(
      std::move(front.elaborated->compilation), front.elaborated->source_mapper,
      lyra::compiler::LoweringPolicy{}, sink);
  EXPECT_TRUE(design.has_value()) << "the probe design did not lower";
  if (!design.has_value()) return 0;

  for (const lyra::hir::CompilationUnit& unit : design->hir.units) {
    for (const lyra::hir::Generate& generate : unit.root_scope.generates) {
      if (generate.child_scopes.size() != 0) {
        return generate.child_scopes.size();
      }
    }
  }
  return 0;
}

TEST(ArtifactCount, RepeatedBlocksAreCompiledOnce) {
  // Eight blocks, one body: every block declares the same thing and reads the
  // index, which is a value each construction is handed (LRM 27.4).
  EXPECT_EQ(
      CompiledBlocksOfFirstGenerate(R"(
module Top;
  int sink [8];
  for (genvar i = 0; i < 8; i += 1) begin : g
    int here;
    initial begin
      here = i;
      sink[i] = here;
    end
  end
endmodule
)"),
      1U);
}

TEST(ArtifactCount, BlocksThatDifferAreCompiledApart) {
  // The same loop, except each block declares a variable whose width its own
  // index fixes. A width settles a type, and two types are two bodies, so the
  // count has to follow the indices here.
  EXPECT_EQ(
      CompiledBlocksOfFirstGenerate(R"(
module Top;
  for (genvar i = 1; i < 5; i += 1) begin : g
    logic [i:0] wide;
    initial wide = '0;
  end
endmodule
)"),
      4U);
}

TEST(ArtifactCount, BlocksBoundingAQueueByTheirIndexAreCompiledApart) {
  // A queue's declared bound (LRM 7.10.5) is part of what a declaration
  // settles to here and is not part of type identity in the front end, so the
  // question that decides one body has to be asked of what a declaration
  // lowers to. Asked of the front end's own matching instead, these blocks
  // answer that they are alike and then lower into different scopes.
  EXPECT_EQ(
      CompiledBlocksOfFirstGenerate(R"(
module Top;
  for (genvar i = 1; i < 5; i += 1) begin : g
    int held [$:i];
    initial held.push_back(i);
  end
endmodule
)"),
      4U);
}

}  // namespace
