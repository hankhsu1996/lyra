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

#include <cstddef>
#include <filesystem>
#include <fstream>
#include <gtest/gtest.h>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

#include <slang/driver/Driver.h>

#include "lyra/compiler/compile.hpp"
#include "lyra/compiler/lower_design.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/sink.hpp"
#include "lyra/hir/compilation_unit.hpp"

namespace {

// The blocks one generate of `source` was compiled into, for the generate the
// design's top declares. A shared loop holds one scope however many indices it
// counts out; an unshared one holds a scope per index. `depth` walks into the
// first block of that generate and asks the same of the generate inside it,
// which is how a conditional written inside a loop is reached.
auto CompiledBlocksOfGenerate(std::string_view source, std::size_t depth)
    -> std::size_t {
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
  auto design = lyra::compiler::DeclareUnits(
      std::move(front.elaborated->compilation), front.elaborated->source_mapper,
      lyra::compiler::LoweringPolicy{}, sink);
  EXPECT_TRUE(design.has_value()) << "the probe design did not declare";
  if (!design.has_value()) return 0;

  std::optional<std::size_t> blocks;
  lyra::compiler::LowerToHir(
      *design, sink, 1,
      [depth](const lyra::hir::CompilationUnit& unit)
          -> lyra::diag::Result<std::optional<std::size_t>> {
        const lyra::hir::StructuralScope* scope = &unit.root_scope;
        for (std::size_t level = 0;; ++level) {
          const lyra::hir::Generate* found = nullptr;
          for (const lyra::hir::Generate& generate : scope->generates) {
            if (generate.child_scopes.size() != 0) {
              found = &generate;
              break;
            }
          }
          if (found == nullptr) return std::nullopt;
          if (level == depth) return found->child_scopes.size();
          scope = &*found->child_scopes.begin();
        }
      },
      [&blocks](std::optional<std::size_t> in_unit) {
        if (!blocks.has_value()) blocks = in_unit;
      });
  EXPECT_FALSE(sink.HasErrors()) << "the probe design did not lower";
  return blocks.value_or(0);
}

TEST(ArtifactCount, RepeatedBlocksAreCompiledOnce) {
  // Eight blocks, one body: every block declares the same thing and reads the
  // index, which is a value each construction is handed (LRM 27.4).
  EXPECT_EQ(
      CompiledBlocksOfGenerate(
          R"(
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
)",
          0),
      1U);
}

TEST(ArtifactCount, BlocksThatDifferAreCompiledApart) {
  // The same loop, except each block declares a variable whose width its own
  // index fixes. A width settles a type, and two types are two bodies, so the
  // count has to follow the indices here.
  EXPECT_EQ(
      CompiledBlocksOfGenerate(
          R"(
module Top;
  for (genvar i = 1; i < 5; i += 1) begin : g
    logic [i:0] wide;
    initial wide = '0;
  end
endmodule
)",
          0),
      4U);
}

TEST(ArtifactCount, BlocksDerivingAConstantFromTheIndexAreCompiledOnce) {
  // A block naming a constant it works out from its own index states that
  // derivation, so every block states the same thing and the count does not
  // follow the indices. The constant is a value a field holds and nothing about
  // the block is decided by it -- which is what separates this from the width
  // below, where the value reaches a type.
  EXPECT_EQ(
      CompiledBlocksOfGenerate(
          R"(
module Top;
  int sink [4];
  for (genvar i = 0; i < 4; i += 1) begin : g
    localparam int K = i * 3 + 1;
    localparam int M = K * 2;
    initial sink[i] = M;
  end
endmodule
)",
          0),
      1U);
}

TEST(
    ArtifactCount,
    BlocksSizingADeclarationThroughSuchAConstantAreCompiledApart) {
  // The same constant, reaching a declared width instead of a value. A width
  // settles a type whatever it was written through, so naming it first changes
  // nothing: these blocks declare different things and are compiled apart.
  EXPECT_EQ(
      CompiledBlocksOfGenerate(
          R"(
module Top;
  for (genvar i = 1; i < 5; i += 1) begin : g
    localparam int K = i;
    logic [K:0] wide;
    initial wide = '0;
  end
endmodule
)",
          0),
      4U);
}

TEST(ArtifactCount, BlocksBoundingAQueueByTheirIndexAreCompiledApart) {
  // A queue's declared bound (LRM 7.10.5) is part of what a declaration
  // settles to here and is not part of type identity in the front end, so the
  // question that decides one body has to be asked of what a declaration
  // lowers to. Asked of the front end's own matching instead, these blocks
  // answer that they are alike and then lower into different scopes.
  EXPECT_EQ(
      CompiledBlocksOfGenerate(
          R"(
module Top;
  for (genvar i = 1; i < 5; i += 1) begin : g
    int held [$:i];
    initial held.push_back(i);
  end
endmodule
)",
          0),
      4U);
}

// A loop whose blocks take different arms of one conditional. The blocks do
// not agree, and what they disagree about is which alternative of a construct
// that selects at most one of them stood (LRM 27.5) -- which is decided by an
// expression reading the index, and so by a value each construction is handed.
// So the loop is one body, and the conditional inside it holds both
// alternatives rather than the one its own index took.
constexpr std::string_view kOneSpecialIndex = R"(
module Top;
  int sink [8];
  for (genvar i = 0; i < 8; i += 1) begin : g
    if (i == 0) begin : seed
      initial sink[i] = 1;
    end else begin : step
      initial sink[i] = sink[i - 1] + 1;
    end
  end
endmodule
)";

TEST(ArtifactCount, ALoopWhoseBlocksChooseIsCompiledOnce) {
  EXPECT_EQ(CompiledBlocksOfGenerate(kOneSpecialIndex, 0), 1U);
}

TEST(ArtifactCount, AChosenBlockHoldsEveryAlternative) {
  EXPECT_EQ(CompiledBlocksOfGenerate(kOneSpecialIndex, 1), 2U);
}

// An `if ... else if ... else` chain, which the standard treats as one
// construct with three alternatives rather than nested ones (LRM 27.5). Every
// alternative's condition is stated where the construct is, so the blocks
// agree on everything but which alternative each of them selected.
constexpr std::string_view kChain = R"(
module Top;
  int sink [6];
  for (genvar i = 0; i < 6; i += 1) begin : g
    if (i == 0) begin : u
      initial sink[i] = 1;
    end else if (i == 5) begin : u
      initial sink[i] = 9;
    end else begin : u
      initial sink[i] = 5;
    end
  end
endmodule
)";

TEST(ArtifactCount, ALoopWhoseBlocksChooseAlongAChainIsCompiledOnce) {
  EXPECT_EQ(CompiledBlocksOfGenerate(kChain, 0), 1U);
}

TEST(ArtifactCount, AChainHoldsEveryAlternative) {
  EXPECT_EQ(CompiledBlocksOfGenerate(kChain, 1), 3U);
}

// A `case` selects on its own expression read against each item's labels, in
// the order the items are written, with the default taken only where every
// comparison failed (LRM 12.5, 27.5).
constexpr std::string_view kCase = R"(
module Top;
  int sink [6];
  for (genvar i = 0; i < 6; i += 1) begin : g
    case (i)
      0: begin : u
        initial sink[i] = 1;
      end
      1, 2: begin : u
        initial sink[i] = 2;
      end
      default: begin : u
        initial sink[i] = 7;
      end
    endcase
  end
endmodule
)";

TEST(ArtifactCount, ALoopWhoseBlocksChooseByCaseIsCompiledOnce) {
  EXPECT_EQ(CompiledBlocksOfGenerate(kCase, 0), 1U);
}

TEST(ArtifactCount, ACaseHoldsEveryAlternative) {
  EXPECT_EQ(CompiledBlocksOfGenerate(kCase, 1), 3U);
}

// A conditional whose condition fails contributes nothing at that index, and
// the construct is still the same construct there: what a block states is the
// alternatives the source wrote, never the one this index happened to get.
constexpr std::string_view kNoElse = R"(
module Top;
  int sink [6];
  for (genvar i = 0; i < 6; i += 1) begin : g
    if (i > 0) begin : u
      initial sink[i] = i;
    end
  end
endmodule
)";

TEST(ArtifactCount, ALoopWhoseConditionalContributesNothingIsCompiledOnce) {
  EXPECT_EQ(CompiledBlocksOfGenerate(kNoElse, 0), 1U);
}

// A conditional the source wrote inside another's selected side belongs to the
// outer construct (LRM 27.5), so one construct holds all three alternatives.
// What selects each of the inner two is the outer condition and its own label,
// and the blocks agree on that at every index.
constexpr std::string_view kNestedInSelectedSide = R"(
module Top;
  int sink [6];
  for (genvar i = 0; i < 6; i += 1) begin : g
    if (i < 3)
      case (i)
        0: begin : u
          initial sink[i] = 1;
        end
        default: begin : u
          initial sink[i] = 2;
        end
      endcase
    else begin : u
      initial sink[i] = 3;
    end
  end
endmodule
)";

TEST(ArtifactCount, ALoopWhoseConditionalsNestIsCompiledOnce) {
  EXPECT_EQ(CompiledBlocksOfGenerate(kNestedInSelectedSide, 0), 1U);
}

TEST(ArtifactCount, ANestedConditionalHoldsEveryAlternative) {
  EXPECT_EQ(CompiledBlocksOfGenerate(kNestedInSelectedSide, 1), 3U);
}

}  // namespace
