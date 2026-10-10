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
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/sink.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/structural_scope.hpp"

namespace {

// Every unit of a design, lowered to HIR. The command line holds one unit at a
// time; a probe holds them all, so it can ask anything any unit says.
using LoweredUnits = std::vector<lyra::hir::CompilationUnit>;

// The design `source` states, lowered to HIR through the same entries the
// command line uses, reporting into `sink`.
auto LowerDesignInto(std::string_view source, lyra::diag::DiagnosticSink& sink)
    -> std::optional<LoweredUnits> {
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

  auto design = lyra::compiler::DeclareUnits(
      std::move(front.elaborated->compilation),
      lyra::compiler::LoweringPolicy{}, sink);
  EXPECT_TRUE(design.has_value()) << "the probe design did not declare";
  if (!design.has_value()) return std::nullopt;

  LoweredUnits units;
  lyra::compiler::LowerToHir(
      *design, sink, 1,
      [](lyra::hir::CompilationUnit unit)
          -> lyra::diag::Result<lyra::hir::CompilationUnit> { return unit; },
      [&units](lyra::hir::CompilationUnit unit) {
        units.push_back(std::move(unit));
      });
  EXPECT_FALSE(sink.HasErrors()) << "the probe design did not lower";
  if (sink.HasErrors()) return std::nullopt;
  return units;
}

// How many times the lowering said it lost sharing.
auto LostSharingRemarks(const lyra::diag::DiagnosticSink& sink) -> std::size_t {
  std::size_t count = 0;
  for (const lyra::diag::Diagnostic& reported : sink.Diagnostics()) {
    if (reported.primary.code == lyra::diag::DiagCode::kRemarkLostSharing) {
      ++count;
    }
  }
  return count;
}

// The design `source` states, held to lowering with nothing lost. A count can
// come out right because the lowering caught a parameter taken for a value that
// something read as more, and kept its definition whole. That costs only
// sharing, and is still a defect in how parameters are classified, so every
// probe here is held to needing none of it.
auto LowerDesign(std::string_view source) -> std::optional<LoweredUnits> {
  lyra::diag::DiagnosticSink sink;
  auto design = LowerDesignInto(source, sink);
  EXPECT_EQ(LostSharingRemarks(sink), 0U);
  return design;
}

// How many units `definition` was compiled into: one per specialization,
// whatever the number of instances.
auto UnitsOf(const LoweredUnits& design, std::string_view definition)
    -> std::size_t {
  std::size_t count = 0;
  for (const lyra::hir::CompilationUnit& unit : design) {
    const std::string_view name = unit.name;
    if (name == definition ||
        (name.starts_with(definition) &&
         name.substr(definition.size()).starts_with("__"))) {
      ++count;
    }
  }
  return count;
}

// The blocks one generate of `design` was compiled into, for the generate the
// design's top declares. A shared loop holds one scope however many indices it
// counts out; an unshared one holds a scope per index. `depth` walks into the
// first block of that generate and asks the same of the generate inside it,
// which is how a conditional written inside a loop is reached.
auto BlocksOfGenerate(const LoweredUnits& design, std::size_t depth)
    -> std::size_t {
  for (const lyra::hir::CompilationUnit& unit : design) {
    const lyra::hir::StructuralScope* scope = &unit.root_scope;
    for (std::size_t level = 0;; ++level) {
      const lyra::hir::Generate* found = nullptr;
      for (const lyra::hir::Generate& generate : scope->generates) {
        if (generate.blocks.size() != 0) {
          found = &generate;
          break;
        }
      }
      if (found == nullptr) break;
      if (level == depth) return found->blocks.size();
      scope = &found->blocks.begin()->scope;
    }
  }
  return 0;
}

// The same, for the design `source` states.
auto CompiledBlocksOfGenerate(std::string_view source, std::size_t depth)
    -> std::size_t {
  const auto design = LowerDesign(source);
  if (!design.has_value()) return 0;
  return BlocksOfGenerate(*design, depth);
}

// How many units `definition` was compiled into across the design `source`.
auto CompiledUnitsOf(std::string_view source, std::string_view definition)
    -> std::size_t {
  const auto design = LowerDesign(source);
  if (!design.has_value()) return 0;
  return UnitsOf(*design, definition);
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

TEST(ArtifactCount, BlocksOfTwoShapesAreCompiledOncePerShape) {
  // Sixteen blocks whose widths alternate between two: two bodies, each built
  // at the indices that take it, so the count follows the shapes and not the
  // indices.
  EXPECT_EQ(
      CompiledBlocksOfGenerate(
          R"(
module Top;
  for (genvar i = 0; i < 16; i += 1) begin : g
    logic [i % 2:0] wide;
    initial wide = '0;
  end
endmodule
)",
          0),
      2U);
}

TEST(ArtifactCount, BlocksDerivingAConstantFromTheIndexAreCompiledOnce) {
  // A block naming a constant it works out from its own index states that
  // derivation, so every block states the same thing and the count does not
  // follow the indices -- whether the block declares the constant itself or a
  // subroutine or a procedure inside it does. The constant is a value a field
  // holds and nothing about the block is decided by it -- which is what
  // separates this from the width below, where the value reaches a type.
  EXPECT_EQ(
      CompiledBlocksOfGenerate(
          R"(
module Top;
  int sink [4];
  int more [4];
  for (genvar i = 0; i < 4; i += 1) begin : g
    localparam int K = i * 3 + 1;
    localparam int M = K * 2;
    function automatic int f();
      localparam int F = i + 7;
      return F;
    endfunction
    initial sink[i] = M;
    initial begin
      localparam int P = i * 5;
      more[i] = f() + P;
    end
  end
endmodule
)",
          0),
      1U);
}

TEST(ArtifactCount, BlocksReachingThePartTheirIndexSelectsAreCompiledOnce) {
  // A select whose index is a genvar is a constant select (LRM 11.5.3), so each
  // block reaches a different part of a vector: the bits it waits on, the bits
  // an indexed part-select yields, the positions a port joins. Which part is
  // still the index, a value each construction is handed, so the blocks state
  // the same thing -- through a continuous assignment, an event control,
  // `always_comb`, `wait`, an element of a multidimensional packed array, an
  // indexed part-select, and a bidirectional port alike.
  EXPECT_EQ(
      CompiledBlocksOfGenerate(
          R"(
module Pad(inout wire p);
endmodule

module Top;
  logic [7:0] v;
  logic [3:0][7:0] m;
  wire [3:0] n;
  logic d [8];
  logic e [8];
  logic f [8];
  int wakes [8];
  for (genvar i = 0; i < 4; i += 1) begin : g
    assign d[i] = v[i];
    always_comb e[i] = m[i][3] ^ v[i * 2 +: 2] == 2'b01;
    always @(v[i + 4]) wakes[i]++;
    initial wait (v[i]) f[i] = 1;
    Pad pad (.p(n[i]));
  end
endmodule
)",
          0),
      1U);
}

TEST(ArtifactCount, BlocksWhoseIndexSettlesAConditionAreCompiledOnce) {
  // Each block's index settles a condition its body writes, so at one index a
  // branch is never taken and at the next it is. What the body reads is still
  // what its text reads (LRM 9.2.2.2.1, 9.4.2.2), so every block waits on the
  // same things -- under an `if`, a `case`, a conditional operator, a logical
  // operator's second operand, a loop the index bounds, `@*` and `wait`, in a
  // procedure and in a continuous assignment alike.
  EXPECT_EQ(
      CompiledBlocksOfGenerate(
          R"(
module Top;
  logic [7:0] a;
  logic [7:0] by_if, by_case, by_conditional, by_operand, by_star, by_wait;
  logic by_loop [8];
  for (genvar i = 0; i < 8; i += 1) begin : g
    always_comb begin
      if ((i % 2) == 0) by_if[i] = a[i];
      else by_if[i] = 1'b0;
    end
    always_comb begin
      case (i % 2)
        0: by_case[i] = a[i];
        default: by_case[i] = 1'b0;
      endcase
    end
    assign by_conditional[i] = ((i % 2) == 0) ? a[i] : 1'b0;
    always_comb by_operand[i] = ((i % 2) == 0) && a[i];
    always_comb begin
      by_loop[i] = 1'b0;
      for (int j = 0; j <= i; j++) by_loop[i] ^= a[j];
    end
    always @* begin
      if (i == 0) by_star[i] = a[i];
      else by_star[i] = 1'b0;
    end
    initial begin
      wait (((i % 2) == 0) ? a[i] : 1'b1);
      by_wait[i] = 1'b1;
    end
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

// A conditional on the outer index, written inside an inner loop. Every inner
// block of one outer block selects the same alternative, so the inner loop of
// that outer block is one body holding that alternative alone, and two outer
// blocks then differ in what their inner loops hold. That is still only which
// alternative stood, so both loops are one body each and the conditional holds
// both alternatives.
constexpr std::string_view kChoiceUnderAnInnerLoop = R"(
module Top;
  int sink [8][4];
  for (genvar g = 0; g < 8; g += 1) begin : gg
    for (genvar i = 0; i < 4; i += 1) begin : gi
      if ((g % 2) == 0) begin : t
        initial sink[g][i] = 1;
      end else begin : f
        initial sink[g][i] = 2;
      end
    end
  end
endmodule
)";

TEST(ArtifactCount, ALoopWhoseInnerLoopChoosesByTheOuterIndexIsCompiledOnce) {
  EXPECT_EQ(CompiledBlocksOfGenerate(kChoiceUnderAnInnerLoop, 0), 1U);
}

TEST(ArtifactCount, TheInnerLoopOfSuchALoopIsCompiledOnce) {
  EXPECT_EQ(CompiledBlocksOfGenerate(kChoiceUnderAnInnerLoop, 1), 1U);
}

TEST(ArtifactCount, AChoiceUnderAnInnerLoopHoldsEveryAlternative) {
  EXPECT_EQ(CompiledBlocksOfGenerate(kChoiceUnderAnInnerLoop, 2), 2U);
}

// The same conditional, inside an inner loop whose own blocks are compiled
// apart because each declares a width its index fixes. The outer blocks agree
// on those inner blocks one for one, and differ only in which alternative
// stood inside each, so the outer loop is one body around an inner loop that
// is not.
constexpr std::string_view kChoiceUnderAnInnerLoopCompiledApart = R"(
module Top;
  for (genvar g = 0; g < 4; g += 1) begin : gg
    for (genvar i = 1; i < 4; i += 1) begin : gi
      logic [i:0] wide;
      if (g == 0) begin : t
        initial wide = '0;
      end else begin : f
        initial wide = '1;
      end
    end
  end
endmodule
)";

TEST(ArtifactCount, ALoopAroundBlocksCompiledApartThatChooseIsCompiledOnce) {
  EXPECT_EQ(
      CompiledBlocksOfGenerate(kChoiceUnderAnInnerLoopCompiledApart, 0), 1U);
}

TEST(ArtifactCount, TheBlocksCompiledApartStayApart) {
  EXPECT_EQ(
      CompiledBlocksOfGenerate(kChoiceUnderAnInnerLoopCompiledApart, 1), 3U);
}

TEST(ArtifactCount, ALoopWhoseInnerLoopsDeclareDifferentlyIsCompiledApart) {
  // Each outer block's inner loop is one body, and that body declares a width
  // the outer index fixes. The inner loops then differ in a type, which no
  // construction supplies, so the outer count has to follow the indices.
  EXPECT_EQ(
      CompiledBlocksOfGenerate(
          R"(
module Top;
  for (genvar g = 1; g < 5; g += 1) begin : gg
    for (genvar i = 0; i < 4; i += 1) begin : gi
      logic [g:0] wide;
      initial wide = '0;
    end
  end
endmodule
)",
          0),
      4U);
}

// A child handed a different value at every index, which it only ever reads as
// a value (LRM 23.10) -- in its body, and in the default of an input port the
// instance leaves unconnected, which is the child's own expression (LRM
// 23.2.2.4). The value is supplied when each instance is built, so the child
// is one unit, and the loop around it -- which names that one unit at every
// index -- is one body as well.
constexpr std::string_view kValuePerIndex = R"(
module Leaf #(parameter int K = 0) (
    input bit c, input int base = K * 3, output int o);
  always @(posedge c) o <= K + base;
endmodule

module Top;
  bit clk;
  int o [8];
  for (genvar i = 0; i < 8; i += 1) begin : g
    Leaf #(.K(i)) u (.c(clk), .o(o[i]));
  end
endmodule
)";

TEST(ArtifactCount, AChildGivenAValuePerIndexIsCompiledOnceAndSoIsTheLoop) {
  const auto design = LowerDesign(kValuePerIndex);
  ASSERT_TRUE(design.has_value());
  EXPECT_EQ(UnitsOf(*design, "Leaf"), 1U);
  EXPECT_EQ(BlocksOfGenerate(*design, 0), 1U);
}

TEST(ArtifactCount, BlocksReadingSeveralOfTheirOwnVariablesAreCompiledOnce) {
  // Every block declares the variables its procedures wait on, so each block's
  // are declarations of its own. What a procedure waits on is a set (LRM
  // 9.2.2.2.1, 9.4.2.1, 9.4.2.2), and the blocks write the same text, so they
  // state the same thing -- through `always_comb`, `@*`, a continuous
  // assignment, and a `wait`.
  EXPECT_EQ(
      CompiledBlocksOfGenerate(
          R"(
module Top;
  logic by_comb [8];
  logic by_star [8];
  logic by_assign [8];
  logic by_wait [8];
  for (genvar i = 0; i < 8; i += 1) begin : g
    logic a, b, c, d, e, f, h, k;
    always_comb by_comb[i] = f ^ c ^ h ^ a ^ e ^ k ^ b ^ d;
    always @* by_star[i] = k & a & f & b & h & c & e & d;
    assign by_assign[i] = d | h | b | f | a | k | c | e;
    initial begin
      wait (e + a + k + c + f + b + h + d > 3);
      by_wait[i] = 1'b1;
    end
  end
endmodule
)",
          0),
      1U);
}

TEST(ArtifactCount, InstancesReadingSeveralOfTheirOwnVariablesAreOneUnit) {
  // Each instance has variables of its own and is handed its own value, which
  // it only reads. Two instances wait on the same things in the same text, so
  // one unit serves every value and nothing is caught lowering apart.
  const auto design = LowerDesign(R"(
module Leaf #(parameter int K = 0) (input logic clk, output logic [7:0] o);
  logic [7:0] a, b, c, d, e, f, h, k;
  always_ff @(posedge clk) a <= 8'(K);
  always_comb o = f ^ c ^ h ^ a ^ e ^ k ^ b ^ d;
endmodule

module Top;
  logic clk;
  logic [7:0] o [4];
  Leaf #(.K(1)) one (.clk(clk), .o(o[0]));
  Leaf #(.K(2)) two (.clk(clk), .o(o[1]));
  Leaf #(.K(3)) three (.clk(clk), .o(o[2]));
  Leaf #(.K(4)) four (.clk(clk), .o(o[3]));
endmodule
)");
  ASSERT_TRUE(design.has_value());
  EXPECT_EQ(UnitsOf(*design, "Leaf"), 1U);
}

TEST(ArtifactCount, AValueSelectingWhatAChildWatchesIsCompiledOnce) {
  // Each child selects by its parameter the bit it reads, so each watches a
  // different bit (LRM 11.5.3) -- through a continuous assignment, an event
  // control, and `always_comb`. The parameter is still a value the instance is
  // handed, so one unit serves every value and nothing is caught lowering
  // apart.
  const auto design = LowerDesign(R"(
module Watches #(parameter int N = 0) (input logic [7:0] v, output int o);
  logic a;
  logic c;
  assign a = v[N];
  always_comb c = v[N + 1 -: 2] == 2'b10;
  always @(v[N]) o++;
endmodule

module Top;
  logic [7:0] v;
  int o [3];
  Watches #(.N(1)) one (.v(v), .o(o[0]));
  Watches #(.N(3)) three (.v(v), .o(o[1]));
  Watches #(.N(6)) six (.v(v), .o(o[2]));
endmodule
)");
  ASSERT_TRUE(design.has_value());
  EXPECT_EQ(UnitsOf(*design, "Watches"), 1U);
}

TEST(ArtifactCount, AValueThatDecidesTheChildIsCompiledPerValue) {
  // Each child is handed a value that decides what it is, in a different way:
  // a width at the scope or inside a procedural body, which settles a type; a
  // read through its own hierarchical name, which reaches the value one
  // elaboration gave (LRM 23.6); a parameter with no type, whose type is the
  // value's (LRM 6.20.2); and a value handed on to a grandchild that sizes a
  // declaration with it, which decides which grandchild is built.
  const auto design = LowerDesign(R"(
module ScopeWidth #(parameter int W = 1);
  logic [W-1:0] w;
  initial w = '0;
endmodule

module ProcessWidth #(parameter int W = 1);
  initial begin
    logic [W-1:0] t;
    t = '1;
  end
endmodule

module OwnName #(parameter int K = 0) (output int o);
  initial o = OwnName.K;
endmodule

module NoType #(parameter K = 0);
  int o;
  initial o = K;
endmodule

module Sized #(parameter int W = 1);
  logic [W-1:0] w;
  initial w = '0;
endmodule

module HandedOn #(parameter int K = 1);
  Sized #(.W(K)) inner ();
endmodule

module Top;
  for (genvar i = 1; i <= 2; i += 1) begin : g
    int o;
    ScopeWidth #(.W(i)) scope ();
    ProcessWidth #(.W(i)) process ();
    OwnName #(.K(i)) own (.o(o));
    HandedOn #(.K(i)) handed ();
  end
  NoType #(.K(3'd1)) narrow ();
  NoType #(.K(8'd1)) wide ();
endmodule
)");
  ASSERT_TRUE(design.has_value());
  for (const std::string_view definition :
       {"ScopeWidth", "ProcessWidth", "OwnName", "NoType", "HandedOn",
        "Sized"}) {
    EXPECT_EQ(UnitsOf(*design, definition), 2U) << definition;
  }
}

TEST(ArtifactCount, AValueInATypeASiteWritesIsSeenWhereItIsWritten) {
  // Each child reads its parameter as a value, and also writes it inside a
  // type the front end settles on the spot, where no reference to it is left
  // in what the type became: the operand a type is taken from (LRM 6.23), the
  // type a cast or an assignment pattern names, and the path a defparam names
  // its target through (LRM 23.10.1). The parameter decides the unit there,
  // and that is seen where it is written, so no instance is compiled only to
  // find out.
  const auto design = LowerDesign(R"(
package p;
  class C #(parameter int W = 1);
    typedef logic [W-1:0] T;
  endclass
endpackage

module TypeOf #(parameter int P = 1) (output int o);
  logic [7:0] a;
  var type(a[P-1:0]) x;
  initial begin
    x = '1;
    o = x + P;
  end
endmodule

module CastToType #(parameter int P = 1) (output int o);
  initial o = int'(p::C#(P)::T'(8'hFF)) + P;
endmodule

module CastToTypeOf #(parameter int P = 1) (output int o);
  logic [7:0] a;
  initial o = int'(type(a[P-1:0])'(8'hFF)) + P;
endmodule

module PatternOfType #(parameter int P = 1) (output int o);
  logic [7:0] v;
  initial begin
    v = p::C#(P)::T'{default: 1'b1};
    o = v + P;
  end
endmodule

module Inner #(parameter int V = 0) (output int o);
  initial o = V;
endmodule

module DefparamPath #(parameter int P = 0) (output int o);
  int got [2];
  for (genvar i = 0; i < 2; i++) begin : g
    Inner k (.o(got[i]));
  end
  defparam g[P].k.V = 7;
  initial #1 o = got[0] + P;
endmodule

module Top;
  for (genvar i = 1; i <= 2; i++) begin : g
    int o[5];
    TypeOf #(.P(i)) type_of (.o(o[0]));
    CastToType #(.P(i)) cast_to_type (.o(o[1]));
    CastToTypeOf #(.P(i)) cast_to_type_of (.o(o[2]));
    PatternOfType #(.P(i)) pattern_of_type (.o(o[3]));
    DefparamPath #(.P(i - 1)) defparam_path (.o(o[4]));
  end
endmodule
)");
  ASSERT_TRUE(design.has_value());
  for (const std::string_view definition :
       {"TypeOf", "CastToType", "CastToTypeOf", "PatternOfType",
        "DefparamPath"}) {
    EXPECT_EQ(UnitsOf(*design, definition), 2U) << definition;
  }
}

TEST(ArtifactCount, AValueWorkedOutFromASuppliedOneIsCompiledOnce) {
  // A constant written from a supplied parameter varies with it, instance by
  // instance, and is read only as a value; it decides no more than the value
  // it is written from.
  EXPECT_EQ(
      CompiledUnitsOf(
          R"(
module Leaf #(parameter int K = 0, parameter int D = K + 100)
    (input bit c, output int o);
  localparam int L = K * 2 + 1;
  always @(posedge c) o <= L + D;
endmodule

module Top;
  bit clk;
  int o [4];
  for (genvar i = 0; i < 4; i += 1) begin : g
    Leaf #(.K(i)) u (.c(clk), .o(o[i]));
  end
endmodule
)",
          "Leaf"),
      1U);
}

TEST(ArtifactCount, ARangeTheFrontEndFoldsIsSeenAndCompiledPerValue) {
  // Each module sizes something with its parameter at a place the front end
  // evaluates while binding. The parameter decides the module, so each value is
  // its own unit -- and it is seen where it is written, so no instance has to
  // be lowered apart to find it out.
  constexpr std::string_view kFolded = R"(
package p;
  typedef logic [3:0] nib_t;
endpackage

interface bus_if;
endinterface

module Child;
endmodule

module NamedType #(parameter int N = 1);
  p::nib_t [N-1:0] q;
  initial q = '0;
endmodule

module StructMember #(parameter int N = 1);
  typedef struct packed { logic [N-1:0] a; } s_t;
  s_t s;
  initial s = '0;
endmodule

module EnumBase #(parameter int N = 1);
  typedef enum logic [N:0] { A, B } e_t;
  e_t e;
  initial e = A;
endmodule

module InstanceArray #(parameter int N = 1);
  Child c[N] ();
endmodule

module TypeOperand #(parameter int N = 1) (output int o);
  initial o = $bits(logic [N-1:0]);
endmodule

module AssociativeIndex #(parameter int N = 1);
  int a[logic [N-1:0]];
  initial a[0] = 1;
endmodule

module PortArray #(parameter int N = 1) (bus_if b[N]);
endmodule

module Top;
  bus_if one[1] ();
  bus_if two[2] ();
  for (genvar i = 1; i <= 2; i += 1) begin : g
    int o;
    NamedType #(.N(i)) named ();
    StructMember #(.N(i)) member ();
    EnumBase #(.N(i)) base ();
    InstanceArray #(.N(i)) array ();
    TypeOperand #(.N(i)) operand (.o(o));
    AssociativeIndex #(.N(i)) index ();
  end
  PortArray #(.N(1)) p1 (.b(one));
  PortArray #(.N(2)) p2 (.b(two));
endmodule
)";
  const auto design = LowerDesign(kFolded);
  ASSERT_TRUE(design.has_value());
  for (const std::string_view definition :
       {"NamedType", "StructMember", "EnumBase", "InstanceArray", "TypeOperand",
        "AssociativeIndex", "PortArray"}) {
    EXPECT_EQ(UnitsOf(*design, definition), 2U) << definition;
  }
}

TEST(
    ArtifactCount,
    AValueHandedToASharedSpecializationIsSeenAndCompiledPerValue) {
  // Each module hands its parameter to a class specialization at a different
  // kind of site, or to the interface a virtual interface type names (LRM
  // 25.9). Either is shared by every site handing it the same value, so what
  // decides the module is the value this site wrote -- and it is seen there,
  // so no instance has to be lowered apart to find it out.
  constexpr std::string_view kSpecialized = R"(
interface bus_if #(parameter int W = 1);
  logic [W-1:0] data;
endinterface

package p;
  class C #(parameter int W = 1);
    static int X = W;
    int v;
    static function int f();
      return W;
    endfunction
  endclass
endpackage

module DeclaredType #(parameter int N = 1) (output int o);
  p::C #(N) c;
  initial begin
    c = new;
    o = c.v;
  end
endmodule

module StaticValue #(parameter int N = 1) (output int o);
  initial o = p::C#(N)::X;
endmodule

module StaticCall #(parameter int N = 1) (output int o);
  initial o = p::C#(N)::f();
endmodule

module ScopedNew #(parameter int N = 1) (output int o);
  p::C #(N) c;
  initial begin
    c = p::C#(N)::new;
    o = c.v;
  end
endmodule

module Extends #(parameter int N = 1) (output int o);
  class D extends p::C #(N);
  endclass
  initial o = D::f();
endmodule

module VirtualHandle #(parameter int N = 1) (output int o);
  virtual bus_if #(.W(N)) vif;
  initial o = N;
endmodule

module Top;
  for (genvar i = 1; i <= 2; i += 1) begin : g
    int o[6];
    DeclaredType #(.N(i)) declared (.o(o[0]));
    StaticValue #(.N(i)) value (.o(o[1]));
    StaticCall #(.N(i)) call (.o(o[2]));
    ScopedNew #(.N(i)) made (.o(o[3]));
    Extends #(.N(i)) extended (.o(o[4]));
    VirtualHandle #(.N(i)) handle (.o(o[5]));
  end
endmodule
)";
  const auto design = LowerDesign(kSpecialized);
  ASSERT_TRUE(design.has_value());
  for (const std::string_view definition :
       {"DeclaredType", "StaticValue", "StaticCall", "ScopedNew", "Extends",
        "VirtualHandle"}) {
    EXPECT_EQ(UnitsOf(*design, definition), 2U) << definition;
  }
}

TEST(ArtifactCount, AValueReadThroughAChildIsCaughtAndKeptApart) {
  // The parent reads the value it handed its child back through the child's
  // name. Nothing the parent wrote refers to its own parameter there, so it is
  // taken for one only read as a value; the instances lower apart, and the
  // parent is compiled once per value instead, which is said as a remark.
  lyra::diag::DiagnosticSink sink;
  const auto design = LowerDesignInto(
      R"(
module Inner #(parameter int K = 0) (output int o);
  initial o = K;
endmodule

module Leaf #(parameter int Q = 0) (output int o, output int seen);
  Inner #(.K(Q)) u (.o(o));
  initial seen = u.K;
endmodule

module Top;
  for (genvar i = 0; i < 3; i += 1) begin : g
    int o, seen;
    Leaf #(.Q(i)) l (.o(o), .seen(seen));
  end
endmodule
)",
      sink);
  ASSERT_TRUE(design.has_value());
  EXPECT_EQ(LostSharingRemarks(sink), 1U);
  EXPECT_EQ(UnitsOf(*design, "Leaf"), 3U);
  EXPECT_EQ(UnitsOf(*design, "Inner"), 1U);
}

TEST(ArtifactCount, RealValuesDifferingOnlyInSignAreKeptApart) {
  // 0.0 and -0.0 are equal as numbers and still two values: dividing by each
  // gives infinities of opposite sign (LRM 6.12, IEEE 754). Read back through
  // the child's name, each is folded into the parent, so the parents have to be
  // told apart by what they hold, bit for bit.
  lyra::diag::DiagnosticSink sink;
  const auto design = LowerDesignInto(
      R"(
module Inner #(parameter real R = 1.0) (output real o);
  initial o = R;
endmodule

module Leaf #(parameter real Q = 1.0) (output real seen);
  real o;
  Inner #(.R(Q)) u (.o(o));
  initial seen = 1.0 / u.R;
endmodule

module Top;
  real pos, neg;
  Leaf #(.Q(0.0)) a (.seen(pos));
  Leaf #(.Q(-0.0)) b (.seen(neg));
endmodule
)",
      sink);
  ASSERT_TRUE(design.has_value());
  EXPECT_EQ(LostSharingRemarks(sink), 1U);
  EXPECT_EQ(UnitsOf(*design, "Leaf"), 2U);
}

TEST(ArtifactCount, AValueWorkedOutInsideTheBodyIsFollowedToWhatItDecides) {
  // A parameter the body declares is written from the module's parameter.
  // Where it sizes a declaration -- declared in a block, in a subroutine, or
  // with no type and a width that follows the value (LRM 6.20.2) -- the
  // module's parameter decides the unit through it, and that is seen where it
  // is written. Where it is only read as a value, even with no type of its own,
  // it varies with the instance and the unit is shared -- declared in a
  // subroutine, a sequential block or a parallel one too, where each instance's
  // value is read rather than folded. The parallel block stands directly inside
  // a `begin` that declares nothing, which opens no scope of its own.
  const auto design = LowerDesign(R"(
module InBlock #(parameter int N = 1) (output int o);
  if (1) begin : g
    localparam int W = N + 1;
    logic [W-1:0] v;
    initial o = $bits(v);
  end
endmodule

module InSubroutine #(parameter int N = 1) (output int o);
  function automatic int f();
    localparam int W = N + 1;
    logic [W-1:0] v;
    return $bits(v);
  endfunction
  initial o = f();
endmodule

module WidthFromValue #(parameter int N = 1) (output int o);
  localparam X = {N{1'b1}};
  initial o = $bits(X);
endmodule

module UntypedRead #(parameter int N = 1) (output int o);
  localparam W = N * 2;
  initial o = W;
endmodule

module UntypedReadInBlock #(parameter int N = 1) (output int o);
  if (1) begin : g
    localparam W = N * 2;
    initial o = W;
  end
endmodule

module ReadInSubroutine #(parameter int N = 1) (output int o);
  function automatic int f();
    localparam int W = N + 1;
    return W;
  endfunction
  initial o = f();
endmodule

module ReadInProcedure #(parameter int N = 1) (output int o);
  int p;
  initial begin
    localparam int W = N + 1;
    o = W;
  end
  initial begin
    fork : par
      localparam int F = N + 2;
      p = F;
    join
  end
endmodule

module Top;
  for (genvar i = 1; i <= 2; i += 1) begin : g
    int o[7];
    InBlock #(.N(i)) block (.o(o[0]));
    InSubroutine #(.N(i)) sub (.o(o[1]));
    WidthFromValue #(.N(i)) width (.o(o[2]));
    UntypedRead #(.N(i)) read (.o(o[3]));
    UntypedReadInBlock #(.N(i)) read_in_block (.o(o[4]));
    ReadInSubroutine #(.N(i)) read_in_sub (.o(o[5]));
    ReadInProcedure #(.N(i)) read_in_proc (.o(o[6]));
  end
endmodule
)");
  ASSERT_TRUE(design.has_value());
  for (const std::string_view definition :
       {"InBlock", "InSubroutine", "WidthFromValue"}) {
    EXPECT_EQ(UnitsOf(*design, definition), 2U) << definition;
  }
  for (const std::string_view definition :
       {"UntypedRead", "UntypedReadInBlock", "ReadInSubroutine",
        "ReadInProcedure"}) {
    EXPECT_EQ(UnitsOf(*design, definition), 1U) << definition;
  }
}

TEST(ArtifactCount, StringValuesThatSpellAlikeWhenJoinedAreKeptApart) {
  // Two arrays of strings whose elements, joined with commas and quoted, read
  // the same: a quote and a comma are ordinary characters in a string (LRM
  // 5.9). The generate condition makes the array decide the unit, so the two
  // are two units, and nothing about how their elements happen to join may make
  // them one.
  const auto design = LowerDesign(R"(
module Leaf #(parameter string S[2] = '{"", ""}) (output int o);
  if (S[0] == "a") begin : g
    initial o = 1;
  end else begin : h
    initial o = 2;
  end
endmodule

module Top;
  int x, y;
  Leaf #(.S('{"a\",\"b", "c"})) first (.o(x));
  Leaf #(.S('{"a", "b\",\"c"})) second (.o(y));
endmodule
)");
  ASSERT_TRUE(design.has_value());
  EXPECT_EQ(UnitsOf(*design, "Leaf"), 2U);
}

TEST(ArtifactCount, AValueHandedOnToBeReadIsCompiledOnce) {
  // A value handed on from one child to its own child, which only reads it:
  // both are one unit.
  const auto design = LowerDesign(R"(
module Inner #(parameter int V = 0);
  int o;
  initial o = V;
endmodule

module Leaf #(parameter int K = 0);
  Inner #(.V(K + 1)) inner ();
endmodule

module Top;
  for (genvar i = 0; i < 4; i += 1) begin : g
    Leaf #(.K(i)) u ();
  end
endmodule
)");
  ASSERT_TRUE(design.has_value());
  EXPECT_EQ(UnitsOf(*design, "Leaf"), 1U);
  EXPECT_EQ(UnitsOf(*design, "Inner"), 1U);
}

}  // namespace
