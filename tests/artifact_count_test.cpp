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
// run that shared what it must not are each a failure. That a design compiles
// nothing of Lyra's own it did not ask for is a property of the same kind, and
// is asserted here too.

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <filesystem>
#include <format>
#include <fstream>
#include <gtest/gtest.h>
#include <map>
#include <optional>
#include <regex>
#include <set>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include <slang/driver/Driver.h>

#include "lyra/base/overloaded.hpp"
#include "lyra/compiler/compile.hpp"
#include "lyra/compiler/lower_design.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/sink.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/scope_construction.hpp"
#include "lyra/hir/specialization.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/dump.hpp"
#include "lyra/mir_transform/pass.hpp"
#include "lyra/support/def_path.hpp"

namespace {

// Every unit of a design, lowered to HIR. The command line holds one unit at a
// time; a probe holds them all, so it can ask anything any unit says.
using LoweredUnits = std::vector<lyra::hir::Specialization>;

// The design `source` states, its units declared through the same entries the
// command line uses, handed to `use` with the source text its spans resolve
// through and the sink a lowering reports to.
template <typename Use>
auto WithDeclaredDesign(std::string_view source, const Use& use) -> bool {
  lyra::diag::DiagnosticSink sink;
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
  if (!front.elaborated.has_value()) return false;

  auto design = lyra::compiler::DeclareUnits(
      std::move(front.elaborated->compilation),
      lyra::compiler::LoweringPolicy{}, sink);
  EXPECT_TRUE(design.has_value()) << "the probe design did not declare";
  if (!design.has_value()) return false;

  use(*design, front.elaborated->diag_sources, sink);
  EXPECT_FALSE(sink.HasErrors()) << "the probe design did not lower";
  return !sink.HasErrors();
}

// The design `source` states, lowered to HIR through the same entries the
// command line uses.
auto LowerDesign(std::string_view source) -> std::optional<LoweredUnits> {
  // What is compiled is each implementation, once.
  LoweredUnits units;
  const bool lowered = WithDeclaredDesign(
      source,
      [&](lyra::compiler::ElaboratedDesign& design,
          const lyra::diag::SourceManager&, lyra::diag::DiagnosticSink& sink) {
        lyra::compiler::LowerToHir(
            design, sink, 1,
            [](lyra::hir::Specialization lowered)
                -> lyra::diag::Result<
                    std::optional<lyra::hir::Specialization>> {
              if (!lowered.IsFirstOfItsImplementation()) return std::nullopt;
              return lowered;
            },
            [&units](std::optional<lyra::hir::Specialization> lowered) {
              if (lowered.has_value()) units.push_back(*std::move(lowered));
            });
      });
  if (!lowered) return std::nullopt;
  return units;
}

// Whether `unit` is one `definition` was compiled into.
auto IsOf(const lyra::hir::CompilationUnit& unit, std::string_view definition)
    -> bool {
  const std::string_view name = unit.name;
  return name == definition ||
         (name.starts_with(definition) &&
          name.substr(definition.size()).starts_with("__"));
}

// How many units `definition` was compiled into: one per specialization,
// whatever the number of instances.
auto UnitsOf(const LoweredUnits& design, std::string_view definition)
    -> std::size_t {
  std::set<std::string_view> units;
  for (const lyra::hir::Specialization& lowered : design) {
    const lyra::hir::CompilationUnit& unit = lowered.implementation;
    if (IsOf(unit, definition)) units.insert(unit.name);
  }
  return units.size();
}

// How many bodies those units were compiled to: one per way the values their
// instances are handed fix their code, whatever the number of instances.
auto BodiesOf(const LoweredUnits& design, std::string_view definition)
    -> std::size_t {
  std::size_t count = 0;
  for (const lyra::hir::Specialization& lowered : design) {
    if (IsOf(lowered.implementation, definition)) ++count;
  }
  return count;
}

// The block bodies one generate of `design` builds, for the generate the
// design's top declares: blocks that compile alike share one body however many
// indices the loop counts out, and blocks that do not have a body each.
// `depth` asks the same of the generate inside the blocks that one builds,
// gathered over every one of them, which is how a conditional written inside a
// loop is reached.
auto BodiesOfGenerate(const LoweredUnits& design, std::size_t depth)
    -> std::size_t {
  for (const lyra::hir::Specialization& unit : design) {
    std::vector<const lyra::hir::ScopeConstruction*> level{&unit.construction};
    for (std::size_t at = 0;; ++at) {
      std::set<std::uint32_t> bodies;
      std::vector<const lyra::hir::ScopeConstruction*> inside;
      for (const lyra::hir::ScopeConstruction* built : level) {
        for (const std::vector<lyra::hir::BlockConstruction>& places :
             built->generates) {
          if (std::ranges::none_of(
                  places, [](const lyra::hir::BlockConstruction& place) {
                    return place.body.has_value();
                  })) {
            continue;
          }
          for (const lyra::hir::BlockConstruction& place : places) {
            if (!place.body.has_value()) continue;
            bodies.insert(place.body->value);
            inside.push_back(&place.built);
          }
          break;
        }
      }
      if (bodies.empty()) break;
      if (at == depth) return bodies.size();
      level = std::move(inside);
    }
  }
  return 0;
}

// The same, for the design `source` states.
auto CompiledBodiesOfGenerate(std::string_view source, std::size_t depth)
    -> std::size_t {
  const auto design = LowerDesign(source);
  if (!design.has_value()) return 0;
  return BodiesOfGenerate(*design, depth);
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
      CompiledBodiesOfGenerate(
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
      CompiledBodiesOfGenerate(
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

TEST(ArtifactCount, BlocksDeclaringAlikeAreCompiledOnceWhateverTheirIndex) {
  // Sixteen blocks whose widths alternate between two. A block is named by
  // what it declares as resolved, so two indices that settle one width are one
  // class, and the blocks compile once per width.
  EXPECT_EQ(
      CompiledBodiesOfGenerate(
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
      CompiledBodiesOfGenerate(
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
      CompiledBodiesOfGenerate(
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
      CompiledBodiesOfGenerate(
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
      CompiledBodiesOfGenerate(
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
      CompiledBodiesOfGenerate(
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

// A loop whose blocks take different arms of one conditional. Which
// alternative a block built is its construction's and no part of what it
// declares (LRM 27.5), so the blocks are one class and one body however their
// indices chose, and each alternative is a body of its own.
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
  EXPECT_EQ(CompiledBodiesOfGenerate(kOneSpecialIndex, 0), 1U);
}

TEST(ArtifactCount, EachAlternativeOfTheChoiceIsABodyOfItsOwn) {
  EXPECT_EQ(CompiledBodiesOfGenerate(kOneSpecialIndex, 1), 2U);
}

// An `if ... else if ... else` chain, which the standard treats as one
// construct with three alternatives rather than nested ones (LRM 27.5), so
// the blocks are one class and the alternatives three bodies.
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
  EXPECT_EQ(CompiledBodiesOfGenerate(kChain, 0), 1U);
}

TEST(ArtifactCount, EachAlternativeOfTheChainIsABodyOfItsOwn) {
  EXPECT_EQ(CompiledBodiesOfGenerate(kChain, 1), 3U);
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
  EXPECT_EQ(CompiledBodiesOfGenerate(kCase, 0), 1U);
}

TEST(ArtifactCount, EachAlternativeOfTheCaseIsABodyOfItsOwn) {
  EXPECT_EQ(CompiledBodiesOfGenerate(kCase, 1), 3U);
}

// A conditional whose condition fails builds nothing at that index. That too
// is the construction's, so a block building nothing and one building the
// alternative declare the same thing and are one class.
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

TEST(ArtifactCount, ALoopWhoseConditionalBuildsNothingSomewhereIsOneClass) {
  EXPECT_EQ(CompiledBodiesOfGenerate(kNoElse, 0), 1U);
}

// A conditional the source wrote inside another's selected side belongs to the
// outer construct (LRM 27.5), so one construct holds all three alternatives,
// and the blocks holding it are one class.
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
  EXPECT_EQ(CompiledBodiesOfGenerate(kNestedInSelectedSide, 0), 1U);
}

TEST(ArtifactCount, EachAlternativeOfANestedConditionalIsABodyOfItsOwn) {
  EXPECT_EQ(CompiledBodiesOfGenerate(kNestedInSelectedSide, 1), 3U);
}

// A conditional on the outer index, written inside an inner loop. Which
// alternative the inner blocks select is their construction's, so the inner
// blocks are one class, and so are the outer ones holding them, whatever the
// counts; each alternative is a body of its own.
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

TEST(ArtifactCount, ALoopWhoseInnerLoopChoosesByTheOuterIndexIsOneClass) {
  EXPECT_EQ(CompiledBodiesOfGenerate(kChoiceUnderAnInnerLoop, 0), 1U);
}

TEST(ArtifactCount, TheInnerLoopOfSuchALoopIsOneClass) {
  EXPECT_EQ(CompiledBodiesOfGenerate(kChoiceUnderAnInnerLoop, 1), 1U);
}

TEST(ArtifactCount, EachAlternativeUnderAnInnerLoopIsABodyOfItsOwn) {
  EXPECT_EQ(CompiledBodiesOfGenerate(kChoiceUnderAnInnerLoop, 2), 2U);
}

// The same conditional, inside an inner loop whose own blocks are compiled
// apart because each declares a width its index fixes. The inner blocks are
// one class per width, whichever alternative they built, and the outer blocks
// holding them one class.
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

TEST(ArtifactCount, ALoopAroundBlocksCompiledApartThatChooseIsOneClass) {
  EXPECT_EQ(
      CompiledBodiesOfGenerate(kChoiceUnderAnInnerLoopCompiledApart, 0), 1U);
}

TEST(ArtifactCount, TheBlocksCompiledApartStayApartByWidthAlone) {
  EXPECT_EQ(
      CompiledBodiesOfGenerate(kChoiceUnderAnInnerLoopCompiledApart, 1), 3U);
}

// Each outer block's inner loop builds blocks declaring a width the outer
// index fixes, so the inner blocks differ in a type, which no construction
// supplies, and are compiled apart. Which of them an outer block's loop built
// is that block's construction, so the outer blocks are one class.
constexpr std::string_view kInnerLoopsDeclareDifferently = R"(
module Top;
  for (genvar g = 1; g < 5; g += 1) begin : gg
    for (genvar i = 0; i < 4; i += 1) begin : gi
      logic [g:0] wide;
      initial wide = '0;
    end
  end
endmodule
)";

TEST(ArtifactCount, ALoopAroundInnerLoopsThatDeclareDifferentlyIsOneClass) {
  EXPECT_EQ(CompiledBodiesOfGenerate(kInnerLoopsDeclareDifferently, 0), 1U);
  EXPECT_EQ(CompiledBodiesOfGenerate(kInnerLoopsDeclareDifferently, 1), 4U);
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
  EXPECT_EQ(BodiesOfGenerate(*design, 0), 1U);
}

TEST(ArtifactCount, BlocksReadingSeveralOfTheirOwnVariablesAreCompiledOnce) {
  // Every block declares the variables its procedures wait on, so each block's
  // are declarations of its own. What a procedure waits on is a set (LRM
  // 9.2.2.2.1, 9.4.2.1, 9.4.2.2), and the blocks write the same text, so they
  // state the same thing -- through `always_comb`, `@*`, a continuous
  // assignment, and a `wait`.
  EXPECT_EQ(
      CompiledBodiesOfGenerate(
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
  // a width at the scope, which settles the type of what it publishes; a
  // parameter with no type, whose type is the value's (LRM 6.20.2); and a value
  // handed on to a grandchild that sizes a declaration with it, which decides
  // which grandchild is built. A width inside a procedural body settles a type
  // no other unit sees, so the child is one class compiled to a body per value.
  // A read through the module's own hierarchical name decides nothing: it
  // reaches the value the object was built with, as the plain name does (LRM
  // 23.6).
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
       {"ScopeWidth", "NoType", "HandedOn", "Sized"}) {
    EXPECT_EQ(UnitsOf(*design, definition), 2U) << definition;
  }
  EXPECT_EQ(UnitsOf(*design, "ProcessWidth"), 1U);
  EXPECT_EQ(BodiesOf(*design, "ProcessWidth"), 2U);
  EXPECT_EQ(UnitsOf(*design, "OwnName"), 1U);
}

TEST(ArtifactCount, AValueInATypeASiteWritesIsSeenWhereItIsWritten) {
  // Each child reads its parameter as a value, and also writes it inside a
  // type the front end settles on the spot, where no reference to it is left
  // in what the type became: the operand a type is taken from (LRM 6.23), the
  // type a cast or an assignment pattern names, and the path a defparam names
  // its target through (LRM 23.10.1). Where the type is that of a declaration
  // of the scope, the parameter decides the class; where it is a type of the
  // code alone, it decides the body; and the defparam decides which child is
  // handed what, which is how the child is built and no part of either.
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
  EXPECT_EQ(UnitsOf(*design, "TypeOf"), 2U);
  for (const std::string_view definition :
       {"CastToType", "CastToTypeOf", "PatternOfType"}) {
    EXPECT_EQ(UnitsOf(*design, definition), 1U) << definition;
    EXPECT_EQ(BodiesOf(*design, definition), 2U) << definition;
  }
  // The defparam hands one block's child a value, which that block's code
  // passes as a constant, so the code follows which block it reaches.
  EXPECT_EQ(UnitsOf(*design, "DefparamPath"), 1U);
  EXPECT_EQ(BodiesOf(*design, "DefparamPath"), 2U);
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
  // evaluates while binding. Where that is a declaration of the scope, the
  // parameter decides the class; where it is an operand of the code, it decides
  // the body.
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
       {"NamedType", "StructMember", "EnumBase", "InstanceArray",
        "AssociativeIndex", "PortArray"}) {
    EXPECT_EQ(UnitsOf(*design, definition), 2U) << definition;
  }
  EXPECT_EQ(UnitsOf(*design, "TypeOperand"), 1U);
  EXPECT_EQ(BodiesOf(*design, "TypeOperand"), 2U);
}

TEST(
    ArtifactCount,
    AValueHandedToASharedSpecializationIsSeenAndCompiledPerValue) {
  // Each module hands its parameter to a class specialization at a different
  // kind of site, or to the interface a virtual interface type names (LRM
  // 25.9). Either is shared by every site handing it the same value, so the
  // value this site wrote decides the module: its class where a declaration of
  // the scope names the specialization, and its body where only the code does.
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
       {"DeclaredType", "ScopedNew", "Extends", "VirtualHandle"}) {
    EXPECT_EQ(UnitsOf(*design, definition), 2U) << definition;
  }
  for (const std::string_view definition : {"StaticValue", "StaticCall"}) {
    EXPECT_EQ(UnitsOf(*design, definition), 1U) << definition;
    EXPECT_EQ(BodiesOf(*design, definition), 2U) << definition;
  }
}

TEST(ArtifactCount, AValueReadThroughAChildIsTheChildsOwn) {
  // The parent reads the value it handed its child back through the child's
  // name. The child takes that value when it is built, so the name reads it
  // from the child, and the parent is one body for every value it is given.
  const auto design = LowerDesign(
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
)");
  ASSERT_TRUE(design.has_value());
  EXPECT_EQ(UnitsOf(*design, "Leaf"), 1U);
  EXPECT_EQ(UnitsOf(*design, "Inner"), 1U);
}

TEST(ArtifactCount, RealValuesDifferingOnlyInSignAreOneClassAndOneBody) {
  // 0.0 and -0.0 are equal as numbers and still two values (LRM 6.12, IEEE
  // 754). A parameter written with no type takes the type of its value (LRM
  // 6.20.2), which is `real` for both, and the code reads the value the object
  // was built with, so the two instances are one class and one body handed two
  // values.
  const auto design = LowerDesign(
      R"(
module Leaf #(parameter Q = 1.0) (output real seen);
  initial seen = 1.0 / Q;
endmodule

module Top;
  real pos, neg;
  Leaf #(.Q(0.0)) a (.seen(pos));
  Leaf #(.Q(-0.0)) b (.seen(neg));
endmodule
)");
  ASSERT_TRUE(design.has_value());
  EXPECT_EQ(UnitsOf(*design, "Leaf"), 1U);
  EXPECT_EQ(BodiesOf(*design, "Leaf"), 1U);
}

TEST(ArtifactCount, AValueWorkedOutInsideTheBodyIsFollowedToWhatItDecides) {
  // A parameter the body declares is written from the module's parameter.
  // Where it sizes a declaration of the module's own scope -- with no type and
  // a width that follows the value (LRM 6.20.2) -- the module's parameter
  // decides the class through it; where it sizes one of a generate block, the
  // class of that block, which is the construction's and no part of the unit;
  // where it sizes a declaration of a subroutine, only the code, so the body.
  // Where it is only read as a value, even with no type of its own, it varies
  // with the instance and the unit is shared -- declared in a subroutine, a
  // sequential block or a parallel one too, where each instance's value is read
  // rather than folded. The parallel block stands directly inside a `begin`
  // that declares nothing, which opens no scope of its own.
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
  EXPECT_EQ(UnitsOf(*design, "WidthFromValue"), 2U);
  EXPECT_EQ(UnitsOf(*design, "InBlock"), 1U);
  EXPECT_EQ(BodiesOf(*design, "InBlock"), 2U);
  EXPECT_EQ(UnitsOf(*design, "InSubroutine"), 1U);
  EXPECT_EQ(BodiesOf(*design, "InSubroutine"), 2U);
  for (const std::string_view definition :
       {"UntypedRead", "UntypedReadInBlock", "ReadInSubroutine",
        "ReadInProcedure"}) {
    EXPECT_EQ(UnitsOf(*design, definition), 1U) << definition;
  }
}

TEST(ArtifactCount, AValueSayingWhatIsBuiltIsOneUnit) {
  // A parameter read where a construction decides what it builds (LRM 27.4,
  // 27.5) -- a conditional's condition, a case's selector, a loop's bound, and
  // any of them through a parameter the module writes from it -- is a value
  // each instance is built with, and which blocks the instance built is its
  // construction and no part of its unit. So a module handed four values is
  // one unit whatever they build. A block is lowered only where it was built,
  // so the values come out as one body per set of blocks they built: two
  // here, whatever counts or alternatives make up each set.
  const auto design = LowerDesign(R"(
module ByCondition #(parameter int P = 0) (output int o);
  if (P != 0) begin : on
    initial o = P;
  end else begin : off
    initial o = -1;
  end
endmodule

module ByCase #(parameter int P = 0) (output int o);
  case (P)
    0: begin : zero
      initial o = 10;
    end
    default: begin : other
      initial o = 20;
    end
  endcase
endmodule

module ByBound #(parameter int P = 0) (output int o);
  for (genvar i = 0; i < P; i++) begin : g
    initial o = i;
  end
endmodule

module ThroughADeclaredParameter #(parameter int P = 0) (output int o);
  localparam bit ENABLED = P > 1;
  if (ENABLED) begin : on
    initial o = 1;
  end
endmodule

module Top;
  int c [4];
  int k [4];
  int b [4];
  int d [4];
  for (genvar i = 0; i < 4; i++) begin : g
    ByCondition #(.P(i)) by_condition (.o(c[i]));
    ByCase #(.P(i)) by_case (.o(k[i]));
    ByBound #(.P(i)) by_bound (.o(b[i]));
    ThroughADeclaredParameter #(.P(i)) through (.o(d[i]));
  end
endmodule
)");
  ASSERT_TRUE(design.has_value());
  for (const std::string_view definition :
       {"ByCondition", "ByCase", "ByBound", "ThroughADeclaredParameter"}) {
    EXPECT_EQ(UnitsOf(*design, definition), 1U) << definition;
    EXPECT_EQ(BodiesOf(*design, definition), 2U) << definition;
  }
}

TEST(ArtifactCount, AValueSayingWhatIsBuiltStillDecidesWhereItSizesAType) {
  // The same parameter read in a declared range as well is held by the type,
  // so each value is a unit of its own whatever else reads it.
  const auto design = LowerDesign(R"(
module Sized #(parameter int P = 1) (output int o);
  logic [P-1:0] held;
  if (P != 1) begin : on
    initial o = P;
  end
endmodule

module Top;
  int x, y;
  Sized #(.P(1)) narrow (.o(x));
  Sized #(.P(2)) wide (.o(y));
endmodule
)");
  ASSERT_TRUE(design.has_value());
  EXPECT_EQ(UnitsOf(*design, "Sized"), 2U);
}

TEST(ArtifactCount, AClassAModuleHoldsIsOneClassOfItsUnit) {
  // A class a module holds -- one it declares, one a generate block of it
  // declares, or a specialization of a generic placed in it because it is
  // specialized on a type of the module -- is a type of each instance's own
  // (LRM 8.25), and what it declares is the same in every one. What its
  // methods do is the code of the body the instance is, so a class tells no
  // instances apart. Which blocks of a loop an instance built is its
  // construction's (LRM 27.4), so instances counting out different numbers of
  // blocks are one unit, whatever the blocks declare.
  const auto design = LowerDesign(R"(
package p;
  class Box #(type T = int);
    T v;
  endclass
endpackage

module Declares #(parameter int P = 0) (output int o);
  class Counter;
    int n;
  endclass
  Counter c = new;
  if (P != 0) begin : on
    initial begin c.n = 1; o = c.n; end
  end else begin : off
    initial begin c.n = 2; o = c.n; end
  end
endmodule

module Placed #(parameter int P = 0) (output int o);
  typedef struct { int a; } own_t;
  p::Box #(own_t) b = new;
  if (P != 0) begin : on
    initial begin b.v.a = 1; o = b.v.a; end
  end else begin : off
    initial begin b.v.a = 2; o = b.v.a; end
  end
endmodule

module Counted #(parameter int N = 1) (output int o);
  for (genvar i = 0; i < N; i++) begin : g
    class Cell;
      int v;
    endclass
    Cell c = new;
    initial c.v = i;
  end
  initial o = N;
endmodule

module Top;
  int d [2];
  int q [2];
  int n [2];
  Declares #(.P(1)) declares_one (.o(d[0]));
  Declares #(.P(2)) declares_two (.o(d[1]));
  Placed #(.P(1)) placed_one (.o(q[0]));
  Placed #(.P(2)) placed_two (.o(q[1]));
  Counted #(.N(1)) one_block (.o(n[0]));
  Counted #(.N(3)) three_blocks (.o(n[1]));
endmodule
)");
  ASSERT_TRUE(design.has_value());
  EXPECT_EQ(UnitsOf(*design, "Declares"), 1U);
  EXPECT_EQ(UnitsOf(*design, "Placed"), 1U);
  EXPECT_EQ(UnitsOf(*design, "Counted"), 1U);
}

TEST(ArtifactCount, AParameterLeftToItsDefaultIsHandedLikeOneAssigned) {
  // Whether an instantiation assigns a parameter the unit only reads, or leaves
  // it to its default, is the instantiation's to say and decides nothing about
  // what is compiled (LRM 6.20.1, 23.10): the unit evaluates the default
  // itself, including one written from an earlier parameter.
  const auto design = LowerDesign(R"(
module Leaf #(parameter int W = 8, parameter int D = W * 2) (output int o);
  initial o = W + D;
endmodule

module Top;
  int o [4];
  Leaf #(.W(4), .D(100)) both (.o(o[0]));
  Leaf #(.W(4)) width_only (.o(o[1]));
  Leaf #(.D(3)) depth_only (.o(o[2]));
  Leaf neither (.o(o[3]));
endmodule
)");
  ASSERT_TRUE(design.has_value());
  EXPECT_EQ(UnitsOf(*design, "Leaf"), 1U);
}

TEST(ArtifactCount, StringValuesThatSpellAlikeWhenJoinedAreToldApart) {
  // Two arrays of strings whose elements, joined with commas and quoted, read
  // the same: a quote and a comma are ordinary characters in a string (LRM
  // 5.9). The generate condition reads the array when an instance is built, so
  // each instance selects its own alternative, and is a body of its own. They
  // are two bodies only because the two values are told apart, and nothing
  // about how their elements happen to join may make them one.
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
  EXPECT_EQ(BodiesOf(*design, "Leaf"), 2U);
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

TEST(ArtifactCount, AValueFixingOnlyCodeIsOneUnitCompiledPerBody) {
  // A value only a process's code relies on leaves the unit one class, and a
  // value a declaration relies on decides the class by what the declaration
  // came out as. Which body a unit is compiled to is found from what its code
  // came out as: `Code` is one unit compiled to a body per value, and
  // `Folded` and `Declared`, whose two values each settle one constant, one
  // unit compiled to one body.
  const auto design = LowerDesign(R"(
module Code #(parameter int P = 1) (input logic [7:0] in, output logic [7:0] out);
  always_comb out = 8'(in[P:0]);
endmodule

module Folded #(parameter int P = 1) (input logic [7:0] in, output logic [7:0] out);
  always_comb out = 8'(in[P % 2:0]);
endmodule

module Declared #(parameter int P = 1);
  logic [P % 2:0] r;
endmodule

module Top;
  logic [7:0] in, a, b, c, d;
  Code #(.P(1)) code_a (.in(in), .out(a));
  Code #(.P(3)) code_b (.in(in), .out(b));
  Folded #(.P(1)) folded_a (.in(in), .out(c));
  Folded #(.P(3)) folded_b (.in(in), .out(d));
  Declared #(.P(2)) declared_a ();
  Declared #(.P(4)) declared_b ();
endmodule
)");
  ASSERT_TRUE(design.has_value());
  EXPECT_EQ(UnitsOf(*design, "Code"), 1U);
  EXPECT_EQ(BodiesOf(*design, "Code"), 2U);
  EXPECT_EQ(UnitsOf(*design, "Folded"), 1U);
  EXPECT_EQ(BodiesOf(*design, "Folded"), 1U);
  EXPECT_EQ(UnitsOf(*design, "Declared"), 1U);
  EXPECT_EQ(BodiesOf(*design, "Declared"), 1U);
}

TEST(ArtifactCount, AUnitDeclaringAClassIsCompiledPerBodyAsAnyOther) {
  // A class a unit declares is declared once by the unit, and what its methods
  // do is the code of each body, so a unit declaring one whose specializations
  // differ only in code is one class compiled to a body per value, as a unit
  // declaring none is.
  const auto design = LowerDesign(R"(
module Holds #(parameter int P = 1) (input logic [7:0] in, output logic [7:0] out);
  class Probe;
    int v;
  endclass
  always_comb out = 8'(in[P:0]);
endmodule

module Top;
  logic [7:0] in, a, b;
  Holds #(.P(1)) narrow (.in(in), .out(a));
  Holds #(.P(3)) wide (.in(in), .out(b));
endmodule
)");
  ASSERT_TRUE(design.has_value());
  EXPECT_EQ(UnitsOf(*design, "Holds"), 1U);
  EXPECT_EQ(BodiesOf(*design, "Holds"), 2U);
}

// Every body the design `source` is compiled to, lowered to MIR, each
// rewritten as the command line rewrites it.
auto LowerBodiesToMir(std::string_view source)
    -> std::vector<lyra::mir::CompilationUnit> {
  std::vector<lyra::mir::CompilationUnit> bodies;
  WithDeclaredDesign(
      source, [&](lyra::compiler::ElaboratedDesign& design,
                  const lyra::diag::SourceManager& sources,
                  lyra::diag::DiagnosticSink& sink) {
        const auto semantic = lyra::compiler::LowerToSemantic(
            design, sources, lyra::mir_transform::PassOptions{}, sink, 1,
            [](lyra::mir::CompilationUnit unit)
                -> lyra::diag::Result<lyra::mir::CompilationUnit> {
              return unit;
            },
            [&bodies](lyra::mir::CompilationUnit unit) {
              bodies.push_back(std::move(unit));
            });
        EXPECT_TRUE(semantic.has_value()) << "the probe design did not lower";
      });
  return bodies;
}

// The class `id` of `unit` as its identity reads, and the name of a root
// otherwise.
auto ClassText(const lyra::mir::CompilationUnit& unit, lyra::mir::ClassId id)
    -> std::string {
  const lyra::support::DefPath* path =
      lyra::support::PathOf(unit.GetClass(id).identity);
  return path == nullptr ? "unnamed" : lyra::support::DisplayOf(*path);
}

// The type `type` of `unit` as what it is, a class it names by the class's
// identity, so two bodies numbering their types and classes apart agree on it.
auto TypeText(const lyra::mir::CompilationUnit& unit, lyra::mir::TypeId type)
    -> std::string {
  static const std::regex kNamed{R"((Type|Class)\[#?(\d+)\])"};
  // The dump leads with the type's own id, which is this body's numbering.
  const std::string listed = lyra::mir::DumpType(unit, type);
  const std::string dumped = listed.substr(listed.find(' ') + 1);
  std::string text;
  std::size_t at = 0;
  for (auto it = std::sregex_iterator(dumped.begin(), dumped.end(), kNamed);
       it != std::sregex_iterator(); ++it) {
    const std::smatch& named = *it;
    text += dumped.substr(at, static_cast<std::size_t>(named.position()) - at);
    const auto value = static_cast<std::uint32_t>(std::stoul(named[2].str()));
    text += named[1].str() == "Type"
                ? TypeText(unit, lyra::mir::TypeId{.value = value})
                : ClassText(unit, lyra::mir::ClassId{value});
    at = static_cast<std::size_t>(named.position() + named.length());
  }
  return text + dumped.substr(at);
}

// What a callable of `unit` takes and answers, and the part it takes in
// dispatch, which is what a class states of it to every unit; its body is the
// body's own.
auto CallableText(
    const lyra::mir::CompilationUnit& unit, const lyra::mir::CallableDecl& decl)
    -> std::string {
  std::string text = "(";
  for (const lyra::mir::LocalId param : decl.code.params) {
    text += TypeText(unit, decl.code.locals.Get(param).type) + ",";
  }
  text += std::format(
      ") receiver={} -> {}", decl.code.TakesReceiver(),
      TypeText(unit, decl.code.result_type));
  if (decl.virtual_dispatch.has_value()) {
    text += std::visit(
        lyra::Overloaded{
            [](const lyra::mir::IntroducesVirtualSlot& introduces) {
              return std::format(" introduces pure={}", introduces.is_pure);
            },
            [&](const lyra::mir::OverridesVirtualSlot& overrides) {
              return std::format(
                  " overrides {}.{}",
                  ClassText(unit, overrides.overridden.owner_class),
                  overrides.overridden.slot.value);
            }},
        *decl.virtual_dispatch);
  }
  return text;
}

// What `unit` states of every class each body of its unit states, by the
// class's identity: what it extends, its fields, and what each of its
// callables and its construction take and answer.
auto ClassesEveryBodyStates(const lyra::mir::CompilationUnit& unit)
    -> std::map<std::string, std::string> {
  std::map<std::string, std::string> stated;
  for (const lyra::mir::ClassId id : unit.classes.Ids()) {
    if (!unit.classes.IsDefined(id) ||
        lyra::mir::IsDefinedElsewhere(unit, id) ||
        !lyra::mir::IsStatedByEveryBody(unit, id)) {
      continue;
    }
    const lyra::mir::Class& cls = unit.GetClass(id);
    std::string text;
    if (const std::optional<lyra::mir::ClassId> base =
            lyra::mir::DeclaredBase(cls)) {
      text += "extends " + ClassText(unit, *base) + "\n";
    }
    for (const lyra::mir::FieldDecl& field : cls.fields) {
      text += "field " + TypeText(unit, field.type) + "\n";
    }
    for (const lyra::mir::NamedField& named : cls.named_fields) {
      text += std::format("named field {} {}\n", named.name, named.slot.value);
    }
    for (const lyra::mir::CallableDecl& callable : cls.callables) {
      text += "callable " + CallableText(unit, callable) + "\n";
    }
    for (const lyra::mir::NamedCallable& named : cls.named_callables) {
      text +=
          std::format("named callable {} {}\n", named.name, named.body.value);
    }
    text += std::format("conforms to {}\n", cls.conforming.size());
    if (cls.constructor.has_value()) {
      text += "constructor " +
              CallableText(
                  unit,
                  lyra::mir::CallableDecl{
                      .code = cls.constructor->code,
                      .foreign = std::nullopt,
                      .virtual_dispatch = std::nullopt,
                      .defining_unit = std::nullopt}) +
              "\n";
    }
    stated.emplace(ClassText(unit, id), std::move(text));
  }
  return stated;
}

TEST(ArtifactCount, EveryBodyOfAUnitStatesItsClassesAlike) {
  // A unit is compiled to a body per distinct code, and every class another
  // unit can name of it -- what it published of each of its scopes, and each
  // class and covergroup it declares -- is stated by every one of those bodies
  // for the link to keep one. So every body states each alike, including a
  // body whose construction did not build the block declaring the class (LRM
  // 27.5): a program built of either body's statement runs the same, and
  // nothing a simulation does can tell which the link kept.
  const std::vector<lyra::mir::CompilationUnit> bodies = LowerBodiesToMir(R"(
class Base;
endclass

module Chooses #(parameter int P = 0) (output int o);
  class Kept;
    function int get();
      return P;
    endfunction
  endclass
  Kept kept = new;
  if (P == 0) begin : off
    class Held extends Base;
      static int made = 0;
      virtual function int get();
        return 10 + made;
      endfunction
      task later(output int r);
        #1 r = get();
      endtask
    endclass
    class Local;
      function int get();
        return 3;
      endfunction
    endclass
    covergroup cg;
      coverpoint o;
    endgroup
    Held h = new;
    cg c = new;
    initial begin
      Local l;
      l = new;
      o = h.get() + l.get();
      c.sample();
    end
  end else begin : on
    initial o = kept.get();
  end
endmodule

module Top;
  int o[2];
  Chooses #(.P(0)) off (.o(o[0]));
  Chooses #(.P(1)) on (.o(o[1]));
endmodule
)");
  std::map<std::string, std::vector<const lyra::mir::CompilationUnit*>> units;
  for (const lyra::mir::CompilationUnit& body : bodies) {
    units[body.name].push_back(&body);
  }
  std::size_t compared = 0;
  for (const auto& [name, of_unit] : units) {
    if (of_unit.size() < 2) continue;
    ++compared;
    const auto first = ClassesEveryBodyStates(*of_unit.front());
    for (const lyra::mir::CompilationUnit* other : of_unit) {
      const auto stated = ClassesEveryBodyStates(*other);
      for (const auto& [cls, text] : first) {
        const auto found = stated.find(cls);
        if (found == stated.end()) {
          ADD_FAILURE() << "body '" << other->body_name << "' of '" << name
                        << "' does not state the class '" << cls << "'";
          continue;
        }
        EXPECT_EQ(found->second, text)
            << "the bodies of '" << name << "' state the class '" << cls
            << "' differently";
      }
      EXPECT_EQ(stated.size(), first.size())
          << "the bodies of '" << name << "' state different classes";
    }
  }
  EXPECT_EQ(compared, 1U)
      << "the design written to have a unit of two bodies no longer has one";
}

TEST(ArtifactCount, AValueFixingAGrandchildsCodeLeavesTheBodyHandingItOnOne) {
  // A value handed on to a grandchild whose code alone it fixes: the
  // grandchild is one unit compiled to a body per value, and which of those is
  // built is how each object of the child is built, which is no part of the
  // child's code, so the child is one unit compiled to one body.
  const auto design = LowerDesign(R"(
module Leaf #(parameter int P = 1) (input logic [7:0] in, output logic [7:0] out);
  always_comb out = 8'(in[P:0]);
endmodule

module Mid #(parameter int K = 1) (input logic [7:0] in, output logic [7:0] out);
  Leaf #(.P(K)) leaf (.in(in), .out(out));
endmodule

module Top;
  logic [7:0] in;
  logic [7:0] out [2];
  for (genvar i = 0; i < 2; i += 1) begin : g
    Mid #(.K(i + 1)) mid (.in(in), .out(out[i]));
  end
endmodule
)");
  ASSERT_TRUE(design.has_value());
  EXPECT_EQ(UnitsOf(*design, "Leaf"), 1U);
  EXPECT_EQ(BodiesOf(*design, "Leaf"), 2U);
  EXPECT_EQ(UnitsOf(*design, "Mid"), 1U);
  EXPECT_EQ(BodiesOf(*design, "Mid"), 1U);
}

// Whether a unit of `design` declares a class named `name`.
auto DeclaresAClassNamed(const LoweredUnits& design, std::string_view name)
    -> bool {
  for (const lyra::hir::Specialization& lowered : design) {
    const lyra::hir::CompilationUnit& unit = lowered.implementation;
    for (const lyra::hir::ClassId id : unit.classes.OwnIds()) {
      if (lyra::support::DisplayOf(unit.classes.PathOf(id)) == name) {
        return true;
      }
    }
  }
  return false;
}

// What keeps every covergroup type of the run, which whatever counts coverage
// is reached through.
constexpr std::string_view kCoverageRegistry = "coverage_registry";

TEST(ArtifactCount, ADesignAskingForNoCoverageCompilesNothingOfIt) {
  // Counting coverage is asked for by a covergroup or a coverage function (LRM
  // 19.3, 19.9); a design writing neither compiles nothing that counts it.
  const auto design = LowerDesign(R"(
module Top;
  initial $display("no coverage");
endmodule
)");
  ASSERT_TRUE(design.has_value());
  EXPECT_FALSE(DeclaresAClassNamed(*design, kCoverageRegistry));
}

TEST(ArtifactCount, ACoverageFunctionAsksForWhatCountsCoverage) {
  // `$get_coverage` answers over every covergroup type of the run, none here
  // (LRM 19.11), and asks what keeps them.
  const auto design = LowerDesign(R"(
module Top;
  real overall;
  initial overall = $get_coverage();
endmodule
)");
  ASSERT_TRUE(design.has_value());
  EXPECT_TRUE(DeclaresAClassNamed(*design, kCoverageRegistry));
}

}  // namespace
