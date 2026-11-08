// What a lowering read of what other units publish is what it depends on, and
// no program it simulates can observe it: a lowering that read one unit too
// few runs exactly as one that read them all, until a build reuses it after
// the unit it missed has changed. So nothing in the conformance corpus can
// state it, and it is stated here, read off the lowered design through the
// same entry the command line uses.
//
// The reads asserted are the ones a lowering takes to decide what a construct
// means and then writes nothing of -- the lineage a hook call searches and
// finds no hook in -- beside the ones its code also names, so a record worked
// out from what the lowering wrote fails here.

#include "lyra/hir/signature_reads.hpp"

#include <gtest/gtest.h>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/compiler/compile.hpp"
#include "lyra/compiler/lower_design.hpp"
#include "lyra/diag/sink.hpp"
#include "lyra/hir/lowering.hpp"
#include "lyra/hir/lowering_values.hpp"
#include "lyra/hir/unit_signature.hpp"
#include "lyra/hir/unit_signatures.hpp"
#include "tests/elaborated_text.hpp"

namespace lyra::hir {
namespace {

auto Namespace(std::string name) -> UnitSignature {
  UnitSignature signature;
  signature.unit_name = std::move(name);
  signature.unit = NamespaceDeclaration{};
  return signature;
}

TEST(SignatureReaderTest, EveryAnswerIsRecordedAndTheReadersOwnIsNot) {
  UnitSignatures published;
  published.PublishSignature(Namespace("P"));
  published.PublishSignature(Namespace("Own"));
  published.PublishValues("P", LoweringValues{});

  SignatureReads reads;
  const SignatureReader reader(published, "Own", reads);
  EXPECT_NE(reader.Find("Own"), nullptr);
  EXPECT_TRUE(reads.empty());

  EXPECT_NE(reader.Find("P"), nullptr);
  // A unit the design does not have answers nothing, and a lowering that went
  // on without it depends on its absence.
  EXPECT_EQ(reader.Find("Absent"), nullptr);
  EXPECT_TRUE(reader.ValuesOf("P").parameters.empty());
  EXPECT_EQ(
      reader.NamespaceUnitNames(), (std::vector<std::string>{"Own", "P"}));
  EXPECT_EQ(
      reads, (SignatureReads{
                 UnitSignatureRead{.unit_name = "Absent"},
                 UnitSignatureRead{.unit_name = "P"},
                 ValuesRead{.lowering = "P"}, UnitListRead{}}));
}

// Every lowering of the design `source` states, declared and lowered through
// the same entries the command line uses.
auto LowerDesign(std::string_view source) -> std::vector<Lowering> {
  test::ElaboratedText text = test::ElaborateText(source);
  if (!text.front.elaborated.has_value()) return {};

  diag::DiagnosticSink sink;
  auto design = compiler::DeclareUnits(
      std::move(text.front.elaborated->compilation), compiler::LoweringPolicy{},
      sink, text.front.elaborated->diag_sources);
  EXPECT_TRUE(design.has_value()) << "the design did not declare";
  if (!design.has_value()) return {};

  std::vector<Lowering> lowerings;
  compiler::LowerToHir(
      *design, sink, 1,
      [](Lowering lowered) -> diag::Result<Lowering> { return lowered; },
      [&lowerings](Lowering lowered) {
        lowerings.push_back(std::move(lowered));
      });
  EXPECT_FALSE(sink.HasErrors()) << "the design did not lower";
  return lowerings;
}

// What the lowering named `name` read, or nothing where the design has no such
// lowering.
auto ReadsOf(const std::vector<Lowering>& lowerings, std::string_view name)
    -> std::optional<SignatureReads> {
  for (const Lowering& lowered : lowerings) {
    if (lowered.name == name) return lowered.reads;
  }
  return std::nullopt;
}

TEST(SignatureReadsTest, AModuleReadingNothingOfAnotherUnitRecordsNothing) {
  const std::vector<Lowering> lowerings = LowerDesign(R"(
module Top;
  int x;
  initial x = 1;
endmodule
)");
  EXPECT_EQ(ReadsOf(lowerings, "Top"), SignatureReads{});
}

// The class the handle names is declared in `p` and extends one of `q`, and no
// class declares the hook, so the call runs the built-in one and what is
// written names no class; finding that out read both packages. The object is
// made in `p`, so nothing else in `Top` reads the lineage.
TEST(SignatureReadsTest, AHookNoClassOfTheLineageDeclaresReadsTheLineage) {
  const std::vector<Lowering> lowerings = LowerDesign(R"(
package q;
  class Base;
  endclass
endpackage

package p;
  class Derived extends q::Base;
  endclass

  function automatic Derived make();
    Derived made = new;
    return made;
  endfunction
endpackage

module Top;
  p::Derived d;
  initial begin
    d = p::make();
    d.pre_randomize();
  end
endmodule
)");
  EXPECT_EQ(
      ReadsOf(lowerings, "Top"), (SignatureReads{
                                     UnitSignatureRead{.unit_name = "p"},
                                     UnitSignatureRead{.unit_name = "q"}}));
}

// A packed structure and an enumeration a package declares are no class and
// no member of its namespace the code names, and a call leaving a default
// takes the default's type off the package too.
TEST(SignatureReadsTest, APackagesTypesAndCalleesAreRead) {
  const std::vector<Lowering> lowerings = LowerDesign(R"(
package p;
  typedef struct packed { logic [3:0] a; } pair_t;
  typedef enum { kA, kB } choice_t;
  function automatic int f(int x = 2);
    return x;
  endfunction
endpackage

module Top;
  p::pair_t pair;
  p::choice_t choice;
  int y;
  initial y = p::f();
endmodule
)");
  EXPECT_EQ(
      ReadsOf(lowerings, "Top"),
      (SignatureReads{UnitSignatureRead{.unit_name = "p"}}));
}

// A child's ports are connected and its parameter handed at the types its unit
// published, so the parent reads the child's unit.
TEST(SignatureReadsTest, AChildsPortsAndParametersAreRead) {
  const std::vector<Lowering> lowerings = LowerDesign(R"(
module Child #(parameter int W = 1) (input logic [W-1:0] a);
endmodule

module Top;
  logic [2:0] s;
  Child #(.W(3)) c(.a(s));
endmodule
)");
  const std::optional<SignatureReads> reads = ReadsOf(lowerings, "Top");
  ASSERT_TRUE(reads.has_value());
  ASSERT_EQ(reads->size(), 1U);
  const auto* child = std::get_if<UnitSignatureRead>(&*reads->begin());
  ASSERT_NE(child, nullptr);
  EXPECT_TRUE(child->unit_name.starts_with("Child"));
}

// Two instances of one module are two lowerings, each building the block its
// parameter chooses, and each depends on what it read and not on what the
// other did.
TEST(SignatureReadsTest, EachLoweringOfAUnitHasItsOwnReads) {
  const std::vector<Lowering> lowerings = LowerDesign(R"(
package p;
  function automatic int f();
    return 1;
  endfunction
endpackage

package q;
  function automatic int g();
    return 2;
  endfunction
endpackage

module M #(parameter bit kSel = 0);
  int y;
  if (kSel) begin : chosen
    initial y = p::f();
  end else begin : other
    initial y = q::g();
  end
endmodule

module Top;
  M #(.kSel(1)) a();
  M #(.kSel(0)) b();
endmodule
)");
  EXPECT_EQ(
      ReadsOf(lowerings, "Top.a"),
      (SignatureReads{UnitSignatureRead{.unit_name = "p"}}));
  EXPECT_EQ(
      ReadsOf(lowerings, "Top.b"),
      (SignatureReads{UnitSignatureRead{.unit_name = "q"}}));
}

// A package's parameter is the value its lowering published, folded where the
// name reaching it is written.
TEST(SignatureReadsTest, APackagesParameterIsReadAsItsPublishedValue) {
  const std::vector<Lowering> lowerings = LowerDesign(R"(
package p;
  parameter int kWidth = 3;
endpackage

module Top;
  int y;
  initial y = p::kWidth;
endmodule
)");
  const std::optional<SignatureReads> reads = ReadsOf(lowerings, "Top");
  ASSERT_TRUE(reads.has_value());
  EXPECT_TRUE(reads->contains(ValuesRead{.lowering = "p"}));
}

// A name leaving an instance and reaching another instance of the same module
// lands on the unit's own declarations (LRM 23.6), which the unit depends on
// anyway, so only the unit the name passes through is read.
TEST(SignatureReadsTest, ANameReachingAnotherInstanceOfItsOwnUnitReadsItNot) {
  const std::vector<Lowering> lowerings = LowerDesign(R"(
module M;
  int x;
  int y;
  initial y = Top.b.x;
endmodule

module Top;
  M a();
  M b();
endmodule
)");
  EXPECT_EQ(
      ReadsOf(lowerings, "Top.a"),
      (SignatureReads{UnitSignatureRead{.unit_name = "Top"}}));
}

}  // namespace
}  // namespace lyra::hir
