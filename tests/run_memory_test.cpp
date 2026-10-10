// What a built program holds for a process that is waiting: the place it
// resumes at, a record for each place it watches, and a few words of everything
// else a process has. What is held here is that a design of many waiting
// processes runs inside an address limit set by that, and so that nothing a
// process rarely uses is paid for by every one of them.
//
// How much a program asks for belongs to the build of the runtime, so this is a
// statement about the program as it is shipped.

#include <filesystem>
#include <fstream>
#include <gtest/gtest.h>
#include <string>
#include <string_view>
#include <vector>

#include "tests/framework/cli_fixture.hpp"
#include "tests/framework/process.hpp"

namespace {

using lyra::test::MakeScratchDir;
using lyra::test::ResolveLyra;
using lyra::test::RunChildProcess;
using lyra::test::RunUnderAddressLimit;
using lyra::test::TerminationKind;
using namespace std::chrono_literals;

// Eight thousand instances of a leaf that holds three procedures and is reached
// through three ports, every procedure waiting on one signal for the whole run.
constexpr std::string_view kDesign = R"(
module Leaf (
    input logic clk,
    input logic [7:0] d,
    output logic [7:0] q
);
  always @(posedge clk) q <= d;
  always @(d) if (d === 8'hff) $display("d is all ones");
  always @(q) if (q === 8'hff) $display("q is all ones");
endmodule

module Test;
  logic clk = 0;
  logic [7:0] d[8000];
  logic [7:0] q[8000];

  for (genvar i = 0; i < 8000; i++) begin : leaves
    Leaf u (
        .clk(clk),
        .d  (d[i]),
        .q  (q[i])
    );
  end

  initial begin
    #1 clk = 1;
    #1 $finish;
  end
endmodule
)";

// Twelve kibibytes an instance. The run asks for between sixty and seventy
// thousand, and one more kilobyte in each of its processes would take it past
// this.
constexpr int kAddressLimitKib = 96'000;

TEST(RunMemory, AWaitingProcessHoldsWhatItHas) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();
  auto tmp_or = MakeScratchDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();
  const auto src = *tmp_or / "test.sv";
  std::ofstream(src) << kDesign;
  const auto program = *tmp_or / "sim";

  const std::vector<std::string> build = {
      "build",
      "--backend",
      "llvm",
      "--top",
      "Test",
      "--cache-dir",
      (*tmp_or / "cache").string(),
      "-o",
      program.string(),
      src.string()};
  const auto built = RunChildProcess(lyra, build, 300s);
  ASSERT_EQ(built.termination, TerminationKind::kExitedNormally)
      << built.stdout_text << built.stderr_text;

  const auto ran = RunUnderAddressLimit(program, kAddressLimitKib);
  EXPECT_EQ(ran.termination, TerminationKind::kExitedNormally)
      << "under " << kAddressLimitKib << " KiB the program said:\n"
      << ran.stderr_text;
}

}  // namespace
