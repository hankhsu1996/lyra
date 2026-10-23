// A built program, and the compiler building it, that is refused memory says
// so in one line and exits with a failing status, whatever it was doing when it
// asked. What is held here is that under any address limit each either
// finishes or ends that way, and never by a signal or as a report of its own
// defect.
//
// Where a limit falls among the requests belongs to the build of the runtime
// and of the compiler, so this is a statement about each as it is shipped.

#include <filesystem>
#include <fstream>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <string>
#include <string_view>
#include <vector>

#include "tests/framework/cli_fixture.hpp"
#include "tests/framework/process.hpp"

namespace {

using lyra::test::ExitCodeOf;
using lyra::test::ExitedWithCode;
using lyra::test::MakeScratchDir;
using lyra::test::ResolveLyra;
using lyra::test::RunChildProcess;
using lyra::test::RunUnderAddressLimit;

// Three things that ask for memory at three moments, each large enough that a
// run of limits lands in it: storage held by instances, which is asked for as
// the design is built; procedures, each registered before time 0; and calls of
// a task that waits sixty-four calls deep, each call holding its own
// activation.
constexpr std::string_view kDesign = R"(
module Store;
  logic [7:0] held[4096];
endmodule

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
  logic [7:0] d[1500];
  logic [7:0] q[1500];

  for (genvar i = 0; i < 6000; i++) begin : stores
    Store s ();
  end
  for (genvar i = 0; i < 1500; i++) begin : leaves
    Leaf u (
        .clk(clk),
        .d  (d[i]),
        .q  (q[i])
    );
  end

  task automatic descend(int levels);
    if (levels == 0) @(posedge clk);
    else descend(levels - 1);
  endtask

  initial begin
    for (int i = 0; i < 2000; i++) begin
      fork
        descend(64);
      join_none
    end
    #1 clk = 1;
    #1 $finish;
  end
endmodule
)";

constexpr std::string_view kSaidInWords =
    "error: the simulation ran out of memory";

TEST(OutOfMemory, IsSaidInWordsWhereverTheRequestWasMade) {
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
  const auto built = RunChildProcess(lyra, build);
  ASSERT_THAT(built, ExitedWithCode(0))
      << built.stdout_text << built.stderr_text;

  // From what the program needs to start at all to past what the whole run
  // needs, in steps that are no multiple of anything an allocator grows by.
  constexpr int kLowestKib = 24'000;
  constexpr int kHighestKib = 400'000;
  constexpr int kStepKib = 7'919;
  int refused = 0;
  int finished = 0;
  for (int limit = kLowestKib; limit <= kHighestKib; limit += kStepKib) {
    const auto ran = RunUnderAddressLimit(program, {}, limit);
    if (ExitCodeOf(ran) == 0) {
      ++finished;
      continue;
    }
    ++refused;
    EXPECT_THAT(ran, ExitedWithCode(1)) << "under " << limit << " KiB\n"
                                        << ran.stderr_text;
    EXPECT_NE(ran.stderr_text.find(kSaidInWords), std::string::npos)
        << "under " << limit << " KiB the program said:\n"
        << ran.stderr_text;
  }
  // A run of limits that refused nothing, or let nothing through, would hold
  // for a program that never asks for memory or never starts.
  EXPECT_GT(refused, 0);
  EXPECT_GT(finished, 0);
}

// Enough procedures that compiling the design's units asks for memory at every
// stage a build passes through: lowering, starting the threads that compile,
// and the code generator splitting each procedure into its resumable parts.
constexpr std::string_view kDesignToCompile = R"(
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
  logic [7:0] d[64];
  logic [7:0] q[64];
  for (genvar i = 0; i < 64; i++) begin : leaves
    Leaf u (.clk(clk), .d(d[i]), .q(q[i]));
    always @(posedge clk) d[i] <= d[i] + 8'(i);
  end
  initial begin
    #1 clk = 1;
    #1 $finish;
  end
endmodule
)";

constexpr std::string_view kCompilerSaidInWords =
    "error: the compiler ran out of memory";

TEST(OutOfMemory, TheCompilerSaysItInWordsWhereverItWasRefused) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();
  auto tmp_or = MakeScratchDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();
  const auto src = *tmp_or / "test.sv";
  std::ofstream(src) << kDesignToCompile;

  // From where the compiler starts at all to past what building the design
  // needs, each build with a store of its own so that it compiles everything.
  constexpr int kLowestKib = 140'000;
  constexpr int kHighestKib = 560'000;
  constexpr int kStepKib = 9'973;
  int refused = 0;
  int finished = 0;
  for (int limit = kLowestKib; limit <= kHighestKib; limit += kStepKib) {
    const auto at = *tmp_or / std::to_string(limit);
    const std::vector<std::string> build = {
        "build",
        "--backend",
        "llvm",
        "--top",
        "Test",
        "--cache-dir",
        (at / "cache").string(),
        "-o",
        (at / "sim").string(),
        src.string()};
    const auto built = RunUnderAddressLimit(lyra, build, limit);
    if (ExitCodeOf(built) == 0) {
      ++finished;
      continue;
    }
    ++refused;
    EXPECT_THAT(built, ExitedWithCode(1)) << "under " << limit << " KiB\n"
                                          << built.stderr_text;
    EXPECT_NE(built.stderr_text.find(kCompilerSaidInWords), std::string::npos)
        << "under " << limit << " KiB the compiler said:\n"
        << built.stderr_text;
  }
  EXPECT_GT(refused, 0);
  EXPECT_GT(finished, 0);
}

}  // namespace
