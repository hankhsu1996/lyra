// A built program that is refused memory says so in one line and exits with a
// failing status, whatever it was doing when it asked: building the design,
// registering its processes, or entering a subroutine. What is held here is
// that under any address limit the program either finishes or ends that way,
// and never by a signal.
//
// Where a limit falls among the program's requests belongs to the build of the
// runtime, so this is a statement about the program as it is shipped.

#include <filesystem>
#include <format>
#include <fstream>
#include <gtest/gtest.h>
#include <string>
#include <string_view>
#include <vector>

#include "lyra/driver/subprocess.hpp"
#include "tests/framework/cli_fixture.hpp"
#include "tests/framework/process.hpp"

namespace {

using lyra::test::MakeScratchDir;
using lyra::test::ProcessOutcome;
using lyra::test::ResolveLyra;
using lyra::test::RunChildProcess;
using lyra::test::TerminationKind;
using namespace std::chrono_literals;

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

constexpr std::string_view kSaidInWords = "lyra: out of memory";

// Runs `program` in a shell that has first limited its own address space to
// `kibibytes`.
auto RunUnderAddressLimit(const std::filesystem::path& program, int kibibytes)
    -> ProcessOutcome {
  auto sh_or = lyra::driver::FindOnPath("sh");
  EXPECT_TRUE(sh_or.has_value());
  if (!sh_or) return {};
  const std::vector<std::string> args = {
      "-c",
      std::format("ulimit -v {} && exec '{}'", kibibytes, program.string())};
  return RunChildProcess(*sh_or, args, 60s);
}

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
  const auto built = RunChildProcess(lyra, build, 300s);
  ASSERT_EQ(built.termination, TerminationKind::kExitedNormally)
      << built.stdout_text << built.stderr_text;

  // From what the program needs to start at all to past what the whole run
  // needs, in steps that are no multiple of anything an allocator grows by.
  constexpr int kLowestKib = 24'000;
  constexpr int kHighestKib = 400'000;
  constexpr int kStepKib = 7'919;
  int refused = 0;
  int finished = 0;
  for (int limit = kLowestKib; limit <= kHighestKib; limit += kStepKib) {
    const auto ran = RunUnderAddressLimit(program, limit);
    if (ran.termination == TerminationKind::kExitedNormally) {
      ++finished;
      continue;
    }
    ++refused;
    EXPECT_EQ(ran.termination, TerminationKind::kExitedNonZero)
        << "under " << limit << " KiB the program ended by signal "
        << ran.signal_number << "\n"
        << ran.stderr_text;
    EXPECT_EQ(ran.exit_code, 1) << "under " << limit << " KiB";
    EXPECT_NE(ran.stderr_text.find(kSaidInWords), std::string::npos)
        << "under " << limit << " KiB the program said:\n"
        << ran.stderr_text;
  }
  // A run of limits that refused nothing, or let nothing through, would hold
  // for a program that never asks for memory or never starts.
  EXPECT_GT(refused, 0);
  EXPECT_GT(finished, 0);
}

}  // namespace
