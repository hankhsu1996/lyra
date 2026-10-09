// How long an expression a design may write is bounded by the stack the
// compiler follows it on: a run of one binary operator groups from the left
// (LRM Table 11-2), so it is as deep as it is long. What is held here is that a
// run of three thousand operands compiles and evaluates like its short form,
// that the command takes the stack for it where it was started with less, and
// that running out is said in words.
//
// A frame's size belongs to the build of the compiler, so this is a statement
// about the compiler as it is shipped.

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
using lyra::test::ResolveLyra;
using lyra::test::RunChildProcess;
using lyra::test::TerminationKind;
using namespace std::chrono_literals;

// A sum and a logical or of three thousand operands each, two-state and
// four-state, checked against what their short forms give. The runs of one
// repeated operand are spelled by a macro.
constexpr std::string_view kChainsOfThousands = R"(
`define TEN(x) x x x x x x x x x x
`define THOUSAND(x) `TEN(`TEN(`TEN(x)))
`define THREE_THOUSAND(x) `THOUSAND(x) `THOUSAND(x) `THOUSAND(x)

module Test;
  int v;
  int sum;
  bit z, t;
  bit any_true, none_true;
  logic lz, lt;
  logic any_true4;

  initial begin
    v = 1;
    z = 0;
    t = 1;
    lz = 1'b0;
    lt = 1'b1;

    sum = `THREE_THOUSAND(v +) v;
    if (sum !== 3001) $fatal(1, "the sum was %0d, expected 3001", sum);

    any_true = `THREE_THOUSAND(z ||) t;
    none_true = `THREE_THOUSAND(z ||) z;
    if (any_true !== 1'b1) $fatal(1, "the logical or ending in 1 was %b", any_true);
    if (none_true !== 1'b0) $fatal(1, "the logical or of zeros was %b", none_true);

    any_true4 = `THREE_THOUSAND(lz ||) lt;
    if (any_true4 !== 1'b1) $fatal(1, "the four-state logical or was %b", any_true4);
  end
endmodule
)";

// A design whose one expression is a sum of `operands` operands.
auto WriteLongSum(const std::filesystem::path& path, int operands) -> void {
  std::ofstream out(path);
  out << "module Test;\n  int v, sum;\n  initial sum = v";
  for (int i = 1; i < operands; ++i) {
    out << " + v";
  }
  out << ";\nendmodule\n";
}

// Runs `lyra check` on `src` in a shell that has first set its own stack limit
// with `limit`, a `ulimit` invocation.
auto CheckUnderStackLimit(
    const std::filesystem::path& lyra, const std::filesystem::path& src,
    std::string_view limit) -> lyra::test::ProcessOutcome {
  auto sh_or = lyra::driver::FindOnPath("sh");
  EXPECT_TRUE(sh_or.has_value());
  if (!sh_or) return {};
  const std::vector<std::string> args = {
      "-c", std::format(
                "{} && exec '{}' check --top Test '{}'", limit, lyra.string(),
                src.string())};
  return RunChildProcess(*sh_or, args, 60s);
}

TEST(CompileStack, ARunOfThousandsOfOperandsEvaluatesLikeItsShortForm) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();
  auto tmp_or = MakeScratchDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();
  const auto src = *tmp_or / "test.sv";
  std::ofstream(src) << kChainsOfThousands;

  const std::vector<std::string> args = {
      "run",
      "--backend",
      "llvm",
      "--top",
      "Test",
      "--cache-dir",
      (*tmp_or / "cache").string(),
      src.string()};
  const auto ran = RunChildProcess(lyra, args, 300s);
  ASSERT_EQ(ran.termination, TerminationKind::kExitedNormally)
      << ran.stderr_text;
  EXPECT_EQ(ran.exit_code, 0) << ran.stdout_text << ran.stderr_text;
}

// The command takes what a long expression needs for itself where it was
// started with less and is allowed more.
TEST(CompileStack, IsTakenWhereTheCommandWasStartedWithLess) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();
  auto tmp_or = MakeScratchDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();
  const auto src = *tmp_or / "test.sv";
  WriteLongSum(src, 3000);

  const auto checked = CheckUnderStackLimit(lyra, src, "ulimit -S -s 256");
  EXPECT_EQ(checked.termination, TerminationKind::kExitedNormally)
      << checked.stderr_text;
}

// Where it is allowed no more, running out is said in words before the run
// ends, so the reader is not left with a signal's name.
TEST(CompileStack, RunningOutIsSaidInWords) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();
  auto tmp_or = MakeScratchDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();
  const auto src = *tmp_or / "test.sv";
  WriteLongSum(src, 3000);

  const auto checked = CheckUnderStackLimit(lyra, src, "ulimit -s 256");
  EXPECT_NE(checked.termination, TerminationKind::kExitedNormally);
  EXPECT_NE(
      checked.stderr_text.find("the compiler ran out of stack"),
      std::string::npos)
      << checked.stderr_text;
}

}  // namespace
