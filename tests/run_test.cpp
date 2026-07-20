#include <filesystem>
#include <fstream>
#include <gtest/gtest.h>
#include <memory>
#include <string>
#include <vector>

#include "lyra/support/subprocess.hpp"
#include "tests/framework/build.hpp"
#include "tests/framework/process.hpp"
#include "tools/cpp/runfiles/runfiles.h"

namespace {

using bazel::tools::cpp::runfiles::Runfiles;
using lyra::test::MakeTempCaseDir;
using lyra::test::RunChildProcess;
using lyra::test::TerminationKind;
using namespace std::chrono_literals;

auto ResolveLyra() -> std::filesystem::path {
  std::string err;
  std::unique_ptr<Runfiles> runfiles{Runfiles::CreateForTest(&err)};
  EXPECT_TRUE(runfiles) << err;
  return runfiles ? std::filesystem::path(runfiles->Rlocation("_main/lyra"))
                  : std::filesystem::path{};
}

auto WriteTrivialSource(const std::filesystem::path& path) -> void {
  std::ofstream out(path);
  out << "module Test;\n"
      << "  initial $display(\"ran %0d\", 6 * 7);\n"
      << "endmodule\n";
}

// A two-level hierarchy: the top instantiates a submodule, and each level runs
// an initial block. Elaborating it exercises the design-root unit constructing
// the top and the top constructing its submodule as owned children.
auto WriteHierarchicalSource(const std::filesystem::path& path) -> void {
  std::ofstream out(path);
  out << "module Leaf;\n"
      << "  initial $display(\"leaf ran\");\n"
      << "endmodule\n"
      << "module Test;\n"
      << "  Leaf a();\n"
      << "  initial $display(\"top ran\");\n"
      << "endmodule\n";
}

// Straight-line procedural code over the integral and string value domains:
// module and process variables, arithmetic, comparison, a conditional
// expression, a loop with early exits, a subroutine that writes its own
// parameter, and a `$display` that formats values rather than a bare literal.
auto WriteProceduralSource(const std::filesystem::path& path) -> void {
  std::ofstream out(path);
  out << "module Test;\n"
      << "  int total = 0;\n"
      << "  string name = \"lyra\";\n"
      << "  bit [3:0] narrow;\n"
      << "  function automatic int scale(int n);\n"
      << "    n = n * 10;\n"
      << "    return n;\n"
      << "  endfunction\n"
      << "  initial begin\n"
      << "    for (int k = 0; k < 5; k++) begin\n"
      << "      if (k == 2) continue;\n"
      << "      if (k == 4) break;\n"
      << "      total = total + k;\n"
      << "    end\n"
      << "    narrow = 8'hFF;\n"
      << "    $display(\"total=%0d\", total);\n"
      << "    $display(\"pick=%0d\", total > 3 ? 111 : 222);\n"
      << "    $display(\"scaled=%0d\", scale(4));\n"
      << "    $display(\"narrow=%0d\", narrow);\n"
      << "    $display(\"name=%s eq=%0d\", name, name == \"lyra\");\n"
      << "    if (total > 0 && total < 100) $display(\"in range\");\n"
      << "  end\n"
      << "endmodule\n";
}

// Timing control: two processes suspend on delays and resume in simulation-time
// order, and a `#0` re-enters this slot's inactive region. Running it exercises
// the execution backend's suspend/resume protocol -- a process parks, the
// scheduler advances time, and the process resumes at its next block -- across
// interleaved processes, which straight-line code never reaches.
auto WriteTimingSource(const std::filesystem::path& path) -> void {
  std::ofstream out(path);
  out << "module Test;\n"
      << "  int a;\n"
      << "  int b;\n"
      << "  initial begin\n"
      << "    a = 1;\n"
      << "    #10;\n"
      << "    a = 2;\n"
      << "    $display(\"a=%0d\", a);\n"
      << "  end\n"
      << "  initial begin\n"
      << "    b = 1;\n"
      << "    #5;\n"
      << "    b = 2;\n"
      << "    $display(\"b=%0d\", b);\n"
      << "    #0;\n"
      << "    $display(\"b0 done\");\n"
      << "  end\n"
      << "endmodule\n";
}

// Scalar DPI-C imports (LRM 35.5): each argument is marshaled to its declared C
// carrier and the result back. The carriers span the widths, so the boundary's
// machine-integer conversion is exercised in both directions -- `byte` narrows
// the widest machine integer to a C `signed char` and widens the returned one
// back.
auto WriteDpiImportSource(const std::filesystem::path& path) -> void {
  std::ofstream out(path);
  out << "module Test;\n"
      << "  import \"DPI-C\" function int add_one(input int x);\n"
      << "  import \"DPI-C\" function byte twice(input byte v);\n"
      << "  import \"DPI-C\" function longint widen(input longint v);\n"
      << "  import \"DPI-C\" function int slen(input string s);\n"
      << "  initial begin\n"
      << "    $display(\"add=%0d\", add_one(41));\n"
      << "    $display(\"twice=%0d\", twice(-5));\n"
      << "    $display(\"widen=%0d\", widen(64'd4294967296));\n"
      << "    $display(\"len=%0d\", slen(\"lyra\"));\n"
      << "  end\n"
      << "endmodule\n";
}

auto WriteDpiImportForeign(const std::filesystem::path& path) -> void {
  std::ofstream out(path);
  out << "#include <string.h>\n"
      << "int add_one(int x) { return x + 1; }\n"
      << "signed char twice(signed char v) { return (signed char)(v * 2); }\n"
      << "long long widen(long long v) { return v + 1; }\n"
      << "int slen(const char* s) { return (int)strlen(s); }\n";
}

// Every SV construct that waits on a value change at once: an implicit
// sensitivity (`always_comb`), an explicit event list with two edges, and a
// `wait (cond)` re-check loop, all driven by a delayed clock spelled out as a
// sequence of edges to keep the value-change focus separate from the
// loop-carried-value concern a separate case covers.
auto WriteValueChangeWaitSource(const std::filesystem::path& path) -> void {
  std::ofstream out(path);
  out << "module Test;\n"
      << "  logic clk = 0;\n"
      << "  logic rst = 0;\n"
      << "  logic go = 0;\n"
      << "  int count = 0;\n"
      << "  int doubled = 0;\n"
      << "  always_comb doubled = count * 2;\n"
      << "  always @(posedge clk or posedge rst) begin\n"
      << "    count = count + 1;\n"
      << "    $display(\"edge count=%0d doubled=%0d\", count, doubled);\n"
      << "  end\n"
      << "  initial begin\n"
      << "    wait (go);\n"
      << "    $display(\"released at go\");\n"
      << "    #5;\n"
      << "    clk = 1;\n"
      << "    #5;\n"
      << "    clk = 0;\n"
      << "    #5;\n"
      << "    rst = 1;\n"
      << "  end\n"
      << "  initial begin\n"
      << "    #2;\n"
      << "    go = 1;\n"
      << "  end\n"
      << "endmodule\n";
}

auto WriteCrossSuspensionLoopSource(const std::filesystem::path& path) -> void {
  std::ofstream out(path);
  out << "module Test;\n"
      << "  logic clk = 0;\n"
      << "  int ticks = 0;\n"
      << "  initial begin\n"
      << "    int n = 3;\n"
      << "    for (int i = 0; i < n; i = i + 1) begin\n"
      << "      #5;\n"
      << "      clk = ~clk;\n"
      << "      ticks = ticks + 1;\n"
      << "      $display(\"i=%0d n=%0d clk=%0b ticks=%0d\", i, n, clk, "
         "ticks);\n"
      << "    end\n"
      << "    $display(\"final ticks=%0d\", ticks);\n"
      << "  end\n"
      << "endmodule\n";
}

auto WriteNestedSuspensionSource(const std::filesystem::path& path) -> void {
  std::ofstream out(path);
  out << "module Test;\n"
      << "  int total = 0;\n"
      << "  initial begin\n"
      << "    int outer = 2;\n"
      << "    for (int i = 0; i < outer; i = i + 1) begin\n"
      << "      automatic int inner_sum = 0;\n"
      << "      for (int j = 0; j < 3; j = j + 1) begin\n"
      << "        #1;\n"
      << "        if (j == 1) inner_sum = inner_sum + 10;\n"
      << "        else inner_sum = inner_sum + 1;\n"
      << "      end\n"
      << "      total = total + inner_sum;\n"
      << "      $display(\"i=%0d inner_sum=%0d total=%0d\", i, inner_sum, "
         "total);\n"
      << "    end\n"
      << "    $display(\"final total=%0d\", total);\n"
      << "  end\n"
      << "endmodule\n";
}

// The real-family value domain end to end: a real and a shortreal constant, a
// shortreal-to-real reshape, an integer-to-real conversion, a real accumulated
// across suspensions (so it is an activation-frame value), a real comparison, a
// real-to-integer round, and real formatting. Exercises the scalar real domain
// the same way the integral and string domains are exercised elsewhere.
auto WriteRealFamilySource(const std::filesystem::path& path) -> void {
  std::ofstream out(path);
  out << "module Test;\n"
      << "  initial begin\n"
      << "    real r = 0.0;\n"
      << "    shortreal s = 2.5;\n"
      << "    int n = 3;\n"
      << "    real from_int = n;\n"
      << "    real widened = s;\n"
      << "    for (int i = 0; i < n; i = i + 1) begin\n"
      << "      #1;\n"
      << "      r = r + 1.5;\n"
      << "    end\n"
      << "    if (r > 4.0)\n"
      << "      $display(\"r=%0.2f widened=%0.2f from_int=%0.2f\", r, "
         "widened, from_int);\n"
      << "    $display(\"rounded=%0d sum=%0.2f\", int'(r), r + from_int);\n"
      << "  end\n"
      << "endmodule\n";
}

auto WriteChandleSource(const std::filesystem::path& path) -> void {
  std::ofstream out(path);
  out << "module Test;\n"
      << "  chandle h;\n"
      << "  chandle g;\n"
      << "  initial begin\n"
      << "    $display(\"h_null=%0d not_h=%0d\", h == null, !h);\n"
      << "    g = h;\n"
      << "    $display(\"g_eq_h=%0d g_ne_h=%0d\", g == h, g != h);\n"
      << "    $display(\"g_ceq_h=%0d g_cne_h=%0d\", g === h, g !== h);\n"
      << "  end\n"
      << "endmodule\n";
}

auto WriteLogicalOperatorSource(const std::filesystem::path& path) -> void {
  std::ofstream out(path);
  out << "module Test;\n"
      << "  real a;\n"
      << "  real b;\n"
      << "  string s;\n"
      << "  initial begin\n"
      << "    a = 1.0;\n"
      << "    b = 0.0;\n"
      << "    s = \"x\";\n"
      << "    $display(\"and=%0d or=%0d\", a && b, a || b);\n"
      << "    $display(\"equiv=%0d impl=%0d\", a <-> b, a -> b);\n"
      << "    $display(\"not_a=%0d not_b=%0d\", !a, !b);\n"
      << "    $display(\"str=%0d\", (s.len() > 0) && a);\n"
      << "  end\n"
      << "endmodule\n";
}

// Exercises the unpacked-struct value domain end to end: default value,
// assignment-pattern construction, whole-value copy with value semantics, the
// equality families, component read and write (including a nested product and a
// string component), a struct local that crosses a suspension, and partial
// writes to an observable struct signal that a reader reacts to.
auto WriteStructSource(const std::filesystem::path& path) -> void {
  std::ofstream out(path);
  out << "module Test;\n"
      << "  typedef struct { int a; int b; } Pair;\n"
      << "  typedef struct { Pair p; int c; string s; } Nest;\n"
      << "  Pair s;\n"
      << "  Pair t;\n"
      << "  Nest n;\n"
      << "  Pair sig;\n"
      << "  int mirror = 0;\n"
      << "  always_comb mirror = sig.a * 100 + sig.b;\n"
      << "  initial begin\n"
      << "    $display(\"def a=%0d b=%0d\", s.a, s.b);\n"
      << "    s = '{a: 3, b: 7};\n"
      << "    $display(\"con a=%0d b=%0d\", s.a, s.b);\n"
      << "    t = s;\n"
      << "    s.a = 100;\n"
      << "    $display(\"copy t.a=%0d s.a=%0d\", t.a, s.a);\n"
      << "    t = '{a: 100, b: 7};\n"
      << "    $display(\"eq=%0d ne=%0d ceq=%0d\", s == t, s != t, s === t);\n"
      << "    t.b = 8;\n"
      << "    $display(\"eq2=%0d\", s == t);\n"
      << "    n = '{p: '{a: 1, b: 2}, c: 9, s: \"hi\"};\n"
      << "    n.p.b = 20;\n"
      << "    n.s = \"bye\";\n"
      << "    $display(\"nest a=%0d b=%0d c=%0d s=%s\", n.p.a, n.p.b, n.c, "
         "n.s);\n"
      << "    begin\n"
      << "      automatic Pair loc = '{a: 42, b: 43};\n"
      << "      #1;\n"
      << "      $display(\"xsusp a=%0d b=%0d\", loc.a, loc.b);\n"
      << "    end\n"
      << "    sig = '{a: 1, b: 2};\n"
      << "    #1;\n"
      << "    $display(\"whole mirror=%0d\", mirror);\n"
      << "    sig.a = 7;\n"
      << "    #1;\n"
      << "    $display(\"partial mirror=%0d sig.a=%0d\", mirror, sig.a);\n"
      << "  end\n"
      << "endmodule\n";
}

auto WriteDynArraySource(const std::filesystem::path& path) -> void {
  std::ofstream out(path);
  out << "module Test;\n"
      << "  int arr[];\n"
      << "  int brr[];\n"
      << "  int sig[];\n"
      << "  int mirror = 0;\n"
      << "  always_comb mirror = sig[0] * 100 + sig[1];\n"
      << "  initial begin\n"
      << "    $display(\"def size=%0d\", arr.size());\n"
      << "    arr = new[3];\n"
      << "    arr[0] = 5; arr[1] = 6; arr[2] = 7;\n"
      << "    $display(\"new size=%0d a0=%0d a2=%0d\", arr.size(), arr[0], "
         "arr[2]);\n"
      << "    arr[9] = 99;\n"
      << "    $display(\"oob r=%0d size=%0d\", arr[9], arr.size());\n"
      << "    brr = arr;\n"
      << "    arr[0] = 100;\n"
      << "    $display(\"alias b0=%0d a0=%0d\", brr[0], arr[0]);\n"
      << "    brr = '{5, 6, 7};\n"
      << "    arr = '{5, 6, 7};\n"
      << "    $display(\"eq=%0d ne=%0d ceq=%0d\", arr == brr, arr != brr, "
         "arr === brr);\n"
      << "    arr[2] = 8;\n"
      << "    $display(\"eq2=%0d\", arr == brr);\n"
      << "    arr = new[2](arr);\n"
      << "    $display(\"resize size=%0d a0=%0d\", arr.size(), arr[0]);\n"
      << "    brr = arr;\n"
      << "    arr.delete();\n"
      << "    $display(\"del a=%0d b=%0d\", arr.size(), brr.size());\n"
      << "    begin\n"
      << "      automatic int loc[] = '{42, 43};\n"
      << "      #1;\n"
      << "      $display(\"xsusp l0=%0d l1=%0d\", loc[0], loc[1]);\n"
      << "    end\n"
      << "    sig = new[2];\n"
      << "    sig[0] = 1; sig[1] = 2;\n"
      << "    #1;\n"
      << "    $display(\"whole mirror=%0d\", mirror);\n"
      << "    sig[0] = 7;\n"
      << "    #1;\n"
      << "    $display(\"partial mirror=%0d sig0=%0d\", mirror, sig[0]);\n"
      << "  end\n"
      << "endmodule\n";
}

TEST(LyraRun, ExecutesSourceEndToEnd) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();

  auto tmp_or = MakeTempCaseDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();
  const auto src = *tmp_or / "test.sv";
  WriteTrivialSource(src);

  const std::vector<std::string> args = {
      "run", "--no-project", "--top", "Test", src.string()};
  const auto run = RunChildProcess(lyra, args, 120s);
  ASSERT_EQ(run.termination, TerminationKind::kExitedNormally)
      << run.stdout_text << run.stderr_text;
  EXPECT_EQ(run.exit_code, 0) << run.stderr_text;
  EXPECT_NE(run.stdout_text.find("ran 42"), std::string::npos)
      << "stdout: " << run.stdout_text;
}

// The JIT backend elaborates the design through the synthesized design-root
// unit's construct: it builds the top as an owned child, and the top builds its
// submodule, so both levels' initial blocks run. This is the execution-backend
// counterpart of the C++ backend's constructor-driven elaboration.
TEST(LyraRun, JitElaboratesHierarchyThroughDesignRoot) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();

  auto tmp_or = MakeTempCaseDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();
  const auto src = *tmp_or / "test.sv";
  WriteHierarchicalSource(src);

  const std::vector<std::string> args = {
      "run", "--backend", "jit", "--no-project", "--top", "Test", src.string()};
  const auto run = RunChildProcess(lyra, args, 120s);
  ASSERT_EQ(run.termination, TerminationKind::kExitedNormally)
      << run.stdout_text << run.stderr_text;
  EXPECT_EQ(run.exit_code, 0) << run.stderr_text;
  EXPECT_NE(run.stdout_text.find("top ran"), std::string::npos)
      << "stdout: " << run.stdout_text;
  EXPECT_NE(run.stdout_text.find("leaf ran"), std::string::npos)
      << "stdout: " << run.stdout_text;
}

// The execution backend runs procedural code: a variable is a runtime-owned
// storage cell reached through a member place, an expression is a library call
// over the value's domain, and structured control flow is a CFG. The two
// backends must agree, so the same source is run through both and the outputs
// compared rather than matched against a transcript written here.
TEST(LyraRun, JitAndCppAgreeOnProceduralCode) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();

  auto tmp_or = MakeTempCaseDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();
  const auto src = *tmp_or / "test.sv";
  WriteProceduralSource(src);

  const std::vector<std::string> jit_args = {
      "run", "--backend", "jit", "--no-project", "--top", "Test", src.string()};
  const auto jit = RunChildProcess(lyra, jit_args, 120s);
  ASSERT_EQ(jit.termination, TerminationKind::kExitedNormally)
      << jit.stdout_text << jit.stderr_text;
  ASSERT_EQ(jit.exit_code, 0) << jit.stderr_text;

  const std::vector<std::string> cpp_args = {
      "run", "--no-project", "--top", "Test", src.string()};
  const auto cpp = RunChildProcess(lyra, cpp_args, 120s);
  ASSERT_EQ(cpp.termination, TerminationKind::kExitedNormally)
      << cpp.stdout_text << cpp.stderr_text;
  ASSERT_EQ(cpp.exit_code, 0) << cpp.stderr_text;

  EXPECT_EQ(jit.stdout_text, cpp.stdout_text);
  EXPECT_NE(jit.stdout_text.find("name=lyra eq=1"), std::string::npos)
      << "stdout: " << jit.stdout_text;
  EXPECT_NE(jit.stdout_text.find("scaled=40"), std::string::npos)
      << "stdout: " << jit.stdout_text;
}

// A process that consumes time suspends and resumes on the execution backend,
// and multiple such processes are driven by one scheduler on one time axis. The
// two backends must agree on both the values and their simulation-time order,
// so the same timed source is run through both and the outputs compared.
TEST(LyraRun, JitAndCppAgreeOnTimingControl) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();

  auto tmp_or = MakeTempCaseDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();
  const auto src = *tmp_or / "test.sv";
  WriteTimingSource(src);

  const std::vector<std::string> jit_args = {
      "run", "--backend", "jit", "--no-project", "--top", "Test", src.string()};
  const auto jit = RunChildProcess(lyra, jit_args, 120s);
  ASSERT_EQ(jit.termination, TerminationKind::kExitedNormally)
      << jit.stdout_text << jit.stderr_text;
  ASSERT_EQ(jit.exit_code, 0) << jit.stderr_text;

  const std::vector<std::string> cpp_args = {
      "run", "--no-project", "--top", "Test", src.string()};
  const auto cpp = RunChildProcess(lyra, cpp_args, 120s);
  ASSERT_EQ(cpp.termination, TerminationKind::kExitedNormally)
      << cpp.stdout_text << cpp.stderr_text;
  ASSERT_EQ(cpp.exit_code, 0) << cpp.stderr_text;

  EXPECT_EQ(jit.stdout_text, cpp.stdout_text);
  EXPECT_EQ(jit.stdout_text, "b=2\nb0 done\na=2\n")
      << "stdout: " << jit.stdout_text;
}

// The execution backend calls foreign C: the import lowers to an external
// symbol, which a JIT image has no link step to resolve, so the design's DPI-C
// sources are compiled into a library the execution session searches. Both
// backends marshal through the same carriers, so the same source and the same C
// must produce the same output.
TEST(LyraRun, JitAndCppAgreeOnDpiScalarImports) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();

  auto tmp_or = MakeTempCaseDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();
  const auto src = *tmp_or / "test.sv";
  const auto foreign = *tmp_or / "dpi.c";
  WriteDpiImportSource(src);
  WriteDpiImportForeign(foreign);

  const std::vector<std::string> jit_args = {
      "run",          "--backend",      "jit",
      "--no-project", "--top",          "Test",
      "--dpi-link",   foreign.string(), src.string()};
  const auto jit = RunChildProcess(lyra, jit_args, 120s);
  ASSERT_EQ(jit.termination, TerminationKind::kExitedNormally)
      << jit.stdout_text << jit.stderr_text;
  ASSERT_EQ(jit.exit_code, 0) << jit.stderr_text;

  const std::vector<std::string> cpp_args = {
      "run",        "--no-project",   "--top",     "Test",
      "--dpi-link", foreign.string(), src.string()};
  const auto cpp = RunChildProcess(lyra, cpp_args, 120s);
  ASSERT_EQ(cpp.termination, TerminationKind::kExitedNormally)
      << cpp.stdout_text << cpp.stderr_text;
  ASSERT_EQ(cpp.exit_code, 0) << cpp.stderr_text;

  EXPECT_EQ(jit.stdout_text, cpp.stdout_text);
  EXPECT_NE(jit.stdout_text.find("add=42"), std::string::npos)
      << "stdout: " << jit.stdout_text;
  EXPECT_NE(jit.stdout_text.find("twice=-10"), std::string::npos)
      << "stdout: " << jit.stdout_text;
  EXPECT_NE(jit.stdout_text.find("widen=4294967297"), std::string::npos)
      << "stdout: " << jit.stdout_text;
  EXPECT_NE(jit.stdout_text.find("len=4"), std::string::npos)
      << "stdout: " << jit.stdout_text;
}

// A process suspended on a value change resumes when a leaf it subscribed to
// changes as its edge demands, and a leaf it did not subscribe to leaves it
// parked. The two backends must agree on which changes wake which process and
// in what simulation-time order, so the same source runs through both and the
// outputs are compared.
TEST(LyraRun, JitAndCppAgreeOnValueChangeWait) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();

  auto tmp_or = MakeTempCaseDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();
  const auto src = *tmp_or / "test.sv";
  WriteValueChangeWaitSource(src);

  const std::vector<std::string> jit_args = {
      "run", "--backend", "jit", "--no-project", "--top", "Test", src.string()};
  const auto jit = RunChildProcess(lyra, jit_args, 120s);
  ASSERT_EQ(jit.termination, TerminationKind::kExitedNormally)
      << jit.stdout_text << jit.stderr_text;
  ASSERT_EQ(jit.exit_code, 0) << jit.stderr_text;

  const std::vector<std::string> cpp_args = {
      "run", "--no-project", "--top", "Test", src.string()};
  const auto cpp = RunChildProcess(lyra, cpp_args, 120s);
  ASSERT_EQ(cpp.termination, TerminationKind::kExitedNormally)
      << cpp.stdout_text << cpp.stderr_text;
  ASSERT_EQ(cpp.exit_code, 0) << cpp.stderr_text;

  EXPECT_EQ(jit.stdout_text, cpp.stdout_text);
  EXPECT_EQ(
      jit.stdout_text,
      "released at go\nedge count=1 doubled=0\nedge count=2 doubled=2\n")
      << "stdout: " << jit.stdout_text;
}

// A value whose lifetime crosses a suspension lives past the stretch that
// produced it on the execution backend: a loop counter reassigned each
// iteration and a read-only local read after a resume both survive the `#5`,
// realized as activation value cells rather than handles into a per-stretch
// arena. Without that storage the counter's handle would dangle after the first
// suspension. The two backends must agree, so the same loop runs through both.
TEST(LyraRun, JitAndCppAgreeOnCrossSuspensionLoop) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();

  auto tmp_or = MakeTempCaseDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();
  const auto src = *tmp_or / "test.sv";
  WriteCrossSuspensionLoopSource(src);

  const std::vector<std::string> jit_args = {
      "run", "--backend", "jit", "--no-project", "--top", "Test", src.string()};
  const auto jit = RunChildProcess(lyra, jit_args, 120s);
  ASSERT_EQ(jit.termination, TerminationKind::kExitedNormally)
      << jit.stdout_text << jit.stderr_text;
  ASSERT_EQ(jit.exit_code, 0) << jit.stderr_text;

  const std::vector<std::string> cpp_args = {
      "run", "--no-project", "--top", "Test", src.string()};
  const auto cpp = RunChildProcess(lyra, cpp_args, 120s);
  ASSERT_EQ(cpp.termination, TerminationKind::kExitedNormally)
      << cpp.stdout_text << cpp.stderr_text;
  ASSERT_EQ(cpp.exit_code, 0) << cpp.stderr_text;

  EXPECT_EQ(jit.stdout_text, cpp.stdout_text);
  EXPECT_EQ(
      jit.stdout_text,
      "i=0 n=3 clk=1 ticks=1\n"
      "i=1 n=3 clk=0 ticks=2\n"
      "i=2 n=3 clk=1 ticks=3\n"
      "final ticks=3\n")
      << "stdout: " << jit.stdout_text;
}

// Cross-suspension values through nested control flow: nested loop counters, a
// local declared in the outer body and carried across the inner loop's
// suspensions, and an if/else that spans a suspension. Each is an activation
// value that must survive the `#1` at its own nesting depth. The two backends
// must agree, so the same nested body runs through both.
TEST(LyraRun, JitAndCppAgreeOnNestedSuspension) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();

  auto tmp_or = MakeTempCaseDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();
  const auto src = *tmp_or / "test.sv";
  WriteNestedSuspensionSource(src);

  const std::vector<std::string> jit_args = {
      "run", "--backend", "jit", "--no-project", "--top", "Test", src.string()};
  const auto jit = RunChildProcess(lyra, jit_args, 120s);
  ASSERT_EQ(jit.termination, TerminationKind::kExitedNormally)
      << jit.stdout_text << jit.stderr_text;
  ASSERT_EQ(jit.exit_code, 0) << jit.stderr_text;

  const std::vector<std::string> cpp_args = {
      "run", "--no-project", "--top", "Test", src.string()};
  const auto cpp = RunChildProcess(lyra, cpp_args, 120s);
  ASSERT_EQ(cpp.termination, TerminationKind::kExitedNormally)
      << cpp.stdout_text << cpp.stderr_text;
  ASSERT_EQ(cpp.exit_code, 0) << cpp.stderr_text;

  EXPECT_EQ(jit.stdout_text, cpp.stdout_text);
  EXPECT_EQ(
      jit.stdout_text,
      "i=0 inner_sum=12 total=12\n"
      "i=1 inner_sum=12 total=24\n"
      "final total=24\n")
      << "stdout: " << jit.stdout_text;
}

TEST(LyraRun, JitAndCppAgreeOnRealFamily) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();

  auto tmp_or = MakeTempCaseDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();
  const auto src = *tmp_or / "test.sv";
  WriteRealFamilySource(src);

  const std::vector<std::string> jit_args = {
      "run", "--backend", "jit", "--no-project", "--top", "Test", src.string()};
  const auto jit = RunChildProcess(lyra, jit_args, 120s);
  ASSERT_EQ(jit.termination, TerminationKind::kExitedNormally)
      << jit.stdout_text << jit.stderr_text;
  ASSERT_EQ(jit.exit_code, 0) << jit.stderr_text;

  const std::vector<std::string> cpp_args = {
      "run", "--no-project", "--top", "Test", src.string()};
  const auto cpp = RunChildProcess(lyra, cpp_args, 120s);
  ASSERT_EQ(cpp.termination, TerminationKind::kExitedNormally)
      << cpp.stdout_text << cpp.stderr_text;
  ASSERT_EQ(cpp.exit_code, 0) << cpp.stderr_text;

  EXPECT_EQ(jit.stdout_text, cpp.stdout_text);
  EXPECT_EQ(
      jit.stdout_text,
      "r=4.50 widened=2.50 from_int=3.00\n"
      "rounded=5 sum=7.50\n")
      << "stdout: " << jit.stdout_text;
}

TEST(LyraRun, JitAndCppAgreeOnChandle) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();

  auto tmp_or = MakeTempCaseDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();
  const auto src = *tmp_or / "test.sv";
  WriteChandleSource(src);

  const std::vector<std::string> jit_args = {
      "run", "--backend", "jit", "--no-project", "--top", "Test", src.string()};
  const auto jit = RunChildProcess(lyra, jit_args, 120s);
  ASSERT_EQ(jit.termination, TerminationKind::kExitedNormally)
      << jit.stdout_text << jit.stderr_text;
  ASSERT_EQ(jit.exit_code, 0) << jit.stderr_text;

  const std::vector<std::string> cpp_args = {
      "run", "--no-project", "--top", "Test", src.string()};
  const auto cpp = RunChildProcess(lyra, cpp_args, 120s);
  ASSERT_EQ(cpp.termination, TerminationKind::kExitedNormally)
      << cpp.stdout_text << cpp.stderr_text;
  ASSERT_EQ(cpp.exit_code, 0) << cpp.stderr_text;

  EXPECT_EQ(jit.stdout_text, cpp.stdout_text);
  EXPECT_EQ(
      jit.stdout_text,
      "h_null=1 not_h=1\n"
      "g_eq_h=1 g_ne_h=0\n"
      "g_ceq_h=1 g_cne_h=0\n")
      << "stdout: " << jit.stdout_text;
}

TEST(LyraRun, JitAndCppAgreeOnLogicalOperators) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();

  auto tmp_or = MakeTempCaseDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();
  const auto src = *tmp_or / "test.sv";
  WriteLogicalOperatorSource(src);

  const std::vector<std::string> jit_args = {
      "run", "--backend", "jit", "--no-project", "--top", "Test", src.string()};
  const auto jit = RunChildProcess(lyra, jit_args, 120s);
  ASSERT_EQ(jit.termination, TerminationKind::kExitedNormally)
      << jit.stdout_text << jit.stderr_text;
  ASSERT_EQ(jit.exit_code, 0) << jit.stderr_text;

  const std::vector<std::string> cpp_args = {
      "run", "--no-project", "--top", "Test", src.string()};
  const auto cpp = RunChildProcess(lyra, cpp_args, 120s);
  ASSERT_EQ(cpp.termination, TerminationKind::kExitedNormally)
      << cpp.stdout_text << cpp.stderr_text;
  ASSERT_EQ(cpp.exit_code, 0) << cpp.stderr_text;

  EXPECT_EQ(jit.stdout_text, cpp.stdout_text);
  EXPECT_EQ(
      jit.stdout_text,
      "and=0 or=1\n"
      "equiv=0 impl=0\n"
      "not_a=0 not_b=1\n"
      "str=1\n")
      << "stdout: " << jit.stdout_text;
}

TEST(LyraRun, JitAndCppAgreeOnStruct) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();

  auto tmp_or = MakeTempCaseDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();
  const auto src = *tmp_or / "test.sv";
  WriteStructSource(src);

  const std::vector<std::string> jit_args = {
      "run", "--backend", "jit", "--no-project", "--top", "Test", src.string()};
  const auto jit = RunChildProcess(lyra, jit_args, 120s);
  ASSERT_EQ(jit.termination, TerminationKind::kExitedNormally)
      << jit.stdout_text << jit.stderr_text;
  ASSERT_EQ(jit.exit_code, 0) << jit.stderr_text;

  const std::vector<std::string> cpp_args = {
      "run", "--no-project", "--top", "Test", src.string()};
  const auto cpp = RunChildProcess(lyra, cpp_args, 120s);
  ASSERT_EQ(cpp.termination, TerminationKind::kExitedNormally)
      << cpp.stdout_text << cpp.stderr_text;
  ASSERT_EQ(cpp.exit_code, 0) << cpp.stderr_text;

  EXPECT_EQ(jit.stdout_text, cpp.stdout_text);
  EXPECT_EQ(
      jit.stdout_text,
      "def a=0 b=0\n"
      "con a=3 b=7\n"
      "copy t.a=3 s.a=100\n"
      "eq=1 ne=0 ceq=1\n"
      "eq2=0\n"
      "nest a=1 b=20 c=9 s=bye\n"
      "xsusp a=42 b=43\n"
      "whole mirror=102\n"
      "partial mirror=702 sig.a=7\n")
      << "stdout: " << jit.stdout_text;
}

TEST(LyraRun, JitAndCppAgreeOnDynArray) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();

  auto tmp_or = MakeTempCaseDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();
  const auto src = *tmp_or / "test.sv";
  WriteDynArraySource(src);

  const std::vector<std::string> jit_args = {
      "run", "--backend", "jit", "--no-project", "--top", "Test", src.string()};
  const auto jit = RunChildProcess(lyra, jit_args, 120s);
  ASSERT_EQ(jit.termination, TerminationKind::kExitedNormally)
      << jit.stdout_text << jit.stderr_text;
  ASSERT_EQ(jit.exit_code, 0) << jit.stderr_text;

  const std::vector<std::string> cpp_args = {
      "run", "--no-project", "--top", "Test", src.string()};
  const auto cpp = RunChildProcess(lyra, cpp_args, 120s);
  ASSERT_EQ(cpp.termination, TerminationKind::kExitedNormally)
      << cpp.stdout_text << cpp.stderr_text;
  ASSERT_EQ(cpp.exit_code, 0) << cpp.stderr_text;

  EXPECT_EQ(jit.stdout_text, cpp.stdout_text);
  EXPECT_EQ(
      jit.stdout_text,
      "def size=0\n"
      "new size=3 a0=5 a2=7\n"
      "oob r=0 size=3\n"
      "alias b0=5 a0=100\n"
      "eq=1 ne=0 ceq=1\n"
      "eq2=0\n"
      "resize size=2 a0=5\n"
      "del a=0 b=2\n"
      "xsusp l0=42 l1=43\n"
      "whole mirror=102\n"
      "partial mirror=702 sig0=7\n")
      << "stdout: " << jit.stdout_text;
}

TEST(LyraCompile, ProducesPortableBuildableProject) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();

  auto tmp_or = MakeTempCaseDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();
  const auto src = *tmp_or / "test.sv";
  WriteTrivialSource(src);
  const auto out_dir = *tmp_or / "out";

  const std::vector<std::string> args = {
      "compile", "--no-project",   "--top",     "Test",
      "-o",      out_dir.string(), src.string()};
  const auto compile = RunChildProcess(lyra, args, 120s);
  ASSERT_EQ(compile.termination, TerminationKind::kExitedNormally)
      << compile.stdout_text << compile.stderr_text;
  ASSERT_EQ(compile.exit_code, 0) << compile.stderr_text;

  const auto program = out_dir / "program";
  ASSERT_TRUE(std::filesystem::exists(program)) << program.string();
  ASSERT_TRUE(std::filesystem::exists(out_dir / "build.sh"));

  // The directory must rebuild standalone, with no Lyra checkout: drop the
  // built program and rebuild via the shipped build.sh from within the dir.
  std::filesystem::remove(program);
  auto sh_or = lyra::support::FindOnPath("sh");
  ASSERT_TRUE(sh_or.has_value()) << sh_or.error();
  const std::vector<std::string> rebuild = {
      "-c", "cd '" + out_dir.string() + "' && sh build.sh"};
  const auto built = RunChildProcess(*sh_or, rebuild, 120s);
  ASSERT_EQ(built.termination, TerminationKind::kExitedNormally)
      << built.stdout_text << built.stderr_text;
  ASSERT_EQ(built.exit_code, 0) << built.stderr_text;
  ASSERT_TRUE(std::filesystem::exists(program)) << program.string();

  const auto run = RunChildProcess(program, {}, 30s);
  EXPECT_EQ(run.exit_code, 0) << run.stderr_text;
  EXPECT_NE(run.stdout_text.find("ran 42"), std::string::npos)
      << "stdout: " << run.stdout_text;
}

TEST(LyraEmit, ReEmitIntoSameDirectorySucceeds) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();

  auto tmp_or = MakeTempCaseDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();
  const auto src = *tmp_or / "test.sv";
  WriteTrivialSource(src);
  const auto out_dir = *tmp_or / "out";

  const std::vector<std::string> args = {
      "emit", "cpp", "--no-project",   "--top",
      "Test", "-o",  out_dir.string(), src.string()};
  // The bundled runtime is copied from a read-only source; emitting twice into
  // the same directory must still succeed (the copy is made writable).
  for (int i = 0; i < 2; ++i) {
    const auto emit = RunChildProcess(lyra, args, 60s);
    ASSERT_EQ(emit.termination, TerminationKind::kExitedNormally)
        << "iteration " << i << ": " << emit.stderr_text;
    ASSERT_EQ(emit.exit_code, 0)
        << "iteration " << i << ": " << emit.stderr_text;
  }
  EXPECT_TRUE(
      std::filesystem::exists(out_dir / "runtime/lib/libcpp_runtime.a"));
}

}  // namespace
