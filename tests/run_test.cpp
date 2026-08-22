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

// Some of these designs index past the end of a packed vector or a string on
// purpose: LRM 11.5.1 gives an out-of-range bit-select write no effect, and
// both backends have to agree on that. The front end promotes those selects to
// errors by default, which is a lint stance rather than a simulation one.
const std::vector<std::string> kAllowOutOfRangeSelects = {
    "-Wno-error=index-oob", "-Wno-error=range-oob"};

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

// Every descent step a write target can take, composed and nested: a product
// component reached through another component, a packed bit-select, a packed
// window (plain and compound), a string character, a union member, and an
// increment. One source so the paths are exercised together rather than each in
// isolation, which is where a shared write path would break first.
auto WriteInteriorWriteSource(const std::filesystem::path& path) -> void {
  std::ofstream out(path);
  out << "module Test;\n"
      << "  typedef struct {\n"
      << "    logic [15:0] w;\n"
      << "    string s;\n"
      << "  } Inner;\n"
      << "  typedef struct {\n"
      << "    Inner i;\n"
      << "    int n;\n"
      << "  } Outer;\n"
      << "  typedef union packed {\n"
      << "    logic [15:0] a;\n"
      << "    logic signed [15:0] b;\n"
      << "  } U;\n"
      << "  typedef struct { int a; int b; } Pair;\n"
      << "  typedef struct { int v[]; } Box;\n"
      << "  Outer o;\n"
      << "  U u;\n"
      << "  int idx;\n"
      << "  Pair pr[];\n"
      << "  Box bx;\n"
      << "  int aa[][];\n"
      << "  Pair psig[];\n"
      << "  int mirror = 0;\n"
      << "  always_comb mirror = psig[0].a * 100 + psig[0].b;\n"
      << "  function automatic logic [15:0] setbits(logic [15:0] x);\n"
      << "    logic [15:0] v;\n"
      << "    v = x;\n"
      << "    v[0] = 1'b1;\n"
      << "    v[7:4] = 4'hC;\n"
      << "    return v;\n"
      << "  endfunction\n"
      << "  initial begin\n"
      << "    idx = 1;\n"
      << "    o.n = 7;\n"
      << "    o.n++;\n"
      << "    $display(\"n=%0d\", o.n);\n"
      << "    o.i.w = 16'h0000;\n"
      << "    o.i.w[idx] = 1'b1;\n"
      << "    o.i.w[20] = 1'b1;\n"
      << "    o.i.w[7:4] = 4'hA;\n"
      << "    o.i.w[11:8] += 4'h3;\n"
      << "    o.i.w[12 +: 4] = 4'h5;\n"
      << "    o.i.w[3 -: 4] = 4'hF;\n"
      << "    $display(\"w=%h\", o.i.w);\n"
      << "    o.i.w = setbits(16'h0000);\n"
      << "    $display(\"fn=%h\", o.i.w);\n"
      << "    o.i.s = \"hello\";\n"
      << "    o.i.s[0] = \"H\";\n"
      << "    o.i.s[100] = \"X\";\n"
      << "    o.i.s[2] = 8'h00;\n"
      << "    $display(\"s=%s\", o.i.s);\n"
      << "    u.a = 16'hFFFF;\n"
      << "    u.b = 16'h0000;\n"
      << "    u.a[3:0] = 4'h5;\n"
      << "    $display(\"u=%h\", u.a);\n"
      << "    pr = new[2];\n"
      << "    pr[0].a = 5;\n"
      << "    pr[0].b = 6;\n"
      << "    pr[0].a += 10;\n"
      << "    $display(\"da a0=%0d b0=%0d\", pr[0].a, pr[0].b);\n"
      << "    bx.v = new[3];\n"
      << "    bx.v[0] = 10;\n"
      << "    bx.v[0] += 5;\n"
      << "    $display(\"sd v0=%0d\", bx.v[0]);\n"
      << "    aa = new[2];\n"
      << "    aa[0] = new[2];\n"
      << "    aa[0][1] = 42;\n"
      << "    aa[0][1] += 8;\n"
      << "    $display(\"aa 01=%0d\", aa[0][1]);\n"
      << "    psig = new[1];\n"
      << "    psig[0].a = 1;\n"
      << "    psig[0].b = 2;\n"
      << "    #1;\n"
      << "    $display(\"obs mirror=%0d\", mirror);\n"
      << "    psig[0].a = 7;\n"
      << "    #1;\n"
      << "    $display(\"obs2 mirror=%0d a=%0d\", mirror, psig[0].a);\n"
      << "  end\n"
      << "endmodule\n";
}

// A multi-dimensional packed value keeps its declared shape wherever it lives:
// a variable, a packed struct's member, a packed union's member. The shape is
// what decides how wide one element is, so an element read, an element write, a
// compound update, and a window inside an element all depend on it reaching the
// runtime. The execution backend carries a packed value behind an opaque
// handle, so a shape it fails to carry is only visible as a wrong element width
// here.
auto WritePackedShapeSource(const std::filesystem::path& path) -> void {
  std::ofstream out(path);
  out << "module Test;\n"
      << "  typedef struct packed {\n"
      << "    logic [1:0][7:0] f;\n"
      << "    logic [15:0] g;\n"
      << "  } S;\n"
      << "  typedef union packed {\n"
      << "    logic [15:0] w;\n"
      << "    logic [1:0][7:0] b;\n"
      << "  } U;\n"
      << "  logic [1:0][3:0] m;\n"
      << "  logic [3:0] e;\n"
      << "  S s;\n"
      << "  U u;\n"
      << "  logic [7:0] c;\n"
      << "  int idx;\n"
      << "  initial begin\n"
      << "    m = 8'h00;\n"
      << "    idx = 1;\n"
      << "    m[1] = 4'hA;\n"
      << "    m[0] = 4'h5;\n"
      << "    $display(\"w m=%h\", m);\n"
      << "    e = m[1];\n"
      << "    $display(\"r e=%h\", e);\n"
      << "    m[idx] += 4'h1;\n"
      << "    $display(\"c m=%h\", m);\n"
      << "    m[0][1] = 1'b1;\n"
      << "    $display(\"b m=%h\", m);\n"
      << "    s = 32'h0;\n"
      << "    s.f[0] = 8'hAB;\n"
      << "    s.f[1] = 8'hCD;\n"
      << "    $display(\"sf=%h g=%h\", s.f, s.g);\n"
      << "    c = s.f[1];\n"
      << "    $display(\"se=%h\", c);\n"
      << "    s.f[0][3:0] = 4'h5;\n"
      << "    $display(\"ss=%h\", s.f);\n"
      << "    u.w = 16'h0000;\n"
      << "    u.b[0] = 8'hAB;\n"
      << "    u.b[1] = 8'hCD;\n"
      << "    $display(\"uw=%h\", u.w);\n"
      << "    c = u.b[0];\n"
      << "    $display(\"ue=%h\", c);\n"
      << "    u.b[0][3:0] = 4'h5;\n"
      << "    $display(\"us=%h\", u.w);\n"
      << "  end\n"
      << "endmodule\n";
}

// An integral literal carries its whole value: every word of a width past one
// machine word, and the X / Z bits of a 4-state one (LRM 5.7.1). A literal that
// arrives short is not only printed wrong -- it decides comparisons, so a case
// equality against a mangled literal answers true for the wrong reason. The
// cases pair each literal with a case equality (LRM 11.4.5) for that reason:
// a positive one alone passes when both sides degrade the same way, so a
// negative one against the value the degradation would produce is what tells
// them apart.
auto WriteIntegralLiteralSource(const std::filesystem::path& path) -> void {
  std::ofstream out(path);
  out << "module Test;\n"
      << "  logic [15:0] xz;\n"
      << "  logic [95:0] wide;\n"
      << "  logic [63:0] edge64;\n"
      << "  logic [71:0] wxz;\n"
      << "  logic [7:0] narrow;\n"
      << "  initial begin\n"
      << "    xz = 16'hAxAz;\n"
      << "    $display(\"xz=%h\", xz);\n"
      << "    if (xz === 16'hAxAz) $display(\"xz ceq yes\");\n"
      << "    else $display(\"xz ceq no\");\n"
      << "    if (xz === 16'hAFA0) $display(\"xz bad yes\");\n"
      << "    else $display(\"xz bad no\");\n"
      << "    wide = 96'h1234_5678_9ABC_DEF0_1111_2222;\n"
      << "    $display(\"wide=%h\", wide);\n"
      << "    if (wide === 96'h1234_5678_9ABC_DEF0_1111_2222)\n"
      << "      $display(\"wide ceq yes\");\n"
      << "    else $display(\"wide ceq no\");\n"
      << "    edge64 = 64'hFEDC_BA98_7654_3210;\n"
      << "    $display(\"e64=%h\", edge64);\n"
      << "    wxz = 72'hAB_CDEF_0123_4567_89xz;\n"
      << "    $display(\"wxz=%h\", wxz);\n"
      << "    narrow = 8'd200;\n"
      << "    $display(\"narrow=%0d\", narrow);\n"
      << "  end\n"
      << "endmodule\n";
}

// A call hands its completion back as a product -- the result, then each value
// an `output` / `inout` formal carries back (LRM 13.5) -- and the caller writes
// each of those into the actual it came from. A callee that carries nothing
// back needs no such sequencing, so the two shapes must still agree: the cases
// pair a callee with all three component kinds against a void one, a
// nothing-back one, and a call nested inside a larger expression, where the
// sequencing has to happen without a statement of its own. `inout` is read
// twice across the two calls so a completion that silently drops a component
// shows up as a stale accumulator rather than only as a wrong return. A `ref`
// formal rides along because it is the opposite shape (LRM 13.5.2): it carries
// nothing back at all, writing the caller's storage as the body runs, so a
// realization that confused the two would answer differently here.
auto WriteSubroutineCallSource(const std::filesystem::path& path) -> void {
  std::ofstream out(path);
  out << "module Test;\n"
      << "  function automatic int split(input int a, output int lo,\n"
      << "                               inout int acc);\n"
      << "    lo = a % 256;\n"
      << "    acc = acc + a;\n"
      << "    return a / 256;\n"
      << "  endfunction\n"
      << "  function automatic void store(input int a, output int dst);\n"
      << "    dst = a + 1;\n"
      << "  endfunction\n"
      << "  function automatic int plain(input int a);\n"
      << "    return a * 3;\n"
      << "  endfunction\n"
      << "  function automatic void bump(ref int r, input int by);\n"
      << "    r = r + by;\n"
      << "  endfunction\n"
      << "  function automatic int aliased(input int seed);\n"
      << "    int tmp;\n"
      << "    tmp = seed;\n"
      << "    bump(tmp, 5);\n"
      << "    return tmp;\n"
      << "  endfunction\n"
      << "  int hi;\n"
      << "  int lo;\n"
      << "  int acc;\n"
      << "  int dst;\n"
      << "  initial begin\n"
      << "    acc = 100;\n"
      << "    hi = split(4660, lo, acc);\n"
      << "    $display(\"res hi=%0d lo=%0d acc=%0d\", hi, lo, acc);\n"
      << "    $display(\"nested=%0d\", split(513, lo, acc) + 1);\n"
      << "    $display(\"after lo=%0d acc=%0d\", lo, acc);\n"
      << "    store(41, dst);\n"
      << "    $display(\"void dst=%0d\", dst);\n"
      << "    $display(\"plain=%0d\", plain(14));\n"
      << "    $display(\"aliased=%0d\", aliased(7));\n"
      << "  end\n"
      << "endmodule\n";
}

TEST(LyraRun, JitAndCppAgreeOnInteriorWrite) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();

  auto tmp_or = MakeTempCaseDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();
  const auto src = *tmp_or / "test.sv";
  WriteInteriorWriteSource(src);

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
      "n=8\n"
      "w=53af\n"
      "fn=00c1\n"
      "s=Hello\n"
      "u=0005\n"
      "da a0=15 b0=6\n"
      "sd v0=15\n"
      "aa 01=50\n"
      "obs mirror=102\n"
      "obs2 mirror=702 a=7\n")
      << "stdout: " << jit.stdout_text;
}

TEST(LyraRun, JitAndCppAgreeOnPackedShape) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();

  auto tmp_or = MakeTempCaseDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();
  const auto src = *tmp_or / "test.sv";
  WritePackedShapeSource(src);

  std::vector<std::string> jit_args = {"run",          "--backend", "jit",
                                       "--no-project", "--top",     "Test"};
  jit_args.insert(
      jit_args.end(), kAllowOutOfRangeSelects.begin(),
      kAllowOutOfRangeSelects.end());
  jit_args.push_back(src.string());
  const auto jit = RunChildProcess(lyra, jit_args, 120s);
  ASSERT_EQ(jit.termination, TerminationKind::kExitedNormally)
      << jit.stdout_text << jit.stderr_text;
  ASSERT_EQ(jit.exit_code, 0) << jit.stderr_text;

  std::vector<std::string> cpp_args = {"run", "--no-project", "--top", "Test"};
  cpp_args.insert(
      cpp_args.end(), kAllowOutOfRangeSelects.begin(),
      kAllowOutOfRangeSelects.end());
  cpp_args.push_back(src.string());
  const auto cpp = RunChildProcess(lyra, cpp_args, 120s);
  ASSERT_EQ(cpp.termination, TerminationKind::kExitedNormally)
      << cpp.stdout_text << cpp.stderr_text;
  ASSERT_EQ(cpp.exit_code, 0) << cpp.stderr_text;

  EXPECT_EQ(jit.stdout_text, cpp.stdout_text);
  EXPECT_EQ(
      jit.stdout_text,
      "w m=a5\n"
      "r e=a\n"
      "c m=b5\n"
      "b m=b7\n"
      "sf=cdab g=0000\n"
      "se=cd\n"
      "ss=cda5\n"
      "uw=cdab\n"
      "ue=ab\n"
      "us=cda5\n")
      << "stdout: " << jit.stdout_text;
}

TEST(LyraRun, JitAndCppAgreeOnIntegralLiteral) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();

  auto tmp_or = MakeTempCaseDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();
  const auto src = *tmp_or / "test.sv";
  WriteIntegralLiteralSource(src);

  std::vector<std::string> jit_args = {"run",          "--backend", "jit",
                                       "--no-project", "--top",     "Test"};
  jit_args.insert(
      jit_args.end(), kAllowOutOfRangeSelects.begin(),
      kAllowOutOfRangeSelects.end());
  jit_args.push_back(src.string());
  const auto jit = RunChildProcess(lyra, jit_args, 120s);
  ASSERT_EQ(jit.termination, TerminationKind::kExitedNormally)
      << jit.stdout_text << jit.stderr_text;
  ASSERT_EQ(jit.exit_code, 0) << jit.stderr_text;

  std::vector<std::string> cpp_args = {"run", "--no-project", "--top", "Test"};
  cpp_args.insert(
      cpp_args.end(), kAllowOutOfRangeSelects.begin(),
      kAllowOutOfRangeSelects.end());
  cpp_args.push_back(src.string());
  const auto cpp = RunChildProcess(lyra, cpp_args, 120s);
  ASSERT_EQ(cpp.termination, TerminationKind::kExitedNormally)
      << cpp.stdout_text << cpp.stderr_text;
  ASSERT_EQ(cpp.exit_code, 0) << cpp.stderr_text;

  EXPECT_EQ(jit.stdout_text, cpp.stdout_text);
  EXPECT_EQ(
      jit.stdout_text,
      "xz=axaz\n"
      "xz ceq yes\n"
      "xz bad no\n"
      "wide=123456789abcdef011112222\n"
      "wide ceq yes\n"
      "e64=fedcba9876543210\n"
      "wxz=abcdef0123456789xz\n"
      "narrow=200\n")
      << "stdout: " << jit.stdout_text;
}

TEST(LyraRun, JitAndCppAgreeOnSubroutineCall) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();

  auto tmp_or = MakeTempCaseDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();
  const auto src = *tmp_or / "test.sv";
  WriteSubroutineCallSource(src);

  std::vector<std::string> jit_args = {"run",          "--backend", "jit",
                                       "--no-project", "--top",     "Test"};
  jit_args.insert(
      jit_args.end(), kAllowOutOfRangeSelects.begin(),
      kAllowOutOfRangeSelects.end());
  jit_args.push_back(src.string());
  const auto jit = RunChildProcess(lyra, jit_args, 120s);
  ASSERT_EQ(jit.termination, TerminationKind::kExitedNormally)
      << jit.stdout_text << jit.stderr_text;
  ASSERT_EQ(jit.exit_code, 0) << jit.stderr_text;

  std::vector<std::string> cpp_args = {"run", "--no-project", "--top", "Test"};
  cpp_args.insert(
      cpp_args.end(), kAllowOutOfRangeSelects.begin(),
      kAllowOutOfRangeSelects.end());
  cpp_args.push_back(src.string());
  const auto cpp = RunChildProcess(lyra, cpp_args, 120s);
  ASSERT_EQ(cpp.termination, TerminationKind::kExitedNormally)
      << cpp.stdout_text << cpp.stderr_text;
  ASSERT_EQ(cpp.exit_code, 0) << cpp.stderr_text;

  EXPECT_EQ(jit.stdout_text, cpp.stdout_text);
  EXPECT_EQ(
      jit.stdout_text,
      "res hi=18 lo=52 acc=4760\n"
      "nested=3\n"
      "after lo=1 acc=5273\n"
      "void dst=42\n"
      "plain=42\n"
      "aliased=12\n")
      << "stdout: " << jit.stdout_text;
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

  std::vector<std::string> jit_args = {"run",          "--backend", "jit",
                                       "--no-project", "--top",     "Test"};
  jit_args.insert(
      jit_args.end(), kAllowOutOfRangeSelects.begin(),
      kAllowOutOfRangeSelects.end());
  jit_args.push_back(src.string());
  const auto jit = RunChildProcess(lyra, jit_args, 120s);
  ASSERT_EQ(jit.termination, TerminationKind::kExitedNormally)
      << jit.stdout_text << jit.stderr_text;
  ASSERT_EQ(jit.exit_code, 0) << jit.stderr_text;

  std::vector<std::string> cpp_args = {"run", "--no-project", "--top", "Test"};
  cpp_args.insert(
      cpp_args.end(), kAllowOutOfRangeSelects.begin(),
      kAllowOutOfRangeSelects.end());
  cpp_args.push_back(src.string());
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

  std::vector<std::string> jit_args = {"run",          "--backend", "jit",
                                       "--no-project", "--top",     "Test"};
  jit_args.insert(
      jit_args.end(), kAllowOutOfRangeSelects.begin(),
      kAllowOutOfRangeSelects.end());
  jit_args.push_back(src.string());
  const auto jit = RunChildProcess(lyra, jit_args, 120s);
  ASSERT_EQ(jit.termination, TerminationKind::kExitedNormally)
      << jit.stdout_text << jit.stderr_text;
  ASSERT_EQ(jit.exit_code, 0) << jit.stderr_text;

  std::vector<std::string> cpp_args = {"run", "--no-project", "--top", "Test"};
  cpp_args.insert(
      cpp_args.end(), kAllowOutOfRangeSelects.begin(),
      kAllowOutOfRangeSelects.end());
  cpp_args.push_back(src.string());
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

  std::vector<std::string> jit_args = {"run",          "--backend", "jit",
                                       "--no-project", "--top",     "Test"};
  jit_args.insert(
      jit_args.end(), kAllowOutOfRangeSelects.begin(),
      kAllowOutOfRangeSelects.end());
  jit_args.push_back(src.string());
  const auto jit = RunChildProcess(lyra, jit_args, 120s);
  ASSERT_EQ(jit.termination, TerminationKind::kExitedNormally)
      << jit.stdout_text << jit.stderr_text;
  ASSERT_EQ(jit.exit_code, 0) << jit.stderr_text;

  std::vector<std::string> cpp_args = {"run", "--no-project", "--top", "Test"};
  cpp_args.insert(
      cpp_args.end(), kAllowOutOfRangeSelects.begin(),
      kAllowOutOfRangeSelects.end());
  cpp_args.push_back(src.string());
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

  std::vector<std::string> jit_args = {"run",          "--backend", "jit",
                                       "--no-project", "--top",     "Test"};
  jit_args.insert(
      jit_args.end(), kAllowOutOfRangeSelects.begin(),
      kAllowOutOfRangeSelects.end());
  jit_args.push_back(src.string());
  const auto jit = RunChildProcess(lyra, jit_args, 120s);
  ASSERT_EQ(jit.termination, TerminationKind::kExitedNormally)
      << jit.stdout_text << jit.stderr_text;
  ASSERT_EQ(jit.exit_code, 0) << jit.stderr_text;

  std::vector<std::string> cpp_args = {"run", "--no-project", "--top", "Test"};
  cpp_args.insert(
      cpp_args.end(), kAllowOutOfRangeSelects.begin(),
      kAllowOutOfRangeSelects.end());
  cpp_args.push_back(src.string());
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

  std::vector<std::string> jit_args = {"run",          "--backend", "jit",
                                       "--no-project", "--top",     "Test"};
  jit_args.insert(
      jit_args.end(), kAllowOutOfRangeSelects.begin(),
      kAllowOutOfRangeSelects.end());
  jit_args.push_back(src.string());
  const auto jit = RunChildProcess(lyra, jit_args, 120s);
  ASSERT_EQ(jit.termination, TerminationKind::kExitedNormally)
      << jit.stdout_text << jit.stderr_text;
  ASSERT_EQ(jit.exit_code, 0) << jit.stderr_text;

  std::vector<std::string> cpp_args = {"run", "--no-project", "--top", "Test"};
  cpp_args.insert(
      cpp_args.end(), kAllowOutOfRangeSelects.begin(),
      kAllowOutOfRangeSelects.end());
  cpp_args.push_back(src.string());
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

  std::vector<std::string> jit_args = {"run",          "--backend", "jit",
                                       "--no-project", "--top",     "Test"};
  jit_args.insert(
      jit_args.end(), kAllowOutOfRangeSelects.begin(),
      kAllowOutOfRangeSelects.end());
  jit_args.push_back(src.string());
  const auto jit = RunChildProcess(lyra, jit_args, 120s);
  ASSERT_EQ(jit.termination, TerminationKind::kExitedNormally)
      << jit.stdout_text << jit.stderr_text;
  ASSERT_EQ(jit.exit_code, 0) << jit.stderr_text;

  std::vector<std::string> cpp_args = {"run", "--no-project", "--top", "Test"};
  cpp_args.insert(
      cpp_args.end(), kAllowOutOfRangeSelects.begin(),
      kAllowOutOfRangeSelects.end());
  cpp_args.push_back(src.string());
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

  std::vector<std::string> jit_args = {"run",          "--backend", "jit",
                                       "--no-project", "--top",     "Test"};
  jit_args.insert(
      jit_args.end(), kAllowOutOfRangeSelects.begin(),
      kAllowOutOfRangeSelects.end());
  jit_args.push_back(src.string());
  const auto jit = RunChildProcess(lyra, jit_args, 120s);
  ASSERT_EQ(jit.termination, TerminationKind::kExitedNormally)
      << jit.stdout_text << jit.stderr_text;
  ASSERT_EQ(jit.exit_code, 0) << jit.stderr_text;

  std::vector<std::string> cpp_args = {"run", "--no-project", "--top", "Test"};
  cpp_args.insert(
      cpp_args.end(), kAllowOutOfRangeSelects.begin(),
      kAllowOutOfRangeSelects.end());
  cpp_args.push_back(src.string());
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

  std::vector<std::string> jit_args = {"run",          "--backend", "jit",
                                       "--no-project", "--top",     "Test"};
  jit_args.insert(
      jit_args.end(), kAllowOutOfRangeSelects.begin(),
      kAllowOutOfRangeSelects.end());
  jit_args.push_back(src.string());
  const auto jit = RunChildProcess(lyra, jit_args, 120s);
  ASSERT_EQ(jit.termination, TerminationKind::kExitedNormally)
      << jit.stdout_text << jit.stderr_text;
  ASSERT_EQ(jit.exit_code, 0) << jit.stderr_text;

  std::vector<std::string> cpp_args = {"run", "--no-project", "--top", "Test"};
  cpp_args.insert(
      cpp_args.end(), kAllowOutOfRangeSelects.begin(),
      kAllowOutOfRangeSelects.end());
  cpp_args.push_back(src.string());
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

  std::vector<std::string> jit_args = {"run",          "--backend", "jit",
                                       "--no-project", "--top",     "Test"};
  jit_args.insert(
      jit_args.end(), kAllowOutOfRangeSelects.begin(),
      kAllowOutOfRangeSelects.end());
  jit_args.push_back(src.string());
  const auto jit = RunChildProcess(lyra, jit_args, 120s);
  ASSERT_EQ(jit.termination, TerminationKind::kExitedNormally)
      << jit.stdout_text << jit.stderr_text;
  ASSERT_EQ(jit.exit_code, 0) << jit.stderr_text;

  std::vector<std::string> cpp_args = {"run", "--no-project", "--top", "Test"};
  cpp_args.insert(
      cpp_args.end(), kAllowOutOfRangeSelects.begin(),
      kAllowOutOfRangeSelects.end());
  cpp_args.push_back(src.string());
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

  std::vector<std::string> jit_args = {"run",          "--backend", "jit",
                                       "--no-project", "--top",     "Test"};
  jit_args.insert(
      jit_args.end(), kAllowOutOfRangeSelects.begin(),
      kAllowOutOfRangeSelects.end());
  jit_args.push_back(src.string());
  const auto jit = RunChildProcess(lyra, jit_args, 120s);
  ASSERT_EQ(jit.termination, TerminationKind::kExitedNormally)
      << jit.stdout_text << jit.stderr_text;
  ASSERT_EQ(jit.exit_code, 0) << jit.stderr_text;

  std::vector<std::string> cpp_args = {"run", "--no-project", "--top", "Test"};
  cpp_args.insert(
      cpp_args.end(), kAllowOutOfRangeSelects.begin(),
      kAllowOutOfRangeSelects.end());
  cpp_args.push_back(src.string());
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

// A design that crosses the DPI-C boundary in both directions (LRM 35): the
// module imports a C function, which calls back the package function the
// package exports.
auto WriteDpiSource(const std::filesystem::path& path) -> void {
  std::ofstream out(path);
  out << "package pkg;\n"
      << "  export \"DPI-C\" function triple;\n"
      << "  function automatic int triple(int x);\n"
      << "    return x * 3;\n"
      << "  endfunction\n"
      << "endpackage\n"
      << "module Test;\n"
      << "  import \"DPI-C\" context function int call_pkg(input int x);\n"
      << "  initial $display(\"dpi %0d\", call_pkg(7));\n"
      << "endmodule\n";
}

// The foreign half, stating no prototype of its own: the generated ABI header
// carries both the import it defines and the export it calls.
auto WriteDpiForeignSource(const std::filesystem::path& path) -> void {
  std::ofstream out(path);
  out << "#include \"dpi.h\"\n"
      << "\n"
      << "int call_pkg(int x) {\n"
      << "  return triple(x);\n"
      << "}\n";
}

TEST(LyraEmit, PortableProjectBuildsItsDpiSources) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();

  auto tmp_or = MakeTempCaseDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();
  const auto src = *tmp_or / "test.sv";
  WriteDpiSource(src);
  const auto foreign = *tmp_or / "foreign.c";
  WriteDpiForeignSource(foreign);
  const auto out_dir = *tmp_or / "out";

  const std::vector<std::string> args = {
      "emit",           "cpp",       "--no-project",   "--top",
      "Test",           "-o",        out_dir.string(), "--dpi-link",
      foreign.string(), src.string()};
  const auto emit = RunChildProcess(lyra, args, 60s);
  ASSERT_EQ(emit.termination, TerminationKind::kExitedNormally)
      << emit.stdout_text << emit.stderr_text;
  ASSERT_EQ(emit.exit_code, 0) << emit.stderr_text;

  // The emitted directory carries the whole foreign boundary: the generated
  // prototypes, the standard header they are spelled in, and a copy of the
  // user's source, so it builds where the originals are not reachable.
  EXPECT_TRUE(std::filesystem::exists(out_dir / "dpi.h"));
  EXPECT_TRUE(std::filesystem::exists(out_dir / "svdpi.h"));
  ASSERT_TRUE(std::filesystem::exists(out_dir / "dpi/foreign.c"));
  std::filesystem::remove(foreign);

  auto sh_or = lyra::support::FindOnPath("sh");
  ASSERT_TRUE(sh_or.has_value()) << sh_or.error();
  const std::vector<std::string> build = {
      "-c", "cd '" + out_dir.string() + "' && sh build.sh"};
  const auto built = RunChildProcess(*sh_or, build, 120s);
  ASSERT_EQ(built.termination, TerminationKind::kExitedNormally)
      << built.stdout_text << built.stderr_text;
  ASSERT_EQ(built.exit_code, 0) << built.stderr_text;

  const auto run = RunChildProcess(out_dir / "program", {}, 30s);
  EXPECT_EQ(run.exit_code, 0) << run.stderr_text;
  EXPECT_NE(run.stdout_text.find("dpi 21"), std::string::npos)
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
