// A program keeps a value larger than the stack it was started with off that
// stack, on either backend. Where a value lies is decided by the compiler, but
// how much of the stack a body then takes is also its host compiler's: clang
// at its default level reserves stack in a coroutine for every value its frame
// holds, so the C++ backend's program is built with clang by name and the case
// runs where clang is.

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

// A design whose every value is twice the stack it is run on: one of each kind
// a frame would otherwise keep or an operation make whole -- a sum, a shift, a
// concatenation, a part, an answer and a member of one, a sort, a stream and
// its inverse, a structure's copy, comparison and stream, a union's copy and
// comparison, a net of each kind resolving two drivers, a write into a
// member of an element, a merge under an unknown condition, a task's variable
// and what it completes with, a variable kept across a wait and captured by
// what it forks, a value named on the command line, a memory loaded from a
// file as text and as bytes, a change waited on, a value forced and released,
// and its sampled value -- each checked against what it must hold. A list of
// arguments longer than a frame keeps is checked beside them.
constexpr int kManyArguments = 200;

constexpr std::string_view kValuesPastTheStack = R"(
module Test;
  typedef logic [4194303:0] wide_t;
  typedef int arr_t [262144];
  typedef struct { int head; arr_t body; } rec_t;

  function automatic wide_t twice(wide_t w);
    return w + w;
  endfunction

  function automatic rec_t headed(int h);
    rec_t q;
    q.head = h;
    return q;
  endfunction

  task automatic later_ones(output wide_t o);
    #1;
    o = '1;
  endtask

  task automatic kept_across(output bit kept);
    wide_t v;
    v = '1;
    #1;
    fork
      begin
        #1;
        kept = v === '1;
      end
    join
  endtask

  typedef logic [7:0] mem_t [524288];
  typedef union { wide_t w; mem_t x; } u_t;

  wide_t a, b, nb;
  mem_t ma;
  u_t u, v;
  wire wide_t wn;
  wire mem_t mn;
  wire u_t un;
  assign wn = nb;
  assign wn = nb;
  assign mn = ma;
  assign mn = ma;
  assign un = u;
  assign un = u;
  logic [8388607:0] cat;
  logic [8388639:0] rbits;
  arr_t x, y;
  rec_t r, s;
  rec_t rs [2];
  wide_t m [2];
  logic c;
  int fd;
  int changes;
  bit kept;
  string text;

  always @(a) changes++;

  initial begin
    nb = 5;
    ma[3] = 8'h7;
    u.w = 5;
    #1;
    if (wn !== 5) $fatal(1, "the wide net");
    if (mn[3] !== 8'h7) $fatal(1, "the array net");
    if (un.w !== 5) $fatal(1, "the union net");
    v = u;
    if (v != u || v.w !== 5) $fatal(1, "the union's copy and comparison");
  end

  initial begin
    a = '1;
    b = 1;
    a = a + b;
    if (a !== '0) $fatal(1, "the sum wrapped to %h", a[7:0]);
    b = twice(b << 100);
    if (b[101] !== 1'b1 || b[100] !== 1'b0) $fatal(1, "the doubled shift");
    cat = {b, a};
    if (cat[4194304 + 101] !== 1'b1) $fatal(1, "the concatenation");
    a = cat[8388607:4194304];
    if (a !== b) $fatal(1, "the part");

    foreach (x[i]) x[i] = 262143 - i;
    x.sort();
    if (x[0] != 0 || x[262143] != 262143) $fatal(1, "the sort");
    x.reverse();
    if (x[0] != 262143) $fatal(1, "the reverse");
    cat = {>>{x}};
    y = {>>{cat}};
    if (y != x) $fatal(1, "the stream");

    r.head = 7;
    r.body = x;
    s = r;
    if (s != r) $fatal(1, "the structure's copy");
    rbits = {>>{r}};
    s.head = 0;
    s = {>>{rbits}};
    if (s != r) $fatal(1, "the structure's stream");
    s = headed(5);
    if (s.head != 5 || headed(9).head != 9) $fatal(1, "the answered structure");
    rs[1].head = 3;
    if (rs[1].head != 3 || rs[0].head != 0) $fatal(1, "the element's member");

    c = 1'bx;
    y = c ? x : x;
    if (y != x) $fatal(1, "the merge");

    later_ones(b);
    if (b !== '1) $fatal(1, "the task's output");
    kept_across(kept);
    if (!kept) $fatal(1, "the variable kept across a wait and captured");
    text = $sformatf("MANY_FORMATS", MANY_ARGUMENTS);
    if (text.len() != MANY_LENGTH) $fatal(1, "the list of many arguments");
    if ($value$plusargs("wide=%h", a)) $fatal(1, "a value named nowhere");

    $readmemh("MEMORY", m);
    if (m[0][3:0] !== 4'ha || m[1][7:4] !== 4'h5) $fatal(1, "the memory");
    fd = $fopen("MEMORY", "rb");
    if ($fread(m, fd) == 0) $fatal(1, "the memory read as bytes");
    $fclose(fd);

    changes = 0;
    force a = '0;
    #1;
    if (a !== '0 || changes == 0) $fatal(1, "the forced value");
    release a;
    a = 1;
    if (a !== 1) $fatal(1, "the released value");
    #1;
    if ($sampled(a) !== 1) $fatal(1, "the sampled value");
  end
endmodule
)";

TEST(ValuesOffTheStack, ValuesLargerThanTheStackAreKeptOffIt) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();
  auto tmp_or = MakeScratchDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();
  const auto memory = *tmp_or / "memory.hex";
  std::ofstream(memory) << "a\n5a\n";
  // A list of arguments longer than a frame keeps, each its own number.
  std::string formats;
  std::string arguments;
  std::string printed;
  for (int i = 0; i < kManyArguments; ++i) {
    formats += "%0d";
    arguments += std::format("{}{}", i == 0 ? "" : ", ", i);
    printed += std::to_string(i);
  }
  std::string design(kValuesPastTheStack);
  const auto replace = [&](std::string_view name, const std::string& with) {
    for (std::size_t at = design.find(name); at != std::string::npos;
         at = design.find(name, at + with.size())) {
      design.replace(at, name.size(), with);
    }
  };
  replace("MEMORY", memory.string());
  replace("MANY_FORMATS", formats);
  replace("MANY_ARGUMENTS", arguments);
  replace("MANY_LENGTH", std::to_string(printed.size()));
  const auto src = *tmp_or / "test.sv";
  std::ofstream(src) << design;
  auto sh_or = lyra::driver::FindOnPath("sh");
  ASSERT_TRUE(sh_or.has_value());

  for (const std::string backend : {"llvm", "cpp"}) {
    const auto program = *tmp_or / ("program_" + backend);
    const std::vector<std::string> build = {
        "build", "--backend",      backend,
        "--cxx", "clang++",        "--top",
        "Test",  "--cache-dir",    (*tmp_or / "cache").string(),
        "-o",    program.string(), src.string()};
    const auto built = RunChildProcess(lyra, build, 600s);
    ASSERT_EQ(built.termination, TerminationKind::kExitedNormally)
        << backend << ": " << built.stderr_text;
    ASSERT_EQ(built.exit_code, 0) << backend << ": " << built.stderr_text;

    const std::vector<std::string> run = {
        "-c", std::format("ulimit -s 512 && exec '{}'", program.string())};
    const auto ran = RunChildProcess(*sh_or, run, 300s);
    EXPECT_EQ(ran.termination, TerminationKind::kExitedNormally)
        << backend << ": " << ran.stderr_text;
    EXPECT_EQ(ran.exit_code, 0)
        << backend << ": " << ran.stdout_text << ran.stderr_text;
  }
}

}  // namespace
