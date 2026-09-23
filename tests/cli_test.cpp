// What the command line itself decides: what a design declares about itself,
// what an invocation adds to or replaces in that declaration, which top a
// design element may be, what a unit publishes to whoever reads it, and how a
// design that fails at run time is reported.
//
// None of it is a statement about a backend, so a case that has to run a design
// asks for the one that spawns no host compiler, and the file costs seconds and
// gates. What an emitted C++ project is worth once built is a different
// question with a different price, and lives in `emitted_project_test.cpp`.

#include <algorithm>
#include <array>
#include <expected>
#include <filesystem>
#include <format>
#include <fstream>
#include <gtest/gtest.h>
#include <iterator>
#include <regex>
#include <set>
#include <string>
#include <string_view>
#include <vector>

#include "lyra/support/subprocess.hpp"
#include "tests/framework/cli_fixture.hpp"
#include "tests/framework/process.hpp"

namespace {

using lyra::test::MakeScratchDir;
using lyra::test::ResolveLyra;
using lyra::test::RunChildProcess;
using lyra::test::TerminationKind;
using lyra::test::WriteTrivialSource;
using namespace std::chrono_literals;

TEST(LyraEmit, ReEmitIntoSameDirectorySucceeds) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();

  auto tmp_or = MakeScratchDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();
  const auto src = *tmp_or / "test.sv";
  WriteTrivialSource(src);
  const auto out_dir = *tmp_or / "out";

  const std::vector<std::string> args = {
      "emit", "cpp", "--top", "Test", "-o", out_dir.string(), src.string()};
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

// A parent and the child it instantiates. `kept` is a declaration the child
// never publishes, and `ports_reversed` swaps the order it publishes its two
// ports in -- the two axes a referrer's dependency on the child is judged on.
auto WriteParentAndChild(
    const std::filesystem::path& path, bool kept, bool ports_reversed) -> void {
  std::ofstream out(path);
  out << "module Leaf (\n";
  out
      << (ports_reversed ? "    output logic [31:0] y,\n"
                           "    input  logic [31:0] a\n"
                         : "    input  logic [31:0] a,\n"
                           "    output logic [31:0] y\n");
  out << ");\n";
  if (kept) {
    out << "  logic [6:0] kept;\n";
  }
  out << "  always_comb begin\n";
  if (kept) {
    out << "    kept = 7'd3;\n";
  }
  out << "    y = a + 32'd1;\n"
      << "  end\n"
      << "endmodule\n"
      << "module Test;\n"
      << "  logic [31:0] src;\n"
      << "  logic [31:0] dst;\n"
      << "  Leaf u (.a(src), .y(dst));\n"
      << "  initial begin\n"
      << "    src = 32'd41;\n"
      << "    #1;\n"
      << "    $display(\"y=%0d\", dst);\n"
      << "  end\n"
      << "endmodule\n";
}

auto ReadWholeFile(const std::filesystem::path& path) -> std::string {
  std::ifstream in(path, std::ios::binary);
  return std::string{
      std::istreambuf_iterator<char>(in), std::istreambuf_iterator<char>()};
}

// Everything a referrer of `unit` compiles against, read out of `dir` as one
// text. A unit's declarations are spread over several headers and which of them
// a piece of text sits in is the backend's own business, so the claim below is
// made against the whole of what a referrer reads.
auto ReadDeclarationsOf(const std::filesystem::path& dir, std::string_view unit)
    -> std::string {
  std::vector<std::filesystem::path> files;
  for (const auto& entry : std::filesystem::directory_iterator(dir)) {
    const std::string name = entry.path().filename().string();
    if (entry.path().extension() == ".hpp" &&
        (name == std::format("{}.hpp", unit) ||
         name.starts_with(std::format("{}.", unit)))) {
      files.push_back(entry.path());
    }
  }
  std::ranges::sort(files);
  std::string out;
  for (const std::filesystem::path& file : files) {
    out += ReadWholeFile(file);
  }
  return out;
}

// Emits one variant of that design into its own directory and answers with
// everything the child promised.
auto EmitChildSignature(
    const std::filesystem::path& lyra, const std::filesystem::path& root,
    std::string_view variant, bool kept, bool ports_reversed) -> std::string {
  const auto dir = root / variant;
  std::filesystem::create_directories(dir);
  const auto src = dir / "test.sv";
  WriteParentAndChild(src, kept, ports_reversed);
  const auto out_dir = dir / "out";
  const std::vector<std::string> args = {
      "emit", "cpp", "--top", "Test", "-o", out_dir.string(), src.string()};
  const auto emit = RunChildProcess(lyra, args, 60s);
  EXPECT_EQ(emit.termination, TerminationKind::kExitedNormally)
      << variant << ": " << emit.stderr_text;
  EXPECT_EQ(emit.exit_code, 0) << variant << ": " << emit.stderr_text;
  return ReadDeclarationsOf(out_dir, "Leaf");
}

// What a unit's referrers compile against is what that unit promised. A
// declaration the unit kept to itself must move none of it, and a change to
// what it published must.
TEST(LyraEmit, TheSignatureCarriesWhatTheUnitPromisedAndNothingElse) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();

  auto tmp_or = MakeScratchDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();

  const std::string promised =
      EmitChildSignature(lyra, *tmp_or, "promised", false, false);
  ASSERT_FALSE(promised.empty());

  EXPECT_EQ(promised, EmitChildSignature(lyra, *tmp_or, "kept", true, false));
  EXPECT_NE(
      promised, EmitChildSignature(lyra, *tmp_or, "reordered", false, true));
}

// One package holding two classes with nothing to do with each other, and two
// modules using one apiece. A flag gives one class a property nobody reads,
// which is a change to what that class declares rather than to a body of it.
auto WriteTwoClassPackage(
    const std::filesystem::path& path, bool counter_grows, bool logger_grows)
    -> void {
  std::ofstream out(path);
  out << "package pkg;\n"
      << "  class Counter;\n"
      << "    int n = 1;\n"
      << (counter_grows ? "    int spare;\n" : "") << "  endclass\n"
      << "  class Logger;\n"
      << "    int m = 2;\n"
      << (logger_grows ? "    int spare;\n" : "") << "  endclass\n"
      << "endpackage\n"
      << "module UsesCounter;\n"
      << "  int seen;\n"
      << "  initial begin\n"
      << "    pkg::Counter c;\n"
      << "    c = new();\n"
      << "    seen = c.n;\n"
      << "  end\n"
      << "endmodule\n"
      << "module UsesLogger;\n"
      << "  int seen;\n"
      << "  initial begin\n"
      << "    pkg::Logger l;\n"
      << "    l = new();\n"
      << "    seen = l.m;\n"
      << "  end\n"
      << "endmodule\n"
      << "module Test;\n"
      << "  UsesCounter a ();\n"
      << "  UsesLogger b ();\n"
      << "endmodule\n";
}

// Every line of text one translation unit is handed: its own, and that of every
// file it reaches through an include of the project's own. A referrer's
// dependency is this and not the set of units it named, so this is what a claim
// about what it compiles against has to be made against.
auto ReadCompileInput(
    const std::filesystem::path& dir, const std::filesystem::path& entry,
    std::set<std::filesystem::path>& seen) -> std::string {
  if (!seen.insert(entry).second) {
    return {};
  }
  const std::string own = ReadWholeFile(dir / entry);
  std::string out = own;
  const std::regex include{"#include \"([^\"/]+)\""};
  for (auto it = std::sregex_iterator(own.begin(), own.end(), include);
       it != std::sregex_iterator(); ++it) {
    const std::filesystem::path named = (*it)[1].str();
    if (std::filesystem::exists(dir / named)) {
      out += ReadCompileInput(dir, named, seen);
    }
  }
  return out;
}

// Emits one variant of that design into its own directory and answers with
// everything `UsesCounter`'s translation unit is handed.
auto EmitAndReadCounterInput(
    const std::filesystem::path& lyra, const std::filesystem::path& root,
    std::string_view variant, bool counter_grows, bool logger_grows)
    -> std::string {
  const auto dir = root / variant;
  std::filesystem::create_directories(dir);
  const auto src = dir / "test.sv";
  WriteTwoClassPackage(src, counter_grows, logger_grows);
  const auto out_dir = dir / "out";
  const std::vector<std::string> args = {
      "emit", "cpp", "--top", "Test", "-o", out_dir.string(), src.string()};
  const auto emit = RunChildProcess(lyra, args, 60s);
  EXPECT_EQ(emit.exit_code, 0) << variant << ": " << emit.stderr_text;
  std::set<std::filesystem::path> seen;
  return ReadCompileInput(out_dir, "UsesCounter.cpp", seen);
}

// What a referrer depends on is the part of a signature it read, never the unit
// holding it. One package holds two unrelated classes and two modules use one
// apiece, so editing the class this module never named must move nothing it
// compiles -- while editing the one it did must.
TEST(LyraEmit, AReferrerCompilesAgainstThePartItRead) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();

  auto tmp_or = MakeScratchDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();

  const std::string base =
      EmitAndReadCounterInput(lyra, *tmp_or, "base", false, false);
  ASSERT_FALSE(base.empty());

  EXPECT_EQ(
      base, EmitAndReadCounterInput(lyra, *tmp_or, "other_class", false, true));
  EXPECT_NE(
      base, EmitAndReadCounterInput(lyra, *tmp_or, "own_class", true, false));
}

// A design spread over a directory, declared by a `lyra.toml` beside it. Every
// path in the declaration is relative, which is what makes the file's own
// directory -- rather than whatever directory a command was typed in -- the
// thing these tests are about.
auto WriteDeclaredDesign(const std::filesystem::path& root) -> void {
  std::filesystem::create_directories(root / "rtl");
  std::filesystem::create_directories(root / "include");
  std::filesystem::create_directories(root / "sub");

  std::ofstream header(root / "include" / "width.svh");
  header << "`ifndef LYRA_WIDTH\n`define LYRA_WIDTH 8\n`endif\n";

  std::ofstream leaf(root / "rtl" / "alu.sv");
  leaf << "`include \"width.svh\"\n"
       << "module alu;\n"
       << "  logic [`LYRA_WIDTH-1:0] y;\n"
       << "  initial $display(\"alu width %0d\", $bits(y));\n"
       << "endmodule\n";

  std::ofstream top(root / "rtl" / "soc_tb.sv");
  top << "module soc_tb;\n"
      << "  alu u_alu ();\n"
      << "  initial $display(\"tb trace %0d\", `TRACE);\n"
      << "endmodule\n";

  std::ofstream manifest(root / "lyra.toml");
  manifest << "[design]\n"
           << "name = \"soc\"\n"
           << "top = [\"soc_tb\"]\n"
           << "files = [\"rtl/alu.sv\", \"rtl/soc_tb.sv\"]\n"
           << "incdir = [\"include\"]\n"
           << "defines = [\"TRACE=1\"]\n"
           << "\n[compile]\nsingle_unit = true\n";
}

// Runs lyra from `dir`, which is what a declaration search reads and what no
// argument can express.
auto RunLyraFrom(
    const std::filesystem::path& lyra, const std::filesystem::path& dir,
    std::string_view args) -> lyra::test::ProcessOutcome {
  auto sh_or = lyra::support::FindOnPath("sh");
  EXPECT_TRUE(sh_or.has_value());
  if (!sh_or) return {};
  const std::vector<std::string> argv = {
      "-c",
      std::format("cd '{}' && '{}' {}", dir.string(), lyra.string(), args)};
  return RunChildProcess(*sh_or, argv, 60s);
}

// The whole point of the file: a design describes itself once, and the command
// line that runs it carries nothing. Running from a subdirectory is the same
// test asking whether a relative path in the declaration was resolved against
// the declaration or against whatever directory the command was typed in.
TEST(LyraDesignManifest, DeclaresTheDesignFromAnyDirectoryWithin) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();
  auto tmp_or = MakeScratchDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();
  WriteDeclaredDesign(*tmp_or);

  for (const auto& from : {*tmp_or, *tmp_or / "sub"}) {
    const auto run = RunLyraFrom(lyra, from, "run --backend jit");
    ASSERT_EQ(run.exit_code, 0) << from.string() << ": " << run.stderr_text;
    EXPECT_NE(run.stdout_text.find("tb trace 1"), std::string::npos)
        << from.string() << ": " << run.stdout_text;
    EXPECT_NE(run.stdout_text.find("alu width 8"), std::string::npos)
        << from.string() << ": " << run.stdout_text;
  }
}

// The precedence rule in both directions at once: a define given on the command
// line joins the declaration's rather than replacing it, while a top given
// there replaces the declaration's rather than joining it.
TEST(LyraDesignManifest, CommandLineJoinsMaterialAndReplacesSelection) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();
  auto tmp_or = MakeScratchDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();
  WriteDeclaredDesign(*tmp_or);

  const auto joined =
      RunLyraFrom(lyra, *tmp_or, "run --backend jit -D LYRA_WIDTH=16");
  ASSERT_EQ(joined.exit_code, 0) << joined.stderr_text;
  // The declaration's own define survived, and the command line's won over the
  // default the header would otherwise have supplied.
  EXPECT_NE(joined.stdout_text.find("tb trace 1"), std::string::npos)
      << joined.stdout_text;
  EXPECT_NE(joined.stdout_text.find("alu width 16"), std::string::npos)
      << joined.stdout_text;

  const auto narrowed =
      RunLyraFrom(lyra, *tmp_or, "run --backend jit --top alu");
  ASSERT_EQ(narrowed.exit_code, 0) << narrowed.stderr_text;
  EXPECT_NE(narrowed.stdout_text.find("alu width 8"), std::string::npos)
      << narrowed.stdout_text;
  EXPECT_EQ(narrowed.stdout_text.find("tb trace"), std::string::npos)
      << "the declaration's top was joined rather than replaced: "
      << narrowed.stdout_text;
}

// Naming a source is naming a design outright, so no declaration is searched
// for -- which is what makes an invocation mean the same thing in any
// directory. The tell is that the design's other half is missing.
TEST(LyraDesignManifest, NamingASourceUsesNoDeclaration) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();
  auto tmp_or = MakeScratchDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();
  WriteDeclaredDesign(*tmp_or);

  const auto checked = RunLyraFrom(lyra, *tmp_or, "check rtl/soc_tb.sv");
  EXPECT_NE(checked.exit_code, 0) << checked.stdout_text;
  EXPECT_NE(checked.stderr_text.find("unknown module 'alu'"), std::string::npos)
      << checked.stderr_text;
}

// Every refusal the schema makes, in one case because they are one feature: a
// declaration states what the design is, and anything else in it is a mistake
// that has to be reported rather than ignored.
TEST(LyraDesignManifest, RefusesWhatADesignCannotDeclare) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();
  auto tmp_or = MakeScratchDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();

  struct Refusal {
    std::string_view name;
    std::string_view body;
    std::string_view expected;
  };
  static constexpr std::array<Refusal, 6> kRefusals = {
      {{.name = "unknown-key.toml",
        .body = "[design]\ntops = [\"a\"]\n",
        .expected = "unrecognized key"},
       {.name = "unknown-table.toml",
        .body = "[designs]\ntop = [\"a\"]\n",
        .expected = "unrecognized table"},
       {.name = "invocation-key.toml",
        .body = "[compile]\nrelease = true\n",
        .expected = "pass it on the command line"},
       {.name = "pattern.toml",
        .body = "[design]\nfiles = [\"rtl/*.sv\"]\n",
        .expected = "is a pattern"},
       {.name = "bad-policy.toml",
        .body = "[compile]\nassertions = \"loud\"\n",
        .expected = "is not one of check, skip"},
       {.name = "no-name.toml",
        .body = "[design]\ntop = [\"a\"]\n",
        .expected = "a design has to say what it is called"}}};

  for (const auto& refusal : kRefusals) {
    const auto path = *tmp_or / refusal.name;
    std::ofstream(path) << refusal.body;
    const auto checked = RunLyraFrom(
        lyra, *tmp_or, std::format("check --config '{}'", path.string()));
    EXPECT_NE(checked.exit_code, 0) << refusal.name << ": accepted";
    EXPECT_NE(checked.stderr_text.find(refusal.expected), std::string::npos)
        << refusal.name << ": " << checked.stderr_text;
  }
}

// An interface port (LRM 23.3.3.4) and a `ref` port (LRM 23.3.3.2) may not be
// left unconnected, and a top's ports are connected to nothing, so a module
// declaring either is a design element and not a design. Every name inside such
// a module still resolves, which is why analysis answers and building a design
// does not.
//
// Both forms are here because one front-end option admits both, and that option
// is what the shape needs to elaborate at all -- so no source file alone
// reaches it and the corpus cannot state it.
TEST(LyraTopSelection, RefusesATopWhosePortNeedsAnInstantiation) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();
  auto tmp_or = MakeScratchDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();

  struct Refusal {
    std::string_view name;
    std::string_view body;
    std::string_view expected;
  };
  static constexpr std::array<Refusal, 2> kRefusals = {
      {{.name = "iface.sv",
        .body = "interface Bus;\n"
                "  logic [7:0] data;\n"
                "endinterface\n"
                "module Dut(Bus b);\n"
                "  initial $display(\"%0d\", b.data);\n"
                "endmodule\n",
        .expected = "an interface port cannot be left unconnected"},
       {.name = "ref.sv",
        .body = "module Dut(ref int x);\n"
                "  initial x = 7;\n"
                "endmodule\n",
        .expected = "a 'ref' port cannot be left unconnected"}}};

  for (const auto& refusal : kRefusals) {
    const auto source = *tmp_or / refusal.name;
    std::ofstream(source) << refusal.body;
    const auto args =
        std::format("--allow-toplevel-iface-ports '{}'", source.string());

    const auto checked = RunLyraFrom(lyra, *tmp_or, "check " + args);
    EXPECT_EQ(checked.exit_code, 0)
        << refusal.name << ": " << checked.stderr_text;

    const auto lowered = RunLyraFrom(lyra, *tmp_or, "dump hir " + args);
    EXPECT_EQ(lowered.exit_code, 1)
        << refusal.name << ": " << lowered.stderr_text;
    EXPECT_NE(lowered.stderr_text.find(refusal.expected), std::string::npos)
        << refusal.name << ": " << lowered.stderr_text;
  }
}

// A command that names nothing says so, and says enough to act on. The two
// ways of arriving there look identical without that: nothing declared
// anywhere, or a declaration that itself named no sources -- and the
// declaration that applied may be several directories above the caller.
TEST(LyraDesignManifest, ReportsNoInputAndWhyThereIsNone) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();
  auto tmp_or = MakeScratchDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();
  std::filesystem::create_directory(*tmp_or / ".git");

  const auto searched = RunLyraFrom(lyra, *tmp_or, "check");
  EXPECT_NE(searched.exit_code, 0) << searched.stdout_text;
  EXPECT_NE(searched.stderr_text.find("no input files"), std::string::npos)
      << searched.stderr_text;
  EXPECT_NE(
      searched.stderr_text.find("searched for lyra.toml"), std::string::npos)
      << searched.stderr_text;

  std::ofstream(*tmp_or / "lyra.toml") << "[design]\nname = \"hollow\"\n";
  const auto declared = RunLyraFrom(lyra, *tmp_or, "check");
  EXPECT_NE(declared.exit_code, 0) << declared.stdout_text;
  EXPECT_NE(declared.stderr_text.find("design 'hollow'"), std::string::npos)
      << declared.stderr_text;
  EXPECT_NE(
      declared.stderr_text.find("declares no source files"), std::string::npos)
      << declared.stderr_text;
}

// A run-time error of the design ends the simulation the way $fatal does (LRM
// 20.10), so the run reaches the end of simulation time and its final
// procedures execute there (LRM 9.2.3). The corpus cannot state this: a case
// passes on a zero exit status, and this run has to fail.
TEST(LyraRun, ADesignErrorEndsTheRunThroughItsFinalProcedures) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();
  auto tmp_or = MakeScratchDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();

  const auto src = *tmp_or / "test.sv";
  std::ofstream(src) << "module Test;\n"
                     << "  int dyn[];\n"
                     << "  initial begin\n"
                     << "    #5;\n"
                     << "    dyn = new[-1];\n"
                     << "  end\n"
                     << "  final $display(\"reached the end\");\n"
                     << "endmodule\n";

  const std::vector<std::string> args = {"run",   "--backend", "jit",
                                         "--top", "Test",      src.string()};
  const auto run = RunChildProcess(lyra, args, 120s);
  ASSERT_EQ(run.termination, TerminationKind::kExitedNonZero)
      << run.stdout_text << run.stderr_text;
  EXPECT_NE(run.stderr_text.find("size operand is negative"), std::string::npos)
      << run.stderr_text;
  EXPECT_NE(run.stdout_text.find("reached the end"), std::string::npos)
      << "stdout: " << run.stdout_text;
}

// Variable initialization is simulation activity at time zero (LRM 4), not
// construction, so an error raised by an initializer is a run-time error of the
// design and is reported as one. Before the simulation's boundary followed the
// elaboration phases it left the emitted program entirely, which aborted.
TEST(LyraRun, AnErrorInTimeZeroInitializationIsReported) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();
  auto tmp_or = MakeScratchDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();

  const auto src = *tmp_or / "test.sv";
  std::ofstream(src) << "module Test;\n"
                     << "  typedef union tagged packed {\n"
                     << "    bit [7:0] a;\n"
                     << "    bit [7:0] b;\n"
                     << "  } U;\n"
                     << "  U src;\n"
                     << "  bit [7:0] v = src.b;\n"
                     << "  initial $display(\"v=%0d\", v);\n"
                     << "endmodule\n";

  const std::vector<std::string> args = {"run",   "--backend", "jit",
                                         "--top", "Test",      src.string()};
  const auto run = RunChildProcess(lyra, args, 120s);
  EXPECT_EQ(run.termination, TerminationKind::kExitedNonZero)
      << run.stdout_text << run.stderr_text;
  EXPECT_NE(
      run.stderr_text.find("inconsistent with the current tag"),
      std::string::npos)
      << run.stderr_text;
}

// LRM 8.4 leaves the result of reaching a member through a null object handle
// indeterminate and lets a tool issue an error, so the corpus cannot state
// this: a case is valid under any conforming simulator and this asks for one
// tool's choice. The choice is the error policy's -- the access depends on a
// value the design computed, so it is the design's own failure and owes a
// report. What separates a report from the signal death it replaces is that the
// output the design had already produced survives.
TEST(LyraRun, ReachingThroughANullObjectHandleIsReported) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();
  auto tmp_or = MakeScratchDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();

  const auto user_class = *tmp_or / "user_class.sv";
  std::ofstream(user_class) << "class C;\n"
                            << "  int x;\n"
                            << "endclass\n"
                            << "module Test;\n"
                            << "  initial begin\n"
                            << "    C h;\n"
                            << "    $display(\"reached the access\");\n"
                            << "    h.x = 5;\n"
                            << "  end\n"
                            << "endmodule\n";

  const std::vector<std::string> class_args = {
      "run", "--backend", "jit", "--top", "Test", user_class.string()};
  const auto through_class = RunChildProcess(lyra, class_args, 120s);
  EXPECT_EQ(through_class.termination, TerminationKind::kExitedNonZero)
      << through_class.stdout_text << through_class.stderr_text;
  EXPECT_NE(
      through_class.stderr_text.find("null object handle"), std::string::npos)
      << through_class.stderr_text;
  EXPECT_NE(
      through_class.stdout_text.find("reached the access"), std::string::npos)
      << "stdout: " << through_class.stdout_text;

  // A `process` handle is an object handle of a class the runtime library
  // provides (LRM 9.7), so it answers to the same rule, and it reaches the
  // object by a different operator than a member access does.
  const auto builtin_class = *tmp_or / "builtin_class.sv";
  std::ofstream(builtin_class) << "module Test;\n"
                               << "  initial begin\n"
                               << "    process p;\n"
                               << "    $display(\"reached the access\");\n"
                               << "    p.kill();\n"
                               << "  end\n"
                               << "endmodule\n";

  const std::vector<std::string> process_args = {
      "run", "--backend", "jit", "--top", "Test", builtin_class.string()};
  const auto through_process = RunChildProcess(lyra, process_args, 120s);
  EXPECT_EQ(through_process.termination, TerminationKind::kExitedNonZero)
      << through_process.stdout_text << through_process.stderr_text;
  EXPECT_NE(
      through_process.stderr_text.find("null object handle"), std::string::npos)
      << through_process.stderr_text;
  EXPECT_NE(
      through_process.stdout_text.find("reached the access"), std::string::npos)
      << "stdout: " << through_process.stdout_text;
}

}  // namespace
