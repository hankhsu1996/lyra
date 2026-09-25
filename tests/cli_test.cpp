// What the command line itself decides: what a design declares about itself,
// what an invocation adds to or replaces in that declaration, which top a
// design element may be, what a unit publishes to whoever reads it, and how a
// design that fails at run time is reported.
//
// None of it is a statement about a backend, so a case that has to run a design
// asks for the one that compiles no C++, and the file costs seconds and gates.
// What an emitted C++ project is worth once built is a different question with
// a different price, and lives in `emitted_project_test.cpp`.

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
    const auto run = RunLyraFrom(lyra, from, "run --backend llvm");
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
      RunLyraFrom(lyra, *tmp_or, "run --backend llvm -D LYRA_WIDTH=16");
  ASSERT_EQ(joined.exit_code, 0) << joined.stderr_text;
  // The declaration's own define survived, and the command line's won over the
  // default the header would otherwise have supplied.
  EXPECT_NE(joined.stdout_text.find("tb trace 1"), std::string::npos)
      << joined.stdout_text;
  EXPECT_NE(joined.stdout_text.find("alu width 16"), std::string::npos)
      << joined.stdout_text;

  const auto narrowed =
      RunLyraFrom(lyra, *tmp_or, "run --backend llvm --top alu");
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

// A construct Lyra does not yet carry out is refused by the compiler, in its
// own words and with one answer whichever backend was asked for, and nothing is
// written -- rather than a project written as text another compiler rejects, or
// each backend failing in a vocabulary of its own. An event used as a handle
// (LRM 15.5.5) is such a construct, in every position a program can put it; the
// first of these once reached the source backend and came out as C++ the host
// compiler would not accept.
TEST(LyraEmit, AConstructNotYetSupportedIsRefusedBeforeAnythingIsWritten) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();
  auto tmp_or = MakeScratchDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();

  struct Position {
    std::string_view name;
    std::string_view body;
  };
  static constexpr std::array<Position, 7> kPositions = {
      {{.name = "assigned", .body = "  event e;\n  initial e = null;\n"},
       {.name = "initialized", .body = "  event e = null;\n"},
       {.name = "initialized-in-a-procedure",
        .body = "  initial begin event e = null; end\n"},
       {.name = "compared",
        .body = "  event e;\n  initial if (e == null) $display(\"x\");\n"},
       {.name = "tested",
        .body = "  event e;\n  initial if (e) $display(\"x\");\n"},
       {.name = "negated", .body = "  event e;\n  initial $display(!e);\n"},
       {.name = "looped-on",
        .body = "  event e;\n  initial while (e) $display(\"x\");\n"}}};

  for (const Position& position : kPositions) {
    const std::string source = std::format("{}.sv", position.name);
    std::ofstream(*tmp_or / source) << "module Test;\n"
                                    << position.body << "endmodule\n";
    const std::string out = std::format("out-{}", position.name);

    const auto emitted = RunLyraFrom(
        lyra, *tmp_or,
        std::format("emit cpp --top Test -o {} {}", out, source));
    EXPECT_NE(emitted.exit_code, 0) << position.name;
    EXPECT_NE(
        emitted.stderr_text.find("is not yet supported"), std::string::npos)
        << position.name << ": " << emitted.stderr_text;
    EXPECT_FALSE(std::filesystem::exists(*tmp_or / out / "Test.cpp"))
        << position.name << ": the refused unit was written anyway";

    const auto ran = RunLyraFrom(
        lyra, *tmp_or, std::format("run --backend llvm --top Test {}", source));
    EXPECT_NE(ran.exit_code, 0) << position.name;
    EXPECT_EQ(ran.stderr_text, emitted.stderr_text)
        << position.name << ": the two backends answered differently";
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
//
// The error is raised inside a task and a block that a `disable` names (LRM
// 9.6.2), so it leaves through the landing of each. Neither may claim it: a
// run-time error is a departure no region lands.
TEST(LyraRun, ADesignErrorEndsTheRunThroughItsFinalProcedures) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();
  auto tmp_or = MakeScratchDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();

  const auto src = *tmp_or / "test.sv";
  std::ofstream(src) << "module Test;\n"
                     << "  int dyn[];\n"
                     << "  task automatic grows();\n"
                     << "    #5;\n"
                     << "    dyn = new[-1];\n"
                     << "  endtask\n"
                     << "  initial begin : outer\n"
                     << "    grows();\n"
                     << "    $display(\"continued past the region\");\n"
                     << "  end\n"
                     << "  initial begin\n"
                     << "    #100;\n"
                     << "    disable outer;\n"
                     << "    disable grows;\n"
                     << "  end\n"
                     << "  final $display(\"reached the end\");\n"
                     << "endmodule\n";

  const std::vector<std::string> args = {"run",   "--backend", "llvm",
                                         "--top", "Test",      src.string()};
  const auto run = RunChildProcess(lyra, args, 120s);
  ASSERT_EQ(run.termination, TerminationKind::kExitedNonZero)
      << run.stdout_text << run.stderr_text;
  EXPECT_NE(run.stderr_text.find("size operand is negative"), std::string::npos)
      << run.stderr_text;
  EXPECT_NE(run.stdout_text.find("reached the end"), std::string::npos)
      << "stdout: " << run.stdout_text;
  EXPECT_EQ(
      run.stdout_text.find("continued past the region"), std::string::npos)
      << "stdout: " << run.stdout_text;
}

// Variable initialization is simulation activity at time zero (LRM 4), not
// construction, so an error raised by an initializer is a run-time error of the
// design: it is reported, and it ends the simulation the way $fatal does, so
// the final procedures still execute at its end (LRM 9.2.3, 20.10).
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
                     << "  final $display(\"reached the end\");\n"
                     << "endmodule\n";

  const std::vector<std::string> args = {"run",   "--backend", "llvm",
                                         "--top", "Test",      src.string()};
  const auto run = RunChildProcess(lyra, args, 120s);
  EXPECT_EQ(run.termination, TerminationKind::kExitedNonZero)
      << run.stdout_text << run.stderr_text;
  EXPECT_NE(
      run.stderr_text.find("inconsistent with the current tag"),
      std::string::npos)
      << run.stderr_text;
  EXPECT_NE(run.stdout_text.find("reached the end"), std::string::npos)
      << "stdout: " << run.stdout_text;
  EXPECT_EQ(run.stdout_text.find("v="), std::string::npos)
      << "stdout: " << run.stdout_text;
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
      "run", "--backend", "llvm", "--top", "Test", user_class.string()};
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
      "run", "--backend", "llvm", "--top", "Test", builtin_class.string()};
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

// What `build` produces on the backend that writes no C++ is a program in its
// own right: moved away from where it was built and run after the compiler has
// exited, it builds a hierarchy spanning units, reads the simulation's
// arguments off its own argv (LRM 21.6), leaves a task through a departure the
// caller lands (LRM 9.6.2), and answers with the design's exit status. It does
// all of that however hard and however wide it was compiled.
TEST(LyraBuild, TheLlvmProgramRunsOnItsOwn) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();
  auto tmp_or = MakeScratchDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();

  const auto src = *tmp_or / "test.sv";
  std::ofstream(src)
      << "module Leaf(input int a, output int y);\n"
      << "  assign y = a + 1;\n"
      << "endmodule\n"
      << "module Test;\n"
      << "  int src;\n"
      << "  int dst;\n"
      << "  int after_disable;\n"
      << "  Leaf u(.a(src), .y(dst));\n"
      << "  task automatic leaves();\n"
      << "    disable leaves;\n"
      << "    after_disable = 1;\n"
      << "  endtask\n"
      << "  initial begin\n"
      << "    src = 41;\n"
      << "    leaves();\n"
      << "    #1;\n"
      << "    if ($test$plusargs(\"fail\")) $fatal(1, \"asked\");\n"
      << "    $display(\"y=%0d after=%0d\", dst, after_disable);\n"
      << "  end\n"
      << "endmodule\n";

  const auto program = *tmp_or / "program";
  const auto moved = *tmp_or / "moved";
  for (const std::vector<std::string>& how :
       {std::vector<std::string>{},
        std::vector<std::string>{"--release", "-j", "4"}}) {
    std::vector<std::string> args = {
        "build",          "--backend",   "llvm",
        "--top",          "Test",        "-o",
        program.string(), "--cache-dir", (*tmp_or / "cache").string()};
    args.insert(args.end(), how.begin(), how.end());
    args.push_back(src.string());
    const std::string label = how.empty() ? "default" : "--release -j 4";
    const auto built = RunChildProcess(lyra, args, 120s);
    ASSERT_EQ(built.termination, TerminationKind::kExitedNormally)
        << label << ": " << built.stdout_text << built.stderr_text;

    std::filesystem::rename(program, moved);

    const auto passed = RunChildProcess(moved, {}, 60s);
    EXPECT_EQ(passed.termination, TerminationKind::kExitedNormally)
        << label << ": " << passed.stdout_text << passed.stderr_text;
    EXPECT_NE(passed.stdout_text.find("y=42 after=0"), std::string::npos)
        << label << " stdout: " << passed.stdout_text;

    const std::vector<std::string> failing = {"+fail"};
    const auto failed = RunChildProcess(moved, failing, 60s);
    EXPECT_EQ(failed.termination, TerminationKind::kExitedNonZero)
        << label << ": " << failed.stdout_text << failed.stderr_text;
    EXPECT_NE(failed.stderr_text.find("asked"), std::string::npos)
        << label << ": " << failed.stderr_text;
  }
}

// A value ends with the expression that made it, so what a running program
// holds is bounded by what it can still read and not by how much it has
// computed. A process that never waits makes a million calls here, each
// answering a value; were those values kept until the process next waited, the
// run would need gigabytes, and the cap it runs under is a small fraction of
// that.
TEST(LyraBuild, AValueEndsWithTheExpressionThatMadeIt) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();
  auto tmp_or = MakeScratchDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();

  const auto src = *tmp_or / "test.sv";
  std::ofstream(src)
      << "module Test;\n"
      << "  function automatic logic [127:0] mix(logic [127:0] x);\n"
      << "    return (x ^ (x << 3)) + 1;\n"
      << "  endfunction\n"
      << "  initial begin\n"
      << "    logic [127:0] acc;\n"
      << "    acc = 0;\n"
      << "    for (int i = 0; i < 1000000; i++) acc = mix(acc);\n"
      << "    $display(\"low=%0d\", acc[7:0]);\n"
      << "  end\n"
      << "endmodule\n";

  const auto program = *tmp_or / "program";
  const std::vector<std::string> args = {
      "build",          "--backend",   "llvm",
      "--top",          "Test",        "-o",
      program.string(), "--cache-dir", (*tmp_or / "cache").string(),
      src.string()};
  const auto built = RunChildProcess(lyra, args, 120s);
  ASSERT_EQ(built.termination, TerminationKind::kExitedNormally)
      << built.stdout_text << built.stderr_text;

  auto sh_or = lyra::support::FindOnPath("sh");
  ASSERT_TRUE(sh_or.has_value());
  const std::vector<std::string> capped = {
      "-c", std::format("ulimit -v 524288 && exec '{}'", program.string())};
  const auto ran = RunChildProcess(*sh_or, capped, 60s);
  EXPECT_EQ(ran.termination, TerminationKind::kExitedNormally)
      << ran.stdout_text << ran.stderr_text;
  EXPECT_NE(ran.stdout_text.find("low=64"), std::string::npos)
      << "stdout: " << ran.stdout_text;
}

// An option means something to a command or it is refused by name, and the
// refusal says which commands do take it. What is refused is a function of the
// command alone: an option the command acts on stands even where this time it
// changes nothing, as a precompiled header does for the backend that compiles
// no C++.
TEST(LyraCommandLine, RefusesAnOptionTheCommandDoesNotActOn) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();
  auto tmp_or = MakeScratchDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();
  const auto src = *tmp_or / "test.sv";
  WriteTrivialSource(src);

  struct Refusal {
    std::vector<std::string> args;
    std::string expected;
  };
  const std::vector<Refusal> refusals = {
      {.args = {"dump", "ast", "--release", src.string()},
       .expected = "--release means nothing to `dump ast`"},
      {.args = {"check", "--backend", "llvm", src.string()},
       .expected = "--backend means nothing to `check`"},
      {.args =
           {"emit", "cpp", "--backend", "llvm", "-o",
            (*tmp_or / "out").string(), src.string()},
       .expected = "--backend means nothing to `emit cpp`"},
      {.args =
           {"build", "-o", (*tmp_or / "p").string(), src.string(), "--",
            "+trace"},
       .expected = "arguments after `--` means nothing to `build`"},
      {.args = {"cache", "clear", "--rebuild"},
       .expected = "--rebuild means nothing to `cache clear`"}};
  for (const Refusal& refusal : refusals) {
    const auto refused = RunChildProcess(lyra, refusal.args, 60s);
    EXPECT_NE(refused.exit_code, 0) << refusal.expected << ": accepted";
    EXPECT_NE(refused.stderr_text.find(refusal.expected), std::string::npos)
        << refused.stderr_text;
    EXPECT_NE(refused.stderr_text.find("it is taken by"), std::string::npos)
        << refused.stderr_text;
  }

  const std::vector<std::string> acted_on = {
      "build",       "--backend",
      "llvm",        "--no-pch",
      "--cache-dir", (*tmp_or / "store").string(),
      "-o",          (*tmp_or / "program").string(),
      src.string()};
  const auto built = RunChildProcess(lyra, acted_on, 120s);
  EXPECT_EQ(built.exit_code, 0) << built.stderr_text;
}

// How many entries a directory holds, none where there is no directory.
auto CountEntries(const std::filesystem::path& dir) -> std::size_t {
  std::error_code ec;
  const std::filesystem::directory_iterator entries(dir, ec);
  return ec ? 0
            : static_cast<std::size_t>(std::ranges::distance(
                  entries, std::filesystem::directory_iterator{}));
}

// How many programs a store keeps.
auto KeptPrograms(const std::filesystem::path& store) -> std::size_t {
  return CountEntries(store / "programs");
}

// A program is kept under what built it, so building a design again reuses
// the one kept, and building a changed design, or the same one compiled at
// another level, keeps a second; how wide it was built changes nothing it
// holds, so that keeps none. `build` writes the
// one file it was asked for and `run` writes nothing, and nothing kept can be
// reached through what a command handed back: clearing the store leaves both
// the built program and a later run whole.
TEST(LyraBuild, KeepsAProgramByWhatBuiltIt) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();
  auto tmp_or = MakeScratchDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();
  const auto work = *tmp_or / "work";
  const auto store = *tmp_or / "store";
  std::filesystem::create_directories(work);
  const auto write_design = [&](int value) {
    std::ofstream(work / "design.sv")
        << "module Test;\n"
        << std::format("  initial $display(\"value=%0d\", {});\n", value)
        << "endmodule\n";
  };
  const std::string common =
      std::format("--backend llvm --cache-dir '{}'", store.string());

  write_design(1);
  auto first = RunLyraFrom(lyra, work, "build " + common + " design.sv");
  ASSERT_EQ(first.exit_code, 0) << first.stderr_text;
  EXPECT_TRUE(std::filesystem::is_regular_file(work / "Test"))
      << "an anonymous design with one top names its program after the top";
  EXPECT_EQ(KeptPrograms(store), 1U);

  auto again = RunLyraFrom(lyra, work, "build " + common + " design.sv");
  ASSERT_EQ(again.exit_code, 0) << again.stderr_text;
  EXPECT_EQ(KeptPrograms(store), 1U)
      << "an unchanged design built again keeps no second program";

  auto wider = RunLyraFrom(lyra, work, "build -j 4 " + common + " design.sv");
  ASSERT_EQ(wider.exit_code, 0) << wider.stderr_text;
  EXPECT_EQ(KeptPrograms(store), 1U)
      << "a design built wider is the same program, and keeps no second";

  auto released =
      RunLyraFrom(lyra, work, "build --release " + common + " design.sv");
  ASSERT_EQ(released.exit_code, 0) << released.stderr_text;
  EXPECT_EQ(KeptPrograms(store), 2U)
      << "a design compiled at another level was handed the program kept for "
         "the first";

  write_design(2);
  auto changed = RunLyraFrom(lyra, work, "run " + common + " design.sv");
  ASSERT_EQ(changed.exit_code, 0) << changed.stderr_text;
  EXPECT_NE(changed.stdout_text.find("value=2"), std::string::npos)
      << "a changed design ran the program kept for the old one: "
      << changed.stdout_text;
  EXPECT_EQ(KeptPrograms(store), 3U);

  EXPECT_EQ(CountEntries(work), 2U)
      << "the working directory holds the design and the one program built, "
         "and nothing a run wrote";

  auto cleared = RunLyraFrom(
      lyra, work, std::format("cache clear --cache-dir '{}'", store.string()));
  ASSERT_EQ(cleared.exit_code, 0) << cleared.stderr_text;
  EXPECT_EQ(KeptPrograms(store), 0U);
  const auto held = RunChildProcess(work / "Test", {}, 30s);
  EXPECT_EQ(held.exit_code, 0) << held.stderr_text;
  EXPECT_NE(held.stdout_text.find("value=1"), std::string::npos)
      << "stdout: " << held.stdout_text;

  auto after_clear = RunLyraFrom(lyra, work, "run " + common + " design.sv");
  ASSERT_EQ(after_clear.exit_code, 0) << after_clear.stderr_text;
  EXPECT_NE(after_clear.stdout_text.find("value=2"), std::string::npos)
      << "stdout: " << after_clear.stdout_text;

  const auto into_directory = RunLyraFrom(
      lyra, work,
      std::format("build {} -o '{}' design.sv", common, store.string()));
  EXPECT_NE(into_directory.exit_code, 0);
  EXPECT_NE(
      into_directory.stderr_text.find("is a directory"), std::string::npos)
      << into_directory.stderr_text;
}

// A command leaves behind what it was asked for and nothing else. What a build
// makes on the way to the program goes when the command does, and the store in
// the platform's cache directory appears only once something is kept in it.
TEST(LyraCommandLine, LeavesBehindOnlyWhatItWasAskedFor) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();
  auto tmp_or = MakeScratchDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();
  const auto work = *tmp_or / "work";
  const auto temporary = *tmp_or / "temporary";
  const auto cache_home = *tmp_or / "cache";
  std::filesystem::create_directories(work);
  std::filesystem::create_directories(temporary);
  std::filesystem::create_directories(cache_home);
  WriteTrivialSource(work / "design.sv");

  auto sh_or = lyra::support::FindOnPath("sh");
  ASSERT_TRUE(sh_or.has_value()) << sh_or.error();
  const auto run = [&](std::string_view args) {
    const std::vector<std::string> argv = {
        "-c",
        std::format(
            "cd '{}' && TMPDIR='{}' XDG_CACHE_HOME='{}' '{}' {}", work.string(),
            temporary.string(), cache_home.string(), lyra.string(), args)};
    return RunChildProcess(*sh_or, argv, 120s);
  };
  const auto store = cache_home / "lyra";

  const auto checked = run("check design.sv");
  ASSERT_EQ(checked.exit_code, 0) << checked.stderr_text;
  EXPECT_FALSE(std::filesystem::exists(store))
      << "a command that keeps nothing created the store";

  const auto ran = run("run --backend llvm design.sv");
  ASSERT_EQ(ran.exit_code, 0) << ran.stderr_text;
  EXPECT_EQ(CountEntries(temporary), 0U)
      << "a run left what it built in the temporary directory";
  EXPECT_EQ(KeptPrograms(store), 1U)
      << "the program is kept in the platform's cache directory";

  const auto built = run("build --backend llvm -o program design.sv");
  ASSERT_EQ(built.exit_code, 0) << built.stderr_text;
  EXPECT_EQ(CountEntries(temporary), 0U)
      << "a build left what it built in the temporary directory";
  EXPECT_TRUE(std::filesystem::is_regular_file(work / "program"));
}

// Foreign code that calls an exported subroutine once its execution thread is
// in the disabled state breaks the protocol, and a simulator is obliged to
// report it (LRM 35.9 item d). The run ending puts it in that state too, and
// what the call is owed then is nothing of the design at all, since the run is
// over. The corpus cannot state this: the report ends the run in failure, and a
// case passes only on a zero exit status.
TEST(LyraRun, CallingInAfterTheRunEndedIsReportedAndRunsNothing) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();
  auto tmp_or = MakeScratchDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();

  const auto src = *tmp_or / "test.sv";
  std::ofstream(src) << "module Test;\n"
                     << "  import \"DPI-C\" context function int ask();\n"
                     << "  export \"DPI-C\" function answer;\n"
                     << "  int entered;\n"
                     << "  function int answer();\n"
                     << "    entered = entered + 1;\n"
                     << "    $finish(0);\n"
                     << "    return 2;\n"
                     << "  endfunction\n"
                     << "  initial begin\n"
                     << "    entered = 0;\n"
                     << "    void'(ask());\n"
                     << "  end\n"
                     << "  final $display(\"entered=%0d\", entered);\n"
                     << "endmodule\n";
  const auto foreign = *tmp_or / "foreign.c";
  std::ofstream(foreign) << "#include \"dpi.h\"\n"
                         << "int32_t ask(void) {\n"
                         << "  return answer() + answer();\n"
                         << "}\n";

  const std::vector<std::string> args = {
      "run",  "--backend",  "llvm",           "--top",
      "Test", "--dpi-link", foreign.string(), src.string()};
  const auto run = RunChildProcess(lyra, args, 120s);
  EXPECT_EQ(run.termination, TerminationKind::kExitedNonZero)
      << run.stdout_text << run.stderr_text;
  EXPECT_NE(
      run.stderr_text.find("entered the disabled state"), std::string::npos)
      << run.stderr_text;
  EXPECT_NE(run.stdout_text.find("entered=1"), std::string::npos)
      << "stdout: " << run.stdout_text;
}

auto CountOccurrences(std::string_view text, std::string_view needle)
    -> std::size_t {
  std::size_t count = 0;
  for (std::size_t at = text.find(needle); at != std::string_view::npos;
       at = text.find(needle, at + needle.size())) {
    ++count;
  }
  return count;
}

// A frame of another language ends only by returning (LRM 35.9), so a design's
// run-time error raised below one stops where the foreign caller entered the
// design: it is reported once and ends the run, and the foreign caller regains
// control told that its caller will not continue. A foreign C++ caller that
// catches everything around the call is the sharpest witness, since anything
// that crossed would land in its handler instead of being reported. The error
// is raised at each depth an exported subroutine can hold it -- in its own
// body, in a function it calls, inside a named block that a `disable` names,
// in an export reached through a second foreign frame, and in an exported task.
// The corpus cannot state this: which way a run-time error ends the run is the
// tool's to choose, and the run has to fail.
TEST(LyraRun, AnErrorBelowAForeignCallerStopsWhereItEnteredTheDesign) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();
  auto tmp_or = MakeScratchDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();

  const auto src = *tmp_or / "test.sv";
  std::ofstream(src)
      << "module Test;\n"
      << "  import \"DPI-C\" context function int ask(input int depth);\n"
      << "  import \"DPI-C\" context function int relay();\n"
      << "  import \"DPI-C\" context task ask_task();\n"
      << "  export \"DPI-C\" function answer;\n"
      << "  export \"DPI-C\" function deeper;\n"
      << "  export \"DPI-C\" task work;\n"
      << "  int dyn[];\n"
      << "  int after;\n"
      << "  function automatic int grow(input int n);\n"
      << "    dyn = new[n];\n"
      << "    return dyn.size();\n"
      << "  endfunction\n"
      << "  function int deeper();\n"
      << "    dyn = new[-1];\n"
      << "    return 3;\n"
      << "  endfunction\n"
      << "  function int answer(input int depth);\n"
      << "    case (depth)\n"
      << "      1: dyn = new[-1];\n"
      << "      2: void'(grow(-1));\n"
      << "      3: begin : blk\n"
      << "        if (depth == 99) disable blk;\n"
      << "        dyn = new[-1];\n"
      << "      end\n"
      << "      4: void'(relay());\n"
      << "    endcase\n"
      << "    return 7;\n"
      << "  endfunction\n"
      << "  task work();\n"
      << "    dyn = new[-1];\n"
      << "  endtask\n"
      << "  initial begin\n"
      << "    int depth;\n"
      << "    after = 0;\n"
      << "    void'($value$plusargs(\"depth=%d\", depth));\n"
      << "    if (depth == 5) ask_task();\n"
      << "    else void'(ask(depth));\n"
      << "    after = 1;\n"
      << "  end\n"
      << "  final $display(\"after=%0d\", after);\n"
      << "endmodule\n";
  const auto foreign = *tmp_or / "foreign.cpp";
  std::ofstream(foreign)
      << "#include <cstdio>\n"
      << "#include <svdpi.h>\n"
      << "#include \"dpi.h\"\n"
      << "static void settle(const char* who) {\n"
      << "  std::printf(\"%s regained control\\n\", who);\n"
      << "  if (svIsDisabledState() != 0) {\n"
      << "    std::printf(\"%s saw the disabled state\\n\", who);\n"
      << "    svAckDisabledState();\n"
      << "  }\n"
      << "}\n"
      << "extern \"C\" int32_t ask(int32_t depth) {\n"
      << "  int32_t got = -1;\n"
      << "  try {\n"
      << "    got = answer(depth);\n"
      << "  } catch (...) {\n"
      << "    std::printf(\"the foreign caller caught something\\n\");\n"
      << "  }\n"
      << "  settle(\"ask\");\n"
      << "  return got;\n"
      << "}\n"
      << "extern \"C\" int32_t relay(void) {\n"
      << "  int32_t got = -1;\n"
      << "  try {\n"
      << "    got = deeper();\n"
      << "  } catch (...) {\n"
      << "    std::printf(\"the foreign caller caught something\\n\");\n"
      << "  }\n"
      << "  settle(\"relay\");\n"
      << "  return got;\n"
      << "}\n"
      << "extern \"C\" int32_t ask_task(void) {\n"
      << "  int32_t answered = -1;\n"
      << "  try {\n"
      << "    answered = work();\n"
      << "  } catch (...) {\n"
      << "    std::printf(\"the foreign caller caught something\\n\");\n"
      << "  }\n"
      << "  std::printf(\"work answered %d\\n\", answered);\n"
      << "  settle(\"ask_task\");\n"
      << "  return 1;\n"
      << "}\n";

  for (const int depth : {1, 2, 3, 4, 5}) {
    SCOPED_TRACE(std::format("+depth={}", depth));
    const std::vector<std::string> args = {
        "run",
        "--backend",
        "llvm",
        "--top",
        "Test",
        "--dpi-link",
        foreign.string(),
        src.string(),
        "--",
        std::format("+depth={}", depth)};
    const auto run = RunChildProcess(lyra, args, 120s);
    EXPECT_EQ(run.termination, TerminationKind::kExitedNonZero)
        << run.stdout_text << run.stderr_text;
    EXPECT_EQ(CountOccurrences(run.stderr_text, "size operand is negative"), 1U)
        << run.stderr_text;
    EXPECT_EQ(
        run.stdout_text.find("the foreign caller caught something"),
        std::string::npos)
        << run.stdout_text;
    const std::string_view entered = depth == 5 ? "ask_task" : "ask";
    EXPECT_NE(
        run.stdout_text.find(std::format("{} regained control", entered)),
        std::string::npos)
        << run.stdout_text;
    if (depth == 4) {
      EXPECT_NE(
          run.stdout_text.find("relay saw the disabled state"),
          std::string::npos)
          << run.stdout_text;
    }
    if (depth == 5) {
      EXPECT_NE(run.stdout_text.find("work answered 1"), std::string::npos)
          << run.stdout_text;
    } else {
      EXPECT_NE(
          run.stdout_text.find("ask saw the disabled state"), std::string::npos)
          << run.stdout_text;
    }
    EXPECT_NE(run.stdout_text.find("after=0"), std::string::npos)
        << run.stdout_text;
  }
}

// LRM 35.9 names the disabled state for a `disable` only, because that is the
// one way it lets an execution be stopped under a foreign call and handed back
// to it. A `kill` stops it the same way as far as the foreign side can tell --
// it must call in no further and return -- so Lyra tells it the same thing: the
// exported task it was inside answers 1. A conforming tool may answer 0, which
// is why this is Lyra's own answer and not the corpus's.
TEST(LyraRun, AKilledProcessTellsItsForeignFramesTheyWereDisabled) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();
  auto tmp_or = MakeScratchDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();

  const auto src = *tmp_or / "test.sv";
  std::ofstream(src) << "module Test;\n"
                     << "  import \"DPI-C\" context task advance();\n"
                     << "  export \"DPI-C\" task step;\n"
                     << "  task step();\n"
                     << "    #10;\n"
                     << "  endtask\n"
                     << "  initial begin\n"
                     << "    process branch;\n"
                     << "    fork\n"
                     << "      begin\n"
                     << "        branch = process::self();\n"
                     << "        advance();\n"
                     << "      end\n"
                     << "    join_none\n"
                     << "    #5;\n"
                     << "    branch.kill();\n"
                     << "  end\n"
                     << "endmodule\n";
  const auto foreign = *tmp_or / "foreign.c";
  std::ofstream(foreign) << "#include <stdio.h>\n"
                         << "#include \"dpi.h\"\n"
                         << "int32_t advance(void) {\n"
                         << "  int32_t answered = step();\n"
                         << "  printf(\"step answered %d\\n\", answered);\n"
                         << "  return answered;\n"
                         << "}\n";

  const std::vector<std::string> args = {
      "run",  "--backend",  "llvm",           "--top",
      "Test", "--dpi-link", foreign.string(), src.string()};
  const auto run = RunChildProcess(lyra, args, 120s);
  EXPECT_EQ(run.termination, TerminationKind::kExitedNormally)
      << run.stdout_text << run.stderr_text;
  EXPECT_NE(run.stdout_text.find("step answered 1"), std::string::npos)
      << run.stdout_text;
}

}  // namespace
