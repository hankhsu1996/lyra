#include <array>
#include <cerrno>
#include <cstring>
#include <expected>
#include <filesystem>
#include <format>
#include <fstream>
#include <gtest/gtest.h>
#include <memory>
#include <string>
#include <string_view>
#include <vector>

#include "lyra/support/subprocess.hpp"
#include "tests/framework/process.hpp"
#include "tools/cpp/runfiles/runfiles.h"

namespace {

using bazel::tools::cpp::runfiles::Runfiles;
using lyra::test::RunChildProcess;
using lyra::test::TerminationKind;
using namespace std::chrono_literals;

// A directory of this test's own to emit into. The name is drawn rather than
// derived from the test's, because a test that reruns must not inherit what a
// previous run left behind. Returns why it failed rather than throwing, so a
// setup failure is reported as the test failing rather than as a crash.
auto MakeScratchDir() -> std::expected<std::filesystem::path, std::string> {
  const auto base = std::filesystem::temp_directory_path() / "lyra-XXXXXX";
  std::string templ = base.string();
  if (mkdtemp(templ.data()) == nullptr) {
    return std::unexpected(
        std::format(
            "mkdtemp('{}') failed: {}", base.string(), std::strerror(errno)));
  }
  return std::filesystem::path(templ);
}

auto ResolveLyra() -> std::filesystem::path {
  std::string err;
  std::unique_ptr<Runfiles> runfiles{Runfiles::CreateForTest(&err)};
  EXPECT_TRUE(runfiles) << err;
  return runfiles ? std::filesystem::path(runfiles->Rlocation("_main/lyra"))
                  : std::filesystem::path{};
}

// The smallest design that produces observable output. What these tests are
// about is the project built around a design, so the design itself carries no
// weight beyond proving the program ran.
auto WriteTrivialSource(const std::filesystem::path& path) -> void {
  std::ofstream out(path);
  out << "module Test;\n"
      << "  initial $display(\"ran %0d\", 6 * 7);\n"
      << "endmodule\n";
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

TEST(LyraCompile, ProducesPortableBuildableProject) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();

  auto tmp_or = MakeScratchDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();
  const auto src = *tmp_or / "test.sv";
  WriteTrivialSource(src);
  const auto out_dir = *tmp_or / "out";

  const std::vector<std::string> args = {
      "compile", "--top", "Test", "-o", out_dir.string(), src.string()};
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

  // The recipe also takes a compiler the project was not produced with, which
  // is the rest of what portable means: the headers satisfy a second
  // implementation, and the program still links the runtime library the first
  // one compiled. Skipped where no second implementation is installed.
  auto other_or = lyra::support::FindOnPath("g++");
  if (!other_or) return;
  std::filesystem::remove(program);
  const std::vector<std::string> rebuild_other = {
      "-c", "cd '" + out_dir.string() + "' && sh build.sh --cxx '" +
                other_or->string() + "'"};
  const auto other_built = RunChildProcess(*sh_or, rebuild_other, 120s);
  ASSERT_EQ(other_built.exit_code, 0) << other_built.stderr_text;

  const auto other_run = RunChildProcess(program, {}, 30s);
  EXPECT_EQ(other_run.exit_code, 0) << other_run.stderr_text;
  EXPECT_NE(other_run.stdout_text.find("ran 42"), std::string::npos)
      << "stdout: " << other_run.stdout_text;
}

// Re-emitting one directory at the other optimization must rebuild. The recipe
// caches a precompiled header beside the project, and clang refuses one built
// under different options, so the two builds need separate cache entries.
TEST(LyraCompile, RebuildsAfterSwitchingOptimization) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();

  auto tmp_or = MakeScratchDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();
  const auto src = *tmp_or / "test.sv";
  WriteTrivialSource(src);
  const auto out_dir = *tmp_or / "out";
  const auto program = out_dir / "program";

  auto sh_or = lyra::support::FindOnPath("sh");
  ASSERT_TRUE(sh_or.has_value()) << sh_or.error();
  const std::vector<std::string> rebuild = {
      "-c", "cd '" + out_dir.string() + "' && sh build.sh"};

  for (const std::string_view mode : {"", "--release"}) {
    std::vector<std::string> args = {"emit", "cpp", "--top",
                                     "Test", "-o",  out_dir.string()};
    if (!mode.empty()) args.emplace_back(mode);
    args.push_back(src.string());
    const auto emitted = RunChildProcess(lyra, args, 120s);
    ASSERT_EQ(emitted.exit_code, 0) << mode << ": " << emitted.stderr_text;

    const auto built = RunChildProcess(*sh_or, rebuild, 120s);
    ASSERT_EQ(built.exit_code, 0) << mode << ": " << built.stderr_text;

    const auto run = RunChildProcess(program, {}, 30s);
    EXPECT_EQ(run.exit_code, 0) << mode << ": " << run.stderr_text;
    EXPECT_NE(run.stdout_text.find("ran 42"), std::string::npos)
        << mode << " stdout: " << run.stdout_text;
  }
}

TEST(LyraEmit, PortableProjectBuildsItsDpiSources) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();

  auto tmp_or = MakeScratchDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();
  const auto src = *tmp_or / "test.sv";
  WriteDpiSource(src);
  const auto foreign = *tmp_or / "foreign.c";
  WriteDpiForeignSource(foreign);
  const auto out_dir = *tmp_or / "out";

  const std::vector<std::string> args = {
      "emit",           "cpp",        "--top",          "Test",      "-o",
      out_dir.string(), "--dpi-link", foreign.string(), src.string()};
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
    const auto run = RunLyraFrom(lyra, from, "run");
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

  const auto joined = RunLyraFrom(lyra, *tmp_or, "run -D LYRA_WIDTH=16");
  ASSERT_EQ(joined.exit_code, 0) << joined.stderr_text;
  // The declaration's own define survived, and the command line's won over the
  // default the header would otherwise have supplied.
  EXPECT_NE(joined.stdout_text.find("tb trace 1"), std::string::npos)
      << joined.stdout_text;
  EXPECT_NE(joined.stdout_text.find("alu width 16"), std::string::npos)
      << joined.stdout_text;

  const auto narrowed = RunLyraFrom(lyra, *tmp_or, "run --top alu");
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

}  // namespace
