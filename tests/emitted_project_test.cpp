// What an emitted C++ project is worth once it has left the compiler: that the
// directory rebuilds standalone through the recipe it ships, that a second
// toolchain accepts its headers and still links the runtime the first one
// compiled, that it is built as wide as a caller asked, and that a build whose
// prepared header is refused still produces a program.
//
// Every case here builds a project with the host compiler, several of them more
// than once, and no two can share what one prepared -- a prepared header is
// bound to the paths of the headers it was made from, and every project carries
// its own copy. So the file costs what it asserts, and it is answered on the
// schedule that answers everything else about that path. Cases about the
// command line itself belong beside it in `cli_test.cpp`, which gates.

#include <algorithm>
#include <array>
#include <chrono>
#include <cstddef>
#include <cstdint>
#include <filesystem>
#include <format>
#include <fstream>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <string>
#include <string_view>
#include <tuple>
#include <utility>
#include <vector>

#include "conformance/framework/process.hpp"
#include "conformance/framework/process_matchers.hpp"
#include "lyra/driver/subprocess.hpp"
#include "tests/framework/cli_fixture.hpp"

namespace {

using conformance::framework::ExitedWithCode;
using conformance::framework::MakeScratchDir;
using conformance::framework::RunChildProcess;
using lyra::test::ResolveLyra;
using lyra::test::WriteTrivialSource;
using testing::Ne;
using namespace std::chrono_literals;

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

TEST(LyraEmittedProject, ProducesPortableBuildableProject) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();

  auto tmp_or = MakeScratchDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();
  const auto src = *tmp_or / "test.sv";
  WriteTrivialSource(src);
  const auto out_dir = *tmp_or / "out";

  const std::vector<std::string> args = {
      "emit", "cpp", "--top", "Test", "-o", out_dir.string(), src.string()};
  const auto emitted = RunChildProcess(lyra, args);
  ASSERT_THAT(emitted, ExitedWithCode(0))
      << emitted.stdout_text << emitted.stderr_text;
  ASSERT_TRUE(std::filesystem::exists(out_dir / "build.sh"));

  // The directory builds standalone, with no Lyra checkout, through the
  // build.sh it ships.
  const auto program = out_dir / "program";
  auto sh_or = lyra::driver::FindOnPath("sh");
  ASSERT_TRUE(sh_or.has_value()) << sh_or.error().Text();
  const std::vector<std::string> rebuild = {
      "-c", "cd '" + out_dir.string() + "' && sh build.sh"};
  const auto built = RunChildProcess(*sh_or, rebuild);
  ASSERT_THAT(built, ExitedWithCode(0))
      << built.stdout_text << built.stderr_text;
  ASSERT_TRUE(std::filesystem::exists(program)) << program.string();

  const auto run = RunChildProcess(program, {});
  EXPECT_THAT(run, ExitedWithCode(0)) << run.stderr_text;
  EXPECT_NE(run.stdout_text.find("ran 42"), std::string::npos)
      << "stdout: " << run.stdout_text;

  // The recipe also takes a compiler the project was not produced with, which
  // is the rest of what portable means: the headers satisfy a second
  // implementation, and the program still links the runtime library the first
  // one compiled. Skipped where no second implementation is installed.
  auto other_or = lyra::driver::FindOnPath("g++");
  if (!other_or) return;
  std::filesystem::remove(program);
  const std::vector<std::string> rebuild_other = {
      "-c", "cd '" + out_dir.string() + "' && sh build.sh --cxx '" +
                other_or->string() + "'"};
  const auto other_built = RunChildProcess(*sh_or, rebuild_other);
  ASSERT_THAT(other_built, ExitedWithCode(0)) << other_built.stderr_text;

  const auto other_run = RunChildProcess(program, {});
  EXPECT_THAT(other_run, ExitedWithCode(0)) << other_run.stderr_text;
  EXPECT_NE(other_run.stdout_text.find("ran 42"), std::string::npos)
      << "stdout: " << other_run.stdout_text;
}

// Re-emitting one directory at the other optimization must rebuild. The recipe
// caches a precompiled header beside the project, and a compiler refuses one
// built under different options, so the two builds need separate cache
// entries.
TEST(LyraEmittedProject, RebuildsAfterSwitchingOptimization) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();

  auto tmp_or = MakeScratchDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();
  const auto src = *tmp_or / "test.sv";
  WriteTrivialSource(src);
  const auto out_dir = *tmp_or / "out";
  const auto program = out_dir / "program";

  auto sh_or = lyra::driver::FindOnPath("sh");
  ASSERT_TRUE(sh_or.has_value()) << sh_or.error().Text();
  const std::vector<std::string> rebuild = {
      "-c", "cd '" + out_dir.string() + "' && sh build.sh"};

  for (const std::string_view mode : {"", "-O2"}) {
    std::vector<std::string> args = {"emit", "cpp", "--top",
                                     "Test", "-o",  out_dir.string()};
    if (!mode.empty()) args.emplace_back(mode);
    args.push_back(src.string());
    const auto emitted = RunChildProcess(lyra, args);
    ASSERT_THAT(emitted, ExitedWithCode(0))
        << mode << ": " << emitted.stderr_text;

    const auto built = RunChildProcess(*sh_or, rebuild);
    ASSERT_THAT(built, ExitedWithCode(0)) << mode << ": " << built.stderr_text;

    const auto run = RunChildProcess(program, {});
    EXPECT_THAT(run, ExitedWithCode(0)) << mode << ": " << run.stderr_text;
    EXPECT_NE(run.stdout_text.find("ran 42"), std::string::npos)
        << mode << " stdout: " << run.stdout_text;
  }
}

// A wrapper leaves a file behind each time the step it watches runs, so a build
// that never attempted the optimization cannot pass, and one that refuses
// nothing leaves another wherever a link takes the prebuilt runtime, which is
// the plain build. A compile names `-c`; a link does not, and the probe's link
// writes a file named `probe`.
namespace {

struct LinkTimeRefusal {
  std::string_view name;
  std::string_view watched;
  bool refuses = true;
};

constexpr std::array<LinkTimeRefusal, 3> kLinkTimeRefusals{{
    {.name = "refuses-compile", .watched = R"x(*" -flto"*" -c "*)x"},
    {.name = "refuses-link", .watched = R"x(*" -c "*) ;; *" -flto"*)x"},
    {.name = "refuses-nothing",
     .watched = R"x(*" -c "*|*"/probe "*) ;; *" -flto"*)x",
     .refuses = false},
}};

// A compiler a case runs under: what the case is called, and the name the
// compiler is found by.
struct HostCompiler {
  std::string_view name;
  std::string_view executable;
};

// The two things that build a design: the recipe a project ships, and the
// build Lyra runs itself.
enum class Builder : std::uint8_t {
  kRecipe,
  kLyra,
};

auto NameOf(Builder builder) -> std::string_view {
  switch (builder) {
    case Builder::kRecipe:
      return "recipe";
    case Builder::kLyra:
      return "lyra";
  }
  std::unreachable();
}

class LinkTimeOptimizationRefused
    : public testing::TestWithParam<
          std::tuple<HostCompiler, std::size_t, Builder>> {};

// What a case is called: the compiler, what its wrapper refuses, and what
// builds.
auto LinkTimeRefusalName(
    const testing::TestParamInfo<LinkTimeOptimizationRefused::ParamType>& info)
    -> std::string {
  const auto& [compiler, refusal, builder] = info.param;
  std::string name = std::format(
      "{}_{}_{}", compiler.name, kLinkTimeRefusals.at(refusal).name,
      NameOf(builder));
  std::ranges::replace(name, '-', '_');
  return name;
}

}  // namespace

// A build under --lto optimizes the design together with the runtime in the
// first way the host compiler and the linker it drives accept, which is theirs
// to say, and a small program built each way decides which. A compiler that
// refuses to compile for it and a linker that refuses to link so each still
// leave a program, from the project's recipe and from the build Lyra runs
// itself; a toolchain refusing nothing links the program optimized and never
// falls back. Each is a wrapper around the compiler that watches, and where it
// refuses fails, exactly that step. A build that optimizes compiles the runtime
// whole, so each builder under each refusal is a case of its own, which is what
// lets a sharded run take them side by side.
TEST_P(LinkTimeOptimizationRefused, StillBuildsAProgram) {
  const auto [compiler, refusal, builder] = GetParam();
  const LinkTimeRefusal& watching = kLinkTimeRefusals.at(refusal);
  auto real = lyra::driver::FindOnPath(compiler.executable);
  if (!real) {
    GTEST_SKIP() << compiler.executable << " is not on the PATH";
  }
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();
  auto sh_or = lyra::driver::FindOnPath("sh");
  ASSERT_TRUE(sh_or.has_value()) << sh_or.error().Text();

  auto tmp_or = MakeScratchDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();
  const auto src = *tmp_or / "test.sv";
  WriteTrivialSource(src);

  const std::string label = std::format("{}-{}", compiler.name, watching.name);
  const auto wrapper = *tmp_or / label;
  const auto attempted_marker = *tmp_or / (label + ".attempted");
  const auto plain_marker = *tmp_or / (label + ".plain");
  std::ofstream(wrapper) << "#!/bin/sh\n"
                         << "case \" $* \" in " << watching.watched
                         << ") touch '" << attempted_marker.string() << "'"
                         << (watching.refuses ? "; exit 1" : "")
                         << " ;; *libcpp_runtime.a*) touch '"
                         << plain_marker.string() << "' ;; esac\n"
                         << "exec '" << real->string() << "' \"$@\"\n";
  std::filesystem::permissions(
      wrapper, std::filesystem::perms::owner_all,
      std::filesystem::perm_options::add);

  std::filesystem::path program;
  switch (builder) {
    case Builder::kRecipe: {
      const auto out_dir = *tmp_or / "out";
      const std::vector<std::string> emit = {"emit",      "cpp",
                                             "-O2",       "--lto",
                                             "--top",     "Test",
                                             "--cxx",     wrapper.string(),
                                             "-o",        out_dir.string(),
                                             src.string()};
      const auto emitted = RunChildProcess(lyra, emit);
      ASSERT_THAT(emitted, ExitedWithCode(0)) << emitted.stderr_text;
      const std::vector<std::string> recipe = {
          "-c", "cd '" + out_dir.string() + "' && sh build.sh -j 0"};
      const auto built = RunChildProcess(*sh_or, recipe);
      ASSERT_THAT(built, ExitedWithCode(0)) << built.stderr_text;
      program = out_dir / "program";
      break;
    }
    case Builder::kLyra: {
      program = *tmp_or / "program";
      const std::vector<std::string> build = {
          "build",
          "--backend",
          "cpp",
          "-O2",
          "--lto",
          "-j",
          "0",
          "--top",
          "Test",
          "--cxx",
          wrapper.string(),
          "--cache-dir",
          (*tmp_or / "cache").string(),
          "-o",
          program.string(),
          src.string()};
      const auto built = RunChildProcess(lyra, build);
      ASSERT_THAT(built, ExitedWithCode(0)) << built.stderr_text;
      break;
    }
  }
  EXPECT_TRUE(std::filesystem::exists(attempted_marker))
      << "the build never attempted link-time optimization";
  EXPECT_EQ(std::filesystem::exists(plain_marker), watching.refuses)
      << "the build's fallback to the plain build";
  const auto ran = RunChildProcess(program, {});
  EXPECT_NE(ran.stdout_text.find("ran 42"), std::string::npos)
      << "stdout: " << ran.stdout_text;
}

INSTANTIATE_TEST_SUITE_P(
    LyraEmittedProject, LinkTimeOptimizationRefused,
    testing::Combine(
        testing::Values(
            HostCompiler{.name = "clang", .executable = "clang++"},
            HostCompiler{.name = "gcc", .executable = "g++"}),
        testing::Range<std::size_t>(0, kLinkTimeRefusals.size()),
        testing::Values(Builder::kRecipe, Builder::kLyra)),
    LinkTimeRefusalName);

// A project built to check its memory compiles the design and the runtime with
// its own compiler's sanitizer, from its recipe and from the build Lyra runs
// itself, and the program ends at the first read of storage that has ended,
// with a report naming where it was freed: here the runtime copying text a
// foreign function freed. Before that the design waits under a foreign call,
// which runs on a stack of its own, so a switch the check mistook for a fault
// would be the report printed instead. The execution backend's own check is
// held to the same program.
TEST(
    LyraEmittedProject,
    AProgramBuiltToCheckItsMemoryReportsAReadOfFreedStorage) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();
  auto sh_or = lyra::driver::FindOnPath("sh");
  ASSERT_TRUE(sh_or.has_value()) << sh_or.error().Text();

  auto tmp_or = MakeScratchDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();
  const auto src = *tmp_or / "test.sv";
  std::ofstream(src) << "module Test;\n"
                     << "  import \"DPI-C\" context task pause();\n"
                     << "  import \"DPI-C\" function string gone();\n"
                     << "  export \"DPI-C\" task wait_a_step;\n"
                     << "  task wait_a_step();\n"
                     << "    #1;\n"
                     << "  endtask\n"
                     << "  initial begin\n"
                     << "    string text;\n"
                     << "    pause();\n"
                     << "    text = gone();\n"
                     << "    $display(\"read %0d characters\", text.len());\n"
                     << "  end\n"
                     << "endmodule\n";
  const auto foreign = *tmp_or / "foreign.c";
  std::ofstream(foreign)
      << "#include <stdlib.h>\n"
      << "#include <string.h>\n"
      << "#include \"dpi.h\"\n"
      << "int pause(void) {\n"
      << "  return wait_a_step();\n"
      << "}\n"
      << "const char* gone(void) {\n"
      << "  char* text = malloc(64);\n"
      << "  strcpy(text, \"text the foreign side has already freed\");\n"
      << "  free(text);\n"
      << "  return text;\n"
      << "}\n";
  const auto reports_the_read =
      [](const conformance::framework::ProcessOutcome& ran) {
        return testing::Matches(ExitedWithCode(Ne(0)))(ran) &&
               ran.stderr_text.find(
                   "ERROR: AddressSanitizer: heap-use-after-free") !=
                   std::string::npos &&
               ran.stderr_text.find("freed by thread") != std::string::npos;
      };

  const auto out_dir = *tmp_or / "out";
  const std::vector<std::string> emit = {
      "emit",      "cpp", "--sanitize",     "address",    "--top",
      "Test",      "-o",  out_dir.string(), "--dpi-link", foreign.string(),
      src.string()};
  const auto emitted = RunChildProcess(lyra, emit);
  ASSERT_THAT(emitted, ExitedWithCode(0)) << emitted.stderr_text;
  const std::vector<std::string> recipe = {
      "-c", "cd '" + out_dir.string() + "' && sh build.sh -j 0"};
  const auto built = RunChildProcess(*sh_or, recipe);
  ASSERT_THAT(built, ExitedWithCode(0)) << built.stderr_text;
  const auto ran = RunChildProcess(out_dir / "program", {});
  EXPECT_TRUE(reports_the_read(ran)) << "recipe: " << ran.stderr_text;

  const auto program = *tmp_or / "program";
  const std::vector<std::string> build = {
      "build",
      "--backend",
      "cpp",
      "--sanitize",
      "address",
      "-j",
      "0",
      "--top",
      "Test",
      "--cache-dir",
      (*tmp_or / "cache").string(),
      "-o",
      program.string(),
      "--dpi-link",
      foreign.string(),
      src.string()};
  const auto in_process = RunChildProcess(lyra, build);
  ASSERT_THAT(in_process, ExitedWithCode(0)) << in_process.stderr_text;
  const auto ran_in_process = RunChildProcess(program, {});
  EXPECT_TRUE(reports_the_read(ran_in_process))
      << "in-process: " << ran_in_process.stderr_text;
}

// Nothing compiled in advance decides whether a build succeeds, at either thing
// that builds a design. The project's recipe prepares a header, and is handed
// it back after every header's timestamp has moved under it, which is what a
// checkout leaves, and what a compiler judging by timestamps would refuse. The
// in-process build is handed back a prepared header that is not one at all.
// Both still owe a program. The timestamps are moved here by hand because Lyra
// no longer moves them itself and a checkout is not something a test can stage.
TEST(LyraEmittedProject, BuildsEvenWhenThePrecompiledHeaderIsRefused) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();

  auto tmp_or = MakeScratchDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();
  const auto src = *tmp_or / "test.sv";
  WriteTrivialSource(src);
  const auto out_dir = *tmp_or / "out";

  const std::vector<std::string> emit = {
      "emit", "cpp", "--top", "Test", "-o", out_dir.string(), src.string()};
  const auto emitted = RunChildProcess(lyra, emit);
  ASSERT_THAT(emitted, ExitedWithCode(0)) << emitted.stderr_text;
  auto sh_or = lyra::driver::FindOnPath("sh");
  ASSERT_TRUE(sh_or.has_value()) << sh_or.error().Text();
  const std::vector<std::string> recipe = {
      "-c", "cd '" + out_dir.string() + "' && sh build.sh"};
  const auto first = RunChildProcess(*sh_or, recipe);
  ASSERT_THAT(first, ExitedWithCode(0)) << first.stderr_text;

  const auto headers = out_dir / "runtime" / "include";
  ASSERT_TRUE(std::filesystem::exists(headers)) << headers.string();
  std::size_t moved = 0;
  for (const auto& entry :
       std::filesystem::recursive_directory_iterator(headers)) {
    if (!entry.is_regular_file()) continue;
    std::filesystem::last_write_time(
        entry.path(), std::filesystem::last_write_time(entry.path()) + 24h);
    ++moved;
  }
  ASSERT_GT(moved, 0U) << "no header to move under " << headers.string();

  const auto second = RunChildProcess(*sh_or, recipe);
  EXPECT_THAT(second, ExitedWithCode(0))
      << "the recipe was failed by the header it had prepared for itself:\n"
      << second.stderr_text;
  const auto ran = RunChildProcess(out_dir / "program", {});
  EXPECT_NE(ran.stdout_text.find("ran 42"), std::string::npos)
      << "stdout: " << ran.stdout_text;

  // A store of this test's own, so what the second build is handed is what the
  // first one left rather than whatever the developer's store happens to hold.
  // The second build is of another design, so it compiles rather than reusing
  // the program the first one kept, while the header it is handed is the one
  // the first prepared.
  const auto store = *tmp_or / "store";
  const auto program = *tmp_or / "program";
  const auto build = [&](const std::filesystem::path& design) {
    return std::vector<std::string>{
        "build",          "--top",       "Test",         "-o",
        program.string(), "--cache-dir", store.string(), design.string()};
  };
  const auto prepared = RunChildProcess(lyra, build(src));
  ASSERT_THAT(prepared, ExitedWithCode(0)) << prepared.stderr_text;
  // A kept result is named by its digest alone, so the prepared header is
  // found by what the compiler writes at the start of one: clang's mark, or
  // GCC's.
  constexpr std::array<std::string_view, 2> kPreparedHeaderMagic = {
      "CPCH", "gpch"};
  std::size_t spoiled = 0;
  for (const auto& entry : std::filesystem::directory_iterator(store / "cas")) {
    std::string magic(kPreparedHeaderMagic.front().size(), '\0');
    std::ifstream(entry.path(), std::ios::binary)
        .read(magic.data(), static_cast<std::streamsize>(magic.size()));
    if (std::ranges::contains(kPreparedHeaderMagic, magic)) {
      std::ofstream(entry.path(), std::ios::trunc) << "not a prepared header";
      ++spoiled;
    }
  }
  ASSERT_GT(spoiled, 0U) << "the build prepared no header to spoil";

  const auto other = *tmp_or / "other.sv";
  std::ofstream(other) << "module Test;\n"
                       << "  initial $display(\"ran %0d again\", 6 * 7);\n"
                       << "endmodule\n";
  const auto rebuilt = RunChildProcess(lyra, build(other));
  EXPECT_THAT(rebuilt, ExitedWithCode(0))
      << "a build was failed by the header it was handed:\n"
      << rebuilt.stderr_text;
  const auto run = RunChildProcess(program, {});
  EXPECT_THAT(run, ExitedWithCode(0)) << run.stderr_text;
  EXPECT_NE(run.stdout_text.find("ran 42"), std::string::npos)
      << "stdout: " << run.stdout_text;
}

// How many units are compiled at once is the caller's to say, at both things
// that build a design, and it changes nothing about the program produced. What
// a width buys is measured elsewhere; asserting on it here would be asserting
// on the machine the test happens to run on.
TEST(LyraEmittedProject, TakesTheWidthItIsGiven) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();

  auto tmp_or = MakeScratchDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();
  const auto src = *tmp_or / "test.sv";
  WriteTrivialSource(src);
  const auto out_dir = *tmp_or / "out";
  const auto program = out_dir / "program";

  const auto built_program = *tmp_or / "built";
  const std::vector<std::string> build = {
      "build",
      "--top",
      "Test",
      "-j",
      "4",
      "-o",
      built_program.string(),
      "--cache-dir",
      (*tmp_or / "store").string(),
      src.string()};
  const auto built = RunChildProcess(lyra, build);
  ASSERT_THAT(built, ExitedWithCode(0)) << built.stderr_text;
  const auto ran = RunChildProcess(built_program, {});
  EXPECT_NE(ran.stdout_text.find("ran 42"), std::string::npos)
      << "stdout: " << ran.stdout_text;

  const std::vector<std::string> emit = {
      "emit", "cpp", "--top", "Test", "-o", out_dir.string(), src.string()};
  const auto emitted = RunChildProcess(lyra, emit);
  ASSERT_THAT(emitted, ExitedWithCode(0)) << emitted.stderr_text;
  auto sh_or = lyra::driver::FindOnPath("sh");
  ASSERT_TRUE(sh_or.has_value()) << sh_or.error().Text();
  // Zero asks the recipe for one compile per processor, which is the spelling
  // a caller uses to say the machine is its own.
  for (const std::string_view width : {"1", "0"}) {
    std::filesystem::remove(program);
    const std::vector<std::string> rebuild = {
        "-c",
        std::format("cd '{}' && sh build.sh -j {}", out_dir.string(), width)};
    const auto built = RunChildProcess(*sh_or, rebuild);
    ASSERT_THAT(built, ExitedWithCode(0)) << width << ": " << built.stderr_text;

    const auto run = RunChildProcess(program, {});
    EXPECT_THAT(run, ExitedWithCode(0)) << width << ": " << run.stderr_text;
    EXPECT_NE(run.stdout_text.find("ran 42"), std::string::npos)
        << width << " stdout: " << run.stdout_text;
  }
}

// Emitting and building twice into one directory, with nothing about the design
// changed. The second emit rewrites the same runtime headers, and clang checks
// the precompiled header the recipe kept against them as it loads it -- so a
// generator that rewrote an unchanged file, under a check reading timestamps,
// would make the build reject a header it had just produced.
TEST(LyraEmittedProject, BuildsTwiceIntoOneDirectory) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();

  auto tmp_or = MakeScratchDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();
  const auto src = *tmp_or / "test.sv";
  WriteTrivialSource(src);
  const auto out_dir = *tmp_or / "out";
  const auto program = out_dir / "program";

  auto sh_or = lyra::driver::FindOnPath("sh");
  ASSERT_TRUE(sh_or.has_value()) << sh_or.error().Text();
  const std::vector<std::string> emit = {
      "emit", "cpp", "--top", "Test", "-o", out_dir.string(), src.string()};
  const std::vector<std::string> recipe = {
      "-c", "cd '" + out_dir.string() + "' && sh build.sh"};
  for (const std::string_view pass : {"first", "second"}) {
    const auto emitted = RunChildProcess(lyra, emit);
    ASSERT_THAT(emitted, ExitedWithCode(0))
        << pass << ": " << emitted.stderr_text;
    const auto built = RunChildProcess(*sh_or, recipe);
    ASSERT_THAT(built, ExitedWithCode(0)) << pass << ": " << built.stderr_text;

    const auto run = RunChildProcess(program, {});
    EXPECT_THAT(run, ExitedWithCode(0)) << pass << ": " << run.stderr_text;
    EXPECT_NE(run.stdout_text.find("ran 42"), std::string::npos)
        << pass << " stdout: " << run.stdout_text;
  }
}

TEST(LyraEmittedProject, PortableProjectBuildsItsDpiSources) {
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
  const auto emit = RunChildProcess(lyra, args);
  ASSERT_THAT(emit, ExitedWithCode(0)) << emit.stdout_text << emit.stderr_text;

  // The emitted directory carries the whole foreign boundary: the generated
  // prototypes, the standard header they are spelled in, and a copy of the
  // user's source, so it builds where the originals are not reachable.
  EXPECT_TRUE(std::filesystem::exists(out_dir / "dpi.h"));
  EXPECT_TRUE(std::filesystem::exists(out_dir / "svdpi.h"));
  ASSERT_TRUE(std::filesystem::exists(out_dir / "dpi/foreign.c"));
  std::filesystem::remove(foreign);

  auto sh_or = lyra::driver::FindOnPath("sh");
  ASSERT_TRUE(sh_or.has_value()) << sh_or.error().Text();
  const std::vector<std::string> build = {
      "-c", "cd '" + out_dir.string() + "' && sh build.sh"};
  const auto built = RunChildProcess(*sh_or, build);
  ASSERT_THAT(built, ExitedWithCode(0))
      << built.stdout_text << built.stderr_text;

  const auto run = RunChildProcess(out_dir / "program", {});
  EXPECT_THAT(run, ExitedWithCode(0)) << run.stderr_text;
  EXPECT_NE(run.stdout_text.find("dpi 21"), std::string::npos)
      << "stdout: " << run.stdout_text;
}

}  // namespace
