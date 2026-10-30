#include <algorithm>
#include <chrono>
#include <filesystem>
#include <format>
#include <fstream>
#include <gtest/gtest.h>
#include <iterator>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "lyra/driver/action_cache.hpp"
#include "lyra/driver/cpp_build.hpp"
#include "lyra/driver/pch.hpp"
#include "lyra/driver/project_layout.hpp"
#include "lyra/driver/runtime_export.hpp"
#include "lyra/driver/subprocess.hpp"
#include "lyra/support/runtime_prelude.hpp"
#include "lyra/support/sanitizer.hpp"
#include "tests/framework/cli_fixture.hpp"

namespace {

auto ReadWhole(const std::filesystem::path& p) -> std::string {
  std::ifstream in(p, std::ios::binary);
  return std::string{
      std::istreambuf_iterator<char>(in), std::istreambuf_iterator<char>()};
}

auto WriteWhole(const std::filesystem::path& p, std::string_view content)
    -> void {
  std::filesystem::create_directories(p.parent_path());
  std::ofstream out(p, std::ios::binary | std::ios::trunc);
  out << content;
}

// Move a file's timestamp far enough back that no clock granularity can hide
// the difference, leaving its bytes untouched.
auto AgeByADay(const std::filesystem::path& p) -> void {
  std::filesystem::last_write_time(
      p, std::filesystem::last_write_time(p) - std::chrono::hours(24));
}

// A compiler a build may meet: what a case is called under it, and the name
// it is found by. Each case runs under every one this host has, since what a
// compiler checks of a prepared header, and which flags it takes, are its own.
struct HostCompiler {
  std::string_view name;
  std::string_view executable;
};

class PreparedHeader : public testing::TestWithParam<HostCompiler> {
 protected:
  void SetUp() override {
    auto found = lyra::driver::FindOnPath(GetParam().executable);
    if (!found) {
      GTEST_SKIP() << GetParam().executable << " is not on the PATH";
    }
    cxx_ = *std::move(found);
  }

  std::filesystem::path cxx_;
};

}  // namespace

// A prepared header is named by every file it was made from, wherever that file
// lies, and by nothing else -- not every compiler checks those files when it
// loads one, so a name that left one out would hand a stale header to a
// compiler that takes it. The file this test rewrites lies outside the runtime
// tree, where the C++ library and the system's headers lie: rewriting it byte
// for byte under an older timestamp keeps the name, and the compiler still
// takes the header; changing its bytes and keeping their count gives another.
// Whether a compile took the prepared header is read off the header it is told
// to include, which here holds an error: only a compile that read the prepared
// one instead gets past it.
//
// The same list names every compile of emitted code, which holds only while
// what an emitted file includes of the runtime is the prelude or something it
// reaches; the program's entry includes a header of its own, so that header has
// to be one the prelude reads.
TEST_P(PreparedHeader, IsNamedByEveryFileItReads) {
  const auto lyra_exe = lyra::test::ResolveLyra();
  ASSERT_FALSE(lyra_exe.empty());
  auto loc_or = lyra::driver::ResolveRuntimeLocation(lyra_exe.string());
  ASSERT_TRUE(loc_or) << loc_or.error().primary.message;

  // A project of this test's own, so the headers it rewrites are copies and the
  // store it fills is not the one the developer is using.
  auto project_or = conformance::framework::MakeScratchDir();
  ASSERT_TRUE(project_or) << project_or.error();
  const auto& project = *project_or;
  auto exported = lyra::driver::ExportRuntimeTree(
      *loc_or, project, lyra::driver::LinkTimeOptimization::kNone,
      lyra::support::Sanitizer::kNone);
  ASSERT_TRUE(exported) << exported.error().primary.message;

  const auto include_root = project / lyra::driver::kRuntimeIncludeDir;
  const auto prelude = include_root / lyra::support::kRuntimePreludeHeader;
  const auto optimization = lyra::driver::OptimizationLevel::kO0;
  const auto sanitizer = lyra::support::Sanitizer::kNone;

  std::vector<std::string> listing =
      lyra::driver::DesignCompileFlags(optimization, sanitizer);
  listing.insert(
      listing.end(),
      {"-I", include_root.string(), "-M", "-xc++-header", prelude.string()});
  const auto listed = lyra::driver::RunProcessCaptured(cxx_, listing);
  ASSERT_TRUE(listed) << listed.error().Text();
  EXPECT_NE(
      listed->stdout_text.find(lyra::support::kHostEntryHeader),
      std::string::npos)
      << "the program entry's header is not one the prelude reads";

  const auto outside = project / "elsewhere" / "outside.hpp";
  WriteWhole(outside, "#pragma once\n// alpha\n");
  WriteWhole(
      prelude,
      ReadWhole(prelude) + std::format("#include \"{}\"\n", outside.string()));

  const auto build_dir = project / "build";
  // Each asks as a build of its own would, of one store.
  const auto offer = [&]() -> std::optional<lyra::driver::pch::Offer> {
    lyra::driver::ActionCache actions(
        project / "store", project / "scratch", true);
    auto inputs = lyra::driver::DigestOfPreludeInputs(
        cxx_, include_root, optimization, sanitizer);
    if (!inputs) {
      ADD_FAILURE() << inputs.error().primary.message;
      return std::nullopt;
    }
    return lyra::driver::pch::OfferPrelude(
        cxx_, include_root, *inputs, lyra::driver::pch::Policy::kAttempt,
        actions, optimization, sanitizer, build_dir);
  };

  const auto unit = project / "unit.cpp";
  WriteWhole(
      unit, std::format(
                "#include \"{}\"\nint main() {{ return 0; }}\n",
                lyra::support::kRuntimePreludeHeader));
  const auto takes = [&](const lyra::driver::pch::Offer& offered) {
    WriteWhole(
        build_dir / lyra::driver::kPreludeOfferHeader,
        "#error the prepared header was not taken\n");
    std::vector<std::string> args =
        lyra::driver::DesignCompileFlags(optimization, sanitizer);
    args.insert(args.end(), {"-I", include_root.string()});
    args.insert(args.end(), offered.flags.begin(), offered.flags.end());
    args.insert(
        args.end(), {"-c", unit.string(), "-o", (project / "unit.o").string()});
    auto compiled = lyra::driver::RunProcessCaptured(cxx_, args);
    ASSERT_TRUE(compiled) << compiled.error().Text();
    EXPECT_EQ(compiled->exit_code, 0) << compiled->stderr_text;
  };

  // Whether a header was prepared for the asking or found kept is what says
  // which name it was asked for under; the bytes cannot, since two headers made
  // from files differing only in a comment may come out the same.
  const auto first = offer();
  ASSERT_TRUE(first.has_value()) << "no header was prepared";
  takes(*first);

  WriteWhole(outside, ReadWhole(outside));
  AgeByADay(outside);
  const auto rewritten = offer();
  ASSERT_TRUE(rewritten.has_value());
  EXPECT_FALSE(rewritten->prepared.performed)
      << "the same bytes were given another prepared header";
  takes(*rewritten);

  WriteWhole(outside, "#pragma once\n// omega\n");
  const auto changed = offer();
  ASSERT_TRUE(changed.has_value());
  EXPECT_TRUE(changed->prepared.performed)
      << "a header made from other bytes was found under the old name";
}

// Which flags a prepared header is made with is asked of the compiler in hand,
// one flag at a time, and a flag it warns of is not taken: a compiler may
// accept a flag it ignores, and only what it printed says so. A compiler that
// cannot prepare a header at all is offered none. Both are wrappers of the
// compiler under test, one that warns of templates instantiated into a
// prepared header and one that refuses to prepare any header.
TEST_P(PreparedHeader, TakesEachFlagTheCompilerTakesAlone) {
  auto dir_or = conformance::framework::MakeScratchDir();
  ASSERT_TRUE(dir_or) << dir_or.error();
  const auto& dir = *dir_or;
  const auto wrap = [&](std::string_view name, std::string_view watched,
                        std::string_view does) {
    const auto wrapper = dir / name;
    WriteWhole(
        wrapper, std::format(
                     "#!/bin/sh\ncase \" $* \" in {}) {} ;; esac\n"
                     "exec '{}' \"$@\"\n",
                     watched, does, cxx_.string()));
    std::filesystem::permissions(
        wrapper, std::filesystem::perms::owner_all,
        std::filesystem::perm_options::add);
    return wrapper;
  };
  lyra::driver::ActionCache actions(dir / "store", dir / "scratch", true);

  const auto warns = wrap(
      "warns",
      std::format("*\" {} \"*", lyra::driver::pch::kInstantiateTemplatesFlag),
      "echo 'warning: argument unused during compilation' >&2");
  const auto accepted =
      lyra::driver::pch::AcceptedPreparedHeaderFlags(warns, actions);
  ASSERT_TRUE(accepted.has_value())
      << "a compiler that prepares headers was said not to";
  EXPECT_FALSE(
      std::ranges::contains(
          accepted->preparing,
          std::string(lyra::driver::pch::kInstantiateTemplatesFlag)))
      << "a flag the compiler warned of was taken";

  const auto refuses = wrap("refuses", "*\" -xc++-header \"*", "exit 1");
  EXPECT_FALSE(
      lyra::driver::pch::AcceptedPreparedHeaderFlags(refuses, actions)
          .has_value())
      << "a compiler that prepares no header was said to";
}

INSTANTIATE_TEST_SUITE_P(
    EachCompiler, PreparedHeader,
    testing::Values(
        HostCompiler{.name = "clang", .executable = "clang++"},
        HostCompiler{.name = "gcc", .executable = "g++"}),
    [](const testing::TestParamInfo<HostCompiler>& info) {
      return std::string(info.param.name);
    });
