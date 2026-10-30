// What a reader is handed to run. Every directory under `examples/` is run the
// way its README says: from that directory, by a command that names nothing, so
// the declaration beside the sources is read by the compiler and by nothing
// else, and a file the design opens by a relative path is found where its
// author put it.
//
// How the run ended is the whole verdict and nothing it printed is read, so an
// example that checks something has to end in a failure when the check does.
//
// The command a reader types builds on the default backend, so each run here
// builds an emitted C++ project and runs the program it makes.

#include <cstddef>
#include <filesystem>
#include <format>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <string>

#include "conformance/framework/process.hpp"
#include "conformance/framework/process_matchers.hpp"
#include "tests/framework/cli_fixture.hpp"

namespace {

using conformance::framework::ExitedWithCode;
using conformance::framework::MakeScratchDir;
using lyra::test::ResolveLyra;
using lyra::test::ResolveShippedExamples;
using lyra::test::RunLyraFrom;

TEST(ShippedExamples, EachRunsFromItsOwnDirectoryWithNothingNamed) {
  const auto lyra = ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();
  const auto examples = ResolveShippedExamples();
  ASSERT_TRUE(std::filesystem::is_directory(examples)) << examples.string();
  auto tmp_or = MakeScratchDir();
  ASSERT_TRUE(tmp_or.has_value()) << tmp_or.error();
  const std::string command =
      std::format("run --cache-dir '{}'", (*tmp_or / "cache").string());

  // Every directory is an example, so one added is run without being named
  // anywhere.
  std::size_t ran = 0;
  for (const auto& entry : std::filesystem::directory_iterator(examples)) {
    if (!entry.is_directory()) continue;
    const std::string name = entry.path().filename().string();
    const auto run = RunLyraFrom(lyra, entry.path(), command);
    EXPECT_THAT(run, ExitedWithCode(0))
        << name << ": " << run.stdout_text << run.stderr_text;
    ++ran;
  }
  EXPECT_GT(ran, 0U) << "no example under " << examples.string();
}

}  // namespace
