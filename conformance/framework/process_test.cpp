// How a test judges a child it ran: a child that did not exit by itself never
// reads as one that succeeded, whatever ended it. And what a child is given to
// run in: the time left of its case, and a directory of its own.

#include "conformance/framework/process.hpp"

#include <chrono>
#include <cstdlib>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <optional>
#include <string>
#include <variant>

#include "conformance/framework/process_matchers.hpp"

namespace {

using conformance::framework::Describe;
using conformance::framework::ExitedWithCode;
using conformance::framework::MakeScratchDir;
using conformance::framework::RunShellScript;
using conformance::framework::Signalled;
using conformance::framework::TestTimedOut;
using conformance::framework::TimedOut;
using conformance::framework::TimeLeft;
using testing::Not;
using namespace std::chrono_literals;

TEST(ChildProcess, AnExitIsJudgedByItsCode) {
  const auto ran = RunShellScript("exit 3");
  EXPECT_THAT(ran, ExitedWithCode(3));
  EXPECT_THAT(ran, Not(ExitedWithCode(0)));
}

TEST(ChildProcess, ASignalIsNoExit) {
  const auto ran = RunShellScript("kill -KILL $$");
  EXPECT_TRUE(std::holds_alternative<Signalled>(ran.end)) << Describe(ran.end);
  EXPECT_THAT(ran, Not(ExitedWithCode(0)));
}

// Ending a child for its time asks it first, and a child that then exits 0 is
// still one that ran out of time.
TEST(ChildProcess, RunningOutOfTimeIsNoExit) {
  const auto ran =
      RunShellScript("trap 'exit 0' TERM; sleep 30 & wait", {}, 1s);
  EXPECT_TRUE(std::holds_alternative<TimedOut>(ran.end)) << Describe(ran.end);
  EXPECT_THAT(ran, Not(ExitedWithCode(0)));
}

// A child given no time of its own is ended before the runner would end the
// test, so the test is the one that reports it, and the ending says it was the
// test's time that ran out rather than the child's.
TEST(ChildProcess, AChildEndsBeforeItsTest) {
  const char* stated = std::getenv("TEST_TIMEOUT");
  const std::string kept = stated == nullptr ? "" : stated;
  ASSERT_EQ(setenv("TEST_TIMEOUT", "16", 1), 0);
  const auto ran = RunShellScript("sleep 30");
  if (stated == nullptr) {
    unsetenv("TEST_TIMEOUT");
  } else {
    setenv("TEST_TIMEOUT", kept.c_str(), 1);
  }
  EXPECT_TRUE(std::holds_alternative<TestTimedOut>(ran.end))
      << Describe(ran.end);
}

// A case given a time in all hands each command what is left of it, so a
// command started once the time is spent is ended at once, and a case given
// no time hands none on.
TEST(ChildProcess, ACommandIsGivenWhatIsLeftOfItsCasesTime) {
  EXPECT_EQ(TimeLeft(std::nullopt), std::nullopt);
  const auto spent = std::chrono::steady_clock::now() - 1s;
  EXPECT_EQ(TimeLeft(spent), 0s);
  const auto ran = RunShellScript("sleep 30", {}, TimeLeft(spent));
  EXPECT_TRUE(std::holds_alternative<TimedOut>(ran.end)) << Describe(ran.end);
  EXPECT_EQ(TimeLeft(std::chrono::steady_clock::now() + 90s), 90s);
}

// A machine with no directory for temporary files is a setup that failed,
// said as such, and never an exception escaping the run.
TEST(ScratchDirectory, IsRefusedWhereNoTemporaryDirectoryExists) {
  const char* stated = std::getenv("TMPDIR");
  const std::string kept = stated == nullptr ? "" : stated;
  ASSERT_EQ(setenv("TMPDIR", "/nonexistent/conformance-test", 1), 0);
  const auto made = MakeScratchDir();
  if (stated == nullptr) {
    unsetenv("TMPDIR");
  } else {
    setenv("TMPDIR", kept.c_str(), 1);
  }
  EXPECT_FALSE(made.has_value());
}

}  // namespace
