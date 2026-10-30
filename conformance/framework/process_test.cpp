// How a test judges a child it ran: a child that did not exit by itself never
// reads as one that succeeded, whatever ended it.

#include "conformance/framework/process.hpp"

#include <chrono>
#include <cstdlib>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <string>
#include <variant>

#include "conformance/framework/process_matchers.hpp"

namespace {

using conformance::framework::Describe;
using conformance::framework::ExitedWithCode;
using conformance::framework::RunShellScript;
using conformance::framework::Signalled;
using conformance::framework::TestTimedOut;
using conformance::framework::TimedOut;
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

}  // namespace
