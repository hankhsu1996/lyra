#include "lyra/driver/action_cache.hpp"

#include <chrono>
#include <filesystem>
#include <fstream>
#include <future>
#include <gtest/gtest.h>
#include <memory>
#include <optional>
#include <thread>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"

namespace {

using lyra::driver::Action;
using lyra::driver::ActionCache;
using lyra::driver::ActionResult;

// Two threads of one build ask for the same action, and the step of the one
// performing it throws. The throw reaches that thread, and the other, which
// was waiting on what the action came to, performs it itself rather than
// waiting on a result nobody is going to produce. The waiting thread is given
// a bounded time to finish, so a regression fails here instead of hanging the
// suite; the short pause before the throw decides only whether the second
// thread is already waiting when it comes, never whether it may finish.
TEST(ActionCache, AStepThatThrowsReleasesWhoeverWaitsOnIt) {
  const auto scratch =
      std::filesystem::path(::testing::TempDir()) / "action_cache_throw";
  std::filesystem::remove_all(scratch);
  // Shared with the second thread, which outlives this test if it never
  // returns.
  const auto cache = std::make_shared<ActionCache>(std::nullopt, scratch, true);
  const Action action("throw once");

  std::promise<void> claimed;
  std::promise<void> asked;
  std::promise<lyra::diag::Result<ActionResult>> answered;
  auto answer = answered.get_future();
  std::thread second([cache, action,
                      claimed_once = claimed.get_future().share(), &asked,
                      answered = std::move(answered)]() mutable {
    claimed_once.wait();
    asked.set_value();
    answered.set_value(
        cache->Execute(action, [](const std::filesystem::path& output) {
          std::ofstream(output) << "performed by the second thread\n";
          return lyra::diag::Result<void>{};
        }));
  });

  bool thrown = false;
  try {
    const auto answered_first = cache->Execute(
        action, [&](const std::filesystem::path&) -> lyra::diag::Result<void> {
          claimed.set_value();
          asked.get_future().wait();
          std::this_thread::sleep_for(std::chrono::milliseconds(50));
          throw lyra::InternalError("the step failed");
        });
    ADD_FAILURE() << "the thread performing the action was answered ("
                  << answered_first.has_value()
                  << ") rather than reached by its step's throw";
  } catch (const lyra::InternalError&) {
    thrown = true;
  }
  EXPECT_TRUE(thrown);

  if (answer.wait_for(std::chrono::seconds(10)) != std::future_status::ready) {
    second.detach();
    FAIL() << "the thread waiting on the action never returned";
  }
  second.join();
  const auto result = answer.get();
  ASSERT_TRUE(result.has_value()) << result.error().primary.message;
  EXPECT_TRUE(result->performed)
      << "the waiting thread was handed a result nobody produced";
}

}  // namespace
