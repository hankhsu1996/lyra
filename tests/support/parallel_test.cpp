#include "lyra/support/parallel.hpp"

#include <atomic>
#include <cstddef>
#include <gtest/gtest.h>
#include <thread>
#include <vector>

namespace {

using lyra::support::ProduceInOrder;

// Results are consumed in index order whatever order they are produced in. The
// first index waits until every other has been produced, so each later one
// finishes before it, and the order consumed is still the order of the
// indices. Nothing is timed, so the order the results finish in is forced
// rather than likely.
TEST(ProduceInOrder, ConsumesInIndexOrderWhateverFinishesFirst) {
  constexpr std::size_t kCount = 8;
  std::atomic<std::size_t> finished = 0;
  std::vector<std::size_t> consumed;
  ProduceInOrder(
      kCount, kCount,
      [&](std::size_t i) {
        if (i == 0) {
          while (finished.load() < kCount - 1) {
            std::this_thread::yield();
          }
        }
        ++finished;
        return i;
      },
      [&](std::size_t i) { consumed.push_back(i); });

  std::vector<std::size_t> expected;
  for (std::size_t i = 0; i < kCount; ++i) {
    expected.push_back(i);
  }
  EXPECT_EQ(consumed, expected);
}

}  // namespace
