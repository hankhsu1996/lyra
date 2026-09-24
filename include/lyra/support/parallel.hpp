#pragma once

#include <algorithm>
#include <atomic>
#include <cstddef>
#include <future>
#include <mutex>
#include <optional>
#include <type_traits>
#include <utility>
#include <vector>

namespace lyra::support {

// Calls `produce(i)` for every index below `count` on at most `width` threads
// at once, and hands each result to `consume` in index order, whichever
// finished first. Each thread takes the next index nobody has taken until none
// is left, so one slow index holds up only the thread working on it. A result
// is consumed as soon as every index before it has been, and released then, so
// what is held is the results waiting on a slower index.
//
// The calls to `produce` may run at the same time, so one must touch nothing
// another writes. `consume` is never called twice at once, and runs on
// whichever thread finished the result that let it proceed. Returns once every
// call has returned; an exception thrown by any of them is rethrown here, after
// the threads still working have finished.
template <typename Produce, typename Consume>
void ProduceInOrder(
    std::size_t count, std::size_t width, Produce produce, Consume consume) {
  using Produced = std::invoke_result_t<Produce, std::size_t>;
  std::vector<std::optional<Produced>> produced(count);
  std::mutex consuming;
  std::size_t consumed = 0;
  std::atomic<std::size_t> next = 0;
  const auto drain = [&] {
    for (std::size_t i = next++; i < count; i = next++) {
      Produced result = produce(i);
      const std::scoped_lock lock(consuming);
      produced[i].emplace(std::move(result));
      for (; consumed < count && produced[consumed].has_value(); ++consumed) {
        consume(*std::move(produced[consumed]));
        produced[consumed].reset();
      }
    }
  };
  std::vector<std::future<void>> threads;
  const std::size_t running = std::min(width, count);
  threads.reserve(running);
  for (std::size_t k = 0; k < running; ++k) {
    threads.push_back(std::async(std::launch::async, drain));
  }
  for (std::future<void>& thread : threads) {
    thread.get();
  }
}

}  // namespace lyra::support
