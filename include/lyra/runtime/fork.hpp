#pragma once

#include <array>
#include <cstddef>
#include <span>

#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/wait.hpp"

namespace lyra::runtime {

// LRM 9.3.2 Table 9-1 dispatch, over the branches a fork states. `ForkWaitAll`
// (`join`) resumes the parent after every branch terminates; `ForkWaitFirst`
// (`join_any`) after the first; `SpawnAll` (`join_none`) returns void so the
// parent never waits at all. Branch ordering falls out of the engine's
// snapshot-drain -- a branch enqueued while the parent runs is reached only on
// the next drain pass, after the parent has parked (for `ForkWaitAll` /
// `ForkWaitFirst`) or moved on (for `SpawnAll`).
//
// The two that wait spawn the branches and answer the parent's wait for its
// join condition, which the parent then stops at.
auto ForkWaitAll(RuntimeEffects& runtime, std::span<Coroutine<void>> branches)
    -> Wait;

auto ForkWaitFirst(RuntimeEffects& runtime, std::span<Coroutine<void>> branches)
    -> Wait;

void SpawnAll(RuntimeEffects& runtime, std::span<Coroutine<void>> branches);

// The same three where the branch count is a constant of the call. How many
// branches a fork has is what the design wrote, so the count is all these
// state; the operation itself is one of the three above.
template <std::size_t N>
auto ForkWaitAll(
    RuntimeEffects& runtime, std::array<Coroutine<void>, N> branches) -> Wait {
  return ForkWaitAll(runtime, std::span<Coroutine<void>>{branches});
}

template <std::size_t N>
auto ForkWaitFirst(
    RuntimeEffects& runtime, std::array<Coroutine<void>, N> branches) -> Wait {
  return ForkWaitFirst(runtime, std::span<Coroutine<void>>{branches});
}

template <std::size_t N>
void SpawnAll(
    RuntimeEffects& runtime, std::array<Coroutine<void>, N> branches) {
  SpawnAll(runtime, std::span<Coroutine<void>>{branches});
}

// LRM 9.6.1 `wait fork`: wait until every immediate child the executing process
// spawned has terminated. The condition is the executing process's; the frame
// parked on it is the one that ran `wait fork` (the task frame when `wait fork`
// sits in a task).
auto WaitFork(RuntimeEffects& runtime) -> Wait;

// LRM 9.6.3 `disable fork`: terminate every descendant of the executing
// process. The caller does not block -- the next statement runs at the same
// simulation time -- so it answers nothing about giving up control. Like
// `wait fork`, it reads the executing process (LRM 9.5), so a `disable fork`
// inside a task reaches the descendants the enclosing process owns.
void DisableFork(RuntimeEffects& runtime);

}  // namespace lyra::runtime
