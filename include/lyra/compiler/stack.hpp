#pragma once

#include <cstddef>
#include <functional>
#include <future>
#include <pthread.h>
#include <vector>

namespace lyra::compiler {

// The front end and every lowering walk an expression by recursing into its
// operands, and a run of one binary operator is as deep as it is long, so the
// stack a thread compiles on bounds how long a run a design may write. Every
// thread that compiles is one of these, given the same amount, enough for a run
// of ten thousand operands, or as much of it as whoever started the process
// allows a stack at all, and one that runs out says so before the process
// ends. The thread a process starts on is not compiled on: its stack is as
// large as whoever started the process allowed, which is theirs to say and what
// a program this process starts is given too.

// A thread with that much stack to compile on. It runs `work` once and is
// joined before it goes away.
class StackThread {
 public:
  explicit StackThread(std::function<void()> work);
  StackThread(const StackThread&) = delete;
  auto operator=(const StackThread&) -> StackThread& = delete;
  StackThread(StackThread&&) = delete;
  auto operator=(StackThread&&) -> StackThread& = delete;
  ~StackThread();

  // Returns once `work` has. What it threw is rethrown here.
  void Join();

 private:
  static auto Run(void* self) -> void*;

  std::packaged_task<void()> work_;
  std::future<void> done_;
  // What the thread handles a stack overflow on, which cannot be the stack.
  std::vector<std::byte> alt_stack_;
  pthread_t thread_{};
  bool joined_ = false;
};

}  // namespace lyra::compiler
