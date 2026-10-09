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
// thread that compiles is given the same amount, enough for a run of ten
// thousand operands, and one that runs out says so before the process ends.

// Gives the thread the process starts on that much stack, as far as whoever
// started the process allows. That thread's stack is supplied as it is used,
// up to the process's limit, so raising the limit costs nothing a compilation
// does not use.
void GiveStartingThreadCompileStack();

// A thread with that much stack. It runs `work` once and is joined before it
// goes away.
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
  // Where the thread is told its stack ran out, which cannot be the stack.
  std::vector<std::byte> signal_stack_;
  pthread_t thread_{};
  bool joined_ = false;
};

}  // namespace lyra::compiler
