#include "lyra/base/stack_thread.hpp"

#include <algorithm>
#include <cstddef>
#include <functional>
#include <future>
#include <pthread.h>
#include <string_view>
#include <sys/resource.h>
#include <utility>

#include "lyra/base/memory_watch.hpp"
#include "lyra/base/stack_watch.hpp"

namespace lyra::base {

namespace {

// A run of ten thousand operands takes between half of this and all of it in
// an unoptimized build of the compiler.
constexpr std::size_t kCompileStackSize = std::size_t{64} << 20;

constexpr std::string_view kOutOfStack =
    "error: the compiler ran out of stack. An expression in the design is "
    "longer, or nested more deeply, than lyra supports yet.\n";

// How much stack a thread that compiles is given: the amount compiling takes,
// or what whoever started the process allows any stack where that is less.
auto CompileStackSize() -> std::size_t {
  rlimit limit{};
  if (::getrlimit(RLIMIT_STACK, &limit) != 0 ||
      limit.rlim_max == RLIM_INFINITY) {
    return kCompileStackSize;
  }
  return std::min(kCompileStackSize, static_cast<std::size_t>(limit.rlim_max));
}

}  // namespace

StackThread::StackThread(std::function<void()> work)
    : work_(std::move(work)),
      done_(work_.get_future()),
      alt_stack_(kAltStackSize) {
  pthread_attr_t attributes;
  ::pthread_attr_init(&attributes);
  ::pthread_attr_setstacksize(&attributes, CompileStackSize());
  const int refused = ::pthread_create(&thread_, &attributes, Run, this);
  ::pthread_attr_destroy(&attributes);
  if (refused != 0) {
    EndForLackOfMemory();
  }
}

StackThread::~StackThread() {
  if (!joined_) {
    ::pthread_join(thread_, nullptr);
  }
}

void StackThread::Join() {
  ::pthread_join(thread_, nullptr);
  joined_ = true;
  done_.get();
}

auto StackThread::Run(void* self) -> void* {
  auto& thread = *static_cast<StackThread*>(self);
  InstallStackOverflowHandler(kOutOfStack);
  RegisterThreadStack(thread.alt_stack_);
  thread.work_();
  return nullptr;
}

}  // namespace lyra::base
