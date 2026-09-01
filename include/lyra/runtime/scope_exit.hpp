#pragma once

#include <utility>

namespace lyra::runtime {

// Runs a body on every way out of the scope that holds it -- falling off the
// end, a return, a break, or an exception unwinding through. C++ states an
// extent's exit through a destructor rather than through a construct of its
// own, so this is how a body paired with a cleanup is realized here.
template <typename OnExit>
class ScopeExit {
 public:
  explicit ScopeExit(OnExit on_exit) : on_exit_(std::move(on_exit)) {
  }
  ScopeExit(const ScopeExit&) = delete;
  auto operator=(const ScopeExit&) -> ScopeExit& = delete;
  ScopeExit(ScopeExit&&) = delete;
  auto operator=(ScopeExit&&) -> ScopeExit& = delete;
  ~ScopeExit() {
    on_exit_();
  }

 private:
  OnExit on_exit_;
};

template <typename OnExit>
ScopeExit(OnExit) -> ScopeExit<OnExit>;

}  // namespace lyra::runtime
