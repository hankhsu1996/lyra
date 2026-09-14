#pragma once

#include <vector>

namespace lyra::runtime {

class Scope;

// The scopes a DPI-C import call chain has made current (LRM 35.5.3), innermost
// last. A `context` import enters the instantiated scope of its declaration for
// the duration of its foreign call and leaves it however that call ends, so the
// chain is non-empty only inside one. An export the foreign side calls back
// resolves against the innermost entry.
class DpiScopeChain {
 public:
  void Enter(Scope* scope) {
    scopes_.push_back(scope);
  }

  void Leave() {
    scopes_.pop_back();
  }

  // `svGetScope`, which reports no scope where no foreign call is running
  // rather than faulting.
  [[nodiscard]] auto Current() const -> Scope* {
    return scopes_.empty() ? nullptr : scopes_.back();
  }

  // `svSetScope`: retarget the innermost entry, reporting the one replaced.
  // Outside any context import there is no entry to retarget, so it reports
  // null rather than fabricating one with no boundary to leave it by.
  auto Replace(Scope* scope) -> Scope* {
    if (scopes_.empty()) {
      return nullptr;
    }
    Scope* previous = scopes_.back();
    scopes_.back() = scope;
    return previous;
  }

 private:
  std::vector<Scope*> scopes_;
};

}  // namespace lyra::runtime
