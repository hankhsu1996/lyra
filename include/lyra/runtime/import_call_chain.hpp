#pragma once

#include <cstddef>
#include <deque>
#include <vector>

#include "lyra/value/string.hpp"

namespace lyra::runtime {

class Scope;

// The DPI-C import calls this execution is inside (LRM 35.5.3), innermost last.
// A `context` import enters one for the duration of its foreign call and leaves
// it however that call ends, so the chain is non-empty only inside one -- which
// is why the clause states its properties over the chain rather than over any
// one imported subroutine.
//
// An entry carries the scope the call made current, which an export the foreign
// side calls back resolves against, and whether the foreign frame has
// acknowledged that this execution must stop (LRM 35.9 item c). Both are facts
// of the call rather than of the execution: a call reached from inside another
// has its own of each.
//
// The chain also keeps every string an export hands to the foreign code that
// called it (LRM Annex H.8.10): the address has to be good when it arrives, and
// the first moment nothing on the foreign side can still be reading it is the
// return of the import call that let that code in. So each call keeps what is
// handed out while it is innermost and releases it when it is left, as a Java
// VM keeps the objects a native method was handed in a registry deleted when
// the method returns. No string is handed out with no call open, because an
// export reached from outside one is refused before its subroutine runs (LRM
// 35.5.3).
class ImportCallChain {
 public:
  void Enter(Scope* scope) {
    calls_.push_back(ImportCall{.scope = scope, .kept_from = kept_.size()});
  }

  void Leave() {
    kept_.resize(calls_.back().kept_from);
    calls_.pop_back();
  }

  // Whether a `context` import's call is running, which is the only place
  // foreign code may call an exported subroutine from (LRM 35.5.3).
  [[nodiscard]] auto InsideACall() const -> bool {
    return !calls_.empty();
  }

  // `svGetScope`, which reports no scope where no foreign call is running
  // rather than faulting.
  [[nodiscard]] auto CurrentScope() const -> Scope* {
    return calls_.empty() ? nullptr : calls_.back().scope;
  }

  // `svSetScope`: retarget the innermost entry, reporting the one replaced.
  // Outside any context import there is no entry to retarget, so it reports
  // null rather than fabricating one with no boundary to leave it by.
  auto ReplaceScope(Scope* scope) -> Scope* {
    if (calls_.empty()) {
      return nullptr;
    }
    Scope* previous = calls_.back().scope;
    calls_.back().scope = scope;
    return previous;
  }

  // `svAckDisabledState` (LRM 35.9 item c): the foreign frame now running says
  // it is following the protocol. Outside any such call there is no frame to
  // say it of, so nothing is recorded -- a query answering for a call that is
  // not there is the caller's mistake and not a reason to end a run.
  void AcknowledgeStop() {
    if (!calls_.empty()) {
      calls_.back().acknowledged_stop = true;
    }
  }

  // Whether the innermost call acknowledged, which is what its boundary holds
  // it to when the execution must stop. A boundary always has its own entry, so
  // the empty answer is reached only by a caller that has none to be held to.
  [[nodiscard]] auto InnermostAcknowledgedStop() const -> bool {
    return calls_.empty() || calls_.back().acknowledged_stop;
  }

  // Keeps a copy of `text` until the innermost call is left, and answers its
  // characters. What is kept never moves, because its address is what the
  // foreign side holds.
  auto HandToForeignCaller(const value::String& text) -> const char* {
    return kept_.emplace_back(text).CStr();
  }

 private:
  struct ImportCall {
    Scope* scope;
    bool acknowledged_stop = false;
    // How many strings were kept when the call was entered, which is how many
    // are left once it is left.
    std::size_t kept_from;
  };

  std::vector<ImportCall> calls_;
  std::deque<value::String> kept_;
};

}  // namespace lyra::runtime
