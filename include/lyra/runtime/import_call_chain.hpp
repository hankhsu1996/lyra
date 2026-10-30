#pragma once

#include <cstddef>
#include <list>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/value/string.hpp"

namespace lyra::runtime {

class Scope;

// An address foreign code gave `svSetScope` that no DPI function returned, so
// no scope (LRM Annex H.9.3). It is kept so that `svGetScope` reports what was
// set, and is never read through.
struct UnissuedHandle {
  void* address;
};

// What an import call has current: a scope -- null where the import's
// declaration sits in no instance -- or an address that is none.
using CurrentDpiHandle = std::variant<Scope*, UnissuedHandle>;

// The DPI-C import calls this execution is inside (LRM 35.5.3), innermost last.
// A `context` import enters one for the duration of its foreign call and leaves
// it however that call ends, so the chain is non-empty only inside one -- which
// is why the clause states its properties over the chain rather than over any
// one imported subroutine.
//
// An entry carries the handle the call has current, which an export the foreign
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
    calls_.push_back(ImportCall{.current = scope, .kept_from = kept_.size()});
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
  [[nodiscard]] auto Current() const -> CurrentDpiHandle {
    if (calls_.empty()) {
      return static_cast<Scope*>(nullptr);
    }
    return calls_.back().current;
  }

  // `svSetScope`: retarget the innermost entry, reporting the one replaced.
  // Outside any context import there is no entry to retarget, so it reports
  // no scope rather than fabricating one with no boundary to leave it by.
  auto Replace(CurrentDpiHandle handle) -> CurrentDpiHandle {
    if (calls_.empty()) {
      return static_cast<Scope*>(nullptr);
    }
    return std::exchange(calls_.back().current, handle);
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
    CurrentDpiHandle current;
    bool acknowledged_stop = false;
    // How many strings were kept when the call was entered, which is how many
    // are left once it is left.
    std::size_t kept_from;
  };

  std::vector<ImportCall> calls_;
  // Each string stays where it was put, and outside a foreign call the list
  // is empty and holds no memory.
  std::list<value::String> kept_;
};

}  // namespace lyra::runtime
