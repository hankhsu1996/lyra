#pragma once

#include <cstddef>
#include <cstdint>
#include <list>
#include <optional>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/source_location.hpp"
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

// A simulation control task foreign code asked for through `vpi_control`
// (LRM 38.4), with the diagnostic level the task is handed (LRM 20.2).
struct ForeignControl {
  enum class Task : std::uint8_t { kFinish, kStop };
  Task task;
  std::int64_t finish_number;
};

// The foreign calls of application routines this execution is inside,
// innermost last: a `context` DPI-C import's (LRM 35.5.3), and a callback's,
// which the run makes (LRM 38.36). Each is entered for the duration of its
// call and left however the call ends.
//
// Every entry carries a control task foreign code asked for through VPI to
// run as the call returns (LRM 38.4), since the standard places it upon the
// return of the application routine, whichever kind that is. Only an import's
// entry carries the rest: the handle the call has current, which an export the
// foreign side calls back resolves against; whether the foreign frame has
// acknowledged that this execution must stop (LRM 35.9 item c); and where the
// design wrote the call, which is where what foreign code does wrong under it
// is reported and what it is told when it asks for its caller (LRM Annex
// H.9.3). A callback's call is none the design wrote and enters no DPI context
// (LRM Annex H.9), so inside one no scope is current and no export is called,
// even where it runs on the native stack of an import's call whose write made
// the occurrence it is called back for. LRM 35.5.3 states these over the chain
// rather than over one imported subroutine, which is why the chain holds
// them; a call reached from inside another has its own.
//
// The chain also keeps every string an export hands to the foreign code that
// called it (LRM Annex H.8.10): the address has to be good when it arrives, and
// the first moment nothing on the foreign side can still be reading it is the
// return of the import call that let that code in. So each call keeps what is
// handed out while it is innermost and releases it when it is left, as a Java
// VM keeps the objects a native method was handed in a registry deleted when
// the method returns. No string is handed out outside an import's call,
// because an export reached from anywhere else is refused before its
// subroutine runs (LRM 35.5.3).
class ApplicationRoutineCalls {
 public:
  void EnterImport(Scope* scope, base::SourceLocation written_at) {
    calls_.push_back(
        Call{
            .kind = ImportCall{
                .current = scope,
                .kept_from = kept_.size(),
                .written_at = written_at}});
  }

  void EnterCallback() {
    calls_.push_back(Call{.kind = CallbackCall{}});
  }

  void Leave() {
    if (const ImportCall* import = InnermostImport(); import != nullptr) {
      kept_.resize(import->kept_from);
    }
    calls_.pop_back();
  }

  // Whether the innermost call is a `context` import's, which is the only
  // place foreign code may call an exported subroutine from (LRM 35.5.3).
  [[nodiscard]] auto InsideAnImportCall() const -> bool {
    return InnermostImport() != nullptr;
  }
  // Whether the innermost call is a callback's.
  [[nodiscard]] auto InsideACallbackCall() const -> bool {
    return !calls_.empty() &&
           std::holds_alternative<CallbackCall>(calls_.back().kind);
  }

  // Where the design wrote the innermost call, which only a caller that knows
  // it is inside an import's asks.
  [[nodiscard]] auto WrittenAt() const -> base::SourceLocation {
    return InnermostImport()->written_at;
  }

  // `svGetScope`, which reports no scope where no import's call is innermost
  // rather than faulting.
  [[nodiscard]] auto Current() const -> CurrentDpiHandle {
    const ImportCall* import = InnermostImport();
    if (import == nullptr) {
      return static_cast<Scope*>(nullptr);
    }
    return import->current;
  }

  // `svSetScope`: retarget the innermost import's call, reporting the handle
  // replaced. Where none is innermost there is no entry to retarget, so it
  // reports no scope rather than fabricating one with no boundary to leave it
  // by.
  auto Replace(CurrentDpiHandle handle) -> CurrentDpiHandle {
    ImportCall* import = InnermostImport();
    if (import == nullptr) {
      return static_cast<Scope*>(nullptr);
    }
    return std::exchange(import->current, handle);
  }

  // `svAckDisabledState` (LRM 35.9 item c): the foreign frame now running says
  // it is following the protocol. Outside an import's call there is no frame
  // to say it of, so nothing is recorded -- a query answering for a call that
  // is not there is the caller's mistake and not a reason to end a run.
  void AcknowledgeStop() {
    if (ImportCall* import = InnermostImport(); import != nullptr) {
      import->acknowledged_stop = true;
    }
  }

  // Whether the innermost import's call acknowledged, which is what its
  // boundary holds it to when the execution must stop. A boundary always has
  // its own entry, so the answer for none is reached only by a caller that
  // has none to be held to.
  [[nodiscard]] auto InnermostAcknowledgedStop() const -> bool {
    const ImportCall* import = InnermostImport();
    return import == nullptr || import->acknowledged_stop;
  }

  // `vpi_control` (LRM 38.4): the task foreign code asks for runs "upon return
  // of the application routine", so it is kept on the innermost call and done
  // as that call returns; a later request replaces an earlier one. Outside any
  // such call no return is coming to do it at, so nothing is kept, which the
  // caller answers as the routine's failure.
  auto RequestControl(ForeignControl control) -> bool {
    if (calls_.empty()) {
      return false;
    }
    calls_.back().requested_control = control;
    return true;
  }

  // What the innermost call was asked to do as it returns, taken.
  auto TakeRequestedControl() -> std::optional<ForeignControl> {
    if (calls_.empty()) {
      return std::nullopt;
    }
    return std::exchange(calls_.back().requested_control, std::nullopt);
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
    base::SourceLocation written_at;
  };
  struct CallbackCall {};
  struct Call {
    std::variant<ImportCall, CallbackCall> kind;
    std::optional<ForeignControl> requested_control = std::nullopt;
  };

  [[nodiscard]] auto InnermostImport() -> ImportCall* {
    return calls_.empty() ? nullptr
                          : std::get_if<ImportCall>(&calls_.back().kind);
  }
  [[nodiscard]] auto InnermostImport() const -> const ImportCall* {
    return calls_.empty() ? nullptr
                          : std::get_if<ImportCall>(&calls_.back().kind);
  }

  std::vector<Call> calls_;
  // Each string stays where it was put, and outside an import's call the list
  // is empty and holds no memory.
  std::list<value::String> kept_;
};

}  // namespace lyra::runtime
