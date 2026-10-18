#pragma once

#include <cstdint>
#include <string_view>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/support/machine_signature.hpp"

namespace lyra::support {

// What a closure's body does to the closure it is entered on, as rustc's
// closure kinds say (`rustc_type_ir::ClosureKind`): it only reads its
// captures (`Fn`), it also writes them, keeping state from one call to the
// next (`FnMut`), or it takes the closure over and ends it when it ends
// (`FnOnce`). The same three name what whoever holds a closure asks of it: a
// holder calling it many times through a shared view asks `Fn`, one calling it
// many times as its only holder asks `FnMut`, and one calling it once asks
// `FnOnce`.
enum class ClosureKind : std::uint8_t { kFn, kFnMut, kFnOnce };

// Whether a closure of kind `kind` also serves where `other` is asked for, as
// rustc's `ClosureKind::extends` says: every `Fn` closure is also `FnMut`, and
// every `FnMut` closure also `FnOnce`.
[[nodiscard]] constexpr auto Extends(ClosureKind kind, ClosureKind other)
    -> bool {
  const auto rank = [](ClosureKind of) -> int {
    switch (of) {
      case ClosureKind::kFn:
        return 0;
      case ClosureKind::kFnMut:
        return 1;
      case ClosureKind::kFnOnce:
        return 2;
    }
    throw InternalError("support::Extends: unknown ClosureKind");
  };
  return rank(kind) <= rank(other);
}

// The name rustc gives the trait `kind` is.
[[nodiscard]] constexpr auto ClosureKindName(ClosureKind kind)
    -> std::string_view {
  switch (kind) {
    case ClosureKind::kFn:
      return "Fn";
    case ClosureKind::kFnMut:
      return "FnMut";
    case ClosureKind::kFnOnce:
      return "FnOnce";
  }
  throw InternalError("support::ClosureKindName: unknown ClosureKind");
}

// The C++ function types the runtime library calls a closure at, one for each
// use its entries make of one: an effect is handed nothing and answers
// nothing, a condition answers whether it holds (LRM 12.4), a watch is told
// whether it is arming and answers whether what it watches moved (LRM 9.4.2),
// and an order is told two positions and answers whether the element at the
// first comes before the one at the second (LRM 7.12.2). An entry's bounds
// and the library's parameter holding the closure both name the signature
// here, so the two cannot disagree.
using EffectSignature = void();
using ConditionSignature = bool();
using WatchSignature = bool(bool);
using OrderSignature = bool(std::int64_t, std::int64_t);

// How long an entry of the runtime library keeps a closure, or a started body,
// it is handed. This is what bounds how long the closure can run, so it is
// what decides what the closure may refer to: storage of the frame calling the
// entry, while the frame is sure to outlast it, and only storage that lives on
// past the frame otherwise.
enum class CallableHold : std::uint8_t {
  // Lent for the call: the entry runs it, if at all, before returning.
  kForTheCall,
  // Kept by what the entry answers -- a wait the caller stops at -- and so
  // ended with it, however the wait ends.
  kWhileWaited,
  // Kept past the caller: the entry may run it after the frame that handed it
  // over has ended.
  kPastTheCaller,
};

// A body an entry starts rather than calls -- a fork's branch, a process --
// which runs from where it is started and so has no signature to be called
// at, and which the entry takes over, so it asks nothing of how the body
// treats its closure.
struct StartedBody {};

// A closure an entry calls at `signature`, asking of its body what `asks`
// says, as a Rust callee's `F: FnMut(bool) -> bool` names both at once.
struct CalledClosure {
  ClosureKind asks;
  MachineSignature signature;
};

// The trait half of an entry's bound on what it is handed: a body it starts,
// or a closure it calls at a signature.
using CallableTrait = std::variant<StartedBody, CalledClosure>;

// What an entry of the runtime library asks of one closure, or started body,
// it is handed, and how long it keeps it: the bounds a Rust callee states on a
// callable parameter, as `F: FnMut(bool) -> bool + 'static` asks a closure it
// may call many times at that signature and keep past its caller.
struct CallableBounds {
  CallableTrait trait;
  CallableHold kept;
};

// Whether a closure kept so can run once the frame that handed it over has
// ended, which is what limits it to storage that outlives that frame.
[[nodiscard]] constexpr auto OutlivesTheCaller(CallableHold hold) -> bool {
  switch (hold) {
    case CallableHold::kForTheCall:
    case CallableHold::kWhileWaited:
      return false;
    case CallableHold::kPastTheCaller:
      return true;
  }
  throw InternalError("support::OutlivesTheCaller: unknown CallableHold");
}

}  // namespace lyra::support
