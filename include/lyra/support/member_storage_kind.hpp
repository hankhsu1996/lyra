#pragma once

#include <cstdint>

#include "lyra/support/value_domain.hpp"

namespace lyra::support {

// How a member's storage is held. Everything that acts on a member reads it
// from here: the runtime side builds the storage the kind names, and code
// generation realizes a read and a write through it, so the storage a member
// gets and the access emitted for it are one statement rather than two.
//
// Two sides name it, and neither imports the other's vocabulary: a backend
// classifies a declaration into a kind and states that kind in what it emits,
// and the runtime realizes the storage the kind asks for. So the enumeration
// lives beside them rather than in either, and a kind's position is part of
// what the two agree on.
enum class MemberStorageKind : std::uint8_t {
  // A subscribable variable: reached only through its own address, and a write
  // through it wakes whoever waited on it.
  kObservableCell,
  // A net's resolution node, likewise reached only through its address; a value
  // reaches it through a driver rather than by being written (LRM 6.5).
  kResolvedNet,
  // What the ticks of one clocking event settled for one expression (LRM
  // 16.9.3), also reached only through its address. It holds values of one
  // domain and answers with the one a read names, so unlike a cell there is no
  // single current value to read out of it.
  kSampledHistory,
  // A variable the owner holds that nothing subscribes to: written and read
  // through its own storage, so a write keeps the representation the
  // declaration gave it and a read copies out rather than aliasing.
  kValueCell,
  // A value filled once where the owner is built and only read afterwards, so
  // the storage itself is what a read hands back.
  kInlineValue,
  // A box holding a handle the owner does not own, so a read reads the box
  // rather than what it names.
  kBorrowedHandle,
  // A hold on the storage a block promoted out of its frame (LRM 6.21), which
  // the owner does keep alive: a read hands back the hold, and the hold ending
  // with its owner is what ends the storage once no owner is left.
  kPromotedScope,
  // A named event (LRM 15.5), a scope's cancellation target (LRM 9.6.2), the
  // joint cancel state of the channels a deferred write targets (LRM 21.3.2),
  // and what one concurrent assertion has in flight (LRM 16.14.1). Each is a
  // runtime record the owner holds and reaches only through its address; none
  // is read out as a value, and none names a value domain.
  kNamedEvent,
  kCancellationTarget,
  kChannelCancellation,
  kEvaluationAttempts,
};

// What an artifact states about one member's storage: the kind its declaration
// asks for, and the value domain that kind holds -- read only for the kinds
// that name one. A backend states this in what it emits and the runtime builds
// the storage from it, so the two agree on the bytes as well as the meaning;
// both members are one byte and nothing can pad between them, which is the
// whole of what that agreement rests on.
struct DeclaredMemberStorage {
  MemberStorageKind kind = MemberStorageKind::kValueCell;
  ValueDomain domain = ValueDomain::kEmpty;
};

}  // namespace lyra::support
