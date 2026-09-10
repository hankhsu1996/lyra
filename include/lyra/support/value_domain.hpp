#pragma once

#include <cstdint>
#include <string_view>

namespace lyra::support {

// The runtime value type a library entry operates on, that a storage cell is
// realized as, and that an erased value holds. It enumerates the value types
// the runtime library has, not the type kinds a source language has: several
// source types share one domain -- an enumeration and an integral are both a
// packed value -- and a source type the runtime has no realization for has no
// domain at all. It grows when the runtime gains a value type, never to mirror
// the source language.
//
// Two sides name it. A backend classifies a type into a domain and mints the
// entry that domain names; the runtime realizes the storage a domain asks for
// and defines those entries. Neither imports the other's vocabulary, so the
// enumeration lives beside them rather than in either.
enum class ValueDomain : std::uint8_t {
  kPacked,
  kString,
  kReal,
  kShortReal,
  kChandle,
  kEmpty,
  kTuple,
  kUnion,
  kTaggedUnion,
  kDynArray,
  kUnpackedArray,
  kQueue,
  kAssocArray,
  kManagedRef,
};

// The spelling a domain-parametric entry's symbol carries. It is part of what
// the two sides must agree on, so it is stated once here rather than composed
// on each side.
auto ValueDomainName(ValueDomain domain) -> std::string_view;

// Whether a value of this domain is the handle rather than something the handle
// points at. A chandle is a host pointer (LRM 6.14), so there is no runtime
// object for a handle to name and the pointer travels as itself; every other
// domain's value lives in storage and its handle is that storage's address.
// Both sides must agree -- the runtime when it hands a value out of storage,
// the backend when it reads one -- so it is stated here rather than decided
// twice.
auto ValueDomainIsItsOwnHandle(ValueDomain domain) -> bool;

}  // namespace lyra::support
