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

// Whether a part of a value in this domain is storage of its own -- an element
// of an unpacked array or a queue, an entry of an associative array, a member
// of an unpacked structure -- rather than a view of one storage object, as a
// bit or element of a packed value, a character of a string and a member of a
// union are. The language gives the first an identity a second name may denote
// (LRM 7.2, 7.4, 7.8, 7.10, 13.5.2), so reading one reaches it where it lies
// and writing one lands in it, whatever else the value holds; a view is read
// and written through the whole it is a view of.
auto PartsAreStorage(ValueDomain domain) -> bool;

}  // namespace lyra::support
