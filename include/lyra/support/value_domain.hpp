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
  kTuple,
  kDynArray,
  kUnpackedArray,
  kQueue,
  kAssocArray,
};

// The spelling a domain-parametric entry's symbol carries. It is part of what
// the two sides must agree on, so it is stated once here rather than composed
// on each side.
auto ValueDomainName(ValueDomain domain) -> std::string_view;

}  // namespace lyra::support
