#pragma once

#include <cstdint>
#include <string_view>

namespace lyra::support {

// The runtime value type a library entry operates on, and that a storage cell
// is realized as. It enumerates the value types the runtime library has, not
// the type kinds a source language has: several source types share one domain,
// and a source type the runtime has no realization for has no domain at all. It
// grows when the runtime gains a value type, never to mirror the source
// language.
//
// An integral value no wider than a machine word is held as its own bytes, and
// what holds one -- a variable's cell, a net, a history -- stores it, copies
// it and compares it without reading how many of its bits the type declares or
// whether they are read as signed. So the domain of such a value is its layout
// alone: the storage unit one plane takes, and whether a second plane follows
// it for x and z. A `logic signed [4:0]` and a `logic [7:0]` are one domain.
// A wider integral value is held as its words, as many as its width asks for,
// which what holds one is told where it is installed; so the domain of such a
// value is whether an unknown plane follows its value plane, and a
// `bit [99:0]` and a `bit [199:0]` are one domain.
//
// An index of a wildcard-indexed associative array is of whatever integral
// type the expression naming it carried (LRM 7.8.1), so one kept in a value is
// held with that type.
//
// Two sides name it. A backend classifies a type into a domain and mints the
// entry that domain names; the runtime realizes the storage a domain asks for
// and defines those entries. Neither imports the other's vocabulary, so the
// enumeration lives beside them rather than in either.
enum class ValueDomain : std::uint8_t {
  kBit8,
  kBit16,
  kBit32,
  kBit64,
  kLogic8,
  kLogic16,
  kLogic32,
  kLogic64,
  kBitWide,
  kLogicWide,
  kWildcardIndex,
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

// Whether a value of this domain is an integral value no wider than a word,
// whose layout the domain states whole. What holds one is the bytes
// themselves, read where the holder lies.
auto IsIntegralLayout(ValueDomain domain) -> bool;

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
