#pragma once

#include <cstdint>
#include <string_view>
#include <variant>

#include "lyra/support/value_domain.hpp"

namespace lyra::support {

// An object the runtime library defines that is not a value of the design:
// what an entry builds for one use -- what a print is assembled from, what a
// wait registers, a write in progress into storage a wrapper stands for, a part
// designated within one -- and what the generated side holds for a while --
// the owner of a closure, an execution, a counted hold on a value, a reference
// to storage somebody else owns.
enum class LibraryObject : std::uint8_t {
  kClosure,
  kPrintItem,
  kFormatSpec,
  kFormatArg,
  kHierarchySegment,
  kTrigger,
  kObservation,
  kReadReport,
  kDpiBitBuffer,
  kDpiLogicBuffer,
  kDpiOpenArray,
  kChannelCancellation,
  kExecution,
  kSharedPointer,
  kOpenWrite,
  kDesignation,
  kObjectWrite,
  kReference,
};

// An object generated code holds by value: it gives the object storage in its
// own frame, the library builds the object there, and the object ends where
// the program that made it says. A value of every domain is one, and so is
// each library object -- except a tuple, which generated code lays out itself
// as its type states; the tuple domain's object is how the library holds one.
// What the library keeps for the whole run, and storage an owner holds, is
// reached by address instead and is not one of these.
//
// Two sides name it, as they do a value domain: a backend gives the storage
// and calls the entries that build and end an object, and the runtime defines
// those entries over the object's own type.
using RuntimeObject = std::variant<ValueDomain, LibraryObject>;

// The storage an object needs, and whether ending one has anything to do.
// Nothing here depends on what the object holds: every domain realizes one
// runtime type whatever source type it stands for, so one size serves every
// value of it. The runtime answers it for each object it builds, read off that
// object's own type.
struct ObjectLayout {
  std::uint32_t size;
  std::uint32_t align;
  bool ends_with_nothing_to_do;
};

// The spelling the entries over an object carry, stated once for both sides.
// A value domain's object is spelled as the domain is, so every entry over a
// value leads with the same word, and a library object as its own name is.
auto LibraryObjectName(LibraryObject object) -> std::string_view;
auto RuntimeObjectName(const RuntimeObject& object) -> std::string_view;

}  // namespace lyra::support
