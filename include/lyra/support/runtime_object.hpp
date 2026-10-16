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
  kWait,
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
// each library object -- except a tuple, a union and a fixed-size unpacked
// array, which generated code lays out itself as the type states; the object
// of each of those domains is how the library holds one.
// What the library keeps for the whole run, and storage an owner holds, is
// reached by address instead and is not one of these.
//
// Two sides name it, as they do a value domain: a backend gives the storage
// and calls the entries that build and end an object, and the runtime defines
// those entries over the object's own type.
using RuntimeObject = std::variant<ValueDomain, LibraryObject>;

// The storage an object needs, and whether ending one has anything to do. The
// runtime answers it for each object it builds, read off that object's own
// type; a holder keeping a value at its tail answers it with that value's
// layout, so its size follows the type it holds.
struct ObjectLayout {
  std::uint32_t size;
  std::uint32_t align;
  bool ends_with_nothing_to_do;

  friend auto operator==(const ObjectLayout&, const ObjectLayout&)
      -> bool = default;
};

// How large an object the runtime defines is, for each one generated code
// holds by value, stated where a layer compiled without the runtime reads it:
// MIR sizes a frame holding one, a list of triggers among them, from these, and
// the runtime answers its own layout queries with them too, after checking each
// against the class it defines.
inline constexpr ObjectLayout kTriggerStorage{
    .size = 80, .align = 8, .ends_with_nothing_to_do = false};
inline constexpr ObjectLayout kObservationStorage{
    .size = 16, .align = 8, .ends_with_nothing_to_do = false};
inline constexpr ObjectLayout kReadReportStorage{
    .size = 96, .align = 8, .ends_with_nothing_to_do = false};
inline constexpr ObjectLayout kWaitStorage{
    .size = 8, .align = 8, .ends_with_nothing_to_do = false};
inline constexpr ObjectLayout kHierarchySegmentStorage{
    .size = 56, .align = 8, .ends_with_nothing_to_do = false};
inline constexpr ObjectLayout kChannelCancellationStorage{
    .size = 24, .align = 8, .ends_with_nothing_to_do = false};

// The spelling the entries over an object carry, stated once for both sides.
// A value domain's object is spelled as the domain is, so every entry over a
// value leads with the same word, and a library object as its own name is.
auto LibraryObjectName(LibraryObject object) -> std::string_view;
auto RuntimeObjectName(const RuntimeObject& object) -> std::string_view;

}  // namespace lyra::support
