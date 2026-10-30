#pragma once

#include <cstdint>
#include <optional>
#include <string_view>
#include <variant>

#include "lyra/support/layout.hpp"
#include "lyra/support/value_domain.hpp"

namespace lyra::support {

// An object the runtime library defines that is not a value of the design:
// what an entry builds for one use -- what a print is assembled from, what a
// wait registers, a write in progress into storage a wrapper stands for, a part
// designated within one -- and what the generated side holds for a while --
// an execution, a counted hold on a value, a reference to storage somebody
// else owns.
enum class LibraryObject : std::uint8_t {
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
  // A reference to a value of any type (LRM 15.4.9).
  kAnyReference,
  // A reference to an object that does not keep it (LRM 8.30).
  kWeakReference,
  // A reference a call binds, with the lease keeping what it names in
  // existence until the call returns (LRM 13.5.2).
  kLeasedReference,
};

// An object generated code holds by value: it gives the object storage in its
// own frame, the library builds the object there, and the object ends where
// the program that made it says. A value of every domain is one, and so is
// each library object -- except a value laid out by its type (a structure, a
// union, a tuple a lowering composes, or a fixed-size unpacked array), which
// generated code lays out itself as the type states; the object of that domain
// is how the library holds one.
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
  Layout layout;
  bool ends_with_nothing_to_do;

  friend auto operator==(const ObjectLayout&, const ObjectLayout&)
      -> bool = default;
};

// How large an object the runtime defines is, for each one generated code
// holds by value, stated where a layer compiled without the runtime reads it:
// MIR sizes a frame holding one, a list of triggers among them, from these, and
// the runtime answers its own layout queries with them too, after checking each
// against the class it defines. A compiler building a program lays the classes
// out itself, and checks that each fits what is stated.
inline constexpr ObjectLayout kTriggerStorage{
    .layout = {.size = 80, .align = 8}, .ends_with_nothing_to_do = false};
inline constexpr ObjectLayout kObservationStorage{
    .layout = {.size = 16, .align = 8}, .ends_with_nothing_to_do = false};
inline constexpr ObjectLayout kReadReportStorage{
    .layout = {.size = 120, .align = 8}, .ends_with_nothing_to_do = false};
inline constexpr ObjectLayout kWaitStorage{
    .layout = {.size = 8, .align = 8}, .ends_with_nothing_to_do = false};
inline constexpr ObjectLayout kHierarchySegmentStorage{
    .layout = {.size = 56, .align = 8}, .ends_with_nothing_to_do = false};
inline constexpr ObjectLayout kChannelCancellationStorage{
    .layout = {.size = 24, .align = 8}, .ends_with_nothing_to_do = false};
inline constexpr ObjectLayout kWeakReferenceStorage{
    .layout = {.size = 16, .align = 8}, .ends_with_nothing_to_do = false};
inline constexpr ObjectLayout kLeasedReferenceStorage{
    .layout = {.size = 112, .align = 8}, .ends_with_nothing_to_do = false};
inline constexpr ObjectLayout kReferenceStorage{
    .layout = {.size = 72, .align = 8}, .ends_with_nothing_to_do = true};
inline constexpr ObjectLayout kAnyReferenceStorage{
    .layout = {.size = 40, .align = 8}, .ends_with_nothing_to_do = true};
inline constexpr ObjectLayout kSharedPointerStorage{
    .layout = {.size = 16, .align = 8}, .ends_with_nothing_to_do = false};
inline constexpr ObjectLayout kOpenWriteStorage{
    .layout = {.size = 448, .align = 8}, .ends_with_nothing_to_do = false};
inline constexpr ObjectLayout kDesignationStorage{
    .layout = {.size = 16, .align = 8}, .ends_with_nothing_to_do = true};
inline constexpr ObjectLayout kObjectWriteStorage{
    .layout = {.size = 8, .align = 8}, .ends_with_nothing_to_do = false};

// The same for each class the value library keeps a value in where it does not
// lay the value out by its type, and for each object of the value library a
// runtime entry is handed.
inline constexpr ObjectLayout kStringStorage{
    .layout = {.size = 32, .align = 8}, .ends_with_nothing_to_do = false};
inline constexpr ObjectLayout kRealStorage{
    .layout = {.size = 8, .align = 8}, .ends_with_nothing_to_do = true};
inline constexpr ObjectLayout kShortRealStorage{
    .layout = {.size = 4, .align = 4}, .ends_with_nothing_to_do = true};
inline constexpr ObjectLayout kChandleStorage{
    .layout = {.size = 8, .align = 8}, .ends_with_nothing_to_do = true};
inline constexpr ObjectLayout kObjectRefStorage{
    .layout = {.size = 16, .align = 8}, .ends_with_nothing_to_do = false};
inline constexpr ObjectLayout kDynamicArrayStorage{
    .layout = {.size = 40, .align = 8}, .ends_with_nothing_to_do = false};
inline constexpr ObjectLayout kQueueStorage{
    .layout = {.size = 64, .align = 8}, .ends_with_nothing_to_do = false};
inline constexpr ObjectLayout kAssociativeArrayStorage{
    .layout = {.size = 80, .align = 8}, .ends_with_nothing_to_do = false};
inline constexpr ObjectLayout kWildcardKeyStorage{
    .layout = {.size = 80, .align = 8}, .ends_with_nothing_to_do = false};
inline constexpr ObjectLayout kAnyStorage{
    .layout = {.size = 24, .align = 8}, .ends_with_nothing_to_do = false};
inline constexpr ObjectLayout kEmptyStorage{
    .layout = {.size = 1, .align = 1}, .ends_with_nothing_to_do = true};
inline constexpr ObjectLayout kBitVecStorage{
    .layout = {.size = 32, .align = 8}, .ends_with_nothing_to_do = false};
inline constexpr ObjectLayout kPrintItemStorage{
    .layout = {.size = 72, .align = 8}, .ends_with_nothing_to_do = true};
inline constexpr ObjectLayout kPrintLiteralItemStorage{
    .layout = {.size = 16, .align = 8}, .ends_with_nothing_to_do = true};
inline constexpr ObjectLayout kPrintValueItemStorage{
    .layout = {.size = 64, .align = 8}, .ends_with_nothing_to_do = true};
inline constexpr ObjectLayout kFormatSpecStorage{
    .layout = {.size = 20, .align = 4}, .ends_with_nothing_to_do = true};
inline constexpr ObjectLayout kFormatArgStorage{
    .layout = {.size = 40, .align = 8}, .ends_with_nothing_to_do = true};
inline constexpr ObjectLayout kTimeFormatStorage{
    .layout = {.size = 48, .align = 8}, .ends_with_nothing_to_do = false};
inline constexpr ObjectLayout kDpiBitBufferStorage{
    .layout = {.size = 24, .align = 8}, .ends_with_nothing_to_do = false};
inline constexpr ObjectLayout kDpiLogicBufferStorage{
    .layout = {.size = 24, .align = 8}, .ends_with_nothing_to_do = false};
inline constexpr ObjectLayout kDpiOpenArrayStorage{
    .layout = {.size = 64, .align = 8}, .ends_with_nothing_to_do = false};

// The storage stated above for an object of the library's own, and nothing
// for one it is not: a value held as its bits or as the bytes its type lays
// out, and an execution, which whoever drives it takes the moment it is made.
auto StatedStorageOf(const RuntimeObject& object)
    -> std::optional<ObjectLayout>;

// The spelling the entries over an object carry, stated once for both sides.
// A value domain's object is spelled as the domain is, so every entry over a
// value leads with the same word, and a library object as its own name is.
auto LibraryObjectName(LibraryObject object) -> std::string_view;
auto RuntimeObjectName(const RuntimeObject& object) -> std::string_view;

}  // namespace lyra::support
