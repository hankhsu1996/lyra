#pragma once

#include <cstdint>
#include <string_view>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/support/value_domain.hpp"

namespace lyra::support {

// An object the runtime library defines that is not a value of the design:
// what an entry builds for one use -- what a print is assembled from, what a
// wait registers, the storage a closure captures into -- and what the
// generated side holds for a while -- an execution, a hold on a promoted scope.
enum class LibraryObject : std::uint8_t {
  kClosure,
  kPrintItem,
  kFormatSpec,
  kFormatArg,
  kHierarchySegment,
  kTrigger,
  kObservation,
  kDpiBitBuffer,
  kDpiLogicBuffer,
  kDpiOpenArray,
  kChannelCancellation,
  kErasedValue,
  kExecution,
  kPromotedScope,
};

// An object generated code holds by value: it gives the object storage in its
// own frame, the library builds the object there, and the object ends where
// the program that made it says. A value of every domain is one, and so is
// each library object. What the library keeps for the whole run, and storage
// an owner holds, is reached by address instead and is not one of these.
//
// Two sides name it, as they do a value domain: a backend gives the storage
// and calls the entries that build and end an object, and the runtime defines
// those entries over the object's own type.
using RuntimeObject = std::variant<ValueDomain, LibraryObject>;

// The storage an object needs, and whether ending one has anything to do.
// Nothing here depends on what the object holds: every domain realizes one
// runtime type whatever source type it stands for, so one size serves every
// value of it. The runtime asserts each against the type it builds, which is
// what holds the two sides together.
struct ObjectLayout {
  std::uint32_t size;
  std::uint32_t align;
  bool ends_with_nothing_to_do;
};

constexpr auto LayoutOf(ValueDomain domain) -> ObjectLayout {
  switch (domain) {
    case ValueDomain::kPacked:
      return {.size = 48, .align = 8, .ends_with_nothing_to_do = false};
    case ValueDomain::kString:
      return {.size = 32, .align = 8, .ends_with_nothing_to_do = false};
    case ValueDomain::kReal:
      return {.size = 8, .align = 8, .ends_with_nothing_to_do = true};
    case ValueDomain::kShortReal:
      return {.size = 4, .align = 4, .ends_with_nothing_to_do = true};
    case ValueDomain::kChandle:
      return {.size = 8, .align = 8, .ends_with_nothing_to_do = true};
    case ValueDomain::kEmpty:
      return {.size = 1, .align = 1, .ends_with_nothing_to_do = true};
    case ValueDomain::kTuple:
      return {.size = 24, .align = 8, .ends_with_nothing_to_do = false};
    case ValueDomain::kUnion:
      return {.size = 32, .align = 8, .ends_with_nothing_to_do = false};
    case ValueDomain::kTaggedUnion:
      return {.size = 32, .align = 8, .ends_with_nothing_to_do = false};
    case ValueDomain::kDynArray:
      return {.size = 32, .align = 8, .ends_with_nothing_to_do = false};
    case ValueDomain::kUnpackedArray:
      return {.size = 32, .align = 8, .ends_with_nothing_to_do = false};
    case ValueDomain::kQueue:
      return {.size = 104, .align = 8, .ends_with_nothing_to_do = false};
    case ValueDomain::kAssocArray:
      return {.size = 48, .align = 8, .ends_with_nothing_to_do = false};
    case ValueDomain::kManagedRef:
      return {.size = 16, .align = 8, .ends_with_nothing_to_do = false};
  }
  throw InternalError("runtime object: unknown value domain");
}

constexpr auto LayoutOf(LibraryObject object) -> ObjectLayout {
  switch (object) {
    case LibraryObject::kClosure:
      return {.size = 32, .align = 8, .ends_with_nothing_to_do = false};
    case LibraryObject::kPrintItem:
      return {.size = 56, .align = 8, .ends_with_nothing_to_do = true};
    case LibraryObject::kFormatSpec:
      return {.size = 20, .align = 4, .ends_with_nothing_to_do = true};
    case LibraryObject::kFormatArg:
      return {.size = 24, .align = 8, .ends_with_nothing_to_do = true};
    case LibraryObject::kHierarchySegment:
      return {.size = 56, .align = 8, .ends_with_nothing_to_do = false};
    case LibraryObject::kTrigger:
      return {.size = 40, .align = 8, .ends_with_nothing_to_do = false};
    case LibraryObject::kObservation:
      return {.size = 16, .align = 8, .ends_with_nothing_to_do = false};
    case LibraryObject::kDpiBitBuffer:
      return {.size = 24, .align = 8, .ends_with_nothing_to_do = false};
    case LibraryObject::kDpiLogicBuffer:
      return {.size = 24, .align = 8, .ends_with_nothing_to_do = false};
    case LibraryObject::kDpiOpenArray:
      return {.size = 64, .align = 8, .ends_with_nothing_to_do = false};
    case LibraryObject::kChannelCancellation:
      return {.size = 24, .align = 8, .ends_with_nothing_to_do = false};
    case LibraryObject::kErasedValue:
      return {.size = 112, .align = 8, .ends_with_nothing_to_do = false};
    case LibraryObject::kExecution:
      return {.size = 8, .align = 8, .ends_with_nothing_to_do = false};
    case LibraryObject::kPromotedScope:
      return {.size = 24, .align = 8, .ends_with_nothing_to_do = false};
  }
  throw InternalError("runtime object: unknown library object");
}

constexpr auto LayoutOf(const RuntimeObject& object) -> ObjectLayout {
  return std::visit(
      Overloaded{
          [](ValueDomain domain) { return LayoutOf(domain); },
          [](LibraryObject library) { return LayoutOf(library); }},
      object);
}

// The spelling the entries over an object carry, stated once for both sides.
// A value domain's object is spelled as the domain is, so every entry over a
// value leads with the same word, and a library object as its own name is.
auto LibraryObjectName(LibraryObject object) -> std::string_view;
auto RuntimeObjectName(const RuntimeObject& object) -> std::string_view;

}  // namespace lyra::support
