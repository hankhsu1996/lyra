#pragma once

#include <cstdint>
#include <optional>
#include <string_view>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/support/layout.hpp"
#include "lyra/support/library_decl.hpp"
#include "lyra/support/value_domain.hpp"

namespace lyra::support {

// An object the runtime library defines for a type the IR owns rather than
// one the library declares: an execution, a counted hold on a value, a write
// in progress into storage a wrapper stands for, a part designated within one,
// a write in progress into the properties of an object (LRM 8.4), which tells
// the object it was written as it ends, and a reference to storage somebody
// else owns, whether of a known type or of any type (LRM 15.4.9).
enum class LibraryObject : std::uint8_t {
  kExecution,
  kSharedPointer,
  kOpenWrite,
  kDesignation,
  kObjectWrite,
  kReference,
  kAnyReference,
};

// What a holder builds, copies and ends a value as through the library's
// entries, and what those entries are named over: the value domain a value of
// the design is realized in, an object a declaration of the library's states
// as its own, named by that declaration, or an object above. A value held as
// its bits -- an integral value, a real, a reference owning nothing -- and a
// value laid out by its type are named by their domain too, where a cell or a
// container holds them, though a frame holds each as the bytes its type lays
// out and the library builds no object of it there. What the library keeps for
// the whole run, and storage an owner holds, is reached by address instead and
// is none of these.
//
// Two sides name it, as they do a value domain: a backend gives the storage
// and calls the entries that build and end an object, and the runtime defines
// those entries over the object's own type.
using RuntimeObject = std::variant<ValueDomain, LibraryDecl, LibraryObject>;

// The declaration whose values are objects of `domain`, and nothing for a
// domain no declaration's values are realized in.
constexpr auto DeclarationRealizedIn(ValueDomain domain)
    -> std::optional<LibraryDecl> {
  std::optional<LibraryDecl> found;
  ForEachLibraryDecl([&](LibraryDecl decl) {
    const auto* held = std::get_if<DomainObject>(&DeclarationOf(decl).holding);
    if (held != nullptr && held->domain == domain) {
      found = decl;
    }
  });
  return found;
}

// How large an object the runtime defines for a type the IR owns is, for each
// one generated code holds by value, stated where a layer compiled without the
// runtime reads it: MIR sizes a frame holding one from it, every backend gives
// one that storage, and the runtime checks each class a backend builds there
// against it. An execution lies there only until whoever drives it takes it,
// the moment it is made.
constexpr auto StatedStorageOf(LibraryObject object) -> ObjectLayout {
  switch (object) {
    case LibraryObject::kExecution:
      return ObjectLayout{.layout = {.size = 8, .align = 8}, .needs_end = true};
    case LibraryObject::kSharedPointer:
      return ObjectLayout{
          .layout = {.size = 16, .align = 8}, .needs_end = true};
    case LibraryObject::kOpenWrite:
      return ObjectLayout{
          .layout = {.size = 448, .align = 8}, .needs_end = true};
    case LibraryObject::kDesignation:
      return ObjectLayout{
          .layout = {.size = 16, .align = 8}, .needs_end = false};
    case LibraryObject::kObjectWrite:
      return ObjectLayout{
          .layout = {.size = 16, .align = 8}, .needs_end = true};
    case LibraryObject::kReference:
      return ObjectLayout{
          .layout = {.size = 72, .align = 8}, .needs_end = false};
    case LibraryObject::kAnyReference:
      return ObjectLayout{
          .layout = {.size = 40, .align = 8}, .needs_end = false};
  }
  throw InternalError("runtime object: unknown library object");
}

// The same for the object a value of `domain` is: a class handle's, which MIR
// owns, stated here, and a value of a type the library declares, on its
// declaration. Nothing for a value held as its bits or laid out by its type,
// which is no object of the library's where a holder keeps it.
constexpr auto StatedStorageOf(ValueDomain domain)
    -> std::optional<ObjectLayout> {
  switch (domain) {
    case ValueDomain::kManagedRef:
      return ObjectLayout{
          .layout = {.size = 16, .align = 8}, .needs_end = true};
    case ValueDomain::kWildcardIndex:
    case ValueDomain::kAny:
    case ValueDomain::kString:
    case ValueDomain::kBitVec:
    case ValueDomain::kDynArray:
    case ValueDomain::kQueue:
    case ValueDomain::kAssocArray:
      return DeclarationRealizedIn(domain).and_then(ObjectStorageOf);
    case ValueDomain::kReal:
    case ValueDomain::kShortReal:
    case ValueDomain::kUnmanagedRef:
    case ValueDomain::kBit8:
    case ValueDomain::kBit16:
    case ValueDomain::kBit32:
    case ValueDomain::kBit64:
    case ValueDomain::kLogic8:
    case ValueDomain::kLogic16:
    case ValueDomain::kLogic32:
    case ValueDomain::kLogic64:
    case ValueDomain::kBitWide:
    case ValueDomain::kLogicWide:
    case ValueDomain::kLaidOut:
      return std::nullopt;
  }
  throw InternalError("runtime object: unknown value domain");
}

// The storage stated for an object, read where each kind of object states it.
constexpr auto StatedStorageOf(const RuntimeObject& object)
    -> std::optional<ObjectLayout> {
  return std::visit(
      Overloaded{
          [](ValueDomain domain) { return StatedStorageOf(domain); },
          [](LibraryDecl decl) { return ObjectStorageOf(decl); },
          [](LibraryObject library) -> std::optional<ObjectLayout> {
            return StatedStorageOf(library);
          }},
      object);
}

// What a holder builds, copies and ends a value of `decl` as, where it holds
// one by value as an object of the library's: the value domain a value of the
// design is realized in, or the declaration itself for an object of its own.
auto ObjectOf(LibraryDecl decl) -> std::optional<RuntimeObject>;

// The spelling the entries over an object carry, stated once for both sides.
// A value domain's object is spelled as the domain is, so every entry over a
// value leads with the same word; an object a declaration states as its own is
// spelled as its declaration says, and one above as its own name is.
auto RuntimeObjectName(const RuntimeObject& object) -> std::string_view;

}  // namespace lyra::support
