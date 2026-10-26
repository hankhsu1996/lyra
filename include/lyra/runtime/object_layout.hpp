#pragma once

#include <cstdint>
#include <optional>
#include <variant>

#include "lyra/support/layout.hpp"
#include "lyra/support/member_storage_kind.hpp"
#include "lyra/support/runtime_class.hpp"
#include "lyra/support/runtime_object.hpp"
#include "lyra/support/value_domain.hpp"

namespace lyra::runtime {

// The storage an object this library builds occupies, and whether ending one
// has anything to do, read off the type the library builds it as. A target
// that gives such an object storage and the library that builds it there then
// agree by construction, because there is one statement of the object and
// both read it: the library as the type itself, a target through this.
auto LayoutOf(support::ValueDomain domain) -> support::ObjectLayout;
auto LayoutOf(support::LibraryObject object) -> support::ObjectLayout;
auto LayoutOf(const support::RuntimeObject& object) -> support::ObjectLayout;

// The same for the storage one member of a declaration occupies where the
// declaration is laid out, for each kind over each domain it is realized for,
// holding values laid out as `held` is, none for storage holding no value of
// the program's. Storage that holds a value whose size the library is told
// keeps that value at its tail, so it is as large as the value where the value
// lies inline; every other holds it in the library's own type. A pair it is not
// realized for is a compiler bug, since the kind is chosen from the member's
// type and a type names only realized pairs.
auto LayoutOf(
    support::DeclaredMemberStorage storage,
    std::optional<support::ObjectLayout> held) -> support::ObjectLayout;

// Where the value a member's storage holds is found in that storage, which a
// target reads and writes it at as a compiler holding the storage's class
// reaches the member through the class's own accessor. The value lies at an
// offset in storage holding an integral value no wider than a word as its own
// bytes. Storage that keeps the value's address keeps it at an offset, which is
// the same whatever the value's type: a net, whose value is its own or that of
// the object its one driver carries, and a variable or a procedural local
// laying its value out after itself, where it lies at its end or apart by its
// size. Nothing for storage whose contents its own access hands out.
struct ContentsAt {
  std::uint64_t offset;
};
struct ContentsIndirectAt {
  std::uint64_t offset;
};
using HeldContents = std::variant<ContentsAt, ContentsIndirectAt>;
auto HeldContentsOf(support::DeclaredMemberStorage storage)
    -> std::optional<HeldContents>;

// The same for the part of a value this library builds where a class of the
// source extends one of its classes: what that part occupies, which is where
// the class's own storage begins.
auto LayoutOf(support::RuntimeClass klass) -> support::ObjectLayout;

// What this library keeps at the start of a closure value: the code building
// a closure lays its captures out after it, as a class extending one of this
// library's classes lays its own storage out after that class's part.
auto ClosurePart() -> support::Layout;

// Where, in the definition of a class, what generated code reads of it lies:
// how an object of the class ends, and the table of the body each behavior of
// it runs. Every object holds its class's definition at its start.
struct DefinitionLayout {
  std::uint64_t end;
  std::uint64_t methods;
};
auto DefinitionLayoutOf() -> DefinitionLayout;

// Builds that part at `at`, where a value of a class extending it is created,
// before any member of the class's own; and ends it there, after every one of
// them, as a C++ class's construction and destruction call its base's.
void BeginLibraryPart(support::RuntimeClass klass, void* at);
void EndLibraryPart(support::RuntimeClass klass, void* at);

}  // namespace lyra::runtime
