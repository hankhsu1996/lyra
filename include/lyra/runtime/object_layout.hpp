#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <string_view>

#include "lyra/support/member_storage_kind.hpp"
#include "lyra/support/runtime_class.hpp"
#include "lyra/support/runtime_object.hpp"
#include "lyra/support/value_domain.hpp"
#include "lyra/value/value_type.hpp"

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
// declaration is laid out, for each kind over each domain it is realized for.
// A pair it is not realized for is a compiler bug, since the kind is chosen
// from the member's type and a type names only realized pairs.
auto LayoutOf(support::DeclaredMemberStorage storage) -> support::ObjectLayout;

// Where the value a member's storage holds lies in that storage, for storage
// that holds an integral value as its own bytes and answers a read with them.
// A target reads such a value at this offset, as a compiler holding the
// storage's class reads the member through the class's own accessor. Nothing
// for any other storage, whose contents its own access hands out.
auto ContentsOffsetOf(support::DeclaredMemberStorage storage)
    -> std::optional<std::uint64_t>;

// Where a place designated within a write in progress states the address of
// the part it designates.
auto DesignatedPartAt() -> std::uint64_t;

// The constant that tells the library an integral type's numbers, as a target
// stating one lays it out: what the whole occupies, the symbol of the table
// the library defines for it, which the constant opens with the address of
// (C++ ABI 2.4), and where it states the size and the alignment of a value of
// the type and the type's shape.
struct IntegralTypeConstantLayout {
  support::ObjectLayout object;
  std::string table_symbol;
  value::StatedAt value_size;
  value::StatedAt value_align;
  std::uint64_t shape_at;
};
auto LayoutOfIntegralTypeConstant() -> IntegralTypeConstantLayout;

// The same for the part of a value this library builds where a class of the
// source extends one of its classes: what that part occupies, which is where
// the class's own storage begins.
auto LayoutOf(support::RuntimeClass klass) -> support::ObjectLayout;

// Where a closure value's captures begin, after what this library keeps in the
// value: the code building a closure lays its captures out from there, as a
// class extending one of this library's classes lays its own storage out from
// where that class's part ends.
auto ClosureCapturesAt() -> std::uint64_t;

// The symbol of that class's type information (C++ ABI 2.9.4), which the
// description of a class extending it names as its base. Read off the class
// itself, so it follows the class wherever its declaration moves.
auto TypeInfoSymbolOf(support::RuntimeClass klass) -> std::string;

// The symbols of that class's base object constructor and base object
// destructor (C++ ABI 5.1.4.3, C2 and D2), which the constructor and destructor
// of a class extending it call as a C++ class's call its base's.
auto BaseObjectConstructorSymbolOf(support::RuntimeClass klass)
    -> std::string_view;
auto BaseObjectDestructorSymbolOf(support::RuntimeClass klass)
    -> std::string_view;

// The symbol of the library's own body of one of its virtual functions, which
// the table of a class extending its class holds where the class does not
// override it.
auto VirtualFunctionSymbolOf(support::LibraryVirtual function)
    -> std::string_view;

}  // namespace lyra::runtime
