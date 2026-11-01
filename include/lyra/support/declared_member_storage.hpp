#pragma once

#include <string_view>
#include <variant>

#include "lyra/support/library_decl.hpp"
#include "lyra/support/runtime_object.hpp"
#include "lyra/support/value_domain.hpp"

namespace lyra::support {

// How a member's storage is held. Everything that acts on a member reads it
// from here: the runtime side builds the storage it names, and code generation
// realizes a read and a write through it, so the storage a member gets and the
// access emitted for it are one statement rather than two. Storage over values
// of one domain says which, and an object held whole says which object it is.
//
// Two sides name it, and neither imports the other's vocabulary: a backend
// classifies a declaration into one of these and states it in what it emits,
// and the runtime realizes the storage it asks for. So the set lives beside
// them rather than in either.

// A subscribable variable: reached only through its own address, and a write
// through it wakes whoever waited on it.
struct ObservableCellMember {
  ValueDomain domain;
};
// A net's resolution node, likewise reached only through its address; a value
// reaches it through a driver rather than by being written (LRM 6.5).
struct ResolvedNetMember {
  ValueDomain domain;
};
// What the ticks of one clocking event settled for one expression (LRM
// 16.9.3), also reached only through its address. It holds values of one
// domain and answers with the one a read names, so unlike a cell there is no
// single current value to read out of it.
struct SampledHistoryMember {
  ValueDomain domain;
};
// A variable the owner holds that nothing subscribes to: written and read
// through its own storage, so a write keeps the representation the declaration
// gave it and a read copies out rather than aliasing.
struct ValueCellMember {
  ValueDomain domain;
};
// A value of the program filled once where the owner is built and only read
// afterwards, so the storage itself is what a read hands back.
struct InlineValueMember {
  ValueDomain domain;
};
// Machine data the generated code lays out itself and fills once where the
// owner is built -- a machine integer, a pointer owning what it points at --
// which no entry of the library's builds or ends.
struct InlineBytesMember {};
// A box holding a handle the owner does not own, so a read reads the box
// rather than what it names.
struct BorrowedHandleMember {};
// An object of a type the library declares, held whole as the member and
// built and ended there by the entries its declaration names.
struct LibraryDeclMember {
  LibraryDecl decl;
};
// An object the library defines for a type the IR owns, held whole: a
// reference to storage the owner does not own, of a known type or of any
// (LRM 13.5.2, 15.4.9), and a counted hold on the cell of a local a scope
// keeps past its end (LRM 6.21), whose ending with its owner is what ends the
// cell once no holder is left. A read hands back the object itself, and
// filling it copies one.
struct LibraryObjectMember {
  LibraryObject object;
};

using DeclaredMemberStorage = std::variant<
    ObservableCellMember, ResolvedNetMember, SampledHistoryMember,
    ValueCellMember, InlineValueMember, InlineBytesMember, BorrowedHandleMember,
    LibraryDeclMember, LibraryObjectMember>;

// The spelling the entries building and ending one member's storage carry,
// after the domain it holds values of where it holds any.
auto MemberStorageName(const DeclaredMemberStorage& storage)
    -> std::string_view;

}  // namespace lyra::support
