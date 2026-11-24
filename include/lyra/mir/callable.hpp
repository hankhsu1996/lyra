#pragma once

#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <variant>

#include "lyra/base/overloaded.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/callable_id.hpp"
#include "lyra/mir/callable_ordinal.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/closure_id.hpp"
#include "lyra/mir/foreign_linkage.hpp"
#include "lyra/mir/record_id.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/value_operation.hpp"

namespace lyra::mir {

// Who declares a callable, where it stands among what that owner declares, and
// what only that kind of owner states of it, as clang's declaration context
// says whether a function is a member of a namespace or of a class and rustc's
// `DefKey` states a definition's parent and its disambiguator.
//
// The namespace's callables and a class's stand at an ordinal, counted in the
// order the owner declares them. A construction, a closure's body and a
// record's method are each the one of its kind its owner declares, a record's
// told apart by the operation it answers, so none stands at one. What an owner
// declares is read off these, never a list stored beside them.

// A callable of the unit's namespace: a package's subroutine (LRM 26.3), a
// DPI-C import or export (LRM 35.4), or a body the compiler synthesizes for
// what a type decides about a value of it (LRM 6.19.5, 21.2.1.6).
//
// `foreign`, when present, is the C linkage the callable is reached under. It
// is orthogonal to the body: bodyless plus foreign is an import the user's C
// defines, bodied plus foreign is the entry point of an export the user's C
// calls. A foreign name is program-global and belongs to no class (LRM 35.4,
// 35.7), which is why only the namespace states one. `defining_unit`, where
// present, names the unit whose namespace a subroutine of another unit is a
// callable of; such a callable's code states nothing, its signature being that
// unit's too.
struct DeclaredByNamespace {
  CallableOrdinal ordinal;
  std::optional<ForeignLinkage> foreign;
  std::optional<std::string> defining_unit;

  auto operator==(const DeclaredByNamespace&) const -> bool = default;
};

// A callable of a class: a method (LRM 8.6, 8.10), a process or a lifecycle
// body. `virtual_dispatch`, when present, is its role in the class's dispatch
// table (LRM 8.20) -- introducing a new slot or overriding an ancestor's -- so
// a backend renders the marker off stated structure, never re-deriving
// virtualness by name; it is absent for a direct-only callable. It is the
// class's to state because only a class dispatches. Which unit holds its body
// is its class's: a class another unit defines states nothing of its own.
struct DeclaredByClass {
  ClassId cls;
  CallableOrdinal ordinal;
  std::optional<VirtualDispatchRole> virtual_dispatch;

  auto operator==(const DeclaredByClass&) const -> bool = default;
};

// A class's construction (LRM 8.7), a declaration of its own kind as clang's
// constructor is, so nothing asks a class whether a callable is its
// constructor. It never dispatches.
struct ConstructsClass {
  ClassId cls;

  auto operator==(const ConstructsClass&) const -> bool = default;
};

// The one body a closure is invoked through.
struct DeclaredByClosure {
  ClosureId closure;

  auto operator==(const DeclaredByClosure&) const -> bool = default;
};

// The body of a structure or a union answering one operation on its whole
// value (LRM 11.4.5). Which unit holds it is its record's.
struct DeclaredByRecord {
  RecordId record;
  support::ValueOperation answers;

  auto operator==(const DeclaredByRecord&) const -> bool = default;
};

using CallableOwner = std::variant<
    DeclaredByNamespace, DeclaredByClass, ConstructsClass, DeclaredByClosure,
    DeclaredByRecord>;

// A callable a declaration of the unit owns: who declares it and its code.
// Every SystemVerilog function and task, every process body, every synthesized
// lifecycle body, a class's constructor, a closure's body, a record's
// operation and both directions of the DPI-C boundary are this one concept,
// held in one table per unit.
//
// `code` carries the signature wherever this unit states one; its body is
// present exactly when this declaration also defines the callable. A pure
// virtual method (LRM 8.21) and a DPI-C import (LRM 35.4) are the two this unit
// states and does not define. The receiver is not a kind either: `code` names
// it where the body is entered on an object, and a static callable has none.
//
// Access is not stated here. A scope draws no access boundary at all --
// anything lexically inside a module reaches its subroutines, and a
// hierarchical name reaches them from outside it (LRM 23.8) -- and a class's
// own `local` / `protected` (LRM 8.9) is a source-declared, three-valued fact
// over members of every kind, which is a different thing than a callable-only
// flag.
//
// A callable carries no name. Its identity is its declaration's entry in the
// unit's table, which every callable has and which is already what a reference
// names; being reachable by a name is a separate relation, held by whatever
// answers that name (`NamedCallable` below). A body the source never wrote
// simply does not take part in it, rather than taking part with nothing in the
// slot -- and it needs no name of the compiler's own, which is as well, since a
// SystemVerilog identifier admits every printable character but white space
// (LRM 5.6.1) and so leaves no spelling reserved to mint one from.
struct CallableDecl {
  CallableOwner owner;
  CallableCode code;
};

// The part `callable` takes in its class's dispatch: what its class states of
// it, and none for a callable no class declares, since only a class dispatches.
[[nodiscard]] inline auto VirtualDispatchOf(const CallableDecl& callable)
    -> const std::optional<VirtualDispatchRole>& {
  static const std::optional<VirtualDispatchRole> kNone;
  const auto* of_class = std::get_if<DeclaredByClass>(&callable.owner);
  return of_class != nullptr ? of_class->virtual_dispatch : kNone;
}

// What a callable declaration is, read off its owner and its code rather than
// stated beside them: a body this unit defines; a function the foreign program
// defines (LRM 35.4), which is no body and a foreign linkage; a behavior a
// class declares and leaves to what extends it (LRM 8.21), which is no body and
// a pure dispatch role; or otherwise a body another unit of the program
// defines. The forms fall out of the combination, and every consumer reads
// them here.
struct DefinedHere {};
struct DefinedByAnotherUnit {};
struct LeftAbstract {};
struct DefinedByForeignCode {};

using CallableForm = std::variant<
    DefinedHere, DefinedByAnotherUnit, LeftAbstract, DefinedByForeignCode>;

[[nodiscard]] inline auto FormOf(const CallableDecl& callable) -> CallableForm {
  if (callable.code.body.has_value()) {
    return DefinedHere{};
  }
  return std::visit(
      Overloaded{
          [](const DeclaredByNamespace& owner) -> CallableForm {
            if (owner.foreign.has_value()) {
              return DefinedByForeignCode{};
            }
            return DefinedByAnotherUnit{};
          },
          [](const DeclaredByClass& owner) -> CallableForm {
            if (owner.virtual_dispatch.has_value() &&
                IsLeftAbstract(*owner.virtual_dispatch)) {
              return LeftAbstract{};
            }
            return DefinedByAnotherUnit{};
          },
          [](const ConstructsClass&) -> CallableForm {
            return DefinedByAnotherUnit{};
          },
          [](const DeclaredByClosure&) -> CallableForm {
            return DefinedByAnotherUnit{};
          },
          [](const DeclaredByRecord&) -> CallableForm {
            return DefinedByAnotherUnit{};
          }},
      callable.owner);
}

// Whether this unit holds the body of `callable`, which then becomes code of
// the unit.
[[nodiscard]] inline auto IsDefinedHere(const CallableDecl& callable) -> bool {
  return std::visit(
      Overloaded{
          [](const DefinedHere&) { return true; },
          [](const DefinedByAnotherUnit&) { return false; },
          [](const LeftAbstract&) { return false; },
          [](const DefinedByForeignCode&) { return false; }},
      FormOf(callable));
}

// One entry of the relation between a name space and the bodies it answers: the
// identifier written in the source, and the body it reaches. A class's methods
// and a namespace's subroutines are each such a relation, held by the class or
// the unit rather than by the bodies, so the bodies nothing names carry
// nothing.
struct NamedCallable {
  std::string name;
  CallableId body;
};

// The identifier `body` answers to among `named`, or nothing where nothing
// names it. Answering nothing is the answer, not a case to work around.
[[nodiscard]] inline auto NameOf(
    std::span<const NamedCallable> named, CallableId body)
    -> std::optional<std::string_view> {
  for (const NamedCallable& entry : named) {
    if (entry.body == body) {
      return std::string_view{entry.name};
    }
  }
  return std::nullopt;
}

// What a unit states about one foreign name whose entries sit on scopes (LRM
// 35.5.3): the prototype the name publishes, and the definition this unit
// writes for the program-global symbol.
//
// The definition is not one of the unit's own callables and is kept apart from
// them for the reason that decides everything else about it. The subroutine
// behind the name exists once per elaborated scope while the name is one symbol
// (LRM 35.4), so the symbol resolves an instance at call time instead of being
// one -- and a body doing that names the linkage name and the prototype and
// nothing the unit owns. Every unit declaring such a scope therefore writes the
// same text, and what reaches one definition is a merge rule rather than an
// owner. A name a unit's own namespace owns is the opposite on both counts: its
// definition calls that unit's subroutine, only that unit can write it, and it
// is a callable of the unit like any other.
struct ForeignScopeEntry {
  ForeignLinkage linkage;
  TypeId signature;
  CallableCode definition;
};

}  // namespace lyra::mir
