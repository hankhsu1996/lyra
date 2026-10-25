#pragma once

#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/callable_id.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/foreign_linkage.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/callable_bounds.hpp"

namespace lyra::mir {

// A bound saying that the type a generic definition is given for one of its
// parameters is a closure, and how the definition uses it, as Rust's
// `F: Fn(A) -> R + 'static` does. `asks` is how the body calls it (to read,
// to change, or once), `params` and `result` are the types it is called with
// and answers, and `kept` is how long the body holds it (for the call, or past
// the caller). `result` and `params` may name another parameter of the same
// definition, as a maker's bound names the type it makes.
struct CalledClosureBound {
  support::ClosureKind asks;
  std::vector<TypeId> params;
  TypeId result;
  support::CallableHold kept;
};

// What a generic definition asks of the type it is handed for one of its
// parameters where it keeps deferred effects in it: that the type is a keeper
// of them, which hands out storage for an effect and commits the one made in
// it -- a Rust bound on a trait whose impls are the keeper types'.
struct KeeperBound {};

using ParameterBound = std::variant<CalledClosureBound, KeeperBound>;

// One type parameter of a generic definition, and what the definition asks of
// the type a call names for it, as a Rust `where` clause lists one bound after
// another.
struct TypeParameter {
  std::vector<ParameterBound> bounds;
};

// A named callable a class or a unit namespace owns. Every SystemVerilog
// function and task, every process body, every synthesized lifecycle body, and
// both directions of the DPI-C boundary are this one concept. What varies among
// them is stated by independent structure, never by a kind:
//
//   - `code` carries the signature wherever this unit states one; its body is
//     present exactly when this declaration also defines the callable. A pure
//     virtual method (LRM 8.21) and a DPI-C import (LRM 35.4) are the two this
//     unit states and does not define.
//   - `foreign`, when present, is the C linkage the callable is reached under.
//     It is orthogonal to the body: bodyless plus foreign is an import the
//     user's C defines, bodied plus foreign is the entry point of an export the
//     user's C calls. A foreign name is program-global and belongs to no class
//     (LRM 35.4, 35.7), so only a unit's own callables ever carry one.
//   - `virtual_dispatch`, when present, states this callable's role in the
//     class's dispatch table (LRM 8.20) -- introducing a new slot or overriding
//     an ancestor's -- so a backend renders the marker off stated structure,
//     never re-deriving virtualness by name. It is absent for a direct-only
//     callable: every static callable and every foreign one.
//   - The receiver is not a kind either: an instance method carries `self` as
//     its first parameter and a static callable omits it, which the signature
//     already says.
//   - `defining_unit`, where present, names the unit whose namespace a
//     subroutine of another unit is a member of; such a callable's `code`
//     states nothing, its signature being that unit's too. Every entry another
//     unit defines -- a class, a record, a variable -- says so the same way. A
//     method of a class another unit defines states nothing of its own: its
//     class says which unit defines it.
//
// Access is not stated here. A scope draws no access boundary at all --
// anything lexically inside a module reaches its subroutines, and a
// hierarchical name reaches them from outside it (LRM 23.8) -- and a class's
// own `local` / `protected` (LRM 8.9) is a source-declared, three-valued fact
// over members of every kind, which is a different thing than a callable-only
// flag.
//
// A callable carries no name. Its identity is the position its declaration
// sits at, which every callable has and which is already what a reference
// names; being reachable by a name is a separate relation, held by
// whatever answers that name (`NamedCallable` below). A body the source never
// wrote simply does not take part in it, rather than taking part with nothing
// in the slot -- and it needs no name of the compiler's own, which is as well,
// since a SystemVerilog identifier admits every printable character but white
// space (LRM 5.6.1) and so leaves no spelling reserved to mint one from.
struct CallableDecl {
  CallableCode code;
  std::optional<ForeignLinkage> foreign;
  std::optional<VirtualDispatchRole> virtual_dispatch;
  // The types the definition is generic over, numbered by position; a body
  // names one as a type parameter type, and a call names the type it is at.
  std::vector<TypeParameter> type_parameters;
  std::optional<std::string> defining_unit;
};

// What a callable declaration is, read off the facts above rather than stated
// beside them: a body this unit defines; a function the foreign program
// defines (LRM 35.4), which is no body and a foreign linkage; a behavior a
// class declares and leaves to what extends it (LRM 8.21), which is no body
// and a pure dispatch role; or otherwise a body another unit of the program
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
  if (callable.foreign.has_value()) {
    return DefinedByForeignCode{};
  }
  if (callable.virtual_dispatch.has_value() &&
      IsLeftAbstract(*callable.virtual_dispatch)) {
    return LeftAbstract{};
  }
  return DefinedByAnotherUnit{};
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

// The body `name` reaches among `named`, or nothing where this name space
// answers no such identifier. The relation read the other way: a reference
// written inside the declaring unit arrives carrying what the source spelled,
// and what it names is a position in that unit's own arena.
[[nodiscard]] inline auto CallableNamed(
    std::span<const NamedCallable> named, std::string_view name)
    -> std::optional<CallableId> {
  for (const NamedCallable& entry : named) {
    if (entry.name == name) {
      return entry.body;
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
