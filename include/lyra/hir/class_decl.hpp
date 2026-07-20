#pragma once

#include <optional>
#include <string>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/hir/class_ref.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/field_id.hpp"
#include "lyra/hir/method_id.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/static_property_id.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/hir/type_id.hpp"

namespace lyra::hir {

// A class property (LRM 8.4): a named member variable of a class, with its
// own per-object storage. The declared `= value` initializer (LRM 8.7),
// when the source writes one, is a construction-protocol fact and lives on
// `ClassDecl.field_inits`, not on the property's declaration -- the two
// facets belong to different concerns.
struct ClassField {
  std::string name;
  TypeId type;
};

// A class static property (LRM 8.9): a named type-associated storage cell
// the class owns, shared by every instance. Distinct from `ClassField` (an
// instance member replicated per object) because a static property is one
// cell owned by the type, not a field the instance member set includes.
// The declared `= value` initializer, when the source writes one, is a
// design-init fact (LRM 10.5) and lives on `ClassDecl.static_property_inits`
// -- it runs once before any initial or always procedure, not inside the
// constructor.
struct ClassStaticProperty {
  std::string name;
  TypeId type;
};

// A source-written property initializer (LRM 8.7): the assignment that
// runs against the receiver during construction, before the user
// constructor body, on the property named by `target`. `value` lives in
// the constructor body's expression arena so an initializer that reads
// another property or a formal parameter reaches the same procedural var
// the body would.
struct FieldInit {
  FieldId target;
  ExprId value;
};

// A source-written static property initializer (LRM 8.9 / 10.5): the
// assignment that runs once at design init, before any initial or always
// procedure, on the static property named by `target`. Distinct from
// `FieldInit` because the timing is class-level program startup, not
// per-instance construction. `value` lives in the class's `static_init`
// body arena, an arena separate from the constructor body's, because the
// initializer cannot read a per-instance formal or self and must not share
// the constructor body's expression identities.
struct StaticPropertyInit {
  StaticPropertyId target;
  ExprId value;
};

// A construction-protocol fact (LRM 8.7): the arguments to forward to the
// base class's constructor before this class's own body runs. Its expressions
// live in the enclosing constructor body's expression arena, so a captured
// argument that reads a formal parameter reaches the same procedural var the
// body would. Absent means the source did not write an explicit `super.new`;
// with a base present, HIR-to-MIR still emits a stated base call (LRM 8.7
// implicit `super.new()`) so the ordering is never left to a backend
// convention. Empty (0-arg) means the source wrote `super.new()` explicitly.
struct BaseCall {
  std::vector<ExprId> arguments;
};

// A SystemVerilog class declaration (LRM 8). The class's properties and its
// instance methods; references to a class name resolve to this declaration's
// id. A class is reached through a handle, so it carries no structural
// position of its own. Each method (LRM 8.6) is a subroutine reached through
// the instance, reading the receiver and the class's properties through it.
//
// `is_interface_class` marks an `interface class` declaration (LRM 8.26): a
// class whose body carries only pure virtual method contracts -- no
// instance storage, no constructor, no static storage. Interface classes
// and regular classes share this one declaration form because their
// object-model shape coincides: an interface class is a regular class
// whose non-contract slots are empty by LRM 8.26 syntax. The bit is what
// consumers read to route emission and dispatch; the "which slots are
// empty" facts hold because the frontend admits only the LRM 8.26 syntax
// for a class flagged interface.
//
// `base` names the class this one extends (LRM 8.13), absent when the class
// extends no other. Only own members appear in `fields` / `methods`;
// inherited members are reached through the base's registry entry. An
// interface class carries no concrete base -- its parent interface classes
// live in `implements`.
//
// `implements` names the interface classes this one commits to satisfying
// (LRM 8.26.2): for a regular class, the source `implements` clause; for
// an interface class, the source `extends` clause, which aggregates parent
// interface class contracts into this one's requirement set. The two
// source keywords land in one field because at the object-model layer
// they name the same relation -- aggregate these interface classes' pure
// virtual method contracts. Multiple entries are allowed (multiple
// inheritance among interface class contracts is legal, LRM 8.26.2); the
// concrete-base single-value rule stays with `base`.
//
// `constructor` is the class's `new` (LRM 8.7). Every regular class has
// one: the user-written `function new` when the source declares it,
// otherwise a synthesized empty constructor, matching the implicit `new`
// the LRM provides when the source omits one. An interface class carries
// a synthesized stub that no consumer invokes -- an interface class
// object cannot be constructed (LRM 8.26.5).
//
// `base_call` peers with `constructor` because base-constructor forwarding
// is a class-level construction fact, not a statement inside the ctor body
// (LRM 8.7): source-written `super.new(args)` populates it, and any
// expression referencing the ctor's own formals lives in the same body
// arena.
//
// `field_inits` peers with `constructor` for the same reason: an initializer
// executes as part of the class's construction protocol before the user
// body's statements, and its expressions live in the constructor body's
// arena. Only fields the source declared with an explicit `= value` appear
// here; a field without one takes its type's Table 7-1 default at
// construction.
//
// `static_properties` peers with `fields` but stores type-associated cells
// (LRM 8.9), not instance members. Inherited static properties are reached
// through the base's registry entry, same as inherited instance fields.
//
// `static_init` is the class-level design-init body (LRM 10.5): the arena
// hosting each static property's initializer expression tree, kept separate
// from the constructor body because the initializer runs once at program
// startup, not per instance construction. It is a bare `ProceduralBody` --
// no `self`, no receiver -- because the code carries no per-instance context.
//
// `static_property_inits` pairs each source-written `= value` initializer
// with its static property in source order; expressions live in
// `static_init.exprs`. A static property without a source initializer takes
// its type's Table 7-1 default and does not appear here.
struct ClassDecl {
  std::string name;
  bool is_interface_class = false;
  std::optional<ClassRef> base;
  std::vector<ClassRef> implements;
  base::Arena<ClassField, FieldId> fields;
  base::Arena<ClassStaticProperty, StaticPropertyId> static_properties;
  base::Arena<SubroutineDecl, MethodId> methods;
  SubroutineDecl constructor;
  std::optional<BaseCall> base_call;
  std::vector<FieldInit> field_inits;
  ProceduralBody static_init;
  std::vector<StaticPropertyInit> static_property_inits;
};

}  // namespace lyra::hir
