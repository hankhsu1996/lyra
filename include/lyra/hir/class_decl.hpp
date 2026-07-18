#pragma once

#include <optional>
#include <string>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/hir/class_id.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/field_id.hpp"
#include "lyra/hir/method_id.hpp"
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
// `base` names the class this one extends (LRM 8.13), absent when the class
// extends no other. Only own members appear in `fields` / `methods`;
// inherited members are reached through the base's registry entry.
//
// `constructor` is the class's `new` (LRM 8.7). Every class has one: the
// user-written `function new` when the source declares it, otherwise a
// synthesized empty constructor, matching the implicit `new` the LRM
// provides when the source omits one.
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
struct ClassDecl {
  std::string name;
  std::optional<ClassId> base;
  base::Arena<ClassField, FieldId> fields;
  base::Arena<SubroutineDecl, MethodId> methods;
  SubroutineDecl constructor;
  std::optional<BaseCall> base_call;
  std::vector<FieldInit> field_inits;
};

}  // namespace lyra::hir
