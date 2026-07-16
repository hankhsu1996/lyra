#pragma once

#include <optional>
#include <string>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/hir/class_id.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/method_id.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/hir/type_id.hpp"

namespace lyra::hir {

// A class property (LRM 8.4): a named member variable of a class, with its own
// per-object storage. `initializer`, when present, is the declared `= value`
// expression (LRM 8.7): during construction the property is set to this value
// -- which may read another property through the receiver -- before the user
// constructor body runs; absent means the property takes its type's Table 7-1
// default. Property initialization is part of the constructor's execution, so
// the expression is held in the constructor body's expression arena.
struct ClassField {
  std::string name;
  TypeId type;
  std::optional<ExprId> initializer;
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
// synthesized empty constructor, matching the implicit `new` the LRM provides
// when the source omits one. Property initializers are evaluated as part of
// its execution, so their expressions live in its body's expression arena.
struct ClassDecl {
  std::string name;
  std::optional<ClassId> base;
  std::vector<ClassField> fields;
  base::Arena<SubroutineDecl, MethodId> methods;
  SubroutineDecl constructor;
};

}  // namespace lyra::hir
