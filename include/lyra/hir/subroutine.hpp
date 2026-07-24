#pragma once

#include <optional>
#include <string>
#include <vector>

#include "lyra/hir/class_ref.hpp"
#include "lyra/hir/param_direction.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/subroutine_kind.hpp"
#include "lyra/hir/type_id.hpp"

namespace lyra::hir {

// A formal is also a local variable of the subroutine body; `var` indexes the
// body's procedural-var arena.
struct SubroutineParam {
  ProceduralVarId var = {};
  ParamDirection direction = ParamDirection::kInput;
};

// LRM 13.4.1 implicit result variable: a non-void function implicitly declares
// a body-local variable of the return type that the body reads and writes
// through the function name. `result_var` indexes the body's procedural-var
// arena for that variable; it is absent for void functions and tasks, which
// yield no value.
//
// Two facts on a class method carry its participation in virtual dispatch
// (LRM 8.20); each is atomic and reflects the source directly. `is_virtual`
// records the `virtual` keyword. `overrides`, when present, is the frontend-
// resolved reference to the base method this one overrides -- carried as an
// already-resolved identity so downstream consumers never repeat the name and
// signature matching the frontend has done.
//
// `is_prototype` records that the source declared this method as a pure
// virtual prototype (LRM 8.21 `pure virtual function ...;`) -- the signature
// exists but the source supplied no body. `body.procedural_vars` still
// carries the parameter var arena so a backend can emit the declaration,
// but every other body field (stmts, exprs, root_stmt, root_scope) is left
// at construction default and never consumed. This bit is source-level
// truth: a bodyless prototype and a legal empty body (LRM 8.21 note) are
// distinct forms that emptiness alone cannot separate. Only class methods
// carry it; free subroutines and processes always ship a body.
//
// `is_static` records that the source declared this method with the `static`
// keyword (LRM 8.10) -- the method has no receiver, cannot be virtual, and
// cannot be a constructor. At HIR-to-MIR this bit decides whether the lowered
// `CallableCode.params` prepends `self`: instance methods do, static methods
// omit it. Only class methods carry it; free subroutines and processes are
// never static in this sense.
struct SubroutineDecl {
  std::string name;
  SubroutineKind kind;
  TypeId result_type;
  std::vector<SubroutineParam> params;
  std::optional<ProceduralVarId> result_var;
  ProceduralBody body;
  bool is_virtual = false;
  bool is_prototype = false;
  bool is_static = false;
  std::optional<ClassMethodTarget> overrides;
};

}  // namespace lyra::hir
