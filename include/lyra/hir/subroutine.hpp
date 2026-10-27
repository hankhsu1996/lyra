#pragma once

#include <optional>
#include <string>
#include <vector>

#include "lyra/hir/class_ref.hpp"
#include "lyra/hir/param_direction.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/subroutine_id.hpp"
#include "lyra/hir/subroutine_kind.hpp"
#include "lyra/hir/timing.hpp"
#include "lyra/hir/type_id.hpp"

namespace lyra::hir {

// A formal is also a local variable of the subroutine body; `var` indexes the
// body's procedural-var arena. `default_evaluator` is, for a subroutine a
// structural scope declares, the subroutine of that scope evaluating the
// formal's default (LRM 13.5.3), which a call omitting the formal calls; it is
// absent for a formal with no default, and for a method, whose class states
// its defaults with the rest of what a call reads of it.
struct SubroutineParam {
  ProceduralVarId var = {};
  ParamDirection direction = ParamDirection::kInput;
  std::optional<StructuralSubroutineId> default_evaluator;

  auto operator==(const SubroutineParam&) const -> bool = default;
};

// LRM 13.4.1 implicit result variable: a non-void function implicitly declares
// a body-local variable of the return type that the body reads and writes
// through the function name. `result_var` indexes the body's procedural-var
// arena for that variable; it is absent for void functions and tasks, which
// yield no value.
//
// A class method's part in dispatch (LRM 8.20) is what the class declares of
// it, not a fact of its body, so no body carries one.
//
// `root_stmt` is the statement a call enters at, absent for a method the
// source declared as a pure virtual prototype (LRM 8.21 `pure virtual function
// ...;`), which has a signature and no body. That is distinct from a legal
// empty body (LRM 8.21 note), which has a statement to enter at. The
// parameters are still in `body.procedural_vars` and their scope in
// `body.root_scope`, so a backend can emit the declaration. Whether a method
// has a receiver (LRM 8.10) is what its class declares of it, not a fact of
// its body.
//
// `reads` is what a call of a function can read (LRM 9.4.2), stated in terms of
// its own formals and the object it runs on so a wait calling it can ask. A
// task is never called from an expression and states nothing.
struct SubroutineDecl {
  std::string name;
  SubroutineKind kind;
  TypeId result_type;
  std::vector<SubroutineParam> params;
  std::optional<ProceduralVarId> result_var;
  ProceduralBody body;
  std::optional<StmtId> root_stmt;
  Reads reads;

  auto operator==(const SubroutineDecl&) const -> bool = default;
};

}  // namespace lyra::hir
