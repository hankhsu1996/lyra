#pragma once

#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/base/overloaded.hpp"
#include "lyra/hir/namespace.hpp"
#include "lyra/hir/param_direction.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/subroutine_id.hpp"
#include "lyra/hir/subroutine_kind.hpp"
#include "lyra/hir/timing.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/hir/value_ref.hpp"

namespace lyra::hir {

// The variable a formal is, which a call writes what it hands in to and takes
// what it hands back from (LRM 13.5): a variable of the subroutine's body, or,
// for a static subroutine of a package or the compilation-unit scope, one of
// the variables that namespace keeps (LRM 13.3.2, 26.2), which every name
// reaching it -- the body's own included -- names the same way.
using FormalVariable = std::variant<ProceduralVarId, NamespaceValueRef>;

// A formal as its subroutine declares it: the variable a call binds at its
// position, how it binds it, and `default_evaluator` -- for a subroutine a
// structural scope declares, the subroutine of that scope evaluating the
// formal's default (LRM 13.5.3), which a call omitting the formal calls; absent
// for a formal with no default, and for a method, whose class states its
// defaults with the rest of what a call reads of it.
struct SubroutineParam {
  FormalVariable variable;
  ParamDirection direction = ParamDirection::kInput;
  std::optional<StructuralSubroutineId> default_evaluator;

  auto operator==(const SubroutineParam&) const -> bool = default;
};

// LRM 13.4.1 implicit result variable: a non-void function implicitly declares
// a body-local variable of the return type that the body reads and writes
// through the function name. `result_var` indexes the body's procedural-var
// arena for that variable; it is absent for void functions and tasks, which
// yield no value, and for a function whose result stands as the actual of a
// formal binding a place, whose one return names the place and which no
// variable could hold.
//
// `result_direction` is the direction of the formal whose actual the result
// stands as: `input` for every function the source writes, whose result the
// call takes as a value, and the formal's own for the function the compiler
// makes for a formal's default (LRM 13.5.3), whose call binds the result as it
// binds a written actual of that direction -- the place to write back into for
// an `output` or `inout`, the storage itself for a `ref`.
//
// A class method's part in dispatch (LRM 8.20) is what the class declares of
// it, not a fact of its body, so no body carries one.
//
// `root_stmt` is the statement a call enters at, absent for a method the
// source declared as a pure virtual prototype (LRM 8.21 `pure virtual function
// ...;`) and for a subroutine an interface leaves to a connected module to
// define (LRM 25.7), each of which has a signature and no body. That is
// distinct from a legal empty body (LRM 8.21 note), which has a statement to
// enter at. Its formals are still declared, so a backend can emit the
// declaration. Whether a method has a receiver (LRM 8.10) is what its class
// declares of it, not a fact of its body.
//
// `reads` is what a call of a function can read (LRM 9.4.2), stated in terms of
// its own formals and the object it runs on so a wait calling it can ask. A
// task is never called from an expression and states nothing.
//
// `track_caller` is whether a failure of the design this body reports is
// placed at the call that reached it rather than at the body's own lines,
// which is so for a body no design wrote: one of the tool's own text, and the
// one the tool writes for a subroutine an interface leaves to a connected
// module (LRM 25.7). It says of the body what the callee interface says of a
// call to it.
//
// `span` is where the subroutine is declared, which a body the tool writes for
// a declaration with none of its own -- one an interface leaves to a connected
// module (LRM 25.7) -- is lowered from.
struct SubroutineDecl {
  std::string name;
  SubroutineKind kind;
  TypeId result_type;
  ParamDirection result_direction = ParamDirection::kInput;
  std::vector<SubroutineParam> params;
  std::optional<ProceduralVarId> result_var;
  ProceduralBody body;
  std::optional<StmtId> root_stmt;
  Reads reads;
  bool track_caller = false;
  diag::SourceSpan span;

  auto operator==(const SubroutineDecl&) const -> bool = default;
};

// The type of the variable the formal `param` of `decl` binds: the body's own
// variable's, or the one a namespace in `namespaces` declares for a static
// subroutine's formal.
inline auto FormalTypeOf(
    const NamespaceRegistry& namespaces, const SubroutineDecl& decl,
    const SubroutineParam& param) -> TypeId {
  return std::visit(
      Overloaded{
          [&](ProceduralVarId var) {
            return decl.body.procedural_vars.Get(var).type;
          },
          [&](const NamespaceValueRef& kept) {
            return namespaces.VariableOf(kept.owner, kept.variable).type;
          }},
      param.variable);
}

}  // namespace lyra::hir
