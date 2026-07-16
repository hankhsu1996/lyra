#pragma once

#include <cstdint>
#include <optional>
#include <string>

#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/class_ref.hpp"

namespace lyra::mir {

// Whether a method is part of the object's externally callable surface or an
// internal mechanism. A class instance method (LRM 8.6) is callable through a
// handle, so it is public; a scope's processes, lifecycle hooks, and helper
// subroutines are reached only by the owning runtime or by the scope's own
// bodies, so they are internal. A backend reads which access a method has from
// here rather than inferring it from the object's base or the method's role.
enum class MethodVisibility : std::uint8_t { kPublic, kInternal };

// A named class instance-method callable (LRM 8.6). Every SystemVerilog
// function and task, every process body, and the synthesized lifecycle body
// is one of these -- callables whose receiver is the enclosing object,
// bound implicitly at the call site. The signature -- the parameter list
// (with `self` at `code.params[0]`) and the result type carrying the call
// protocol -- lives in `code`, so a backend reads task-versus-function and
// the output pack from `code.result_type`, not a side enum. `visibility`
// states whether the method is part of the object's externally callable
// surface. `virtual_dispatch`, when present, states this method's role in
// the class's dispatch table -- either introducing a new slot or filling an
// ancestor's -- so a backend renders the introduction or override marker
// off stated structure, never re-deriving virtualness by name matching.
struct MethodDecl {
  std::string name;
  CallableCode code;
  std::optional<VirtualDispatchRole> virtual_dispatch;
  MethodVisibility visibility;
};

// A named class-owned callable whose identity is a plain function pointer
// the runtime library holds and calls back through -- the shape a
// `void (*)(RuntimeScopeBase*)` lifecycle hook requires. Structurally a
// distinct callable species from `MethodDecl`: its receiver is an explicit
// parameter (never bound implicitly), it participates in no dispatch table,
// and it is reached only as a code address, never through a CallExpr. A
// backend renders it in the target language's function-pointer-compatible
// form, which is not the form an instance method takes.
struct AbiAdapter {
  std::string name;
  CallableCode code;
};

}  // namespace lyra::mir
