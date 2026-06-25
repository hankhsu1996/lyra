#pragma once

#include <optional>
#include <string>

#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/class_ref.hpp"

namespace lyra::mir {

// A named, class-level callable: callable code plus a name. Every SystemVerilog
// function and task, every process body, and the synthesized lifecycle bodies
// are this one concept, distinguished only by how a referencing site uses them.
// The signature -- the parameter list (with `self` at `code.params[0]`) and the
// result type that carries the call protocol and completion payload -- lives in
// `code`, so the backend reads task-versus-function and the output pack from
// `code.result_type`, not a side enum. The body is uniform across every
// callable: a static function over the explicit receiver `self`. How a
// referencing site reaches the callable -- a direct call, a constructor-time
// process registration, an engine-dispatched lifecycle hook -- is the
// referencing site's concern, realized as separate dispatch plumbing. The
// declaration records one dispatch fact of its own, the base method it
// overrides when it overrides one, which leaves the uniform body untouched.
struct MethodDecl {
  std::string name;
  CallableCode code;
  // The base method this declaration overrides, resolved to a declaration
  // reference, or absent for a method that introduces no override. A lifecycle
  // body overrides the runtime base's matching hook; a backend reads the
  // override target here rather than re-deriving it from the method name.
  std::optional<OverriddenMethodRef> overrides;
};

}  // namespace lyra::mir
