#pragma once

#include <string>

#include "lyra/mir/callable_code.hpp"

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
// referencing site's concern, realized as separate dispatch plumbing, not a
// property carried on the body.
struct MethodDecl {
  std::string name;
  CallableCode code;
};

}  // namespace lyra::mir
