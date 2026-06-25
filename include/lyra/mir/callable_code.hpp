#pragma once

#include <vector>

#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

// The code half of a callable: a signature plus an internal body. The
// signature is `params` (a prefix of `body.vars`, identified by `LocalId`) and
// `result_type` (the call protocol -- a coroutine type for a time-consuming
// task or process, a value or void type for a zero-time function -- and the
// completion payload). `params[0]` is always the receiver `self`; the remaining
// entries are the call-supplied or environment-bound formals in signature
// order. A backend looks each parameter's name and type up from `body.vars`.
//
// A directly-invoked callable receives every parameter from the caller. A
// callable value (a closure) binds a prefix of these into its environment and
// leaves the rest to be supplied per invocation; which parameter is realized as
// a passed argument versus a captured field is a backend choice, not encoded
// here.
struct CallableCode {
  std::vector<LocalId> params;
  TypeId result_type;
  Block body;
};

}  // namespace lyra::mir
