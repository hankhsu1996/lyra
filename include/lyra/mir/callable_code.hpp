#pragma once

#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

// The code half of a callable: a signature plus an internal body. The binding
// arena is the callable's, not a per-block one: `locals` holds every activation
// local and parameter of the whole body, and a `LocalRef` resolves against it
// directly, with no hops. `params` is a prefix of `locals` in signature order;
// for an instance callable `params[0]` is the receiver, absent for a static
// one. `result_type` is the call protocol -- a coroutine type for a
// time-consuming task or process, a value or void type for a zero-time
// function -- carrying the completion payload.
//
// A closure's invoke body has no `params[0]` receiver in its signature; instead
// its `locals[0]` is the closure receiver, a read-only borrow of the closure
// record, and a captured binding is read as a field access over it. A directly
// invoked callable receives every parameter from the caller; a closure binds
// its captured fields at construction and supplies the per-invocation `params`
// at each call. A backend reads each binding's name and type from `locals`.
struct CallableCode {
  std::vector<LocalId> params;
  TypeId result_type;
  base::Arena<LocalDecl, LocalId> locals;
  Block body;
};

}  // namespace lyra::mir
