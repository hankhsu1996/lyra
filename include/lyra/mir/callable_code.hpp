#pragma once

#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/mir/capture_id.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

// The code half of a callable: a signature plus an internal body. The binding
// arena is the callable's, not a per-block one: `locals` holds every
// activation local and parameter of the whole body (a `LocalRef` resolves
// against it directly, with no hops), and `captures` holds the environment
// fields a closure value supplies at construction (a `CaptureRef` resolves
// against it). `params` is a prefix of `locals` in signature order;
// `params[0]` is the receiver `self` for an instance callable, absent for a
// static one. `result_type` is the call protocol -- a coroutine type for a
// time-consuming task or process, a value or void type for a zero-time
// function -- carrying the completion payload.
//
// A directly-invoked callable receives every parameter from the caller and has
// no captures. A closure binds its `captures` at construction and supplies the
// non-prefix `params` per invocation. A backend reads each binding's name and
// type from the two arenas.
struct CallableCode {
  std::vector<LocalId> params;
  TypeId result_type;
  base::Arena<LocalDecl, LocalId> locals;
  base::Arena<LocalDecl, CaptureId> captures;
  Block body;
};

}  // namespace lyra::mir
