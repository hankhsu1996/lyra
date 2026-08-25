#pragma once

#include "lyra/lowering/hir_to_mir/static_var_binding.hpp"
#include "lyra/mir/callable_id.hpp"

namespace lyra::lowering::hir_to_mir {

// What a declaration scope settled about one of its body-bearing callables --
// a class method, a module task or function -- before any body lowered.
//
// `callable` is the identity a call resolves to, taken from the shape's own
// pool so a forward or mutual call reaches it whatever order the two bodies
// lower in (LRM 13.7). `statics` is the per-instance storage the callable's
// static-lifetime locals were given (LRM 13.3.1), settled just as early
// because a peer can name one of them too.
//
// Both answer for the same callable and the body pass reads them against it
// together, so they are one entry rather than two sequences kept in step.
struct DeclaredCallable {
  mir::CallableId callable;
  StaticVarBindings statics;
};

}  // namespace lyra::lowering::hir_to_mir
