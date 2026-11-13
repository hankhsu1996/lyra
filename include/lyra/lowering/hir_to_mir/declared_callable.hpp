#pragma once

#include "lyra/lowering/hir_to_mir/static_cell_binding.hpp"
#include "lyra/mir/callable_id.hpp"

namespace lyra::lowering::hir_to_mir {

// What a declaration scope settled about one of its body-bearing callables --
// a class method, a module task or function -- before any body lowered.
//
// `callable` is the identity a call resolves to, taken from the shape's own
// pool so a forward or mutual call reaches it whatever order the two bodies
// lower in (LRM 13.7). `statics` are the cells the callable keeps past a call
// -- its statements' assertion controls among them (LRM 20.11) -- which take
// their places on the same shape, so they too are settled before any body
// lowers.
//
// Both answer for the same callable and the body pass reads them against it
// together, so they are one entry rather than two sequences kept in step.
struct DeclaredCallable {
  mir::CallableId callable;
  StaticCellBindings statics;
};

}  // namespace lyra::lowering::hir_to_mir
