#pragma once

#include <vector>

#include "lyra/mir/closure_id.hpp"
#include "lyra/mir/field.hpp"

namespace lyra::mir {

// Construction of a closure value -- the lambda (`[caps](args){body}`), the way
// a `ClosureType` is built. `closure` names the declaration (its capture fields
// and its invoke body) in the unit's closure registry; `field_inits` supplies
// each captured field's value in evaluation order. The `Expr::type` is the
// `ClosureType` naming `closure`.
//
// Distinct from `StructConstructExpr` because a closure is a distinct type
// category: its declaration carries an invoke body, and a backend realizes it
// as an anonymous callable object (a C++ lambda whose captures are the fields
// `[field = init]`), or a coroutine closure passing those fields as
// frame-copied parameters supplied by an immediate call so nothing dangles once
// a spawned branch outlives the construction site.
struct ClosureExpr {
  ClosureId closure{};
  std::vector<FieldInit> field_inits;
};

}  // namespace lyra::mir
