#pragma once

#include <vector>

#include "lyra/mir/closure_id.hpp"
#include "lyra/mir/field.hpp"

namespace lyra::mir {

// Construction of a closure value -- the lambda (`[caps](args){body}`).
// `closure` names the declaration (its capture fields and its invoke body) in
// the unit's closure registry; `field_inits` supplies each captured field's
// value in evaluation order.
//
// What is constructed follows from the invoke's protocol, and `Expr::type`
// states it. A synchronous invoke constructs a callable object -- a
// `ClosureType` naming `closure`, invoked through an `Indirect` call. A
// coroutine invoke constructs the coroutine: its captures are frame-copied at
// construction so nothing dangles once a spawned branch outlives the site,
// which makes constructing it inseparable from starting it, so the type is the
// coroutine and the site awaits or spawns it with no call in between.
//
// Distinct from `StructConstructExpr` because a closure is a distinct type
// category: its declaration carries an invoke body.
struct ClosureExpr {
  ClosureId closure{};
  std::vector<FieldInit> field_inits;
};

}  // namespace lyra::mir
