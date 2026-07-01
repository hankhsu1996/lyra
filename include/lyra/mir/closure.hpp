#pragma once

#include <vector>

#include "lyra/mir/closure_record_id.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/member.hpp"

namespace lyra::mir {

// One field of a closure record's construction: which field (`target`, a stable
// `MemberId`) receives which value (`value`). The value is a pure read of an
// already-materialized capture -- a side-effecting capture source is sequenced
// into a local before the closure is constructed, so the order of these entries
// is a physical-layout order (canonical, by binding origin), independent of the
// order the sources were evaluated in.
struct FieldInit {
  MemberId target;
  ExprId value;
};

// A closure value expression: the construction of a per-closure-site nominal
// value record. `record` names the declaration (its fields and its one invoke
// body) in the unit's closure-record registry; `field_inits` supplies each
// captured field's value in canonical layout order. The `Expr::type` is the
// `ClosureRecordType` naming `record`.
//
// This node is only the construction; the invoke body lives on the record, not
// here. Closures are synthesized exclusively by HIR-to-MIR lowering -- no
// SystemVerilog source construct produces one directly.
//
// A backend realizes the value from the record's fields and this construction:
// the C++ backend as a lambda whose captures are the record's fields (`[field =
// init]`), a coroutine closure passing those fields as frame-copied parameters
// supplied by an immediate call so nothing dangles once a spawned branch
// outlives the construction site. The capture list is derived solely from the
// record's fields and layout, never re-inferred from the body.
struct ClosureExpr {
  ClosureRecordId record{};
  std::vector<FieldInit> field_inits;
};

}  // namespace lyra::mir
