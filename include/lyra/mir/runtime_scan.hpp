#pragma once

#include <vector>

#include "lyra/mir/expr_id.hpp"

namespace lyra::mir {

// LRM 21.3.4.3 $sscanf. `input` and `format` are string-typed expressions.
// `slots` are ExprIds pointing at the per-output-arg ProceduralVarRef temps
// that the scanner writes through; the surrounding BlockStmt (built by
// FinalizeOutputArgCallStmt) copies each temp back to the user's actual
// lvalue after the call returns. `slots[i]`'s MIR-level type is the
// underlying procedural var's type, which the backend uses to pick the
// matching ScanSlot::Make overload.
struct RuntimeSScanCall {
  ExprId input{};
  ExprId format{};
  std::vector<ExprId> slots;
};

}  // namespace lyra::mir
