#pragma once

#include <vector>

#include "lyra/mir/expr_id.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::mir {

// LRM 21.3.4.3 $sscanf / $fscanf. `source_kind` selects the runtime entry
// and constrains `source`'s MIR type:
//   kString -> `source` is string-typed
//   kFile   -> `source` is int-typed (FD per LRM 21.3.1)
// `format` is always string-typed. `slots` reference the per-output-arg
// procedural temps the scanner writes through; the LRM 13.5 copy-out
// desugaring at the statement boundary copies each temp back to the
// user's actual lvalue after the call returns. Each slot's MIR-level
// type drives the backend's ScanSlot::Make overload selection.
struct RuntimeScanCall {
  support::ScanSourceKind source_kind{};
  ExprId source{};
  ExprId format{};
  std::vector<ExprId> slots;
};

}  // namespace lyra::mir
