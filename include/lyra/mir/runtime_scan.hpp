#pragma once

#include <vector>

#include "lyra/mir/expr_id.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::mir {

// LRM 21.3.4.3 $sscanf / $fscanf. `source_kind` selects the runtime entry
// and constrains `source`'s MIR type:
//   kString -> `source` is string-typed
//   kFile   -> `source` is int-typed (FD per LRM 21.3.1)
// `format` is always string-typed. Each slot is a bare lvalue (Structural
// or Procedural var ref) bound by `ScanSlot::Make` at the backend; the
// runtime writes through it directly (observable for structural vars via
// `Var::Set`). Each slot's MIR-level type drives the backend's overload
// selection.
struct RuntimeScanCall {
  support::ScanSourceKind source_kind{};
  ExprId source{};
  ExprId format{};
  std::vector<ExprId> slots;
};

}  // namespace lyra::mir
