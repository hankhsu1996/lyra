#pragma once

#include <optional>
#include <string>

#include <slang/ast/symbols/SubroutineSymbols.h>

#include "lyra/common/source_span.hpp"
#include "lyra/lowering/ast_to_hir/source_mapper.hpp"

namespace lyra::lowering::ast_to_hir {

// Result of checking whether a task body is provably non-suspending.
// Used by DPI export task admission (D7a) and reusable by future DPI import
// task admission (D7b) or any path that needs to distinguish suspending
// from non-suspending tasks.
struct NonSuspendingTaskCheckResult {
  bool ok = false;
  std::optional<SourceSpan> offending_span;
  std::string reason;
};

// Check whether a task body is provably non-suspending.
// Conservative by design: rejects any construct that cannot be proven
// immediate-safe. Walks the slang AST body checking for:
// - Direct timing controls (delay, event wait, event list)
// - Calls to task-shaped routines (may transitively suspend)
// False positives are acceptable; they are unlocked by suspending-task support.
auto CheckNonSuspendingTask(
    const slang::ast::SubroutineSymbol& sub, const SourceMapper& mapper)
    -> NonSuspendingTaskCheckResult;

}  // namespace lyra::lowering::ast_to_hir
