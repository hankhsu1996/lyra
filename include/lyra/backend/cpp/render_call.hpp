#pragma once

#include "lyra/backend/cpp/scope_view.hpp"
#include "lyra/backend/cpp/target_text.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::backend::cpp {

// Writes a MIR call. Its form follows from the callee alone, and a runtime
// function is spelled as the runtime's shared declaration of it says.
void RenderCallExpr(
    const ScopeView& view, const mir::CallExpr& call, mir::TypeId result_type,
    TargetText& out);

// Writes a call to a runtime entry that realizes something MIR states as
// structure rather than as a call, so there is no call node to render: the
// entry acts on nothing and takes nothing, and is spelled as its shared
// declaration says, like every other.
void RenderStructuralCall(
    const ScopeView& view, support::BuiltinFn fn, mir::TypeId result_type,
    TargetText& out);

}  // namespace lyra::backend::cpp
