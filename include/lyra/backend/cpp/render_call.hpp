#pragma once

#include "lyra/backend/cpp/scope_view.hpp"
#include "lyra/backend/cpp/target_text.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::backend::cpp {

// Writes a MIR call. Its form follows from the callee alone, and a runtime
// function is spelled as the runtime's shared declaration of it says.
void RenderCallExpr(
    const ScopeView& view, const mir::CallExpr& call, mir::TypeId result_type,
    TargetText& out);

}  // namespace lyra::backend::cpp
