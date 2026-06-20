#pragma once

#include <string>

#include "lyra/backend/cpp/scope_view.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/mir/expr.hpp"

namespace lyra::backend::cpp {

// Render a runtime call expression as a single C++ expression. Used from
// RenderExpr's RuntimeCallExpr arm; the surrounding RenderStmt::ExprStmt path
// adds the trailing `;` like for any other expression.
auto RenderRuntimeCallExpr(
    const ScopeView& view, const mir::RuntimeCallExpr& expr)
    -> diag::Result<std::string>;

}  // namespace lyra::backend::cpp
