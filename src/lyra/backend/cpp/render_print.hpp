#pragma once

#include <string>

#include "lyra/mir/expr.hpp"
#include "render_context.hpp"

namespace lyra::backend::cpp {

// Render a runtime call expression as a single C++ expression. Used from
// RenderExpr's RuntimeCallExpr arm; the surrounding RenderStmt::ExprStmt path
// adds the trailing `;` like for any other expression.
auto RenderRuntimeCallExpr(
    const RenderContext& ctx, const mir::RuntimeCallExpr& expr) -> std::string;

}  // namespace lyra::backend::cpp
