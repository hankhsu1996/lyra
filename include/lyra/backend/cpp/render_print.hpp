#pragma once

#include <string>

#include "lyra/backend/cpp/render_context.hpp"
#include "lyra/mir/expr.hpp"

namespace lyra::backend::cpp {

// Render a runtime call expression as a single C++ expression. Used from
// RenderExpr's RuntimeCallExpr arm; the surrounding RenderStmt::ExprStmt path
// adds the trailing `;` like for any other expression.
auto RenderRuntimeCallExpr(
    const RenderContext& ctx, const mir::RuntimeCallExpr& expr) -> std::string;

}  // namespace lyra::backend::cpp
