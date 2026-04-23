#pragma once

#include <string>

#include "lyra/mir/expr.hpp"
#include "render_context.hpp"

namespace lyra::projection::cpp {

auto RenderExpr(const RenderContext& ctx, const mir::Expr& expr) -> std::string;

}  // namespace lyra::projection::cpp
