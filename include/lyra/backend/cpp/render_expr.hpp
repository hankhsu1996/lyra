#pragma once

#include <string>

#include "lyra/backend/cpp/render_context.hpp"
#include "lyra/mir/expr.hpp"

namespace lyra::backend::cpp {

auto RenderExpr(const RenderContext& ctx, const mir::Expr& expr) -> std::string;

auto RenderLvalue(const RenderContext& ctx, const mir::Lvalue& target)
    -> std::string;

}  // namespace lyra::backend::cpp
