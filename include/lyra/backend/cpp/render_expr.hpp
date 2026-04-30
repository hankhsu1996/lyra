#pragma once

#include <string>

#include "lyra/backend/cpp/render_context.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/mir/expr.hpp"

namespace lyra::backend::cpp {

auto RenderExpr(const RenderContext& ctx, const mir::Expr& expr)
    -> diag::Result<std::string>;

auto RenderLvalue(const RenderContext& ctx, const mir::Lvalue& target)
    -> diag::Result<std::string>;

auto RenderExprAsNative(const RenderContext& ctx, const mir::Expr& expr)
    -> diag::Result<std::string>;

}  // namespace lyra::backend::cpp
