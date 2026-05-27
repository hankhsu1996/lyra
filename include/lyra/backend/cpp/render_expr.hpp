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

// Bare field name (no `.Get()` wrap) for callsites that need the Var place
// itself rather than its value, e.g. the trigger pointers inside `WaitAny`.
auto RenderStructuralVarName(
    const RenderContext& ctx, const mir::StructuralVarRef& ref)
    -> diag::Result<std::string>;

// Renders `expr` and, when its MIR type is a packed array, appends
// `.IsTruthy()`. Used by every statement that consumes an expression as a C++
// `bool` (`if`, `while`, `do`, `for` condition, etc.).
auto RenderConditionAsBool(const RenderContext& ctx, const mir::Expr& expr)
    -> diag::Result<std::string>;

}  // namespace lyra::backend::cpp
