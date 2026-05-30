#pragma once

#include <string>

#include "lyra/backend/cpp/render_context.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/mir/expr.hpp"

namespace lyra::backend::cpp {

auto RenderExpr(const RenderContext& ctx, const mir::Expr& expr)
    -> diag::Result<std::string>;

// Bare field name (no `.Get()` wrap) for callsites that need the Var place
// itself rather than its value, e.g. the trigger pointers inside `WaitAny`.
auto RenderStructuralVarName(
    const RenderContext& ctx, const mir::StructuralVarRef& ref)
    -> diag::Result<std::string>;

// Renders `expr` for use in a C++ boolean context (`if`, `while`, `do`, `for`
// condition, ternary cond, `&&` / `||` / `!`). When the expression's MIR type
// is a packed array, `PackedArray`'s `explicit operator bool` fires on the
// contextual conversion; no explicit wrapping is needed.
auto RenderConditionAsBool(const RenderContext& ctx, const mir::Expr& expr)
    -> diag::Result<std::string>;

}  // namespace lyra::backend::cpp
