#pragma once

#include <string>
#include <string_view>

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

// Renders `expr` as an lvalue: bare root + optional `mutate_adapter`
// (e.g. `.Mutate(svc)` for partial-write chains) + element / range select
// suffixes. Throws InternalError on non-addressable forms.
auto RenderLhsExpr(
    const RenderContext& ctx, const mir::Expr& expr,
    std::string_view mutate_adapter) -> diag::Result<std::string>;

// Reports whether the lvalue chain `expr` ultimately writes through an
// observable cell (a `mir::ObservableType`-wrapped structural var or an
// extern-ref slot). Sites that need to bind the cell for `Var<T>::Mutate`
// dispatch consult this before choosing the lvalue render path.
[[nodiscard]] auto LhsRootIsObservableScalar(
    const RenderContext& ctx, const mir::Expr& expr) -> bool;

// Renders `expr` for use in a C++ boolean context (`if`, `while`, `do`, `for`
// condition, ternary cond, `&&` / `||` / `!`). When the expression's MIR type
// is a packed array, `PackedArray`'s `explicit operator bool` fires on the
// contextual conversion; no explicit wrapping is needed.
auto RenderConditionAsBool(const RenderContext& ctx, const mir::Expr& expr)
    -> diag::Result<std::string>;

}  // namespace lyra::backend::cpp
