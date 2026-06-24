#pragma once

#include <string>

#include "lyra/backend/cpp/scope_view.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/mir/expr.hpp"

namespace lyra::backend::cpp {

auto RenderExpr(const ScopeView& view, const mir::Expr& expr)
    -> diag::Result<std::string>;

// Renders `expr` as an lvalue: bare root + element / range select suffixes.
// Throws InternalError on non-addressable forms. The cell's `Mutate(svc)`
// adapter (if any) is encoded explicitly in MIR as a
// `DerefExpr(CallExpr(ObservableMethod{kMutate}, ...))` node by HIR-to-MIR,
// so this render is purely mechanical -- it does not inject any wrapper.
auto RenderLhsExpr(const ScopeView& view, const mir::Expr& expr)
    -> diag::Result<std::string>;

// Renders `expr` for use in a C++ boolean context (`if`, `while`, `do`, `for`
// condition, ternary cond, `&&` / `||` / `!`). When the expression's MIR type
// is a packed array, `PackedArray`'s `explicit operator bool` fires on the
// contextual conversion; no explicit wrapping is needed.
auto RenderConditionAsBool(const ScopeView& view, const mir::Expr& expr)
    -> diag::Result<std::string>;

}  // namespace lyra::backend::cpp
