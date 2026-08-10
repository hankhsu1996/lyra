#pragma once

#include <string>

#include "lyra/backend/cpp/scope_view.hpp"
#include "lyra/mir/expr.hpp"

namespace lyra::backend::cpp {

auto RenderExpr(const ScopeView& view, const mir::Expr& expr) -> std::string;

// Renders `expr` as an lvalue: bare root + element / range select suffixes.
// Throws InternalError on non-addressable forms. Where the target reaches
// through a capability wrapper, MIR says so with a dereference and the
// wrapper's own write protocol comes from the place-access dispatch on its
// type, so this render decides nothing.
auto RenderLhsExpr(const ScopeView& view, const mir::Expr& expr) -> std::string;

}  // namespace lyra::backend::cpp
