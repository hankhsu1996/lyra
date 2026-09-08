#pragma once

#include <span>
#include <string>
#include <vector>

#include "lyra/backend/cpp/scope_view.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"

namespace lyra::backend::cpp {

auto RenderExpr(const ScopeView& view, const mir::Expr& expr) -> std::string;

// The renders of `operands`, in order. An entry emitting a list of them
// composes these pieces with the punctuation that separates them, rather than
// walking the operands and interleaving the two.
auto RenderEachExpr(
    const ScopeView& view, std::span<const mir::ExprId> operands)
    -> std::vector<std::string>;

// Renders `expr` as the target of a write, and throws InternalError on a form
// that names none. Where the target reaches through a capability wrapper, MIR
// says so with a dereference and the wrapper's own write protocol comes from
// the place-access dispatch on its type, so this render decides nothing.
auto RenderLhsExpr(const ScopeView& view, const mir::Expr& expr) -> std::string;

}  // namespace lyra::backend::cpp
