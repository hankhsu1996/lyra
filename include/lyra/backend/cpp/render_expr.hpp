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

}  // namespace lyra::backend::cpp
