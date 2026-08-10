#pragma once

#include <string>

#include "lyra/backend/cpp/scope_view.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::backend::cpp {

// Renders a MIR `CallExpr`: dispatches on the callee variant to one of the
// `Builtin` / `FreeFn` / `Static` / `Constructor` / `Method` / `Closure` call
// shapes. The C++ identifier table for `BuiltinFn` lives in this file as well
// so the expression-render dispatcher stays free of the per-id naming detail.
auto RenderCallExpr(
    const ScopeView& view, const mir::CallExpr& call, mir::TypeId result_type)
    -> std::string;

// Renders a `CallExpr` in write position, where the call names a place: a
// callee that hands an argument back unchanged yields the place that argument
// names, so that argument renders as a place rather than as a value.
auto RenderLhsCallExpr(
    const ScopeView& view, const mir::CallExpr& call, mir::TypeId result_type)
    -> std::string;

}  // namespace lyra::backend::cpp
