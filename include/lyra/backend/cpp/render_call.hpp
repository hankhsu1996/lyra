#pragma once

#include <string>

#include "lyra/backend/cpp/scope_view.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::backend::cpp {

// Renders a MIR call. The shape of the emitted call follows from the callee
// and from nothing else. The C++ spelling of each runtime entry is tabulated
// here too, so the expression dispatcher carries no per-entry naming.
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
