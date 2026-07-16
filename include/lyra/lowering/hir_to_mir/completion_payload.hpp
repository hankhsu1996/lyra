#pragma once

#include <cstddef>
#include <vector>

#include "lyra/hir/subroutine.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// The completion-payload component types of a subroutine, in payload order: the
// function return (when the subroutine is a non-void function) followed by each
// `output` / `inout` formal in declaration order. The single source of truth
// the callee body and every call site share, so neither re-derives the layout
// independently (LRM 13.5).
auto CompletionComponentTypes(
    const UnitLowerer& unit_lowerer, const hir::SubroutineDecl& decl)
    -> std::vector<mir::TypeId>;

// Normalizes a completion payload by component count: no component is `Void`,
// one is that component's bare type, two or more a tuple of them.
auto NormalizeCompletionPayload(
    mir::CompilationUnit& unit, const std::vector<mir::TypeId>& components)
    -> mir::TypeId;

// Reads one payload component out of a completion value bound to `completion`,
// in the shape a normalized payload takes: a single-component payload is the
// bare value, a multi-component payload a tuple the component is read from by
// index. The one place the read-back encoding lives, so a call site projecting
// outputs never re-derives whether the value is bare or a tuple.
auto ProjectCompletionComponent(
    mir::Block& block, mir::LocalId completion, mir::TypeId payload_type,
    std::size_t component_count, std::size_t index, mir::TypeId component_type)
    -> mir::ExprId;

}  // namespace lyra::lowering::hir_to_mir
