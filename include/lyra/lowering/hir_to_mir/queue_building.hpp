#pragma once

#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// An unbounded queue of `element` (LRM 7.10).
[[nodiscard]] auto QueueOf(mir::CompilationUnit& unit, mir::TypeId element)
    -> mir::TypeId;

// Appends to `block` the statement adding `value`, brought to the element type,
// to the back of the queue held in `queue`, which then holds it to its bound
// (LRM 7.10.2.7).
void AppendPushBack(
    UnitLowerer& unit_lowerer, mir::Builder block, mir::LocalId queue,
    mir::TypeId queue_type, mir::ExprId value);

}  // namespace lyra::lowering::hir_to_mir
