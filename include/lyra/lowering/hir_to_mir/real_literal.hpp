#pragma once

#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

// A real constant, as the construction that builds it. A real-family value is a
// runtime object wrapping a machine float, so what stands for a constant of one
// in MIR is the construction of that object over a machine float literal. The
// destination decides which machine float that is -- single precision for a
// `shortreal` (LRM 6.12), double for a `real` or a `realtime` -- so the literal
// carries its own precision in its type and every consumer reads it there.
//
// `real_type` must be a real-family type; nothing else is built this way.
[[nodiscard]] auto BuildRealLiteral(
    const mir::CompilationUnit& unit, mir::Block& block, mir::TypeId real_type,
    double value) -> mir::ExprId;

}  // namespace lyra::lowering::hir_to_mir
