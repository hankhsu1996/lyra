#pragma once

#include "lyra/mir/builder.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

// A real constant, a floating literal of `real_type`, whose width -- single
// precision for a `shortreal`, double for a `real` or a `realtime` (LRM 6.12)
// -- is the precision every consumer reads it at.
//
// `real_type` must be a real type; nothing else is built this way.
[[nodiscard]] auto BuildRealLiteral(
    const mir::CompilationUnit& unit, mir::Builder block, mir::TypeId real_type,
    double value) -> mir::ExprId;

}  // namespace lyra::lowering::hir_to_mir
