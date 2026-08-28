#pragma once

#include <vector>

#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// Builds the value a source-level packed join denotes (LRM 11.4.12), given the
// runs it joins in most-significant-first order and the type it lands in.
//
// One run is not a join: the value is that run seen at the join's own type,
// which LRM 11.8.1 fixes as unsigned however the run was declared. Saying so
// here is what lets the join primitive mean a join, so every consumer of it
// reads two or more operands and none carries the degenerate case.
//
// `block` is the destination scope for any intermediate expression this
// interns.
[[nodiscard]] auto BuildPackedConcat(
    mir::CompilationUnit& unit, mir::Block& block,
    std::vector<mir::ExprId> runs, mir::TypeId result_type) -> mir::Expr;

}  // namespace lyra::lowering::hir_to_mir
