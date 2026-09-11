#pragma once

#include "lyra/hir/structural_data_object.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/strength_level.hpp"

namespace lyra::lowering::hir_to_mir {

// What a net's declared net type states (LRM 6.6, Table 6-1): which resolution
// the net performs, named by the entry that installs it, and the contribution
// the net type makes to the net's own resolution -- the scalar the net shows
// where nothing drives it, and the strength it holds that scalar at (LRM 6.6.5,
// 6.6.6, 6.7.1).
struct NetInstall {
  support::BuiltinFn entry;
  mir::ExprId fill;
  mir::ExprId strength;
};

[[nodiscard]] auto BuildNetInstall(
    const mir::CompilationUnit& unit, mir::Block& block,
    const hir::StructuralNetDecl& net) -> NetInstall;

// A strength as the operand a net install or a driver attach carries: the level
// on the scale LRM Table 28-7 numbers, written as an ordinary integral value.
[[nodiscard]] auto BuildStrengthOperand(
    const mir::CompilationUnit& unit, mir::Block& block,
    support::StrengthLevel level) -> mir::ExprId;

}  // namespace lyra::lowering::hir_to_mir
