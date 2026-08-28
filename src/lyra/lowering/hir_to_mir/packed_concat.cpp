#include "lyra/lowering/hir_to_mir/packed_concat.hpp"

#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

auto BuildPackedConcat(
    mir::CompilationUnit& unit, mir::Block& block,
    std::vector<mir::ExprId> runs, mir::TypeId result_type) -> mir::Expr {
  if (runs.empty()) {
    throw InternalError("BuildPackedConcat: a join has at least one run");
  }
  if (runs.size() == 1) {
    return BuildValueConversion(unit, block, runs.front(), result_type);
  }
  return mir::Expr{
      .data = mir::ConcatExpr{.operands = std::move(runs)},
      .type = result_type};
}

}  // namespace lyra::lowering::hir_to_mir
