#include "lyra/lowering/hir_to_mir/packed_replication.hpp"

#include <cstdint>

#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

auto BuildPackedReplication(
    mir::CompilationUnit& unit, mir::Block& block, mir::ExprId run,
    std::uint64_t count, mir::TypeId result_type) -> mir::Expr {
  if (count == 1) {
    return BuildValueConversion(unit, block, run, result_type);
  }
  const mir::ExprId count_id = block.exprs.Add(
      mir::Expr{
          .data =
              mir::MachineIntLiteral{.value = static_cast<std::int64_t>(count)},
          .type = unit.builtins.machine_int64});
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Direct{.target = support::BuiltinFn::kReplicate},
              .arguments = {run, count_id}},
      .type = result_type};
}

}  // namespace lyra::lowering::hir_to_mir
