#include "lyra/lowering/hir_to_mir/real_literal.hpp"

#include "lyra/base/internal_error.hpp"
#include "lyra/mir/expr.hpp"

namespace lyra::lowering::hir_to_mir {

auto BuildRealLiteral(
    const mir::CompilationUnit& unit, mir::Block& block, mir::TypeId real_type,
    double value) -> mir::ExprId {
  const mir::TypeKind kind = unit.types.Get(real_type).Kind();
  if (kind != mir::TypeKind::kReal && kind != mir::TypeKind::kShortReal &&
      kind != mir::TypeKind::kRealTime) {
    throw InternalError(
        "BuildRealLiteral: only a real-family type is built from a real "
        "constant");
  }
  const mir::TypeId machine = kind == mir::TypeKind::kShortReal
                                  ? unit.builtins.machine_float32
                                  : unit.builtins.machine_float64;
  const mir::ExprId carrier = block.exprs.Add(
      mir::Expr{
          .data = mir::MachineFloatLiteral{.value = value}, .type = machine});
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{.callee = mir::Construct{}, .arguments = {carrier}},
          .type = real_type});
}

}  // namespace lyra::lowering::hir_to_mir
