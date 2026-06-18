#include "lyra/lowering/hir_to_mir/self_ref.hpp"

#include "lyra/mir/expr.hpp"

namespace lyra::lowering::hir_to_mir {

auto BuildSelfRefExpr(const WalkFrame& frame, mir::TypeId self_ptr_type)
    -> mir::Expr {
  return mir::MakeProceduralVarRefExpr(
      frame.procedural_depth - frame.self_decl_depth, *frame.self_binding,
      self_ptr_type);
}

}  // namespace lyra::lowering::hir_to_mir
