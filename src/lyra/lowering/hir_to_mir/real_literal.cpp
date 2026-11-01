#include "lyra/lowering/hir_to_mir/real_literal.hpp"

#include "lyra/base/internal_error.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

auto BuildRealLiteral(
    const mir::CompilationUnit& unit, mir::Builder block, mir::TypeId real_type,
    double value) -> mir::ExprId {
  if (!unit.types.Get(real_type).Is<mir::RealType>()) {
    throw InternalError(
        "BuildRealLiteral: only a real type is built from a real constant");
  }
  return block.Add(
      mir::Expr{
          .data = mir::FloatingLiteral{.value = value}, .type = real_type});
}

}  // namespace lyra::lowering::hir_to_mir
