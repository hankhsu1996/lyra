#include "lyra/lowering/hir_to_mir/queue_building.hpp"

#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/container_construction.hpp"
#include "lyra/lowering/hir_to_mir/expression/calls.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

void AppendPushBack(
    UnitLowerer& unit_lowerer, mir::Builder block, mir::LocalId queue,
    mir::TypeId queue_type, mir::ExprId value) {
  const mir::CompilationUnit& unit = unit_lowerer.Unit();
  const mir::TypeId element = RequiredContainerElementType(unit, queue_type);
  const mir::ExprId receiver = block.LocalPlace(queue, queue_type);
  const mir::ExprId pushed = ConvertToType(unit, block, value, element);
  block.AppendStmt(
      mir::ExprStmt{
          .expr = block.Add(BuildBuiltinMethodCall(
              unit_lowerer, block, support::BuiltinFn::kPushBack, receiver,
              {pushed}, unit.builtins.void_type))});
}

}  // namespace lyra::lowering::hir_to_mir
