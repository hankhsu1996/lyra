#include "lyra/lowering/hir_to_mir/queue_building.hpp"

#include <optional>

#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/expression/calls.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

auto QueueOf(mir::CompilationUnit& unit, mir::TypeId element) -> mir::TypeId {
  return unit.types.Intern(
      mir::Type{
          mir::QueueType{.element_type = element, .max_bound = std::nullopt}});
}

void AppendPushBack(
    UnitLowerer& unit_lowerer, mir::Builder block, mir::LocalId queue,
    mir::TypeId queue_type, mir::ExprId value) {
  const mir::CompilationUnit& unit = unit_lowerer.Unit();
  const mir::TypeId element =
      unit.types.Get(queue_type).Get<mir::QueueType>().element_type;
  const mir::ExprId receiver = block.LocalPlace(queue, queue_type);
  const mir::ExprId pushed = ConvertToType(unit, block, value, element);
  block.AppendStmt(
      mir::ExprStmt{
          .expr = block.Add(BuildBuiltinMethodCall(
              unit_lowerer, block, support::BuiltinFn::kPushBack, receiver,
              {pushed}, unit.builtins.void_type))});
}

}  // namespace lyra::lowering::hir_to_mir
