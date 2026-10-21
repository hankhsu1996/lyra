#include "lyra/lowering/hir_to_mir/queue_building.hpp"

#include <optional>

#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/expression/calls.hpp"
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
    UnitLowerer& unit_lowerer, mir::Block& block, mir::LocalId queue,
    mir::TypeId queue_type, mir::ExprId value) {
  const mir::CompilationUnit& unit = unit_lowerer.Unit();
  const mir::TypeId element =
      unit.types.Get(queue_type).Get<mir::QueueType>().element_type;
  block.AppendStmt(
      mir::ExprStmt{
          .expr = block.exprs.Add(BuildBuiltinMethodCall(
              unit_lowerer, block, support::BuiltinFn::kPushBack,
              block.LocalPlace(queue, queue_type),
              {ConvertToType(unit, block, value, element)},
              unit.builtins.void_type))});
}

}  // namespace lyra::lowering::hir_to_mir
