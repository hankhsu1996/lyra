#include "lyra/lowering/hir_to_mir/lower_type.hpp"

#include <variant>

#include "lyra/hir/type.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/overloaded.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerTypeData(const hir::TypeData& data) -> mir::TypeData {
  return std::visit(
      support::Overloaded{
          [](const hir::BuiltinIntType&) -> mir::TypeData {
            return mir::BuiltinIntType{};
          },
          [](const hir::BuiltinLogicType&) -> mir::TypeData {
            return mir::BuiltinLogicType{};
          },
      },
      data);
}

}  // namespace lyra::lowering::hir_to_mir
