#include "lyra/lir/function.hpp"

#include <optional>
#include <variant>

#include "lyra/base/overloaded.hpp"
#include "lyra/lir/type_id.hpp"

namespace lyra::lir {

auto OperandType(const Function& fn, const Operand& operand)
    -> std::optional<TypeId> {
  return std::visit(
      Overloaded{
          [&](const Use& use) -> std::optional<TypeId> {
            return fn.values.Get(use.value).type;
          },
          [](const IntConst& c) -> std::optional<TypeId> { return c.type; },
          [](const StrConst& c) -> std::optional<TypeId> { return c.type; },
          [](const FuncRef&) -> std::optional<TypeId> { return std::nullopt; }},
      operand);
}

}  // namespace lyra::lir
