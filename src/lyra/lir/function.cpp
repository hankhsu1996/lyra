#include "lyra/lir/function.hpp"

#include <optional>
#include <string_view>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lir/type_id.hpp"

namespace lyra::lir {

auto ValueCellOpName(ValueCellTarget::Op op) -> std::string_view {
  switch (op) {
    case ValueCellTarget::Op::kAllocate:
      return "value_cell_alloc";
    case ValueCellTarget::Op::kLoad:
      return "value_cell_load";
    case ValueCellTarget::Op::kStore:
      return "value_cell_store";
  }
  throw InternalError("lir: unknown value-cell operation");
}

auto ControlEffectOpName(ControlEffectTarget::Op op) -> std::string_view {
  switch (op) {
    case ControlEffectTarget::Op::kHasInvalidatedTarget:
      return "has_invalidated_target";
    case ControlEffectTarget::Op::kInvalidatedTarget:
      return "invalidated_target";
    case ControlEffectTarget::Op::kSettleCancelled:
      return "settle_cancelled";
  }
  throw InternalError("lir: unknown control-effect operation");
}

auto CoroutineOpName(CoroutineTarget::Op op) -> std::string_view {
  switch (op) {
    case CoroutineTarget::Op::kEnterBorrowedEnvironment:
      return "enter_coroutine_borrowed_environment";
    case CoroutineTarget::Op::kEnterOwnedEnvironment:
      return "enter_coroutine_owned_environment";
    case CoroutineTarget::Op::kAwait:
      return "await_coroutine";
    case CoroutineTarget::Op::kRelease:
      return "release_coroutine";
  }
  throw InternalError("lir: unknown coroutine operation");
}

auto OperandType(const Function& fn, const Operand& operand)
    -> std::optional<TypeId> {
  return std::visit(
      Overloaded{
          [&](const Use& use) -> std::optional<TypeId> {
            return fn.values.Get(use.value).type;
          },
          [](const IntConst& c) -> std::optional<TypeId> { return c.type; },
          [](const StrConst& c) -> std::optional<TypeId> { return c.type; },
          [](const RealConst& c) -> std::optional<TypeId> { return c.type; },
          [](const NullConst& c) -> std::optional<TypeId> { return c.type; },
          [](const BoolConst& c) -> std::optional<TypeId> { return c.type; },
          [](const PackedTypeRef& c) -> std::optional<TypeId> {
            return c.type;
          },
          [](const FuncRef&) -> std::optional<TypeId> { return std::nullopt; },
          [](const StaticRef& s) -> std::optional<TypeId> { return s.type; }},
      operand);
}

}  // namespace lyra::lir
