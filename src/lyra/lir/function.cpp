#include "lyra/lir/function.hpp"

#include <optional>
#include <string_view>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lir/type_id.hpp"

namespace lyra::lir {

auto ActivationFrameOpName(ActivationFrameTarget::Op op) -> std::string_view {
  switch (op) {
    case ActivationFrameTarget::Op::kAllocate:
      return "activation_frame_alloc";
    case ActivationFrameTarget::Op::kLoad:
      return "activation_frame_load";
    case ActivationFrameTarget::Op::kStore:
      return "activation_frame_store";
  }
  throw InternalError("lir: unknown activation-frame operation");
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
