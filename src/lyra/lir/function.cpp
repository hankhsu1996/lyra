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
    case ControlEffectTarget::Op::kTakeDepartureIfDue:
      return "take_departure_if_due";
    case ControlEffectTarget::Op::kFinishDeparture:
      return "finish_departure";
    case ControlEffectTarget::Op::kDeclineDeparture:
      return "decline_departure";
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

auto CallEndingOf(const CallTarget& target) -> support::CallEnding {
  using support::CallEnding;
  return std::visit(
      Overloaded{
          [](const FunctionTarget&) { return CallEnding::kReturnsOrDeparts; },
          [](const DispatchTarget&) { return CallEnding::kReturnsOrDeparts; },
          [](const IndirectTarget&) { return CallEnding::kReturnsOrDeparts; },
          [](const SymbolTarget&) { return CallEnding::kReturnsOrDeparts; },
          [](const ForeignTarget&) { return CallEnding::kReturns; },
          [](const BuiltinTarget& builtin) {
            return support::RuntimeEntryOf(builtin.fn).ending;
          },
          [](const ConstructTarget&) { return CallEnding::kReturnsOrDeparts; },
          [](const ValueCellTarget&) { return CallEnding::kReturns; },
          [](const OpenVariablesTarget&) { return CallEnding::kReturns; },
          [](const VariableAddressTarget&) { return CallEnding::kReturns; },
          [](const CloseVariablesTarget&) { return CallEnding::kReturns; },
          [](const EndValueTarget&) { return CallEnding::kReturns; },
          [](const CopyValueTarget&) { return CallEnding::kReturns; },
          [](const ControlEffectTarget& effect) {
            switch (effect.op) {
              case ControlEffectTarget::Op::kTakeDepartureIfDue:
                return CallEnding::kReturnsOrDeparts;
              case ControlEffectTarget::Op::kFinishDeparture:
                return CallEnding::kReturns;
              case ControlEffectTarget::Op::kDeclineDeparture:
                return CallEnding::kDeparts;
            }
            throw InternalError("lir: unknown control-effect operation");
          },
          // Entering builds an execution out of a frame and stops before its
          // first statement; awaiting runs the design's code, and releasing
          // raises again whatever that code raised.
          [](const CoroutineTarget& coroutine) {
            switch (coroutine.op) {
              case CoroutineTarget::Op::kEnterBorrowedEnvironment:
              case CoroutineTarget::Op::kEnterOwnedEnvironment:
                return CallEnding::kReturns;
              case CoroutineTarget::Op::kAwait:
              case CoroutineTarget::Op::kRelease:
                return CallEnding::kReturnsOrDeparts;
            }
            throw InternalError("lir: unknown coroutine operation");
          }},
      target);
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
          [](const TypeDescriptorRef& c) -> std::optional<TypeId> {
            return c.type;
          },
          [](const IntegralConstantRef& c) -> std::optional<TypeId> {
            return c.type;
          },
          [](const FuncRef&) -> std::optional<TypeId> { return std::nullopt; },
          [](const StaticRef& s) -> std::optional<TypeId> { return s.type; },
          [](const ObjectRecordRef& r) -> std::optional<TypeId> {
            return r.type;
          }},
      operand);
}

}  // namespace lyra::lir
