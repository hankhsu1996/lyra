#include "lyra/lir/function.hpp"

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

auto OpenWriteOpName(OpenWriteTarget::Op op) -> std::string_view {
  switch (op) {
    case OpenWriteTarget::Op::kLand:
      return "land";
    case OpenWriteTarget::Op::kAssignSlice:
      return "assign_slice";
    case OpenWriteTarget::Op::kReadSlice:
      return "read_slice";
  }
  throw InternalError("lir: unknown open-write operation");
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
          [](const LibraryConstructorTarget&) {
            return CallEnding::kReturnsOrDeparts;
          },
          [](const ValueCellTarget&) { return CallEnding::kReturns; },
          // Keeping a part's value only moves memory; writing or reading a
          // slice is what any storage's slice takes, which can raise.
          [](const OpenWriteTarget& write) {
            switch (write.op) {
              case OpenWriteTarget::Op::kLand:
                return CallEnding::kReturns;
              case OpenWriteTarget::Op::kAssignSlice:
              case OpenWriteTarget::Op::kReadSlice:
                return CallEnding::kReturnsOrDeparts;
            }
            throw InternalError("lir: unknown open-write operation");
          },
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

auto OperandType(const Function& fn, const Operand& operand) -> TypeId {
  return std::visit(
      Overloaded{
          [&](const Use& use) { return fn.values.Get(use.value).type; },
          [](const IntConst& c) { return c.type; },
          [](const StrConst& c) { return c.type; },
          [](const RealConst& c) { return c.type; },
          [](const NullConst& c) { return c.type; },
          [](const BoolConst& c) { return c.type; },
          [](const TypeDescriptorRef& c) { return c.type; },
          [](const IntegralConstantRef& c) { return c.type; },
          [](const StaticRef& s) { return s.type; },
          [](const DefinitionRef& c) { return c.type; }},
      operand);
}

}  // namespace lyra::lir
