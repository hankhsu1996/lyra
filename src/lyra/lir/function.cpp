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
    case OpenWriteTarget::Op::kAssignElementSlice:
    case OpenWriteTarget::Op::kAssignQueueSlice:
      return "assign_slice";
  }
  throw InternalError("lir: unknown open-write operation");
}

auto ControlEffectOpName(ControlEffectTarget::Op op) -> std::string_view {
  switch (op) {
    case ControlEffectTarget::Op::kTakeDepartureIfDue:
      return "take_departure_if_due";
    case ControlEffectTarget::Op::kDeclineDeparture:
      return "decline_departure";
  }
  throw InternalError("lir: unknown control-effect operation");
}

auto DynamicStorageOpName(DynamicStorageTarget::Op op) -> std::string_view {
  switch (op) {
    case DynamicStorageTarget::Op::kAllocate:
      return "allocate";
    case DynamicStorageTarget::Op::kDeallocate:
      return "deallocate";
  }
  throw InternalError("lir: unknown dynamic-storage operation");
}

auto StorageOpName(StorageInstr::Op op) -> std::string_view {
  switch (op) {
    case StorageInstr::Op::kLive:
      return "storage_live";
    case StorageInstr::Op::kDead:
      return "storage_dead";
  }
  throw InternalError("lir: unknown storage operation");
}

auto CoroutineOpName(CoroutineTarget::Op op) -> std::string_view {
  switch (op) {
    case CoroutineTarget::Op::kEnter:
      return "enter_coroutine";
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
          [](const FunctionTarget&) { return CallEnding::kReturnsOrUnwinds; },
          [](const DispatchTarget&) { return CallEnding::kReturnsOrUnwinds; },
          [](const InterfaceDispatchTarget&) {
            return CallEnding::kReturnsOrUnwinds;
          },
          [](const IndirectTarget&) { return CallEnding::kReturnsOrUnwinds; },
          [](const SymbolTarget&) { return CallEnding::kReturnsOrUnwinds; },
          [](const ForeignTarget&) { return CallEnding::kReturnsOrUnwinds; },
          [](const BuiltinTarget& builtin) {
            return support::RuntimeEntryOf(builtin.fn).ending;
          },
          [](const ConstructTarget&) { return CallEnding::kReturnsOrUnwinds; },
          [](const CreationTarget&) { return CallEnding::kReturns; },
          [](const ValueCellTarget&) { return CallEnding::kReturns; },
          // Keeping a part's value only moves memory; writing a slice is what
          // any storage's slice takes, which can raise.
          [](const OpenWriteTarget& write) {
            switch (write.op) {
              case OpenWriteTarget::Op::kLand:
                return CallEnding::kReturns;
              case OpenWriteTarget::Op::kAssignElementSlice:
              case OpenWriteTarget::Op::kAssignQueueSlice:
                return CallEnding::kReturnsOrUnwinds;
            }
            throw InternalError("lir: unknown open-write operation");
          },
          // Writing bits within a write is what a slice write is, which can
          // raise.
          [](const DesignatedBitsTarget&) {
            return CallEnding::kReturnsOrUnwinds;
          },
          [](const EndValueTarget&) { return CallEnding::kReturns; },
          [](const CopyValueTarget&) { return CallEnding::kReturns; },
          [](const DynamicStorageTarget&) { return CallEnding::kReturns; },
          [](const ControlEffectTarget& effect) {
            switch (effect.op) {
              case ControlEffectTarget::Op::kTakeDepartureIfDue:
                return CallEnding::kReturnsOrUnwinds;
              case ControlEffectTarget::Op::kDeclineDeparture:
                return CallEnding::kUnwinds;
            }
            throw InternalError("lir: unknown control-effect operation");
          },
          // Entering builds an execution out of a frame and stops before its
          // first statement; awaiting runs the design's code, and releasing
          // raises again whatever that code raised.
          [](const CoroutineTarget& coroutine) {
            switch (coroutine.op) {
              case CoroutineTarget::Op::kEnter:
                return CallEnding::kReturns;
              case CoroutineTarget::Op::kAwait:
              case CoroutineTarget::Op::kRelease:
                return CallEnding::kReturnsOrUnwinds;
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
          [](const VoidConst& c) { return c.type; },
          [](const LayoutConst& c) { return c.type; },
          [](const EnumTableRef& c) { return c.type; },
          [](const IntegralConstantRef& c) { return c.type; },
          [](const StaticRef& s) { return s.type; },
          [](const DefinitionRef& c) { return c.type; },
          [](const FunctionRef& c) { return c.type; }},
      operand);
}

}  // namespace lyra::lir
