#include "lyra/lir/function.hpp"

#include <string_view>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lir/compilation_unit.hpp"
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
    case OpenWriteTarget::Op::kDesignated:
      return "designated";
    case OpenWriteTarget::Op::kMoved:
      return "moved";
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

auto ReceiveDepartureCall() -> CallInstr {
  return CallInstr{
      .target = BuiltinTarget{.fn = support::BuiltinFn::kReceiveDeparture},
      .args = {}};
}

auto SuccessorsOf(const TerminatorData& terminator) -> std::vector<BlockId> {
  return std::visit(
      Overloaded{
          [](const ReturnTerm&) -> std::vector<BlockId> { return {}; },
          [](const BranchTerm& branch) -> std::vector<BlockId> {
            return {branch.target};
          },
          [](const CondBranchTerm& fork) -> std::vector<BlockId> {
            return {fork.if_true, fork.if_false};
          },
          [](const SwitchTerm& dispatch) {
            std::vector<BlockId> successors;
            successors.reserve(dispatch.targets.size() + 1);
            for (const SwitchTarget& to : dispatch.targets) {
              successors.push_back(to.target);
            }
            successors.push_back(dispatch.otherwise);
            return successors;
          },
          [](const SuspendTerm& suspension) -> std::vector<BlockId> {
            return {suspension.resume, suspension.abandoned};
          },
          [](const AbandonTerm&) -> std::vector<BlockId> { return {}; },
          [](const DepartTerm&) -> std::vector<BlockId> { return {}; },
          [](const UnwindResumeTerm&) -> std::vector<BlockId> { return {}; },
          [](const UnreachableTerm&) -> std::vector<BlockId> { return {}; },
          [](const InvokeTerm& call) -> std::vector<BlockId> {
            return {call.returned, call.unwind};
          },
          [](const ReceiveDepartureTerm& taken) {
            std::vector<BlockId> successors{taken.then};
            if (taken.otherwise.has_value()) {
              successors.push_back(*taken.otherwise);
            }
            return successors;
          }},
      terminator);
}

auto CallReturns(const CallTarget& target) -> bool {
  return std::visit(
      Overloaded{
          [](const FunctionTarget&) { return true; },
          [](const DispatchTarget&) { return true; },
          [](const InterfaceDispatchTarget&) { return true; },
          [](const IndirectTarget&) { return true; },
          [](const SymbolTarget&) { return true; },
          [](const ForeignTarget&) { return true; },
          [](const BuiltinTarget& builtin) {
            return support::RuntimeEntryOf(builtin.fn).returns;
          },
          [](const ConstructTarget&) { return true; },
          [](const CreationTarget&) { return true; },
          [](const ValueCellTarget&) { return true; },
          [](const OpenWriteTarget&) { return true; },
          [](const EndValueTarget&) { return true; },
          [](const CopyValueTarget&) { return true; },
          [](const RelocateValueTarget&) { return true; },
          [](const DynamicStorageTarget&) { return true; },
          [](const BuildCellTarget&) { return true; },
          // Declining a departure carries it on past this frame.
          [](const ControlEffectTarget& effect) {
            switch (effect.op) {
              case ControlEffectTarget::Op::kTakeDepartureIfDue:
                return true;
              case ControlEffectTarget::Op::kDeclineDeparture:
                return false;
            }
            throw InternalError("lir: unknown control-effect operation");
          },
          [](const CoroutineTarget&) { return true; }},
      target);
}

auto CallCanUnwind(
    const CompilationUnit& unit, const CallTarget& target, TypeId answer)
    -> bool {
  return std::visit(
      Overloaded{
          [&](const FunctionTarget& called) {
            return unit.functions.Attributes(called.function).can_unwind;
          },
          [](const DispatchTarget&) { return true; },
          [](const InterfaceDispatchTarget&) { return true; },
          [](const IndirectTarget&) { return true; },
          [](const SymbolTarget&) { return true; },
          [](const ForeignTarget&) { return true; },
          [](const BuiltinTarget& builtin) {
            return support::RuntimeEntryOf(builtin.fn).can_unwind;
          },
          [&](const ConstructTarget&) {
            return !unit.types.Get(answer).IsMoveOnly();
          },
          [](const CreationTarget&) { return false; },
          [](const ValueCellTarget&) { return false; },
          // Landing, answering the part and telling what moved only move
          // memory.
          [](const OpenWriteTarget& write) {
            switch (write.op) {
              case OpenWriteTarget::Op::kLand:
              case OpenWriteTarget::Op::kDesignated:
              case OpenWriteTarget::Op::kMoved:
                return false;
            }
            throw InternalError("lir: unknown open-write operation");
          },
          [](const EndValueTarget&) { return false; },
          [](const CopyValueTarget&) { return false; },
          [](const RelocateValueTarget&) { return false; },
          [](const DynamicStorageTarget&) { return false; },
          [](const BuildCellTarget&) { return false; },
          [](const ControlEffectTarget&) { return true; },
          // Entering builds an execution out of a frame and stops before its
          // first statement; awaiting runs the design's code, and releasing
          // raises again whatever that code raised.
          [](const CoroutineTarget& coroutine) {
            switch (coroutine.op) {
              case CoroutineTarget::Op::kEnter:
                return false;
              case CoroutineTarget::Op::kAwait:
              case CoroutineTarget::Op::kRelease:
                return true;
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
          [](const FloatConst& c) { return c.type; },
          [](const NullConst& c) { return c.type; },
          [](const BoolConst& c) { return c.type; },
          [](const VoidConst& c) { return c.type; },
          [](const LayoutConst& c) { return c.type; },
          [](const ConstantRef& c) { return c.type; },
          [](const AddressConst& c) { return c.type; }},
      operand);
}

}  // namespace lyra::lir
