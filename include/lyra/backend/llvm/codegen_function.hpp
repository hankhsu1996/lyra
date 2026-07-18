#pragma once

#include <unordered_map>
#include <vector>

#include <llvm/IR/IRBuilder.h>

#include "lyra/backend/llvm/runtime_abi.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/type_id.hpp"

namespace llvm {
class BasicBlock;
class Function;
class FunctionCallee;
class Value;
}  // namespace llvm

namespace lyra::backend::llvm_backend {

class CodeGenModule;

// Per-function code generation: lowers one LIR callable body into its LLVM
// function. Each LIR value becomes one LLVM value; reads resolve through the
// per-function value map. Shared services -- types, the runtime ABI, method
// resolution -- are reached through the owning module-level code generation.
class CodeGenFunction {
 public:
  CodeGenFunction(
      CodeGenModule& module, const lir::Function& fn, llvm::Function* value);

  void Run();

 private:
  auto LowerInstr(const lir::Instr& instr) -> llvm::Value*;
  auto LowerCall(const lir::CallInstr& call, lir::TypeId result_type)
      -> llvm::Value*;
  auto ResolveCallee(const lir::CallInstr& call, lir::TypeId result_type)
      -> llvm::FunctionCallee;
  auto LowerAggregate(const lir::AggregateInstr& agg, lir::TypeId result_type)
      -> llvm::Value*;
  auto LowerTupleAggregate(
      const lir::AggregateInstr& agg, const lir::TupleType& tuple)
      -> llvm::Value*;
  auto LowerErasedDynamicArrayConstruct(
      const lir::CallInstr& call, const lir::DynamicArrayType& type)
      -> llvm::Value*;
  auto LowerAggregateExtract(const lir::AggregateExtractInstr& extract)
      -> llvm::Value*;
  auto LowerAggregateUpdate(const lir::AggregateUpdateInstr& update)
      -> llvm::Value*;
  auto LowerBinary(const lir::BinaryInstr& binary) -> llvm::Value*;
  auto LowerMachineBinary(const lir::BinaryInstr& binary) -> llvm::Value*;
  auto LowerUnary(const lir::UnaryInstr& unary) -> llvm::Value*;
  auto LowerMachineUnary(const lir::UnaryInstr& unary) -> llvm::Value*;
  auto LowerBoolCast(const lir::BoolCastInstr& cast) -> llvm::Value*;
  auto LowerIntCast(const lir::IntCastInstr& cast, lir::TypeId result_type)
      -> llvm::Value*;
  auto LowerOperand(const lir::Operand& operand) -> llvm::Value*;

  // Whether this body's call protocol is the coroutine one. Such a body is
  // emitted with LLVM coroutine intrinsics and split into a resumable form by
  // the coroutine passes; the state machine and the frame are theirs, not this
  // emitter's.
  [[nodiscard]] auto IsCoroutine() const -> bool;

  // Emits the coroutine ramp (identity, frame allocation, begin) into the entry
  // block and builds the shared final-suspend, cleanup, and end blocks a
  // coroutine body returns through. Runs before the body's blocks are filled.
  void OpenCoroutine();

  // Emits a suspension: save, `llvm.coro.suspend`, and the switch that resumes
  // at `resume`, returns to the caller, or enters cleanup.
  void EmitCoroutineSuspend(llvm::BasicBlock* resume, bool is_final);

  // The address a place names. The base contributes the storage the chain
  // starts from -- a place local's own frame slot, or the referent of a
  // reference value
  // -- and each further step walks one projection.
  auto ResolvePlaceAddress(const lir::Place& place) -> llvm::Value*;
  auto LowerIntConst(const lir::IntConst& constant) -> llvm::Value*;
  auto LowerStrConst(const lir::StrConst& constant) -> llvm::Value*;
  auto LowerRealConst(const lir::RealConst& constant) -> llvm::Value*;
  auto LowerNullConst(const lir::NullConst& constant) -> llvm::Value*;
  void LowerTerminator(const lir::Terminator& terminator);

  auto BuiltinCallee(
      const lir::BuiltinTarget& target, const lir::CallInstr& call,
      lir::TypeId result_type) -> llvm::FunctionCallee;
  auto ValueBuiltinCallee(
      const lir::BuiltinTarget& target, const lir::CallInstr& call,
      lir::TypeId result_type) -> llvm::FunctionCallee;
  auto ConstructCallee(const lir::CallInstr& call) -> llvm::FunctionCallee;
  auto RealConstructCallee(const lir::CallInstr& call, ValueDomain dst)
      -> llvm::FunctionCallee;
  auto ForeignCallee(
      const lir::ForeignTarget& target, const lir::CallInstr& call,
      lir::TypeId result_type) -> llvm::FunctionCallee;

  // The leading argument a construct call needs beyond its lowered operands: an
  // external-unit construct is prefixed with the child's definition reference;
  // every other construct needs none.
  auto ConstructDefinitionArg(lir::TypeId result) -> llvm::Value*;

  // The type of an operand, and the value domain a library entry is chosen by.
  [[nodiscard]] auto OperandType(const lir::Operand& operand) const
      -> lir::TypeId;
  [[nodiscard]] auto DomainOf(lir::TypeId type) const -> ValueDomain;
  // The domain of the cell an operand addresses, for a cell operation.
  [[nodiscard]] auto CellDomain(const lir::Operand& cell) const -> ValueDomain;

  CodeGenModule* module_;
  const lir::Function* fn_;
  llvm::Function* value_;
  llvm::IRBuilder<> builder_;
  std::unordered_map<lir::ValueId, llvm::Value*> values_;
  std::vector<llvm::BasicBlock*> blocks_;
  // A coroutine body's ramp state: the coroutine identity (which names the
  // frame to release) and its handle, plus the blocks every suspension and
  // return funnels through. The frame's layout and the resume state machine are
  // the coroutine passes' to synthesize, never this emitter's.
  llvm::Value* coro_id_ = nullptr;
  llvm::Value* coro_handle_ = nullptr;
  llvm::BasicBlock* coro_final_ = nullptr;
  llvm::BasicBlock* coro_cleanup_ = nullptr;
  llvm::BasicBlock* coro_end_ = nullptr;
};

}  // namespace lyra::backend::llvm_backend
