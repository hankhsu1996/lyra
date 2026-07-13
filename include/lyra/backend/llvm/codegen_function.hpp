#pragma once

#include <cstdint>
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
  auto LowerBinary(const lir::BinaryInstr& binary) -> llvm::Value*;
  auto LowerUnary(const lir::UnaryInstr& unary) -> llvm::Value*;
  auto LowerBoolCast(const lir::BoolCastInstr& cast) -> llvm::Value*;
  auto LowerOperand(const lir::Operand& operand) -> llvm::Value*;

  // The address a place names. The base contributes the storage the chain
  // starts from -- a place local's own frame slot, or the referent of a
  // reference value
  // -- and each further step walks one projection.
  auto ResolvePlaceAddress(const lir::Place& place) -> llvm::Value*;
  auto LowerIntConst(const lir::IntConst& constant) -> llvm::Value*;
  auto LowerStrConst(const lir::StrConst& constant) -> llvm::Value*;
  void LowerTerminator(const lir::Terminator& terminator);

  auto BuiltinCallee(
      const lir::BuiltinTarget& target, const lir::CallInstr& call,
      lir::TypeId result_type) -> llvm::FunctionCallee;
  auto ValueBuiltinCallee(
      const lir::BuiltinTarget& target, const lir::CallInstr& call,
      lir::TypeId result_type) -> llvm::FunctionCallee;
  auto ConstructCallee(const lir::CallInstr& call) -> llvm::FunctionCallee;

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
  std::unordered_map<std::uint32_t, llvm::Value*> values_;
  std::vector<llvm::BasicBlock*> blocks_;
};

}  // namespace lyra::backend::llvm_backend
