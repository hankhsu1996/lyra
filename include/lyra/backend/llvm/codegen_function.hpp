#pragma once

#include <cstdint>
#include <unordered_map>
#include <utility>

#include <llvm/IR/IRBuilder.h>

#include "lyra/lir/function.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace llvm {
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
  auto LowerCall(const lir::CallInstr& call) -> llvm::Value*;
  auto ResolveCallee(const lir::CallTarget& target) -> llvm::FunctionCallee;
  auto LowerAggregate(const lir::AggregateInstr& agg, lir::TypeId result_type)
      -> llvm::Value*;
  auto LowerOperand(const lir::Operand& operand) -> llvm::Value*;

  // Resolves a place to the base instance and the member slot index its
  // projection selects, the index already a runtime-ABI constant. Realizes a
  // single member step so far; a deeper projection chain is realized when its
  // steps land.
  auto ResolveMemberSlot(const lir::Place& place)
      -> std::pair<llvm::Value*, llvm::Value*>;
  auto LowerIntConst(const lir::IntConst& constant) -> llvm::Value*;
  auto LowerStrConst(const lir::StrConst& constant) -> llvm::Value*;
  void LowerTerminator(const lir::Terminator& terminator);

  auto BuiltinCallee(support::BuiltinFn fn) -> llvm::FunctionCallee;
  auto ConstructCallee(lir::TypeId result) -> llvm::FunctionCallee;

  // The leading argument a construct call needs beyond its lowered operands: an
  // external-unit construct is prefixed with the child's definition reference;
  // every other construct needs none.
  auto ConstructDefinitionArg(lir::TypeId result) -> llvm::Value*;

  CodeGenModule* module_;
  const lir::Function* fn_;
  llvm::Function* value_;
  llvm::IRBuilder<> builder_;
  std::unordered_map<std::uint32_t, llvm::Value*> values_;
};

}  // namespace lyra::backend::llvm_backend
