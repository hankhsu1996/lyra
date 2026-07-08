#pragma once

#include <cstdint>
#include <unordered_map>

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
  auto LowerIntConst(const lir::IntConst& constant) -> llvm::Value*;
  auto LowerStrConst(const lir::StrConst& constant) -> llvm::Value*;
  void LowerTerminator(const lir::Terminator& terminator);

  auto BuiltinCallee(support::BuiltinFn fn) -> llvm::FunctionCallee;
  auto ConstructCallee(lir::TypeId result) -> llvm::FunctionCallee;

  CodeGenModule* module_;
  const lir::Function* fn_;
  llvm::Function* value_;
  llvm::IRBuilder<> builder_;
  std::unordered_map<std::uint32_t, llvm::Value*> values_;
};

}  // namespace lyra::backend::llvm_backend
