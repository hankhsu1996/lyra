#pragma once

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Module.h>

namespace lyra::backend::llvm_backend {

class CodeGenTypes;

// The runtime ABI the generated module calls: each runtime entry point declared
// once with its canonical signature. The ABI is execution-strategy-neutral --
// the same entry points serve a module that is JIT-compiled, AOT-linked, or
// interpreted; only how they resolve differs. Single source of truth for the
// contract: it declares callees and never builds instructions. The runtime owns
// the matching definitions.
class RuntimeAbi {
 public:
  RuntimeAbi(llvm::Module& module, llvm::LLVMContext& ctx, CodeGenTypes& types);

  auto Services() -> llvm::FunctionCallee;
  auto Files() -> llvm::FunctionCallee;
  auto TimeFormat() -> llvm::FunctionCallee;
  auto Format() -> llvm::FunctionCallee;
  auto Writeln() -> llvm::FunctionCallee;
  auto Write() -> llvm::FunctionCallee;

  // Binds a coroutine to an instance's startup or shutdown lifecycle. The
  // coroutine crosses as an opaque handle; the runtime owns the coroutine, so
  // no C++ coroutine frame is built on the generated side.
  auto RegisterInitial() -> llvm::FunctionCallee;
  auto RegisterFinal() -> llvm::FunctionCallee;

  // Builds a coroutine from an entry code reference and its environment; the
  // runtime owns the resulting coroutine and returns an opaque handle.
  auto MakeCoroutine() -> llvm::FunctionCallee;
  auto MakeString() -> llvm::FunctionCallee;
  auto MakePrintLiteralItem() -> llvm::FunctionCallee;
  auto PackedConst() -> llvm::FunctionCallee;

  // Builds a scope's structural identity from its parent-side label and its
  // per-dimension indices; the runtime owns the resulting segment handle.
  auto MakeSegment() -> llvm::FunctionCallee;

  // Allocates a generic instance of the unit named by `definition`, runs its
  // construct entry to build its subtree, and returns the owning handle. The
  // definition is an opaque cross-unit reference the generated code never
  // inspects.
  auto MakeUnit() -> llvm::FunctionCallee;

  // Attaches a freshly built child to its parent's containment edge, returning
  // the child as a borrowed scope handle.
  auto AddOwnedChild() -> llvm::FunctionCallee;

  // Reads / writes a member slot of an instance by slot index. A member is a
  // logical place; the runtime owns the slot storage.
  auto LoadField() -> llvm::FunctionCallee;
  auto StoreField() -> llvm::FunctionCallee;

 private:
  auto Get(
      const char* name, llvm::Type* result, llvm::ArrayRef<llvm::Type*> params)
      -> llvm::FunctionCallee;

  llvm::Module* module_;
  llvm::LLVMContext* ctx_;
  CodeGenTypes* types_;
};

}  // namespace lyra::backend::llvm_backend
