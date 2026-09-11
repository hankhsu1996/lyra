#pragma once

#include <unordered_map>

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Type.h>

#include "lyra/lir/type_id.hpp"

namespace lyra::lir {
struct CompilationUnit;
}  // namespace lyra::lir

namespace lyra::backend::llvm_backend {

// Maps a LIR type to the LLVM type a value of it is held in. A machine type
// names itself, because a target's own type system already spells what the
// target computes with; every other type answers with an address, because what
// it names is realized on the far side of the runtime boundary. Physical layout
// is not decided here.
class CodeGenTypes {
 public:
  CodeGenTypes(llvm::LLVMContext& ctx, const lir::CompilationUnit& unit);

  auto Map(lir::TypeId id) -> llvm::Type*;

  auto Ptr() const -> llvm::PointerType* {
    return ptr_ty_;
  }
  // A contiguous run of values named by its first-element pointer and length,
  // which is the shape a run of them crosses the runtime boundary in.
  auto Span() const -> llvm::StructType* {
    return span_ty_;
  }
  auto Void() const -> llvm::Type*;

 private:
  llvm::LLVMContext* ctx_;
  const lir::CompilationUnit* unit_;
  llvm::PointerType* ptr_ty_;
  llvm::StructType* span_ty_;
  std::unordered_map<lir::TypeId, llvm::Type*> cache_;
};

}  // namespace lyra::backend::llvm_backend
