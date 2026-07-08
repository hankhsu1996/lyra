#pragma once

#include <cstdint>
#include <unordered_map>

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Type.h>

#include "lyra/lir/type_id.hpp"

namespace lyra::lir {
struct CompilationUnit;
}  // namespace lyra::lir

namespace lyra::backend::llvm_backend {

// Maps a LIR type to its LLVM representation. In the opaque-handle value model
// every runtime/value type is a pointer the runtime operates on; only `void`,
// machine integers, and a coroutine value (a `{step_fn, env}` closure) have a
// structural LLVM shape. Physical layout is not decided here.
class CodeGenTypes {
 public:
  CodeGenTypes(llvm::LLVMContext& ctx, const lir::CompilationUnit& unit);

  auto Map(lir::TypeId id) -> llvm::Type*;

  auto Ptr() const -> llvm::PointerType* {
    return ptr_ty_;
  }
  // A contiguous run of values named by its first-element pointer and length;
  // the LLVM shape an unpacked-array value takes.
  auto Span() const -> llvm::StructType* {
    return span_ty_;
  }
  auto Void() const -> llvm::Type*;

 private:
  llvm::LLVMContext* ctx_;
  const lir::CompilationUnit* unit_;
  llvm::PointerType* ptr_ty_;
  llvm::StructType* span_ty_;
  std::unordered_map<std::uint32_t, llvm::Type*> cache_;
};

}  // namespace lyra::backend::llvm_backend
