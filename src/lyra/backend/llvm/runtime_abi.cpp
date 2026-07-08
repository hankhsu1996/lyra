#include "lyra/backend/llvm/runtime_abi.hpp"

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Type.h>

#include "lyra/backend/llvm/codegen_types.hpp"

namespace lyra::backend::llvm_backend {

RuntimeAbi::RuntimeAbi(
    llvm::Module& module, llvm::LLVMContext& ctx, CodeGenTypes& types)
    : module_(&module), ctx_(&ctx), types_(&types) {
}

auto RuntimeAbi::Get(
    const char* name, llvm::Type* result, llvm::ArrayRef<llvm::Type*> params)
    -> llvm::FunctionCallee {
  return module_->getOrInsertFunction(
      name, llvm::FunctionType::get(result, params, false));
}

auto RuntimeAbi::Services() -> llvm::FunctionCallee {
  return Get("lyra_rt_services", types_->Ptr(), {types_->Ptr()});
}

auto RuntimeAbi::Files() -> llvm::FunctionCallee {
  return Get("lyra_rt_files", types_->Ptr(), {types_->Ptr()});
}

auto RuntimeAbi::TimeFormat() -> llvm::FunctionCallee {
  return Get("lyra_rt_time_format", types_->Ptr(), {types_->Ptr()});
}

auto RuntimeAbi::Format() -> llvm::FunctionCallee {
  return Get("lyra_rt_format", types_->Ptr(), {types_->Span(), types_->Ptr()});
}

auto RuntimeAbi::Writeln() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_writeln", types_->Void(),
      {types_->Ptr(), types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::Write() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_write", types_->Void(),
      {types_->Ptr(), types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::RegisterInitial() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_register_initial", types_->Void(),
      {types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::RegisterFinal() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_register_final", types_->Void(), {types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::MakeCoroutine() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_make_coroutine", types_->Ptr(), {types_->Ptr(), types_->Ptr()});
}

auto RuntimeAbi::MakeString() -> llvm::FunctionCallee {
  return Get("lyra_rt_make_string", types_->Ptr(), {types_->Ptr()});
}

auto RuntimeAbi::MakePrintLiteralItem() -> llvm::FunctionCallee {
  return Get("lyra_rt_make_print_literal_item", types_->Ptr(), {types_->Ptr()});
}

auto RuntimeAbi::PackedConst() -> llvm::FunctionCallee {
  return Get(
      "lyra_rt_packed_const", types_->Ptr(),
      {llvm::Type::getInt64Ty(*ctx_), llvm::Type::getInt32Ty(*ctx_),
       llvm::Type::getInt1Ty(*ctx_), llvm::Type::getInt1Ty(*ctx_)});
}

}  // namespace lyra::backend::llvm_backend
