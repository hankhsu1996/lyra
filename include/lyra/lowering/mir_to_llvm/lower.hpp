#pragma once

#include <memory>
#include <string>

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "lyra/common/type_arena.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/design.hpp"

namespace lyra::lowering::mir_to_llvm {

struct LoweringInput {
  const mir::Design* design = nullptr;
  const mir::Arena* mir_arena = nullptr;
  const TypeArena* type_arena = nullptr;
};

struct LoweringResult {
  std::unique_ptr<llvm::LLVMContext> context;
  std::unique_ptr<llvm::Module> module;
};

auto LowerMirToLlvm(const LoweringInput& input) -> LoweringResult;

auto DumpLlvmIr(const LoweringResult& result) -> std::string;

}  // namespace lyra::lowering::mir_to_llvm
