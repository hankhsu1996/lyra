#pragma once

#include <memory>
#include <string>
#include <vector>

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "lyra/common/type_arena.hpp"
#include "lyra/llvm_backend/layout.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/design.hpp"

namespace lyra::lowering::mir_to_llvm {

// Information about a module variable for runtime inspection
struct VariableInfo {
  std::string name;
  size_t slot_id;  // Index into slot_types
};

struct LoweringInput {
  const mir::Design* design = nullptr;
  const mir::Arena* mir_arena = nullptr;
  const TypeArena* type_arena = nullptr;
  std::vector<SlotTypeInfo>
      slot_types;  // Index == slot_id, for init/registration
  std::vector<TypeId>
      slot_type_ids;  // Index == slot_id, for LLVM type derivation
  std::vector<VariableInfo> variables;  // For runtime inspection (optional)
};

struct LoweringResult {
  std::unique_ptr<llvm::LLVMContext> context;
  std::unique_ptr<llvm::Module> module;
};

auto LowerMirToLlvm(const LoweringInput& input) -> LoweringResult;

auto DumpLlvmIr(const LoweringResult& result) -> std::string;

}  // namespace lyra::lowering::mir_to_llvm
