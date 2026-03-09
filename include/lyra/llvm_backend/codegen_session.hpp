#pragma once

#include <cstddef>
#include <memory>
#include <vector>

#include <llvm/IR/Function.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/llvm_backend/layout/layout.hpp"
#include "lyra/llvm_backend/process.hpp"

namespace lyra::lowering::mir_to_llvm {

class Context;
struct LoweringInput;
struct LoweringResult;

// Backend-owned intermediate state between behavioral codegen and assembly.
// This is a strict bridge object: assembly may append IR (main()) to the
// module owned by context, but must not own layout/process compilation
// decisions. Assembly-facing helpers live in emit_design_main.cpp.
//
// Layout and Context are heap-allocated because Context stores
// `const Layout&` internally. Returning CodegenSession from
// CompileDesignProcesses via std::expected may move the struct, so both
// must have stable addresses to keep the reference valid.
struct CodegenSession {
  std::unique_ptr<Layout> layout;
  std::unique_ptr<Context> context;
  std::vector<llvm::Function*> process_funcs;
  std::vector<WaitSiteEntry> wait_sites;
  std::vector<SlotInfo> slot_info;
  size_t num_init_processes = 0;
};

// Backend phase: compile all design processes into LLVM IR.
// Produces a CodegenSession that assembly can append main() into.
auto CompileDesignProcesses(const LoweringInput& input)
    -> Result<CodegenSession>;

// Backend phase: extract LLVM ownership from a completed session.
auto FinalizeModule(CodegenSession session) -> LoweringResult;

}  // namespace lyra::lowering::mir_to_llvm
