#pragma once

#include <optional>
#include <unordered_map>
#include <vector>

#include <llvm/IR/Function.h>

#include "lyra/llvm_backend/codegen_session.hpp"
#include "lyra/llvm_backend/dpi_abi.hpp"
#include "lyra/llvm_backend/process.hpp"
#include "lyra/llvm_backend/spec_planning.hpp"
#include "lyra/mir/module.hpp"

namespace lyra::lowering::mir_to_llvm {

class Context;
struct LoweringInput;

// All products from specialization compilation.
struct SpecializationProducts {
  std::unordered_map<const mir::ModuleBody*, std::vector<llvm::Function*>>
      body_to_compiled_funcs;
  std::unordered_map<
      const mir::ModuleBody*, std::vector<std::optional<ProcessTriggerEntry>>>
      body_to_process_triggers;
  std::vector<WaitSiteEntry> wait_sites;
  std::unordered_map<
      mir::ModuleExportCalleeKey, dpi::ModuleExportCalleeInfo,
      mir::ModuleExportCalleeKeyHash>
      module_export_callees;
  std::vector<DeferredSiteCalleeInfo> deferred_site_callee_info;
};

// Declare and define all design-global functions (packages + generated).
auto CompileGlobalFunctions(Context& context, const LoweringInput& input)
    -> Result<void>;

// Compile all specializations, merging per-session products.
auto CompileSpecializations(
    Context& context, const LoweringInput& input, const SpecPlan& spec_plan)
    -> Result<SpecializationProducts>;

}  // namespace lyra::lowering::mir_to_llvm
