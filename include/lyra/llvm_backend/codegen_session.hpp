#pragma once

#include <cstddef>
#include <cstdint>
#include <memory>
#include <unordered_set>
#include <vector>

#include <llvm/IR/Function.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/llvm_backend/layout/layout.hpp"
#include "lyra/llvm_backend/process.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/design.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/module_body.hpp"

namespace lyra::lowering::mir_to_llvm {

class Context;
struct LoweringInput;
struct LoweringResult;

// Per-instance binding data for specialization compilation.
// Stores stable IDs/values; variant is looked up from Layout when needed.
struct SpecInstanceBinding {
  ModuleIndex module_index;
  uint32_t base_slot_id;  // From placement
};

// Represents one specialization/body and the instances/templates currently
// associated with it.
struct SpecCompilationUnit {
  mir::ModuleBodyId body_id;
  const mir::ModuleBody* body;  // Borrowed
  std::vector<SpecInstanceBinding> instances;
  std::vector<size_t> template_indices;  // Into layout.process_templates
};

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
  const mir::Design* design = nullptr;
  std::vector<llvm::Function*> process_funcs;
  std::vector<WaitSiteEntry> wait_sites;
  std::vector<SlotInfo> slot_info;
  size_t num_init_processes = 0;
};

// Backend phase: compile all design processes into LLVM IR.
// Produces a CodegenSession that assembly can append main() into.
auto CompileDesignProcesses(const LoweringInput& input)
    -> Result<CodegenSession>;

// Prepare one specialization unit: register monitor info for body processes,
// register module-scoped function metadata for each instance, and collect
// body function IDs. Must be called for ALL units before the global function
// declare/define pass.
auto PrepareSpecialization(
    Context& context, const Layout& layout, const mir::Arena& arena,
    const SpecCompilationUnit& unit, std::vector<mir::FunctionId>& all_func_ids,
    std::unordered_set<uint32_t>& seen_func_ids) -> void;

// Compile one specialization unit: generate shared/template process functions.
// Prerequisites: PrepareSpecialization called for all units, functions declared
// and defined. Does not emit per-instance wrappers or inspect package/global
// elements.
auto CompileSpecialization(
    Context& context, const Layout& layout, const mir::Arena& arena,
    const SpecCompilationUnit& unit, std::vector<llvm::Function*>& template_fns,
    std::vector<WaitSiteEntry>& all_wait_sites) -> Result<void>;

// Backend phase: extract LLVM ownership from a completed session.
auto FinalizeModule(CodegenSession session) -> LoweringResult;

}  // namespace lyra::lowering::mir_to_llvm
