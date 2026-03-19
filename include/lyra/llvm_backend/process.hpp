#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <vector>

#include <llvm/IR/Function.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/edge_kind.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/routine.hpp"
#include "lyra/mir/terminator.hpp"
#include "lyra/runtime/wait_site.hpp"

namespace lyra::lowering::mir_to_llvm {

// Codegen-time execution contract for a generated process body.
// This is not a runtime value. It selects which codegen policy applies
// to the generated function: what state to load, whether to emit dirty
// tracking, whether to support resume dispatch, etc.
enum class ProcessExecutionKind {
  // Init processes: no engine, no dirty tracking, no suspend/resume.
  // Design-slot stores are plain writes with no compare or notify.
  kInit,
  // Normal scheduled simulation: engine guaranteed non-null, dirty tracking
  // enabled, suspend/resume supported. Design-slot stores use compare +
  // dirty-mark with no engine-null runtime check.
  kSimulation,
};

// Per-wait-site entry produced during process codegen.
// Index = wait_site_id (assigned sequentially via Context::NextWaitSiteId).
struct WaitSiteEntry {
  uint32_t resume_block;
  uint32_t num_triggers;
  bool has_late_bound;
  bool has_container;
};

// Canonical compile-time process trigger fact.
// Captures the signal identity and edge kind from a MIR WaitTrigger.
// G13 metadata, separate from runtime wait-site plumbing.
struct ProcessTriggerFact {
  mir::SignalRef signal;
  common::EdgeKind edge;
  bool has_observed_place;
};

// Canonical compile-time process trigger entry.
// At most one per scheduled process. Captures all trigger facts from
// the process's Wait terminators. Populated during per-process codegen
// from the MIR, before metadata lowering resolves signal identities
// to design-global slot IDs.
struct ProcessTriggerEntry {
  uint32_t scheduled_process_index = 0;
  std::vector<ProcessTriggerFact> triggers;
  runtime::WaitShapeKind shape = runtime::WaitShapeKind::kStatic;
};

// Output of process code generation.
struct ProcessCodegenResult {
  llvm::Function* function;
  std::vector<WaitSiteEntry> wait_sites;
  // Canonical process-trigger metadata for G13.
  // Present when the process has at least one Wait terminator.
  std::optional<ProcessTriggerEntry> process_trigger;
};

// Generate a standalone process function.
// The execution_kind selects the codegen contract: init processes get direct
// stores and no engine access; simulation processes get full dirty tracking
// and resume dispatch.
auto GenerateProcessFunction(
    Context& context, const mir::Process& process, const std::string& name,
    ProcessExecutionKind execution_kind) -> Result<ProcessCodegenResult>;

// Generate a shared process function with the 2-arg call contract.
// Signature: void(ptr frame, i32 resume)
// Instance binding is loaded from the frame header at entry.
// The context must have template-mode fields configured before calling.
auto GenerateSharedProcessFunction(
    Context& context, const mir::Process& process, const std::string& name)
    -> Result<ProcessCodegenResult>;

// Declare a MIR function without generating its body.
// Used for two-pass generation to enable mutual recursion.
auto DeclareMirFunction(
    Context& context, mir::FunctionId func_id, const std::string& name)
    -> Result<llvm::Function*>;

// Generate the body for a MIR function.
// The function must have been declared first with DeclareMirFunction.
auto DefineMirFunction(
    Context& context, mir::FunctionId func_id, llvm::Function* func)
    -> Result<void>;

// Extract raw canonical process-trigger facts from a MIR process.
// Walks blocks for Wait terminators and captures trigger facts
// (signal, edge, observed_place) and coarse wait-shape classification.
// Does NOT decide Stage-1 groupability -- that is metadata lowering's job.
// Returns nullopt if the process has no Wait terminators.
auto ExtractProcessTriggerEntry(const mir::Process& process)
    -> std::optional<ProcessTriggerEntry>;

}  // namespace lyra::lowering::mir_to_llvm
