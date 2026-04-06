#pragma once

#include <cstdint>
#include <vector>

#include "lyra/common/origin_id.hpp"
#include "lyra/common/symbol_types.hpp"
#include "lyra/common/type.hpp"
#include "lyra/mir/basic_block.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/semantic/decision.hpp"

namespace lyra::mir {

enum class ProcessKind {
  kOnce,     // initial block - runs once at time 0
  kFinal,    // final block - runs once at end of simulation
  kLooping,  // always block - repeats
};

// A process is a coroutine unit that may suspend and resume.
// Allowed terminators: Control, Suspension (Delay/Wait), Completion
// (Finish/Repeat).
struct Process {
  ProcessKind kind = ProcessKind::kOnce;
  BasicBlockId entry;              // Local index within blocks (0, 1, 2...)
  std::vector<BasicBlock> blocks;  // Direct ownership
  common::OriginId origin = common::OriginId::Invalid();  // Source location

  // Authoritative temp metadata (indexed by temp_id).
  // Contains kind (kValue vs kPlace) and type for each temp.
  std::vector<TempMetadata> temp_metadata;

  // Stats: materialize-to-place operations (for --stats output).
  uint64_t materialize_count = 0;

  // Decision site metadata (qualifier, kind, arm count, origin).
  struct MirDecisionSite {
    semantic::DecisionQualifier qualifier{};
    semantic::DecisionKind kind{};
    bool has_fallback = false;
    semantic::DecisionArmCount arm_count;
    common::OriginId origin = common::OriginId::Invalid();
  };

  // Decision site record: pairs body-global DecisionId with metadata.
  // Populated during MIR body building via AllocateDecisionSite().
  // The DecisionId is body-global (unique across all processes and functions
  // in the same module body), assigned by the DecisionSiteAllocator.
  struct MirDecisionSiteRecord {
    semantic::DecisionId id;
    MirDecisionSite site;
  };

  std::vector<MirDecisionSiteRecord> decision_sites;
};

enum class PassingKind {
  kValue,  // Input by value
  kOut,    // Output only (callee writes to caller's storage)
  kInOut,  // Bidirectional (callee reads and may modify)
};

// Return policy - how function returns are handled at the ABI level.
// Frozen at HIR->MIR lowering time based on return type characteristics.
enum class ReturnPolicy {
  kVoid,          // No return value (void functions)
  kDirect,        // Return value in register (scalars, managed handles)
  kSretOutParam,  // Return via out-param pointer (value aggregates only)
};

// Classifies runtime-invoked functions by their ABI family.
// kNone: regular function (signature-derived ABI).
// kStrobe / kMonitorSetup / kMonitorCheck: observer programs that always use
//   the observer ABI (DesignState*, Engine*, ObserverContext*[, prev_buf*]).
// kDeferredAssertionThunk: deferred assertion action thunk using the thunk ABI
//   (DesignState*, Engine*, DeferredAssertionExecContext*, payload*).
// IsObserverProgram() is the canonical predicate for the observer-ABI class.
enum class RuntimeProgramKind {
  kNone,
  kStrobe,
  kMonitorCheck,
  kMonitorSetup,
  kDeferredAssertionThunk,
};

// True for runtime program kinds that use the observer program ABI.
// Does NOT include kDeferredAssertionThunk (different ABI).
inline auto IsObserverProgram(RuntimeProgramKind kind) -> bool {
  return kind == RuntimeProgramKind::kStrobe ||
         kind == RuntimeProgramKind::kMonitorSetup ||
         kind == RuntimeProgramKind::kMonitorCheck;
}

// True for runtime program kinds that use the deferred assertion thunk ABI.
inline auto UsesDeferredAssertionThunkAbi(RuntimeProgramKind kind) -> bool {
  return kind == RuntimeProgramKind::kDeferredAssertionThunk;
}

// ABI contract: what hidden context a callable accepts.
// Formed at callable-definition policy level, not derived from body.
// Orthogonal to BodyExecutionRequirement; verifier checks compatibility.
struct CallableAbiContract {
  // Module binding group: this_ptr + instance_ptr + instance_id.
  bool needs_module_binding = false;
  // Active decision owner context may be threaded to this callable.
  bool accepts_decision_owner = false;
};

struct FunctionParam {
  TypeId type;
  PassingKind kind = PassingKind::kValue;
};

// Frozen at pre-allocation time. Immutable once the FunctionId is reserved.
struct FunctionSignature {
  TypeId return_type;
  std::vector<FunctionParam> params;
  ReturnPolicy return_policy = ReturnPolicy::kDirect;
};

// A function is a callable unit that cannot suspend.
// Allowed terminators: Control (Jump/Branch/Switch), Return.
struct Function {
  FunctionSignature signature;  // Frozen at pre-allocation
  RuntimeProgramKind runtime_kind = RuntimeProgramKind::kNone;

  // Canonical symbol identity for design-global callables (package functions,
  // generated design-level functions). Body-local functions leave this
  // as kInvalidSymbolId. Used by the backend for DesignFunctionRef resolution.
  SymbolId canonical_symbol = kInvalidSymbolId;

  BasicBlockId entry;              // Local index within blocks
  std::vector<BasicBlock> blocks;  // Direct ownership

  // Storage metadata (for interpreter frame allocation)
  std::vector<TypeId> local_types;  // Types for each local slot
  std::vector<TypeId> temp_types;   // Types for each temp slot (deprecated)

  // Authoritative temp metadata (indexed by temp_id).
  // Contains kind (kValue vs kPlace) and type for each temp.
  std::vector<TempMetadata> temp_metadata;

  // Explicit parameter-to-local mapping.
  // param_local_slots[i] = local slot index for parameter i.
  // Size must equal signature.params.size().
  std::vector<uint32_t> param_local_slots;

  // Per-parameter origins for prologue error reporting.
  // param_origins[i] = origin for parameter i's allocation/initialization.
  // Size must equal signature.params.size().
  std::vector<common::OriginId> param_origins;

  common::OriginId origin = common::OriginId::Invalid();  // Source location

  // Stats: materialize-to-place operations (for --stats output).
  uint64_t materialize_count = 0;

  // Decision site metadata for this function body, indexed by body-global
  // DecisionId offset. Stored here so the LLVM backend can include function-
  // body sites in the owner's decision table alongside process-body sites.
  // Decision site records with body-global IDs (same type as Process).
  std::vector<Process::MirDecisionSiteRecord> decision_sites;

  // Body execution requirement: computed once at metadata formation time
  // from the effect ops in this function's body. Stored here as the single
  // source of truth; consumers (verifier, call lowering) read this field
  // rather than re-walking the body.
  BodyExecutionRequirement body_requirement =
      BodyExecutionRequirement::kGenericCallable;

  // ABI contract: what hidden context this callable accepts.
  // Formed from callable-definition policy, independent of body_requirement.
  // The verifier checks that body_requirement is satisfiable by this contract.
  CallableAbiContract abi_contract;
};

// Compute the body execution requirement for a function by walking all
// effect ops in its blocks. Takes the max requirement across all effects.
auto ComputeBodyExecutionRequirement(const Function& func)
    -> BodyExecutionRequirement;

// Seed the ABI contract for a callable from intrinsic body requirement.
// Sets accepts_decision_owner = true iff this body directly contains
// deferred-check-owner-required effects. Observer programs never accept (own
// ABI). needs_module_binding is set separately by the backend.
//
// This is the seed step only. PropagateDeferredOwnerAbi() must run
// after all bodies are set to propagate through the call graph: if A
// calls B, and B accepts, then A must also accept (to carry decision_owner_id).
auto BuildCallableAbiContract(const Function& func) -> CallableAbiContract;

// Check if a function calls any callee that accepts a decision owner.
// Covers both local (FunctionId) and design-global (DesignFunctionRef) edges.
// design_arena may be null if no cross-arena lookup is needed.
class Arena;
auto CallsCalleeAcceptingDecisionOwner(
    const Function& func, const std::vector<Function>& local_functions,
    const Arena* design_arena) -> bool;

}  // namespace lyra::mir
