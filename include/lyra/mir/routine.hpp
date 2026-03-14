#pragma once

#include <cstdint>
#include <vector>

#include "lyra/common/origin_id.hpp"
#include "lyra/common/type.hpp"
#include "lyra/mir/basic_block.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"

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
// IsObserverProgram() is the canonical predicate for the observer-ABI class.
enum class RuntimeProgramKind {
  kNone,
  kStrobe,
  kMonitorCheck,
  kMonitorSetup,
};

// True for runtime program kinds that use the observer program ABI.
inline auto IsObserverProgram(RuntimeProgramKind kind) -> bool {
  return kind == RuntimeProgramKind::kStrobe ||
         kind == RuntimeProgramKind::kMonitorSetup ||
         kind == RuntimeProgramKind::kMonitorCheck;
}

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
};

}  // namespace lyra::mir
