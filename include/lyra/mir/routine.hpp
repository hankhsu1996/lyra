#pragma once

#include <vector>

#include "lyra/common/origin_id.hpp"
#include "lyra/common/type.hpp"
#include "lyra/mir/basic_block.hpp"
#include "lyra/mir/handle.hpp"

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
};

enum class PassingKind {
  kValue,  // Input by value (only mode supported today)
};

enum class ReturnPolicy {
  kDirect,  // Return value in register/local 0
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

  BasicBlockId entry;              // Local index within blocks
  std::vector<BasicBlock> blocks;  // Direct ownership

  // Storage metadata (for interpreter frame allocation)
  std::vector<TypeId> local_types;  // Types for each local slot
  std::vector<TypeId> temp_types;   // Types for each temp slot

  // Explicit parameter-to-local mapping.
  // param_local_slots[i] = local slot index for parameter i.
  // Size must equal signature.params.size().
  std::vector<uint32_t> param_local_slots;

  // Per-parameter origins for prologue error reporting.
  // param_origins[i] = origin for parameter i's allocation/initialization.
  // Size must equal signature.params.size().
  std::vector<common::OriginId> param_origins;

  common::OriginId origin = common::OriginId::Invalid();  // Source location
};

}  // namespace lyra::mir
