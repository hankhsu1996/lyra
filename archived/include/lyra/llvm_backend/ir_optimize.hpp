#pragma once

namespace llvm {
class Module;
}

namespace lyra {
enum class OptLevel;
}

namespace lyra::lowering::mir_to_llvm {

// Run pre-codegen LLVM IR canonicalization on the final completed module.
//
// This is the shared stage between lowering and backend handoff. It runs
// exactly once on the final module, immediately before AOT object emission
// or ORC JIT compilation. Both paths call this function with identical
// semantics.
//
// At O0: verify the module only.
// At O1/O2/O3: verify before optimization, run LLVM's standard default
// per-module optimization pipeline for the corresponding level, then verify
// again after optimization.
//
// Pre-optimization verification catches lowering bugs.
// Post-optimization verification catches pipeline integration issues.
//
// This stage is IR canonicalization, not lowering repair. Do not add
// Lyra-specific passes here unless a concrete gap in LLVM's standard
// pipeline is demonstrated.
void OptimizeModule(llvm::Module& module, OptLevel level);

}  // namespace lyra::lowering::mir_to_llvm
