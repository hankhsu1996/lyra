#pragma once

#include <cstdint>
#include <memory>
#include <string>

#include <llvm/IR/Module.h>
#include <llvm/Target/TargetMachine.h>

#include "lyra/common/opt_level.hpp"

namespace lyra::lowering::mir_to_llvm {

// Emitted-binary ISA policy for x86-64 targets.
//
// kPortable: explicit baseline (x86-64-v3 = AVX2 + FMA + BMI).
//   Safe across all modern x86-64 machines including CI runners,
//   cloud VMs, and heterogeneous fleets. Default for AOT.
//
// kNative: host CPU with all detected features.
//   Maximum performance for the compile host. Default for JIT
//   (compile and execute on the same machine).
enum class IsaPolicy : uint8_t {
  kPortable,
  kNative,
};

// Resolved target configuration from an ISA policy.
struct TargetConfig {
  std::string triple;
  std::string cpu;
  std::string features;
};

// Resolve an ISA policy to a concrete target configuration.
auto ResolveTargetConfig(IsaPolicy policy) -> TargetConfig;

// Create an LLVM TargetMachine from an ISA policy.
// Initializes LLVM native targets on first call (thread-safe).
auto CreateTargetMachine(IsaPolicy policy, OptLevel opt_level = OptLevel::kO2)
    -> std::unique_ptr<llvm::TargetMachine>;

// Set module DataLayout and TargetTriple for x86-64.
// Uses a stable generic configuration (not host-specific tuning).
void SetModuleDataLayout(llvm::Module& module);

}  // namespace lyra::lowering::mir_to_llvm
