#pragma once

#include <string_view>

#include <llvm/Passes/OptimizationLevel.h>
#include <llvm/Support/CodeGen.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/TargetParser/Triple.h>

namespace lyra::backend::llvm_backend {

// The processor every module is compiled for: the one clang names for an
// x86-64 target given no `-march` (`getX86TargetCPU`), which is how the runtime
// library was compiled. A call between two functions compiled for different
// processors is never inlined, so a design's functions and the runtime's meet
// in one optimization only where they name the same one.
inline constexpr std::string_view kTargetCpu = "x86-64";

// The processor a module is tuned for, again clang's default.
inline constexpr std::string_view kTuneCpu = "generic";

// Every module's code is position-independent, because the executable the
// host's linker makes by default is.
inline constexpr llvm::Reloc::Model kRelocation = llvm::Reloc::PIC_;

// The kind of machine every module is compiled for, and the target that
// compiles for it: the kind this process runs on, rather than this machine in
// particular, so a program runs wherever that kind of machine does.
struct CompileTarget {
  llvm::Triple triple;
  const llvm::Target* target = nullptr;
};

// Registered and looked up once for the whole process, the first time anything
// asks.
auto ProcessTarget() -> const CompileTarget&;

// How hard code generation works at a pipeline level, paired as clang pairs
// them for each `-O`.
auto CodeGenLevelOf(llvm::OptimizationLevel level) -> llvm::CodeGenOptLevel;

}  // namespace lyra::backend::llvm_backend
