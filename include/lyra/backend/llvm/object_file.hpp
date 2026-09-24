#pragma once

#include <filesystem>

#include "lyra/backend/llvm/emit.hpp"
#include "lyra/diag/diagnostic.hpp"

namespace llvm {
class Module;
}  // namespace llvm

namespace lyra::backend::llvm_backend {

// Makes a module's suspending bodies executable. Such a body reaches here as an
// ordinary function carrying coroutine intrinsics, and the coroutine passes
// derive its frame, its resume state, and the values that must survive a
// suspension -- that derivation is theirs, since the module states where a body
// suspends and never how it resumes. It is a translation step rather than an
// optimization the module could also be correct without, so whoever compiles a
// module runs it first.
void LowerCoroutines(llvm::Module& module);

// Compiles one module to the relocatable object a system linker takes, for the
// kind of machine this runs on rather than for this machine in particular, so
// a program linked from it runs wherever that kind of machine does.
auto WriteObjectFile(EmittedModule module, const std::filesystem::path& path)
    -> diag::Result<void>;

}  // namespace lyra::backend::llvm_backend
