#pragma once

#include <filesystem>

#include <llvm/Passes/OptimizationLevel.h>

#include "lyra/backend/llvm/emit.hpp"
#include "lyra/diag/diagnostic.hpp"

namespace lyra::backend::llvm_backend {

// Compiles a module to the relocatable object at `object`, for the kind of
// machine this runs on rather than for this machine in particular, so a
// program linked from it runs wherever that kind of machine does. A module
// owns its context and nothing here is shared between two compiles, so several
// modules may be compiled at once.
//
// The module is taken through the toolchain's standard pipeline at `level`,
// the way a C++ compiler takes a translation unit at the `-O` it was given.
// That pipeline also makes a suspending body executable, at every level: such a
// body reaches here as an ordinary function carrying coroutine intrinsics, and
// it is the coroutine passes that derive its frame and resume state. So nothing
// here names a pass, and an unoptimized object is still a correct one.
auto WriteObjectFile(
    EmittedModule module, const std::filesystem::path& object,
    const llvm::OptimizationLevel& level) -> diag::Result<void>;

}  // namespace lyra::backend::llvm_backend
