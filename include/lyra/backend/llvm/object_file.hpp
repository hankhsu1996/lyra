#pragma once

#include <cstdint>
#include <filesystem>

#include <llvm/Passes/OptimizationLevel.h>

#include "lyra/backend/llvm/emit.hpp"
#include "lyra/diag/diagnostic.hpp"

namespace lyra::backend::llvm_backend {

// What compiling a module produces: what clang produces at `-O<n>`, and at
// `-O<n> -flto=thin`.
enum class ModuleOutput : std::uint8_t {
  // Machine code, which a host linker links as it is.
  kObject,
  // For a link that optimizes the whole program: bitcode taken through the
  // ThinLTO pre-link pipeline, carrying the summary that link decides what to
  // import from.
  kThinLtoBitcode,
};

// Compiles a module to `output` in the form asked for, through the pipeline of
// `level`, for the kind of machine this runs on rather than for this machine in
// particular, so a program linked from it runs wherever that kind of machine
// does. A module owns its context
// and nothing here is shared between two compiles, so several modules may be
// compiled at once.
//
// The module is taken through the toolchain's standard pipeline for that form,
// the way a C++ compiler takes a translation unit. That pipeline also makes a
// suspending body executable: such a body reaches here as an ordinary function
// carrying coroutine intrinsics, and it is the coroutine passes that derive its
// frame and resume state. So nothing here names an optimization pass, and an
// unoptimized object is still a correct one.
auto CompileModule(
    EmittedModule module, const std::filesystem::path& output,
    llvm::OptimizationLevel level, ModuleOutput form) -> diag::Result<void>;

}  // namespace lyra::backend::llvm_backend
