#pragma once

#include <cstddef>
#include <filesystem>
#include <optional>
#include <span>
#include <vector>

#include <llvm/Passes/OptimizationLevel.h>

#include "lyra/diag/diagnostic.hpp"

namespace lyra::backend::llvm_backend {

// What a program is made of, as a link that optimizes it whole reads it.
struct LinkTimeInputs {
  // The design's modules, as ThinLTO pre-link bitcode. Every one is part of
  // the program.
  std::span<const std::filesystem::path> modules;
  // The runtime library: an archive whose members carry their bitcode beside
  // their machine code. A member is part of the program where it defines a
  // symbol the program needs, as a linker takes an archive member.
  std::filesystem::path runtime_library;
  // Objects the host link takes as machine code. Whatever they name stays
  // defined, since no optimization here can see them use it.
  std::span<const std::filesystem::path> machine_code;
};

// Optimizes the bitcode a program's inputs carry as one program, as a linker
// doing ThinLTO does: one summary of the whole, then each module optimized at
// `level` with what it imports from the others and compiled to machine code,
// `width` of them at once. Answers the objects written into `dir`, which the
// host link takes in place of every input that carried bitcode. A runtime
// member carrying none is left to the host link, which takes it from the
// archive as it would anyway.
//
// Each module's compile is kept in `cache`, where one is given, under a key
// that covers everything that compile reads, so a link after an edit compiles
// only the modules whose code or imports changed.
auto OptimizeAtLink(
    const LinkTimeInputs& inputs, llvm::OptimizationLevel level,
    const std::filesystem::path& dir, std::size_t width,
    const std::optional<std::filesystem::path>& cache)
    -> diag::Result<std::vector<std::filesystem::path>>;

}  // namespace lyra::backend::llvm_backend
