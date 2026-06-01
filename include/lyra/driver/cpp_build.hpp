#pragma once

#include <filesystem>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/driver/runtime_export.hpp"
#include "lyra/mir/compilation_unit.hpp"

namespace lyra::driver {

// Assemble a self-contained C++ project for `unit` into `dir`: the generated
// translation units, a `build.sh` recipe, and a bundled copy of `runtime`. The
// directory then builds with no external include or link paths.
auto AssembleProject(
    const RuntimeLocation& runtime, const mir::CompilationUnit& unit,
    const std::filesystem::path& dir) -> diag::Result<void>;

// Build the assembled project in `dir` by invoking the C++ compiler directly
// (the same recipe `build.sh` carries). Returns the produced executable's path;
// a non-zero compiler exit surfaces its stderr as a diagnostic.
auto BuildProject(const std::filesystem::path& dir)
    -> diag::Result<std::filesystem::path>;

// Emit `unit`'s sources into `work_dir`, build them in place against `runtime`
// (no bundled copy), execute the result streaming its stdout/stderr, and return
// its exit code. This is the ephemeral path behind `run`: it never materializes
// a portable project.
auto RunInPlace(
    const RuntimeLocation& runtime, const mir::CompilationUnit& unit,
    const std::filesystem::path& work_dir) -> diag::Result<int>;

}  // namespace lyra::driver
