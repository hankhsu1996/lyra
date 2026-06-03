#pragma once

#include <filesystem>
#include <span>

#include "lyra/backend/cpp/api.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/driver/runtime_export.hpp"
#include "lyra/mir/compilation_unit.hpp"

namespace lyra::driver {

// Assemble a self-contained C++ project into `dir`: one translation unit per
// compiled unit, a program `main` constructing each top in `tops`, a `build.sh`
// recipe, and a bundled copy of `runtime`. The directory then builds with no
// external include or link paths.
auto AssembleProject(
    const RuntimeLocation& runtime, std::span<const mir::CompilationUnit> units,
    std::span<const backend::cpp::TopInstance> tops,
    const std::filesystem::path& dir, bool format) -> diag::Result<void>;

// Build the assembled project in `dir` by invoking the C++ compiler directly
// (the same recipe `build.sh` carries). Returns the produced executable's path;
// a non-zero compiler exit surfaces its stderr as a diagnostic.
auto BuildProject(const std::filesystem::path& dir)
    -> diag::Result<std::filesystem::path>;

// Emit `units`' sources into `work_dir`, build them in place against `runtime`
// (no bundled copy), execute the result streaming its stdout/stderr, and return
// its exit code. `tops` are the top-level blocks the program constructs. This
// is the ephemeral path behind `run`: it never materializes a portable project.
auto RunInPlace(
    const RuntimeLocation& runtime, std::span<const mir::CompilationUnit> units,
    std::span<const backend::cpp::TopInstance> tops,
    const std::filesystem::path& work_dir, bool format) -> diag::Result<int>;

}  // namespace lyra::driver
