#pragma once

#include <filesystem>
#include <span>
#include <string>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/driver/dpi_boundary.hpp"
#include "lyra/driver/pch.hpp"
#include "lyra/driver/runtime_export.hpp"
#include "lyra/mir/compilation_unit.hpp"

namespace lyra::driver {

// Assemble a self-contained C++ project into `dir`: one translation unit per
// compiled unit, a program `main` constructing the design-root unit `root`, the
// design's DPI-C boundary surface with a copy of each foreign source, a
// `build.sh` recipe, and a bundled copy of `runtime`. The directory then builds
// with no external include or link paths, and on a machine that has never seen
// the original foreign sources.
auto AssembleProject(
    const RuntimeLocation& runtime, std::span<const mir::CompilationUnit> units,
    const mir::CompilationUnit& root, const std::filesystem::path& dir,
    bool format, std::span<const DpiLinkInput> dpi_inputs)
    -> diag::Result<void>;

// Build the assembled project in `dir` by invoking the C++ compiler directly
// (the same recipe `build.sh` carries). Returns the produced executable's path;
// a non-zero compiler exit surfaces its stderr as a diagnostic.
auto BuildProject(
    const std::filesystem::path& dir, const pch::Options& pch_opts,
    std::span<const DpiLinkInput> dpi_inputs)
    -> diag::Result<std::filesystem::path>;

// Emit, build, and run the design in `work_dir`, returning the program's exit
// code. `root` is the design-root unit the program constructs. `child_args` are
// forwarded verbatim as argv to the built program (LRM 21.6 plusargs land
// here). `dpi_inputs` are the foreign sources compiled and linked into the
// program (LRM 35). This is the ephemeral path behind `run`: it never
// materializes a portable project.
auto RunInPlace(
    const RuntimeLocation& runtime, std::span<const mir::CompilationUnit> units,
    const mir::CompilationUnit& root, const std::filesystem::path& work_dir,
    bool format, const pch::Options& pch_opts,
    std::span<const std::string> child_args,
    std::span<const DpiLinkInput> dpi_inputs) -> diag::Result<int>;

}  // namespace lyra::driver
