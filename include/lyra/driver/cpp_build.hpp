#pragma once

#include <cstdint>
#include <filesystem>
#include <span>
#include <string>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/driver/dpi_boundary.hpp"
#include "lyra/driver/pch.hpp"
#include "lyra/driver/project_layout.hpp"
#include "lyra/driver/runtime_export.hpp"
#include "lyra/mir/compilation_unit.hpp"

namespace lyra::driver {

// Whether to run the emitted C++ through a formatter before writing it. Named
// rather than a bare bool so a call site says which it means.
enum class SourceFormatting : std::uint8_t { kOff, kOn };

// How this host turns emitted C++ into a program: which compiler to invoke,
// what to do about the precompiled header, and how hard to optimize. Resolved
// once at the CLI boundary and passed unchanged down every path, so the recipe
// an emitted project carries and the compile Lyra performs cannot disagree --
// which matters most for optimization, since the two would otherwise produce
// programs that differ by an order of magnitude in speed.
struct HostBuild {
  std::filesystem::path cxx;
  pch::Options pch;
  Optimization optimization = Optimization::kIterate;
};

// Assemble a self-contained C++ project into `dir`: one translation unit per
// compiled unit, a program `main` constructing the design-root unit `root`, the
// design's DPI-C boundary surface with a copy of each foreign source, a
// `build.sh` recipe, and a bundled copy of `runtime`. The directory then builds
// with no external include or link paths, and on a machine that has never seen
// the original foreign sources.
//
// The compiler is baked into the recipe, so the project builds in a shell with
// nothing configured -- which is what self-contained has to mean. Carried to a
// machine where that path means nothing, the recipe takes a replacement as an
// argument.
auto AssembleProject(
    const RuntimeLocation& runtime, std::span<const mir::CompilationUnit> units,
    const mir::CompilationUnit& root, const std::filesystem::path& dir,
    SourceFormatting formatting, const HostBuild& host,
    std::span<const DpiLinkInput> dpi_inputs) -> diag::Result<void>;

// Build the assembled project in `dir`, returning the produced executable's
// path; a non-zero compiler exit surfaces its stderr as a diagnostic.
//
// The compiler is invoked directly rather than through the project's own
// recipe. The two are not interchangeable: the recipe compiles against the copy
// of the runtime bundled beside it, which is what makes the project portable,
// while the ephemeral path below has no such copy and must reach the installed
// runtime instead. Sharing `HostBuild` is what keeps them agreeing on the
// toolchain regardless.
auto BuildProject(
    const std::filesystem::path& dir, const HostBuild& host,
    std::span<const DpiLinkInput> dpi_inputs)
    -> diag::Result<std::filesystem::path>;

// Emit, build, and run the design in `work_dir`, returning the program's exit
// code. `root` is the design-root unit the program constructs. `child_args` are
// forwarded verbatim as argv to the built program (LRM 21.6 plusargs land
// here). `dpi_inputs` are the foreign sources compiled and linked into the
// program (LRM 35). This is the ephemeral path behind `run`: it compiles
// against the installed runtime and never materializes a portable project,
// which is why copying a runtime tree per invocation is not on its critical
// path.
auto RunInPlace(
    const RuntimeLocation& runtime, std::span<const mir::CompilationUnit> units,
    const mir::CompilationUnit& root, const std::filesystem::path& work_dir,
    SourceFormatting formatting, const HostBuild& host,
    std::span<const std::string> child_args,
    std::span<const DpiLinkInput> dpi_inputs) -> diag::Result<int>;

}  // namespace lyra::driver
