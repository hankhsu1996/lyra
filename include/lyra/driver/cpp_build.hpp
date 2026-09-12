#pragma once

#include <cstdint>
#include <filesystem>
#include <span>
#include <string>
#include <utility>
#include <vector>

#include "lyra/backend/cpp/artifact.hpp"
#include "lyra/compiler/unit_program_record.hpp"
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
// an emitted project carries and the compile Lyra performs cannot disagree
// about any of the three.
struct HostBuild {
  std::filesystem::path cxx;
  pch::Options pch;
  Optimization optimization = Optimization::kIterate;
};

// Writes the emitted C++ sources of a project into a directory, one unit at a
// time. A unit's rendered text is written and released before the next unit is
// lowered, so what an emit holds is one unit's worth rather than the design's.
//
// A unit this backend has no form for is refused as it arrives, and the units
// behind it are still attempted, so one run names every gap rather than the
// first. What the directory then holds is the units that did render: a run
// that reported produces nothing any later step carries forward.
class CppProjectSink {
 public:
  CppProjectSink(std::filesystem::path dir, SourceFormatting formatting)
      : dir_(std::move(dir)), formatting_(formatting) {
  }

  auto Take(
      const mir::CompilationUnit& unit,
      const compiler::UnitProgramRecord& record) -> diag::Result<void>;

  // Closes the project: the design root's own translation unit, the program
  // entry, and whatever has to see every file at once. The entry names each
  // unit whose definitions only foreign C reaches, which is what the records
  // state, so neither it nor anything after it can be written until every unit
  // has been taken.
  auto Finish(
      const mir::CompilationUnit& root,
      std::span<const compiler::UnitProgramRecord> records)
      -> diag::Result<void>;

 private:
  auto Write(backend::cpp::CppArtifact file) -> diag::Result<void>;

  std::filesystem::path dir_;
  SourceFormatting formatting_;
  // Formatting runs one process over every file rather than one per file, so
  // what was written is remembered while the text itself is not.
  std::vector<std::string> written_;
};

// Completes a self-contained C++ project in `dir` around sources a
// `CppProjectSink` has already written: the design's DPI-C boundary surface
// with a copy of each foreign source, a `build.sh` recipe, and a bundled copy
// of `runtime`. The directory then builds with no external include or link
// paths, and on a machine that has never seen the original foreign sources.
//
// The compiler is baked into the recipe, so the project builds in a shell with
// nothing configured -- which is what self-contained has to mean. Carried to a
// machine where that path means nothing, the recipe takes a replacement as an
// argument.
auto AssembleProject(
    const RuntimeLocation& runtime,
    std::span<const compiler::UnitProgramRecord> records,
    const std::filesystem::path& dir, const HostBuild& host,
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
    const RuntimeLocation& runtime,
    std::span<const compiler::UnitProgramRecord> records,
    const std::filesystem::path& work_dir, const HostBuild& host,
    std::span<const std::string> child_args,
    std::span<const DpiLinkInput> dpi_inputs) -> diag::Result<int>;

}  // namespace lyra::driver
