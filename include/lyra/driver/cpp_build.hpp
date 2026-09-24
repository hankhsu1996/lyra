#pragma once

#include <cstddef>
#include <filesystem>
#include <optional>
#include <span>
#include <string>
#include <utility>
#include <vector>

#include "lyra/backend/cpp/artifact.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/driver/dpi_boundary.hpp"
#include "lyra/driver/pch.hpp"
#include "lyra/driver/project_layout.hpp"
#include "lyra/driver/runtime_export.hpp"
#include "lyra/mir/compilation_unit.hpp"

namespace lyra::driver {

// How this host turns emitted C++ into a program: which compiler to invoke,
// what to do about the precompiled header, how hard to optimize, how much of
// the machine to take while doing it, and where what it builds is kept for the
// next build. Resolved once at the CLI boundary and passed unchanged down every
// path, so the recipe an emitted project carries and the compile Lyra performs
// cannot disagree about the toolchain.
//
// What a build produces, and whether it works at all, is baked into that
// recipe. How much of a machine to take, and where to keep things, are not:
// both are true of one machine at one moment, so the recipe asks its own
// caller and keeps what it prepares beside itself.
struct HostBuild {
  std::filesystem::path cxx;
  pch::Policy pch = pch::Policy::kAttempt;
  Optimization optimization = Optimization::kIterate;
  // How many host compiles may run at once, already a positive count.
  std::size_t compile_width = 1;
  // Absent when no store could be located, and then nothing is kept.
  std::optional<std::filesystem::path> store;
};

// What the steps after emission need, carried rather than recovered by reading
// the directory back: the translation units a build compiles, and what each
// unit stated of the program's foreign name space (LRM 35), which the project's
// own assembly writes because the party that reads it is the user's C compiler.
struct EmittedCppSources {
  std::vector<std::string> translation_units;
  std::vector<dpi::AbiFragment> dpi_fragments;
};

// What writing one unit's sources left in the project: the files, the one a
// build compiles, and the text of what the unit states of the program's
// foreign name space where it states anything.
struct WrittenUnit {
  std::vector<std::string> files;
  std::string translation_unit;
  std::optional<std::string> dpi_fragment;
};

// Writes the emitted C++ sources of a project into a directory. A unit's
// rendered text is written and released as soon as it is rendered, so what an
// emit holds is the units in flight rather than the design.
//
// A unit this backend has no form for is refused as it arrives, and the other
// units are still attempted, so one run names every gap rather than the first.
// What the directory then holds is the units that did render: a run that
// reported produces nothing any later step carries forward.
class CppProjectSink {
 public:
  CppProjectSink(std::filesystem::path dir, SourceFormatting formatting)
      : dir_(std::move(dir)), formatting_(formatting) {
  }

  // Writes the unit's declarations and the translation unit realizing them.
  // Every file a unit writes is named for that unit, and this reads nothing
  // the sink collects, so several units may be written at once.
  [[nodiscard]] auto Write(const mir::CompilationUnit& unit) const
      -> diag::Result<WrittenUnit>;

  // Collects what writing a unit left, in the order it is called, which is the
  // order a build compiles the units in.
  void Collect(WrittenUnit unit);

  // Closes the project: the design root's own files and the program entry,
  // none of which may be written until every unit has been collected.
  auto Finish(const mir::CompilationUnit& root) -> diag::Result<void>;

  // Hands over what emission produced, in the order the units were collected.
  [[nodiscard]] auto TakeSources() -> EmittedCppSources {
    return EmittedCppSources{
        .translation_units = std::move(translation_units_),
        .dpi_fragments = std::move(dpi_fragments_)};
  }

 private:
  [[nodiscard]] auto WriteArtifact(const backend::cpp::CppArtifact& file) const
      -> diag::Result<void>;

  std::filesystem::path dir_;
  SourceFormatting formatting_;
  // Formatting runs one process over every file rather than one per file, so
  // what was written is remembered while the text itself is not.
  std::vector<std::string> written_;
  std::vector<std::string> translation_units_;
  std::vector<dpi::AbiFragment> dpi_fragments_;
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
    const RuntimeLocation& runtime, const EmittedCppSources& sources,
    const std::filesystem::path& dir, const HostBuild& host,
    std::span<const DpiLinkInput> dpi_inputs) -> diag::Result<void>;

// Compiles the emitted sources in `dir` and links them, with the foreign
// sources' objects, into the program at `program`; a non-zero compiler exit
// surfaces its stderr as a diagnostic.
//
// The compiler is invoked directly rather than through the recipe an emitted
// project ships. The two are not interchangeable: the recipe compiles against
// the copy of the runtime bundled beside it, which is what makes the project
// portable, while this compiles against the installed runtime and copies
// nothing. Sharing `HostBuild` is what keeps them agreeing on the toolchain
// regardless.
auto CompileProgram(
    const std::filesystem::path& dir,
    std::span<const std::string> translation_units,
    const RuntimeLocation& runtime,
    std::span<const std::filesystem::path> foreign_objects,
    const std::filesystem::path& program, const HostBuild& host)
    -> diag::Result<void>;

// Links a program's objects -- whatever produced them -- with the runtime
// library into the executable at `program`; a non-zero linker exit surfaces its
// stderr as a diagnostic. The host C++ compiler drives the link, because the
// runtime library is C++ and that driver is what names the libraries it needs.
auto LinkProgram(
    std::span<const std::filesystem::path> objects,
    const std::filesystem::path& runtime_lib,
    const std::filesystem::path& program, const std::filesystem::path& cxx)
    -> diag::Result<void>;

}  // namespace lyra::driver
