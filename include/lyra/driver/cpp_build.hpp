#pragma once

#include <cstddef>
#include <filesystem>
#include <optional>
#include <span>
#include <string>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/backend/cpp/artifact.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/sink.hpp"
#include "lyra/driver/action_cache.hpp"
#include "lyra/driver/dpi_boundary.hpp"
#include "lyra/driver/pch.hpp"
#include "lyra/driver/project_layout.hpp"
#include "lyra/driver/runtime_export.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/stable_hash/fingerprint.hpp"

namespace lyra::driver {

// How this host turns emitted C++ into a program: which compiler to invoke,
// what to do about the precompiled header, how hard to optimize, and how much
// of the machine to take while doing it. Resolved once at the CLI boundary and
// passed unchanged down every path, so the recipe an emitted project carries
// and the compile Lyra performs cannot disagree about the toolchain.
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
};

// A file a build compiles: where it is in the project, the identifier the
// source declares the unit it was written for under, and each file of the
// project compiling it reads -- every one it reaches through an include, as
// what wrote each file recorded. A file written for nothing the source named
// has no identifier.
struct TranslationUnit {
  std::string relpath;
  std::string source_name;
  std::vector<std::string> includes;
};

// What the steps after emission need, carried rather than recovered by reading
// the directory back: the translation units a build compiles, and what each
// unit stated of the program's foreign name space (LRM 35), which the project's
// own assembly writes because the party that reads it is the user's C compiler.
struct EmittedCppSources {
  std::vector<TranslationUnit> translation_units;
  std::vector<dpi::AbiFragment> dpi_fragments;
};

// What writing one body of a unit left in the project: the files, the one a
// build compiles, and the text of what the unit states of the program's
// foreign name space where it states anything; and its declarations, which
// every body of the unit states alike and which are written where the bodies
// are collected, once.
struct WrittenUnit {
  std::string body_name;
  std::vector<std::string> files;
  TranslationUnit translation_unit;
  std::optional<std::string> dpi_fragment;
  std::vector<backend::cpp::CppArtifact> declarations;
};

// A unit this target has no form for somewhere: every construct it refused,
// and nothing written for it. Units are written side by side, so what one
// refused travels back with it rather than into the run's report directly.
struct RefusedUnit {
  std::vector<diag::Diagnostic> refusals;
};

using EmittedUnit = std::variant<WrittenUnit, RefusedUnit>;

// Writes the emitted C++ sources of a project into a directory. A body's code
// is written and released as soon as it is rendered, and its declarations as
// soon as it is collected, so what an emit holds is the bodies in flight
// rather than the design.
//
// A unit this backend has no form for is refused as it arrives, and the other
// units are still attempted, so one run names every gap rather than the first.
// What the directory then holds is the units that did render: a run that
// reported produces nothing any later step carries forward.
class CppProjectSink {
 public:
  // `refusals` is the run's sink: what a unit has no form for on this target is
  // reported into it, and once anything has been, nothing more is written,
  // because files short of the design are not a program.
  CppProjectSink(
      std::filesystem::path dir, SourceFormatting formatting,
      diag::DiagnosticSink& refusals)
      : dir_(std::move(dir)), formatting_(formatting), refusals_(&refusals) {
  }

  // Writes the translation unit realizing one body of a unit, or nothing where
  // the unit holds a construct this target has no form for, and hands back the
  // unit's declarations. Every file a body writes is named for it, and nothing
  // here reads what the sink collects, so several bodies may be written at
  // once.
  [[nodiscard]] auto Write(const mir::CompilationUnit& unit) const
      -> diag::Result<EmittedUnit>;

  // Collects what writing a body left, in the order it is called, which is the
  // order a build compiles the units in; what a body refused is reported. The
  // declarations it handed back are written here: the bodies of one unit state
  // them alike, so one another body wrote already is not written again, and
  // one that reads otherwise is this compiler's defect.
  void Collect(EmittedUnit unit);

  // Closes the project: the design root's own files and the program entry,
  // none of which may be written until every unit has been collected.
  auto Finish(const mir::CompilationUnit& root) -> diag::Result<void>;

  // Hands over what emission produced, in the order the units were collected,
  // each translation unit with every file of the project it reaches.
  [[nodiscard]] auto TakeSources() -> EmittedCppSources;

 private:
  [[nodiscard]] auto WriteArtifact(const backend::cpp::CppArtifact& file) const
      -> diag::Result<void>;
  // Writes the declarations `written` handed back that no body collected
  // before it wrote, reporting a file that could not be written.
  void WriteDeclarations(WrittenUnit& written);

  std::filesystem::path dir_;
  SourceFormatting formatting_;
  diag::DiagnosticSink* refusals_;
  // Each declaration written so far, by its path, as a digest of what it says.
  std::unordered_map<std::string, stable_hash::Fingerprint> declared_;
  // The files of the project each declaration written so far includes, by its
  // path.
  std::unordered_map<std::string, std::vector<std::string>> includes_of_;
  // Formatting runs one process over every file rather than one per file, so
  // what was written is remembered while the text itself is not.
  std::vector<std::string> written_;
  std::vector<TranslationUnit> translation_units_;
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

// Compiles each emitted translation unit in `dir` to its object, as many at
// once as `host` was told to take, in the order given, and reports every one
// that failed rather than the first; a non-zero compiler exit surfaces its
// stderr. Each compile is an action whose inputs are what the compiler reads
// of the project -- the file and every file of the project it includes -- and
// the runtime's headers, the compiler and the level, so a file whose text and
// whose includes are unchanged is compiled once, in this build or any other.
//
// The compiler is invoked directly rather than through the recipe an emitted
// project ships. The two are not interchangeable: the recipe compiles against
// the copy of the runtime bundled beside it, which is what makes the project
// portable, while this compiles against the installed runtime and copies
// nothing. Sharing `HostBuild` is what keeps them agreeing on the toolchain
// regardless.
auto CompileTranslationUnits(
    const std::filesystem::path& dir,
    std::span<const TranslationUnit> translation_units,
    const RuntimeLocation& runtime, const HostBuild& host, ActionCache& actions)
    -> diag::Result<std::vector<ActionResult>>;

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
