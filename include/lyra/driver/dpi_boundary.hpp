#pragma once

#include <cstddef>
#include <filesystem>
#include <optional>
#include <span>
#include <string>
#include <variant>
#include <vector>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/dpi/abi_header.hpp"
#include "lyra/driver/project_layout.hpp"
#include "lyra/driver/runtime_export.hpp"
#include "lyra/support/sanitizer.hpp"

namespace lyra::driver {

// What the preprocessor of a foreign source is told beyond its language and
// the boundary it compiles against: the macros and the include directories the
// source set naming it states for its foreign code, a fact of that code for
// everyone who builds it, as a C build states its definitions and include
// paths apart from its compiler's options.
struct ForeignPreprocessing {
  std::vector<std::string> defines;
  std::vector<std::filesystem::path> incdir;

  auto operator==(const ForeignPreprocessing&) const -> bool = default;
};

// A DPI-C link input as a declaration or the command line names it, with what
// the source set naming it states for compiling it. One the command line names
// states nothing.
struct DpiLinkRequest {
  std::filesystem::path path;
  ForeignPreprocessing preprocessing;
};

// A user's native source supplying DPI-C foreign symbols (LRM 35), classified
// by the language it must be compiled as. A C source is compiled on its own so
// its symbols keep C linkage -- an import's emitted declaration expects that,
// and a C++ driver invocation would otherwise mangle it -- while a C++ source
// joins the C++ link directly, having given its entry points that linkage
// itself.
struct DpiSource {
  std::filesystem::path path;
  bool compile_as_c;
  ForeignPreprocessing preprocessing;
};

// Foreign code handed over already compiled into a relocatable object or an
// archive of them, which the program's link takes as it is.
struct DpiCompiledCode {
  std::filesystem::path path;
};

// Foreign code handed over as a shared library (LRM Annex J), which the
// program is linked against and which the dynamic loader loads with it. The
// loader finds it by its soname where it declares one, and otherwise by the
// name the link was handed.
struct DpiSharedLibrary {
  std::filesystem::path path;
  std::optional<std::string> soname;
};

using DpiLinkInput = std::variant<DpiSource, DpiCompiledCode, DpiSharedLibrary>;

// What the program's link takes of its foreign code: each source compiled to
// its object, and everything else as it was handed over.
using DpiLinked = std::variant<DpiCompiledCode, DpiSharedLibrary>;

// Classifies and checks every DPI-C link input once, before any backend runs,
// so an unreadable, unsupported, or ambiguously named input is reported against
// the command line rather than surfacing much later as a compiler error, a
// linker error, or a silently overwritten intermediate. A file named a second
// time is taken once (LRM J.4), and a source named again with other macros or
// include directories is refused. Every consumer -- a build of either backend,
// and the recipe an emitted project ships -- reads the classification from
// here rather than re-deriving it.
auto ValidateDpiLinkInputs(std::span<const DpiLinkRequest> inputs)
    -> diag::Result<std::vector<DpiLinkInput>>;

// The file name an input is known by beside the others wherever they are
// gathered into one directory: what is made from a source and the copy an
// emitted project carries are named after it. A shared library is known by the
// file the dynamic loader looks for.
auto GatheredNameOf(const DpiLinkInput& input) -> std::string;

// Writes the design's DPI-C boundary surface into `dir` (LRM 35): the header a
// foreign source includes, naming the per-unit fragments already written there,
// the standard headers those prototypes and the VPI are spelled in, and the
// header of the TF routines, which declares nothing (LRM 36.1). `fragments` is
// a file list rather than anything read out of the design, which is what makes
// assembling the union a step of the build. A foreign source compiles against
// `dir` whichever backend runs the design. Written for every design -- one that
// declares no DPI-C names no fragment -- so no consumer needs a case for its
// absence. Answers every file it wrote, which is the surface and nothing else
// `dir` holds.
auto WriteDpiSurface(
    const RuntimeLocation& runtime, std::span<const dpi::AbiFragment> fragments,
    const std::filesystem::path& dir)
    -> diag::Result<std::vector<std::filesystem::path>>;

// What a foreign source is compiled with: the language it is compiled as and
// the standard where its language has one to name (LRM 35), the macros and
// include directories its source set states, then `boundary`, the directory
// the boundary surface was written in, at `optimization` and checked for
// `sanitizer`. A C source is compiled as C so its symbols keep C linkage, which
// is what the emitted declaration expects and what a C++ compilation would
// mangle away. The in-process build and the recipe an emitted project ships
// both read it here, so the two compile a source alike. How the compiled code
// names its source is not among them: the recipe runs in the project it ships
// and names each source by its path there, and the in-process build, run from
// wherever the command was, says where to name sources from itself.
auto ForeignCompileFlags(
    const DpiSource& source, const std::filesystem::path& boundary,
    OptimizationLevel optimization, support::Sanitizer sanitizer)
    -> std::vector<std::string>;

// Compiles each DPI-C source to a relocatable object in `work_dir` and answers
// every input as the link takes it, in input order. Every program links these
// beside the design's own objects, which is what makes both directions of the
// boundary resolve in one place: the design's call out to a symbol foreign code
// defines, and the foreign side's call back to one only the design defines
// (LRM 35.4). `header_dir` holds the generated ABI header the sources may
// include, which is why these compiles wait for every unit: that header is what
// the units state of the foreign name space. Each is compiled as
// `ForeignCompileFlags` says, checked for `sanitizer`, the one the compiler
// links its own runtime for, and with a source under `root_dir` named in what
// it compiles to by its path from there, as the design's own sources are. As
// many compile at once as `width` allows, and every one that fails is
// reported.
auto CompileDpiLink(
    std::span<const DpiLinkInput> inputs, const std::filesystem::path& cxx,
    OptimizationLevel optimization, support::Sanitizer sanitizer,
    std::size_t width, const std::filesystem::path& root_dir,
    const std::filesystem::path& header_dir,
    const std::filesystem::path& work_dir)
    -> diag::Result<std::vector<DpiLinked>>;

// The words that hand `linked` to the host driver's link of a program built
// in place, in order. A shared library is kept as a dependency of the program
// whether or not the link sees a reference to it, so which libraries a program
// loads does not depend on the host linker's default, and one found by its
// soname is looked for in its own directory when the program runs.
auto DpiLinkArguments(std::span<const DpiLinked> linked)
    -> std::vector<std::string>;

}  // namespace lyra::driver
