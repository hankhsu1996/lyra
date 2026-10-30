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

// A user's native source supplying DPI-C foreign symbols (LRM 35), classified
// by the language it must be compiled as. A C source is compiled on its own so
// its symbols keep C linkage -- an import's emitted declaration expects that,
// and a C++ driver invocation would otherwise mangle it -- while a C++ source
// joins the C++ link directly, having given its entry points that linkage
// itself.
struct DpiSource {
  std::filesystem::path path;
  bool compile_as_c;
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
// time is taken once (LRM J.4). Every consumer -- a build of either backend,
// and the recipe an emitted project ships -- reads the classification from
// here rather than re-deriving it.
auto ValidateDpiLinkInputs(std::span<const std::string> inputs)
    -> diag::Result<std::vector<DpiLinkInput>>;

// The file the user handed over.
auto DpiInputPath(const DpiLinkInput& input) -> const std::filesystem::path&;

// The file name an input is known by beside the others wherever they are
// gathered into one directory: what is made from a source and the copy an
// emitted project carries are named after it. A shared library is known by the
// file the dynamic loader looks for.
auto GatheredNameOf(const DpiLinkInput& input) -> std::string;

// Writes the design's DPI-C boundary surface into `dir` (LRM 35): the header a
// foreign source includes, naming the per-unit fragments already written there,
// and the standard header those prototypes are spelled in. `fragments` is a
// file list rather than anything read out of the design, which is what makes
// assembling the union a step of the build. A foreign source compiles against
// `dir` whichever backend runs the design. Written for every design -- one that
// declares no DPI-C names no fragment -- so no consumer needs a case for its
// absence.
auto WriteDpiSurface(
    const RuntimeLocation& runtime, std::span<const dpi::AbiFragment> fragments,
    const std::filesystem::path& dir) -> diag::Result<void>;

// What a foreign source is compiled with: the language it is compiled as and
// the standard where its language has one to name (LRM 35), at `optimization`
// and checked for `sanitizer`. A C source is compiled as C so its symbols keep
// C linkage, which is what the emitted declaration expects and what a C++
// compilation would mangle away. The in-process build and the recipe an emitted
// project ships both read it here, so the two compile a source alike.
auto ForeignCompileFlags(
    const DpiSource& source, OptimizationLevel optimization,
    support::Sanitizer sanitizer) -> std::vector<std::string>;

// Compiles each DPI-C source to a relocatable object in `work_dir` and answers
// every input as the link takes it, in input order. Every program links these
// beside the design's own objects, which is what makes both directions of the
// boundary resolve in one place: the design's call out to a symbol foreign code
// defines, and the foreign side's call back to one only the design defines
// (LRM 35.4). `header_dir` holds the generated ABI header the sources may
// include, which is why these compiles wait for every unit: that header is what
// the units state of the foreign name space. Each is compiled as
// `ForeignCompileFlags` says, checked for `sanitizer`, the one the compiler
// links its own runtime for. As many compile at once as `width` allows, and
// every one that fails is reported.
auto CompileDpiLink(
    std::span<const DpiLinkInput> inputs, const std::filesystem::path& cxx,
    OptimizationLevel optimization, support::Sanitizer sanitizer,
    std::size_t width, const std::filesystem::path& header_dir,
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
