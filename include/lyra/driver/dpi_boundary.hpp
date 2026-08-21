#pragma once

#include <filesystem>
#include <span>
#include <string>
#include <vector>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/driver/runtime_export.hpp"
#include "lyra/mir/compilation_unit.hpp"

namespace lyra::driver {

// One user-provided native source supplying DPI-C foreign symbols (LRM 35),
// classified by the language it must be compiled as. A C source is compiled on
// its own so its symbols keep C linkage -- an import's emitted declaration
// expects that, and a C++ driver invocation would otherwise mangle it -- while
// a C++ source joins the C++ link directly, having given its entry points that
// linkage itself.
struct DpiLinkInput {
  std::filesystem::path source;
  bool compile_as_c;
};

// Classifies and checks every DPI-C link input once, before any backend runs,
// so an unreadable, unsupported, or ambiguously named input is reported against
// the command line rather than surfacing much later as a compiler error or a
// silently overwritten intermediate. Every consumer -- the ahead-of-time link,
// the JIT's foreign library, the emitted build recipe -- reads the
// classification from here rather than re-deriving it.
auto ValidateDpiLinkInputs(std::span<const std::string> sources)
    -> diag::Result<std::vector<DpiLinkInput>>;

// Writes the design's DPI-C boundary surface into `dir` (LRM 35): the generated
// prototypes of every foreign-linkage callable the design declares, and the
// standard header they are spelled in. A foreign source compiles against `dir`
// whichever backend runs the design. Written for every design -- one that
// declares no DPI-C gets the same header with no prototypes -- so no consumer
// needs a case for its absence.
auto WriteDpiSurface(
    const RuntimeLocation& runtime, std::span<const mir::CompilationUnit> units,
    const mir::CompilationUnit& root, const std::filesystem::path& dir)
    -> diag::Result<void>;

// Compiles the DPI-C link inputs into one shared library and returns its path.
// An ahead-of-time image links these sources into the program, so it needs no
// such library; an in-process JIT has no link step, so its foreign symbols must
// arrive in something the execution session can load. `header_dir` holds the
// generated ABI header the sources may include.
auto BuildDpiSharedLibrary(
    std::span<const DpiLinkInput> inputs, const std::filesystem::path& cxx,
    const std::filesystem::path& header_dir,
    const std::filesystem::path& work_dir)
    -> diag::Result<std::filesystem::path>;

}  // namespace lyra::driver
