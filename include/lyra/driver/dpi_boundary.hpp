#pragma once

#include <filesystem>
#include <span>
#include <string>
#include <vector>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/dpi/abi_header.hpp"
#include "lyra/driver/runtime_export.hpp"

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
// the in-process one, the emitted build recipe -- reads the classification from
// here rather than re-deriving it.
auto ValidateDpiLinkInputs(std::span<const std::string> sources)
    -> diag::Result<std::vector<DpiLinkInput>>;

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

// Compiles each DPI-C link input to a relocatable object and returns their
// paths, in input order. An ahead-of-time image hands these sources to the
// program's own link; an in-process design is linked by its execution session
// instead, and an object is what a linker takes -- which is what makes both
// directions of the boundary resolve in one place: the design's call out to a
// symbol the object defines, and the foreign side's call back to one only the
// session does (LRM 35.4). `header_dir` holds the generated ABI header the
// sources may include.
auto CompileDpiObjects(
    std::span<const DpiLinkInput> inputs, const std::filesystem::path& cxx,
    const std::filesystem::path& header_dir,
    const std::filesystem::path& work_dir)
    -> diag::Result<std::vector<std::filesystem::path>>;

}  // namespace lyra::driver
