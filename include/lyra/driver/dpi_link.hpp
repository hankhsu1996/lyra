#pragma once

#include <filesystem>
#include <span>
#include <string>

#include "lyra/diag/diagnostic.hpp"

namespace lyra::driver {

// Compiles the user's DPI-C sources (LRM 35) into one shared library and
// returns its path. An ahead-of-time image links these sources into the
// program, so it needs no such library; an in-process JIT has no link step, so
// its foreign symbols must arrive in something the execution session can load.
// A `.c` source is compiled as C so its symbols keep C linkage, which is what
// the import declares.
auto BuildDpiSharedLibrary(
    std::span<const std::string> sources, const std::filesystem::path& work_dir)
    -> diag::Result<std::filesystem::path>;

}  // namespace lyra::driver
