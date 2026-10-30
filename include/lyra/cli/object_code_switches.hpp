#pragma once

#include <span>
#include <string>
#include <variant>
#include <vector>

#include "lyra/diag/diagnostic.hpp"

namespace lyra::cli {

// The switches LRM Annex J names compiled foreign code by. What each means
// depends on where it stands among the others, since a root applies to the
// paths after it, so they are kept as given, in order.

// `-sv_root`: the directory each later relative path is taken from (LRM J.3).
struct SvRoot {
  std::string directory;
};

// `-sv_lib`: a shared library, named without its platform's extension
// (LRM J.4).
struct SvLib {
  std::string path_without_extension;
};

// `-sv_liblist`: a bootstrap file listing shared libraries (LRM J.4.1).
struct SvLiblist {
  std::string bootstrap_file;
};

using ObjectCodeSwitch = std::variant<SvRoot, SvLib, SvLiblist>;

// The shared libraries `switches` name, in the order LRM J.4 loads them: each
// bootstrap file's entries in turn, then each `-sv_lib`. A relative path is
// taken from the root in effect where its switch stands, which before any
// `-sv_root` is the working directory (LRM J.3), and is completed with the
// platform's extension. A library named that does not exist is refused here,
// against the switch that named it.
auto SharedLibrariesNamedBy(std::span<const ObjectCodeSwitch> switches)
    -> diag::Result<std::vector<std::string>>;

}  // namespace lyra::cli
