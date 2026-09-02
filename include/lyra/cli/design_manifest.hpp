#pragma once

#include <filesystem>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/support/assertion_policy.hpp"

namespace lyra::cli {

// A design's own declaration of what it is made of, read from a `lyra.toml`.
//
// Every path here is absolute. A relative path in the file is resolved against
// the file's own directory as it is read, so a declaration means the same thing
// from any working directory -- which is what lets the file be found from a
// subdirectory of the design at all.
struct DesignManifest {
  std::filesystem::path path;
  std::string name;
  std::vector<std::string> top;
  std::vector<std::string> files;
  std::vector<std::string> incdir;
  std::vector<std::string> defines;
  std::vector<std::string> undefines;
  std::vector<std::string> params;
  std::vector<std::string> libdir;
  std::vector<std::string> libext;
  std::vector<std::string> dpi_sources;
  std::optional<std::string> language_version;
  std::optional<std::string> timescale;
  std::optional<bool> single_unit;
  std::optional<support::AssertionPolicy> assertions;
};

struct ManifestFound {
  std::filesystem::path path;
};

// No declaration between where the search began and where it stopped. Both are
// carried because a search that stopped at a repository boundary is otherwise
// invisible to whoever reads the message.
struct ManifestAbsent {
  std::filesystem::path started;
  std::filesystem::path stopped;
};

using ManifestSearch = std::variant<ManifestFound, ManifestAbsent>;

// Walks up from `start` for the nearest declaration, stopping at a directory
// holding `.git` or at the filesystem root. The first one found is the whole
// answer: declarations are never merged, so a design above another design's
// root cannot contribute to it.
auto FindDesignManifest(const std::filesystem::path& start) -> ManifestSearch;

// Reads and validates one declaration. Every key is checked against the schema
// -- an unrecognized one is an error rather than a warning, so a typo cannot
// silently compile a different design and so a table this version does not know
// is a loud failure rather than a quiet one.
auto LoadDesignManifest(const std::filesystem::path& path)
    -> diag::Result<DesignManifest>;

}  // namespace lyra::cli
