#pragma once

#include <filesystem>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/support/assertion_policy.hpp"

namespace lyra::cli {

// Source text and everything reading it needs: where an include is searched
// for, what is defined before the first line, where a cell no file lists is
// found by name, and the native sources its DPI-C imports resolve against
// (LRM 35).
//
// Every path here is absolute. A relative path in the file is resolved against
// the file's own directory as it is read, so a declaration means the same thing
// from any working directory -- which is what lets the file be found from a
// subdirectory at all.
struct SourceSet {
  std::vector<std::string> files;
  std::vector<std::string> incdir;
  std::vector<std::string> defines;
  std::vector<std::string> undefines;
  std::vector<std::string> searchdir;
  std::vector<std::string> searchext;
  std::vector<std::string> dpi;
};

// A named collection of cells (LRM 33.2.1): what something depending on this
// library is given.
struct DeclaredLibrary {
  std::string name;
  SourceSet sources;
  // Include directories of this library that a library depending on it
  // searches too. The ones in `sources` are searched by this library alone.
  std::vector<std::string> export_incdir;
};

// Another library whose cells this one's cells may name: what it is called and
// the file declaring it.
struct DeclaredDependency {
  std::string name;
  std::filesystem::path manifest;
};

// What is run where the library is developed: the cells elaboration starts at,
// the values their parameters take, and the sources only they need. Nothing
// here reaches something that depends on the library.
struct DeclaredDesign {
  std::vector<std::string> top;
  std::vector<std::string> params;
  SourceSet sources;
};

// What a `lyra.toml` declares.
struct Manifest {
  std::filesystem::path path;
  DeclaredLibrary library;
  DeclaredDesign design;
  // In the order the file writes them, which is the order a cell is searched
  // for in them.
  std::vector<DeclaredDependency> dependencies;
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
// answer: declarations are never merged, so one above another's root cannot
// contribute to it.
auto FindManifest(const std::filesystem::path& start) -> ManifestSearch;

// The declarations one build is made from: the library being built, and before
// it every library it reaches through a dependency, each once and after
// everything it depends on itself.
struct Declarations {
  std::vector<Manifest> dependencies;
  Manifest root;
};

// Reads and validates the declaration at `path` and every declaration it
// reaches. Every key is checked against the schema -- an unrecognized one is an
// error rather than a warning, so a typo cannot silently compile something else
// and so a table this version does not know is a loud failure rather than a
// quiet one.
//
// A library is one thing in a build, so a name declared by two files is
// refused, as is a library that reaches itself. What a library that is depended
// on asks for and one build cannot give each library separately is refused
// rather than read under the root's answer: a language version or a time scale
// other than the root's.
auto LoadDeclarations(const std::filesystem::path& path)
    -> diag::Result<Declarations>;

}  // namespace lyra::cli
