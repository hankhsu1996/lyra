#pragma once

#include <filesystem>
#include <string_view>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/driver/project_layout.hpp"

namespace lyra::driver {

// The files the running binary ships for an emitted project to consume, as they
// sit on disk. Mostly the Lyra C++ runtime, plus the one standard header the
// user's own foreign sources need; they resolve together because they are
// found the same way and travel into the same output directory.
struct RuntimeLocation {
  // Directory to pass to the compiler's `-I`; contains the `lyra/` header tree.
  std::filesystem::path include_root;
  // The runtime static library to link.
  std::filesystem::path lib;
  // The root of the runtime's own sources and the lists saying how each is
  // compiled, for a build whose compiler builds them beside the design.
  std::filesystem::path sources_root;
  // The standard DPI-C header (LRM 35, Annex I). Not part of the runtime -- no
  // Lyra source includes it -- but shipped alongside so a user's foreign
  // sources compile against the standard declarations.
  std::filesystem::path svdpi_header;
};

// Locate what the binary at `binary_path` ships. Resolution is a single seam:
// today it reads the binary's Bazel runfiles tree; a released binary will
// resolve relative to its own install location. Callers depend only on the
// returned paths.
auto ResolveRuntimeLocation(std::string_view binary_path)
    -> diag::Result<RuntimeLocation>;

// Copy the located runtime into `dest_dir`, matching the relative layout the
// emitted build recipe references, so an emitted project builds with no
// external include or link paths: the header closure and the static library,
// and for a project optimized at the link the runtime's sources as well.
auto ExportRuntimeTree(
    const RuntimeLocation& runtime, const std::filesystem::path& dest_dir,
    LinkTimeOptimization link_time) -> diag::Result<void>;

}  // namespace lyra::driver
