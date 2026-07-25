#pragma once

#include <expected>
#include <filesystem>
#include <string>
#include <string_view>

#include "lyra/diag/diagnostic.hpp"

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
    -> std::expected<RuntimeLocation, std::string>;

// Copy the located runtime (header closure plus the static library) into
// `dest_dir`, matching the relative layout the emitted build recipe references,
// so an emitted project builds with no external include or link paths.
auto ExportRuntimeTree(
    const RuntimeLocation& runtime, const std::filesystem::path& dest_dir)
    -> diag::Result<void>;

}  // namespace lyra::driver
