#pragma once

#include <filesystem>
#include <string>
#include <string_view>
#include <vector>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/driver/project_layout.hpp"
#include "lyra/support/sanitizer.hpp"

namespace lyra::driver {

// The address sanitizer's own runtime of the clang release that compiles the
// runtime library: the archive, its C++ part, and the symbols a program has to
// export for it.
struct AddressSanitizerRuntime {
  std::filesystem::path archive;
  std::filesystem::path cxx_archive;
  std::filesystem::path symbols;
};

// The files the running binary ships for an emitted project to consume, as they
// sit on disk. Mostly the Lyra C++ runtime, plus the one standard header the
// user's own foreign sources need; they resolve together because they are
// found the same way and travel into the same output directory.
struct RuntimeLocation {
  // Directory to pass to the compiler's `-I`; contains the `lyra/` header tree.
  std::filesystem::path include_root;
  // The runtime static library to link.
  std::filesystem::path lib;
  // The same library compiled to check a program's memory, for a program the
  // execution backend builds for the address sanitizer, and the sanitizer's
  // runtime such a program links.
  std::filesystem::path address_sanitized_lib;
  AddressSanitizerRuntime address_sanitizer;
  // The root of the runtime's own sources and the lists saying how each is
  // compiled, for a build whose compiler builds them beside the design.
  std::filesystem::path sources_root;
  // The standard DPI-C header (LRM 35, Annex I). Not part of the runtime -- no
  // Lyra source includes it -- but shipped alongside so a user's foreign
  // sources compile against the standard declarations.
  std::filesystem::path svdpi_header;
};

// What a program links of the runtime: the archives or objects, and what the
// link is told beside them.
struct LinkedRuntime {
  std::vector<std::filesystem::path> files;
  std::vector<std::string> flags;
};

// What a program the execution backend builds links of the runtime. That
// backend instruments a design's code itself, with the release that compiled
// the runtime library, so a program checked for a sanitizer links the library
// compiled for it and that release's own sanitizer runtime, whole, as clang's
// driver links it, and not whatever runtime the host compiler would bring.
auto ExecutionBackendRuntime(
    const RuntimeLocation& runtime, support::Sanitizer sanitizer)
    -> LinkedRuntime;
// The one runtime library among those archives, which a link optimizing the
// program whole reads the bitcode of where its members carry any.
auto ExecutionBackendLibrary(
    const RuntimeLocation& runtime, support::Sanitizer sanitizer)
    -> const std::filesystem::path&;

// Locate what the binary at `binary_path` ships. Resolution is a single seam:
// today it reads the binary's Bazel runfiles tree; a released binary will
// resolve relative to its own install location. Callers depend only on the
// returned paths.
auto ResolveRuntimeLocation(std::string_view binary_path)
    -> diag::Result<RuntimeLocation>;

// Copy the located runtime into `dest_dir`, matching the relative layout the
// emitted build recipe references, so an emitted project builds with no
// external include or link paths: the header closure and the static library,
// and the runtime's sources as well for a project whose compiler builds them
// beside the design -- one optimized at the link, or checked for a sanitizer.
auto ExportRuntimeTree(
    const RuntimeLocation& runtime, const std::filesystem::path& dest_dir,
    LinkTimeOptimization link_time, support::Sanitizer sanitizer)
    -> diag::Result<void>;

}  // namespace lyra::driver
