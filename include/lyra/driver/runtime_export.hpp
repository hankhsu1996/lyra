#pragma once

#include <expected>
#include <filesystem>
#include <string>
#include <string_view>

namespace lyra::driver {

// The Lyra C++ runtime as it sits on disk for the running binary.
struct RuntimeLocation {
  // Directory to pass to the compiler's `-I`; contains the `lyra/` header tree.
  std::filesystem::path include_root;
  // The runtime static library to link.
  std::filesystem::path lib;
};

// Locate the Lyra C++ runtime for the binary at `binary_path`. Resolution is a
// single seam (see docs/architecture/runtime_distribution.md): today it reads
// the binary's Bazel runfiles tree; a released binary will resolve relative to
// its own install location. Callers depend only on the returned paths.
auto ResolveRuntimeLocation(std::string_view binary_path)
    -> std::expected<RuntimeLocation, std::string>;

// Copy the located runtime (header closure plus the static library) into
// `dest_dir`, matching the relative layout the emitted build recipe references,
// so an emitted project builds with no external include or link paths.
auto ExportRuntimeTree(
    const RuntimeLocation& runtime, const std::filesystem::path& dest_dir)
    -> std::expected<void, std::string>;

}  // namespace lyra::driver
