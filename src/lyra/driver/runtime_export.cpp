#include "lyra/driver/runtime_export.hpp"

#include <expected>
#include <filesystem>
#include <format>
#include <memory>
#include <string>
#include <string_view>
#include <system_error>

#include "lyra/driver/project_layout.hpp"
#include "tools/cpp/runfiles/runfiles.h"

namespace lyra::driver {

namespace {

using bazel::tools::cpp::runfiles::Runfiles;

// Copy one file, overwriting, and ensure the result is owner-writable. The
// runtime sources come from the build cache read-only; without this a re-emit
// into the same directory cannot overwrite the previous copy.
auto CopyFileOverwrite(
    const std::filesystem::path& from, const std::filesystem::path& to)
    -> std::expected<void, std::string> {
  std::error_code ec;
  std::filesystem::copy_file(
      from, to, std::filesystem::copy_options::overwrite_existing, ec);
  if (ec) {
    return std::unexpected(
        std::format(
            "failed to copy '{}' to '{}': {}", from.string(), to.string(),
            ec.message()));
  }
  std::filesystem::permissions(
      to, std::filesystem::perms::owner_write,
      std::filesystem::perm_options::add, ec);
  if (ec) {
    return std::unexpected(
        std::format(
            "failed to set permissions on '{}': {}", to.string(),
            ec.message()));
  }
  return {};
}

// Copy a directory tree, dereferencing symlinks into real files. Runfiles
// stages headers as symlinks into the build cache; copying them verbatim would
// leave the exported tree pointing back at the cache instead of standing alone.
auto CopyTree(
    const std::filesystem::path& from, const std::filesystem::path& to)
    -> std::expected<void, std::string> {
  std::error_code ec;
  for (const auto& entry : std::filesystem::recursive_directory_iterator(
           from, std::filesystem::directory_options::follow_directory_symlink,
           ec)) {
    if (!entry.is_regular_file()) {
      continue;
    }
    // Lexical relative only: runfiles entries are symlinks into the build
    // cache, and a filesystem-resolving `relative` would canonicalize them
    // back to their source locations and escape the destination tree.
    const auto rel = entry.path().lexically_relative(from);
    const auto dest = to / rel;
    std::filesystem::create_directories(dest.parent_path(), ec);
    if (ec) {
      return std::unexpected(
          std::format(
              "failed to create '{}': {}", dest.parent_path().string(),
              ec.message()));
    }
    if (auto r = CopyFileOverwrite(entry.path(), dest); !r) {
      return r;
    }
  }
  if (ec) {
    return std::unexpected(
        std::format("failed to walk '{}': {}", from.string(), ec.message()));
  }
  return {};
}

}  // namespace

auto ResolveRuntimeLocation(std::string_view binary_path)
    -> std::expected<RuntimeLocation, std::string> {
  std::string rf_error;
  std::unique_ptr<Runfiles> runfiles{
      Runfiles::Create(std::string(binary_path), &rf_error)};
  if (!runfiles) {
    return std::unexpected(
        std::format("cannot access the Lyra runtime: {}", rf_error));
  }
  // The header closure is staged under `include/lyra/`; resolve one known
  // header and walk up to the `include` root the emitted code includes from.
  const std::filesystem::path engine_hpp =
      runfiles->Rlocation("_main/include/lyra/runtime/engine.hpp");
  if (engine_hpp.empty() || !std::filesystem::exists(engine_hpp)) {
    return std::unexpected("cannot locate the Lyra runtime headers");
  }
  const std::filesystem::path lib =
      runfiles->Rlocation("_main/libcpp_runtime.a");
  if (lib.empty() || !std::filesystem::exists(lib)) {
    return std::unexpected("cannot locate the Lyra runtime library");
  }
  return RuntimeLocation{
      .include_root = engine_hpp.parent_path().parent_path().parent_path(),
      .lib = lib};
}

auto ExportRuntimeTree(
    const RuntimeLocation& runtime, const std::filesystem::path& dest_dir)
    -> std::expected<void, std::string> {
  const auto lyra_include_root = runtime.include_root / "lyra";

  const auto include_dest = dest_dir / kRuntimeIncludeDir / "lyra";
  const auto lib_dest_dir = dest_dir / kRuntimeLibDir;

  std::error_code ec;
  std::filesystem::create_directories(include_dest.parent_path(), ec);
  if (ec) {
    return std::unexpected(
        std::format(
            "failed to create '{}': {}", include_dest.parent_path().string(),
            ec.message()));
  }
  std::filesystem::create_directories(lib_dest_dir, ec);
  if (ec) {
    return std::unexpected(
        std::format(
            "failed to create '{}': {}", lib_dest_dir.string(), ec.message()));
  }

  if (auto r = CopyTree(lyra_include_root, include_dest); !r) {
    return r;
  }
  return CopyFileOverwrite(runtime.lib, lib_dest_dir / kRuntimeLibFile);
}

}  // namespace lyra::driver
