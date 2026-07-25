#include "lyra/driver/runtime_export.hpp"

#include <expected>
#include <filesystem>
#include <format>
#include <memory>
#include <string>
#include <string_view>
#include <system_error>

#include "lyra/diag/diag_code.hpp"
#include "lyra/driver/file_output.hpp"
#include "lyra/driver/project_layout.hpp"
#include "tools/cpp/runfiles/runfiles.h"

namespace lyra::driver {

namespace {

using bazel::tools::cpp::runfiles::Runfiles;

// Copy a directory tree, dereferencing symlinks into real files. Runfiles
// stages headers as symlinks into the build cache; copying them verbatim would
// leave the exported tree pointing back at the cache instead of standing alone.
auto CopyTree(
    const std::filesystem::path& from, const std::filesystem::path& to)
    -> diag::Result<void> {
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
    if (auto r = CopyFileWritable(
            entry.path(), to / entry.path().lexically_relative(from));
        !r) {
      return r;
    }
  }
  if (ec) {
    return std::unexpected(
        diag::Make(
            diag::DiagCode::kHostIoError,
            std::format(
                "failed to walk '{}': {}", from.string(), ec.message())));
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
  const std::filesystem::path anchor =
      runfiles->Rlocation("_main/include/lyra/runtime/runtime.hpp");
  if (anchor.empty() || !std::filesystem::exists(anchor)) {
    return std::unexpected("cannot locate the Lyra runtime headers");
  }
  const std::filesystem::path lib =
      runfiles->Rlocation("_main/libcpp_runtime.a");
  if (lib.empty() || !std::filesystem::exists(lib)) {
    return std::unexpected("cannot locate the Lyra runtime library");
  }
  const std::filesystem::path svdpi =
      runfiles->Rlocation("_main/third_party/systemverilog/svdpi.h");
  if (svdpi.empty() || !std::filesystem::exists(svdpi)) {
    return std::unexpected("cannot locate the standard DPI-C header");
  }
  return RuntimeLocation{
      .include_root = anchor.parent_path().parent_path().parent_path(),
      .lib = lib,
      .svdpi_header = svdpi};
}

auto ExportRuntimeTree(
    const RuntimeLocation& runtime, const std::filesystem::path& dest_dir)
    -> diag::Result<void> {
  if (auto r = CopyTree(
          runtime.include_root / "lyra",
          dest_dir / kRuntimeIncludeDir / "lyra");
      !r) {
    return r;
  }
  return CopyFileWritable(
      runtime.lib, dest_dir / kRuntimeLibDir / kRuntimeLibFile);
}

}  // namespace lyra::driver
