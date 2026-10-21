#include "lyra/driver/runtime_export.hpp"

#include <expected>
#include <filesystem>
#include <format>
#include <memory>
#include <string>
#include <string_view>
#include <system_error>
#include <utility>

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
    if (auto r =
            CopyFile(entry.path(), to / entry.path().lexically_relative(from));
        !r) {
      return r;
    }
  }
  if (ec) {
    return diag::Fail(
        diag::DiagCode::kHostIoError, "cannot read the directory '{}' ({})",
        from.string(), ec.message());
  }
  return {};
}

// A file the binary ships beside it, under `rlocation`, or why there is none.
auto LocateShipped(
    const Runfiles& runfiles, const std::string& rlocation,
    std::string_view what) -> diag::Result<std::filesystem::path> {
  const std::filesystem::path path = runfiles.Rlocation(rlocation);
  if (path.empty() || !std::filesystem::exists(path)) {
    return diag::Fail(diag::DiagCode::kHostIoError, "cannot locate {}", what);
  }
  return path;
}

// Whether a project carries the runtime's own sources: where its compiler may
// optimize them with the design at the link, or checks the program for a
// sanitizer, whose instrumentation goes with that compiler's own runtime and
// not with the prebuilt library's.
auto CarriesRuntimeSources(
    LinkTimeOptimization link_time, support::Sanitizer sanitizer) -> bool {
  switch (sanitizer) {
    case support::Sanitizer::kNone:
      break;
    case support::Sanitizer::kAddress:
      return true;
  }
  switch (link_time) {
    case LinkTimeOptimization::kNone:
      return false;
    case LinkTimeOptimization::kThin:
      return true;
  }
  std::unreachable();
}

}  // namespace

auto ExecutionBackendLibrary(
    const RuntimeLocation& runtime, support::Sanitizer sanitizer)
    -> const std::filesystem::path& {
  switch (sanitizer) {
    case support::Sanitizer::kNone:
      return runtime.lib;
    case support::Sanitizer::kAddress:
      return runtime.address_sanitized_lib;
  }
  std::unreachable();
}

auto ExecutionBackendRuntime(
    const RuntimeLocation& runtime, support::Sanitizer sanitizer)
    -> LinkedRuntime {
  const std::filesystem::path& library =
      ExecutionBackendLibrary(runtime, sanitizer);
  switch (sanitizer) {
    case support::Sanitizer::kNone:
      return {.files = {library}, .flags = {}};
    case support::Sanitizer::kAddress: {
      // The runtime's interceptors stand in for functions that the shared
      // libraries a program loads call, so the program has to export them. The
      // runtime names no list of them, so every symbol is exported, as clang's
      // driver does for a sanitizer runtime that has none.
      const AddressSanitizerRuntime& asan = runtime.address_sanitizer;
      return {
          .files = {library},
          .flags = {
              "-Wl,--whole-archive", asan.archive.string(),
              asan.cxx_archive.string(), "-Wl,--no-whole-archive",
              "-Wl,--export-dynamic", "-lpthread", "-lrt", "-lm", "-ldl"}};
    }
  }
  std::unreachable();
}

auto ResolveRuntimeLocation(std::string_view binary_path)
    -> diag::Result<RuntimeLocation> {
  std::string rf_error;
  std::unique_ptr<Runfiles> runfiles{
      Runfiles::Create(std::string(binary_path), &rf_error)};
  if (!runfiles) {
    return diag::Fail(
        diag::DiagCode::kHostIoError, "cannot access the Lyra runtime ({})",
        rf_error);
  }
  // The surface is staged under a root holding it and nothing else, so the
  // root around any one of its headers is the whole of what a project gets.
  // What makes that true is the staging rather than this walk: a header
  // resolved where this repository keeps it would hand back a directory
  // carrying every header the compiler has.
  auto anchor = LocateShipped(
      *runfiles, "_main/runtime_surface/lyra/runtime/runtime.hpp",
      "the Lyra runtime headers");
  if (!anchor) {
    return std::unexpected(std::move(anchor.error()));
  }
  auto lib = LocateShipped(
      *runfiles, "_main/libcpp_runtime.a", "the Lyra runtime library");
  if (!lib) {
    return std::unexpected(std::move(lib.error()));
  }
  auto sanitized = LocateShipped(
      *runfiles, "_main/libcpp_runtime_address_sanitized.a",
      "the Lyra runtime library compiled for the address sanitizer");
  if (!sanitized) {
    return std::unexpected(std::move(sanitized.error()));
  }
  AddressSanitizerRuntime asan;
  for (auto [path, name] :
       {std::pair{&asan.archive, "libclang_rt.asan.a"},
        std::pair{&asan.cxx_archive, "libclang_rt.asan_cxx.a"},
        std::pair{&asan.symbolizer, "llvm-symbolizer"}}) {
    auto found = LocateShipped(
        *runfiles, std::format("_main/address_sanitizer_runtime/{}", name),
        "the address sanitizer's runtime");
    if (!found) {
      return std::unexpected(std::move(found.error()));
    }
    *path = *std::move(found);
  }
  auto svdpi = LocateShipped(
      *runfiles, "_main/third_party/systemverilog/svdpi.h",
      "the standard DPI-C header");
  if (!svdpi) {
    return std::unexpected(std::move(svdpi.error()));
  }
  auto source_list = LocateShipped(
      *runfiles, std::format("_main/runtime_sources/{}", kRuntimeSourceList),
      "the Lyra runtime sources");
  if (!source_list) {
    return std::unexpected(std::move(source_list.error()));
  }
  return RuntimeLocation{
      .include_root = anchor->parent_path().parent_path().parent_path(),
      .lib = *std::move(lib),
      .address_sanitized_lib = *std::move(sanitized),
      .address_sanitizer = std::move(asan),
      .sources_root = source_list->parent_path(),
      .svdpi_header = *std::move(svdpi)};
}

auto ExportRuntimeTree(
    const RuntimeLocation& runtime, const std::filesystem::path& dest_dir,
    LinkTimeOptimization link_time, support::Sanitizer sanitizer)
    -> diag::Result<void> {
  if (auto r = CopyTree(
          runtime.include_root / "lyra",
          dest_dir / kRuntimeIncludeDir / "lyra");
      !r) {
    return r;
  }
  if (auto r =
          CopyFile(runtime.lib, dest_dir / kRuntimeLibDir / kRuntimeLibFile);
      !r) {
    return r;
  }
  if (!CarriesRuntimeSources(link_time, sanitizer)) {
    return {};
  }
  return CopyTree(runtime.sources_root, dest_dir / kRuntimeSourceDir);
}

}  // namespace lyra::driver
