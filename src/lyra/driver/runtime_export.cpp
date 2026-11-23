#include "lyra/driver/runtime_export.hpp"

#include <expected>
#include <filesystem>
#include <format>
#include <memory>
#include <string>
#include <string_view>
#include <system_error>
#include <utility>
#include <vector>

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

// What the binary at `binary_path` ships beside it.
auto OpenShipped(std::string_view binary_path)
    -> diag::Result<std::unique_ptr<Runfiles>> {
  std::string error;
  std::unique_ptr<Runfiles> runfiles{
      Runfiles::Create(std::string(binary_path), &error)};
  if (!runfiles) {
    return diag::Fail(
        diag::DiagCode::kHostIoError,
        "cannot access what Lyra ships beside itself ({})", error);
  }
  return runfiles;
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

// Where each build of the runtime ships its library and its sources.
struct ShippedRuntimeBuild {
  std::string_view library;
  std::string_view sources;
  std::string_view named;
};

auto ShippedBuildOf(RuntimeStatistics statistics) -> ShippedRuntimeBuild {
  switch (statistics) {
    case RuntimeStatistics::kNotKept:
      return {
          .library = "_main/libcpp_runtime.a",
          .sources = "_main/runtime_sources",
          .named = "the Lyra runtime"};
    case RuntimeStatistics::kKept:
      return {
          .library = "_main/libcpp_runtime_statistics.a",
          .sources = "_main/runtime_sources_statistics",
          .named = "the Lyra runtime built to count its scheduling work"};
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

auto ResolveRuntimeLocation(
    std::string_view binary_path, RuntimeStatistics statistics)
    -> diag::Result<RuntimeLocation> {
  const ShippedRuntimeBuild build = ShippedBuildOf(statistics);
  auto shipped = OpenShipped(binary_path);
  if (!shipped) {
    return std::unexpected(std::move(shipped.error()));
  }
  const Runfiles& runfiles = **shipped;
  // The surface is staged under a root holding it and nothing else, so the
  // root around any one of its headers is the whole of what a project gets.
  // What makes that true is the staging rather than this walk: a header
  // resolved where this repository keeps it would hand back a directory
  // carrying every header the compiler has.
  auto anchor = LocateShipped(
      runfiles, "_main/runtime_surface/lyra/runtime/runtime.hpp",
      "the Lyra runtime headers");
  if (!anchor) {
    return std::unexpected(std::move(anchor.error()));
  }
  auto lib = LocateShipped(
      runfiles, std::string(build.library),
      std::format("{}'s library", build.named));
  if (!lib) {
    return std::unexpected(std::move(lib.error()));
  }
  auto sanitized = LocateShipped(
      runfiles, "_main/libcpp_runtime_address_sanitized.a",
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
        runfiles, std::format("_main/address_sanitizer_runtime/{}", name),
        "the address sanitizer's runtime");
    if (!found) {
      return std::unexpected(std::move(found.error()));
    }
    *path = *std::move(found);
  }
  std::vector<std::filesystem::path> standard_headers;
  for (const std::string_view header : kStandardHeaders) {
    auto found = LocateShipped(
        runfiles, std::format("_main/third_party/systemverilog/{}", header),
        "the standard's C headers");
    if (!found) {
      return std::unexpected(std::move(found.error()));
    }
    standard_headers.push_back(*std::move(found));
  }
  auto licenses = LocateShipped(
      runfiles, std::format("_main/{}", kThirdPartyLicensesFile),
      "the notices of the code the Lyra runtime carries");
  if (!licenses) {
    return std::unexpected(std::move(licenses.error()));
  }
  auto source_list = LocateShipped(
      runfiles, std::format("{}/{}", build.sources, kRuntimeSourceList),
      std::format("{}'s sources", build.named));
  if (!source_list) {
    return std::unexpected(std::move(source_list.error()));
  }
  return RuntimeLocation{
      .include_root = anchor->parent_path().parent_path().parent_path(),
      .lib = *std::move(lib),
      .address_sanitized_lib = *std::move(sanitized),
      .address_sanitizer = std::move(asan),
      .sources_root = source_list->parent_path(),
      .standard_headers = std::move(standard_headers),
      .third_party_licenses = *std::move(licenses)};
}

auto ResolveShippedPortStore(std::string_view binary_path)
    -> diag::Result<std::filesystem::path> {
  auto shipped = OpenShipped(binary_path);
  if (!shipped) {
    return std::unexpected(std::move(shipped.error()));
  }
  return LocateShipped(
      **shipped, "_main/port_store",
      "the ports of the libraries Lyra supports");
}

auto ExportRuntimeTree(
    const RuntimeLocation& runtime, const std::filesystem::path& dest_dir,
    LinkTimeOptimization link_time, support::Sanitizer sanitizer)
    -> diag::Result<void> {
  if (auto r = CopyTree(runtime.include_root, dest_dir / kRuntimeIncludeDir);
      !r) {
    return r;
  }
  if (auto r =
          CopyFile(runtime.lib, dest_dir / kRuntimeLibDir / kRuntimeLibFile);
      !r) {
    return r;
  }
  if (auto r = CopyFile(
          runtime.third_party_licenses, dest_dir / kThirdPartyLicensesFile);
      !r) {
    return r;
  }
  if (!CarriesRuntimeSources(link_time, sanitizer)) {
    return {};
  }
  return CopyTree(runtime.sources_root, dest_dir / kRuntimeSourceDir);
}

}  // namespace lyra::driver
