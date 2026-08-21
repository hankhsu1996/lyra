#include "lyra/driver/dpi_boundary.hpp"

#include <algorithm>
#include <filesystem>
#include <format>
#include <span>
#include <string>
#include <system_error>
#include <utility>
#include <vector>

#include "lyra/diag/diag_code.hpp"
#include "lyra/dpi/abi_header.hpp"
#include "lyra/driver/file_output.hpp"
#include "lyra/driver/project_layout.hpp"
#include "lyra/support/subprocess.hpp"

namespace lyra::driver {

auto ValidateDpiLinkInputs(std::span<const std::string> sources)
    -> diag::Result<std::vector<DpiLinkInput>> {
  std::vector<DpiLinkInput> inputs;
  inputs.reserve(sources.size());
  for (const std::string& source : sources) {
    const std::filesystem::path path{source};
    const std::string ext = path.extension().string();
    const bool is_c = ext == ".c";
    const bool is_cpp = ext == ".cpp" || ext == ".cc" || ext == ".cxx";
    if (!is_c && !is_cpp) {
      return diag::Fail(
          diag::DiagCode::kHostInvalidCliArgs,
          std::format(
              "unsupported DPI-C link input '{}': only .c and .cpp are "
              "supported",
              source));
    }
    std::error_code ec;
    if (!std::filesystem::is_regular_file(path, ec)) {
      return diag::Fail(
          diag::DiagCode::kHostInvalidCliArgs,
          std::format("DPI-C link input '{}' is not a readable file", source));
    }
    // Every path that consumes these inputs lands their intermediates and their
    // copies side by side in one directory, keyed by file name, so two inputs
    // that share one would overwrite each other rather than both reach the
    // link. Reject the ambiguity here instead of letting each consumer discover
    // it -- or, worse, not discover it.
    const auto same_name = [&](const DpiLinkInput& seen) {
      return seen.source.filename() == path.filename();
    };
    if (std::ranges::any_of(inputs, same_name)) {
      return diag::Fail(
          diag::DiagCode::kHostInvalidCliArgs,
          std::format(
              "two DPI-C link inputs are both named '{}'; their file names "
              "must differ",
              path.filename().string()));
    }
    inputs.push_back(DpiLinkInput{.source = path, .compile_as_c = is_c});
  }
  return inputs;
}

auto WriteDpiSurface(
    const RuntimeLocation& runtime, std::span<const mir::CompilationUnit> units,
    const mir::CompilationUnit& root, const std::filesystem::path& dir)
    -> diag::Result<void> {
  if (auto r =
          WriteFile(dir / kDpiAbiHeader, dpi::RenderAbiHeader(units, root));
      !r) {
    return r;
  }
  return CopyFileWritable(runtime.svdpi_header, dir / kSvdpiHeader);
}

auto BuildDpiSharedLibrary(
    std::span<const DpiLinkInput> inputs, const std::filesystem::path& cxx,
    const std::filesystem::path& header_dir,
    const std::filesystem::path& work_dir)
    -> diag::Result<std::filesystem::path> {
  const std::filesystem::path library = work_dir / "libdpi.so";
  std::vector<std::string> args = {
      "-shared", "-fPIC", "-I", header_dir.string()};
  for (const DpiLinkInput& input : inputs) {
    args.emplace_back("-x");
    args.emplace_back(input.compile_as_c ? "c" : "c++");
    args.push_back(input.source.string());
  }
  args.emplace_back("-o");
  args.push_back(library.string());

  auto compiled = support::RunProcessCaptured(cxx, args);
  if (!compiled) {
    return diag::Fail(
        diag::DiagCode::kHostIoError, std::move(compiled.error()));
  }
  if (compiled->exit_code != 0) {
    return diag::Fail(
        diag::DiagCode::kHostBuildFailed,
        std::format(
            "compiling the DPI-C link inputs failed:\n{}",
            compiled->stderr_text));
  }
  return library;
}

}  // namespace lyra::driver
