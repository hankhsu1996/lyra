#include "lyra/driver/dpi_boundary.hpp"

#include <algorithm>
#include <cstddef>
#include <filesystem>
#include <optional>
#include <span>
#include <string>
#include <system_error>
#include <utility>
#include <vector>

#include "lyra/diag/diag_code.hpp"
#include "lyra/dpi/abi_header.hpp"
#include "lyra/driver/file_output.hpp"
#include "lyra/driver/project_layout.hpp"
#include "lyra/driver/subprocess.hpp"
#include "lyra/status/status.hpp"

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
          "DPI-C link input '{}' is neither a '.c' nor a '.cpp' source",
          source);
    }
    std::error_code ec;
    if (!std::filesystem::is_regular_file(path, ec)) {
      return diag::Fail(
          diag::DiagCode::kHostInvalidCliArgs,
          "DPI-C link input '{}' is not a readable file", source);
    }
    // Every path that consumes these inputs names what it derives from them by
    // file name -- a copy of the source, an object -- so two inputs sharing one
    // would overwrite each other rather than both reach the link. Reject the
    // ambiguity here instead of letting each consumer discover it -- or, worse,
    // not discover it.
    const auto same_name = [&](const DpiLinkInput& seen) {
      return seen.source.filename() == path.filename();
    };
    if (std::ranges::any_of(inputs, same_name)) {
      return std::unexpected(
          diag::Make(
              diag::DiagCode::kHostInvalidCliArgs,
              "two DPI-C link inputs are both named '{}'",
              path.filename().string())
              .WithNote("give each DPI-C link input a file name of its own"));
    }
    inputs.push_back(DpiLinkInput{.source = path, .compile_as_c = is_c});
  }
  return inputs;
}

auto WriteDpiSurface(
    const RuntimeLocation& runtime, std::span<const dpi::AbiFragment> fragments,
    const std::filesystem::path& dir) -> diag::Result<void> {
  for (const dpi::AbiFragment& fragment : fragments) {
    if (auto r = WriteFile(dir / fragment.relpath, fragment.text); !r) {
      return r;
    }
  }
  if (auto r = WriteFile(dir / kDpiAbiHeader, dpi::RenderAbiHeader(fragments));
      !r) {
    return r;
  }
  return CopyFile(runtime.svdpi_header, dir / kSvdpiHeader);
}

auto ForeignLanguageFlags(const DpiLinkInput& input)
    -> std::vector<std::string> {
  if (input.compile_as_c) {
    return {"-x", "c"};
  }
  return {std::string(kCxxStandardFlag), "-x", "c++"};
}

auto CompileDpiObjects(
    std::span<const DpiLinkInput> inputs, const std::filesystem::path& cxx,
    OptimizationLevel optimization, std::size_t width,
    const std::filesystem::path& header_dir,
    const std::filesystem::path& work_dir)
    -> diag::Result<std::vector<std::filesystem::path>> {
  std::vector<std::filesystem::path> objects;
  objects.reserve(inputs.size());
  std::error_code created;
  std::filesystem::create_directories(work_dir, created);
  if (created) {
    return diag::Fail(
        diag::DiagCode::kHostIoError, "cannot create '{}' ({})",
        work_dir.string(), created.message());
  }
  std::vector<ProcessRequest> requests;
  std::vector<std::string> names;
  requests.reserve(inputs.size());
  names.reserve(inputs.size());
  for (const DpiLinkInput& input : inputs) {
    // One compilation per input, because the language each is compiled as is
    // its own and a driver invocation carries one output path. The object's
    // name extends the input's whole file name, which is the thing two inputs
    // are already required to differ in; taking the stem instead would let one
    // source and another of a different language overwrite each other.
    objects.push_back(work_dir / (input.source.filename().string() + ".o"));
    std::vector<std::string> args = ForeignLanguageFlags(input);
    args.insert(
        args.end(), {std::string(OptimizationFlag(optimization)), "-c",
                     input.source.string(), "-I", header_dir.string(), "-o",
                     objects.back().string()});
    requests.push_back(ProcessRequest{.exe = cxx, .args = std::move(args)});
    names.push_back(input.source.filename().string());
  }

  const status::Phase phase("Compiling DPI sources");
  auto compiled = RunProcessesCaptured(requests, width, names);
  if (!compiled) {
    return diag::Fail(
        diag::DiagCode::kHostIoError, std::move(compiled.error()));
  }
  // Every input that failed is reported, the first as the error and each
  // later one as notes of it.
  std::optional<diag::Diagnostic> failures;
  for (std::size_t i = 0; i < inputs.size(); ++i) {
    const ProcessResult& result = (*compiled)[i];
    if (result.exit_code == 0) {
      continue;
    }
    const std::string source = inputs[i].source.string();
    if (!failures) {
      failures = diag::Make(
          diag::DiagCode::kHostBuildFailed,
          "compiling the DPI-C link input '{}' failed", source);
    } else {
      failures = std::move(*failures).WithNote(
          "compiling the DPI-C link input '{}' failed", source);
    }
    failures = std::move(*failures).WithNote("{}", result.stderr_text);
  }
  if (failures) {
    return std::unexpected(*std::move(failures));
  }
  return objects;
}

}  // namespace lyra::driver
