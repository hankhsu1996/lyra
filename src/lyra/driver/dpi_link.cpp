#include "lyra/driver/dpi_link.hpp"

#include <filesystem>
#include <format>
#include <span>
#include <string>
#include <utility>
#include <vector>

#include "lyra/diag/diag_code.hpp"
#include "lyra/support/subprocess.hpp"

namespace lyra::driver {

namespace {

// The language a DPI-C link input is compiled as. A `.c` source is forced to C
// so its symbols keep the C linkage the import declares; a C++ source gives its
// entry points that linkage itself, with `extern "C"`.
auto LanguageFlag(const std::filesystem::path& source)
    -> diag::Result<std::string> {
  const std::string ext = source.extension().string();
  if (ext == ".c") {
    return std::string{"c"};
  }
  if (ext == ".cpp" || ext == ".cc" || ext == ".cxx") {
    return std::string{"c++"};
  }
  return diag::Fail(
      diag::DiagCode::kHostBuildFailed,
      std::format(
          "unsupported DPI-C link input '{}': only .c and .cpp are supported",
          source.string()));
}

}  // namespace

auto BuildDpiSharedLibrary(
    std::span<const std::string> sources, const std::filesystem::path& work_dir)
    -> diag::Result<std::filesystem::path> {
  auto cxx = support::ResolveCxxCompiler();
  if (!cxx) {
    return diag::Fail(diag::DiagCode::kHostIoError, std::move(cxx.error()));
  }

  const std::filesystem::path library = work_dir / "libdpi.so";
  std::vector<std::string> args = {"-shared", "-fPIC"};
  for (const std::string& source : sources) {
    auto language = LanguageFlag(std::filesystem::path{source});
    if (!language) {
      return std::unexpected(std::move(language.error()));
    }
    args.emplace_back("-x");
    args.push_back(*std::move(language));
    args.push_back(source);
  }
  args.emplace_back("-o");
  args.push_back(library.string());

  auto compiled = support::RunProcessCaptured(*cxx, args);
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
