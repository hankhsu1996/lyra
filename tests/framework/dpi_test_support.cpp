#include "tests/framework/dpi_test_support.hpp"

#include <filesystem>
#include <format>
#include <span>
#include <string>
#include <vector>

#include "lyra/llvm_backend/toolchain.hpp"
#include "tests/framework/llvm_common.hpp"

namespace lyra::test {

namespace {

auto GetSharedLibraryExtension() -> std::string {
#if defined(__APPLE__)
  return ".dylib";
#elif defined(_WIN32)
  return ".dll";
#else
  return ".so";
#endif
}

}  // namespace

auto CompileDpiSources(
    std::span<const std::string> dpi_sources,
    const std::filesystem::path& work_dir) -> DpiCompileResult {
  namespace fs = std::filesystem;
  DpiCompileResult result;

  auto toolchain = lowering::mir_to_llvm::DetectToolchain();
  if (!toolchain) {
    result.error =
        std::format("toolchain detection failed: {}", toolchain.error());
    return result;
  }

  fs::create_directories(work_dir);

  auto ext = GetSharedLibraryExtension();

  for (const auto& src : dpi_sources) {
    auto src_abs = fs::absolute(src);
    auto out_path = fs::absolute(work_dir / fs::path(src).stem());
    out_path += ext;

    std::vector<std::string> args = {
        "-shared", "-fPIC", "-o", out_path.string(), src_abs.string(),
    };

    auto sub = RunSubprocess(toolchain->cc_path, args);
    if (sub.exit_code != 0) {
      result.error = std::format(
          "DPI C compilation failed for '{}' -> '{}' (exit code {}): {}",
          src_abs.string(), out_path.string(), sub.exit_code, sub.stderr_text);
      return result;
    }
    result.link_inputs.push_back(out_path);
  }

  return result;
}

}  // namespace lyra::test
