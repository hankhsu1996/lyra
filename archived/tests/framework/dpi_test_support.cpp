#include "tests/framework/dpi_test_support.hpp"

#include <cstdlib>
#include <filesystem>
#include <format>
#include <optional>
#include <span>
#include <string>
#include <vector>

#include "lyra/llvm_backend/toolchain.hpp"
#include "tests/framework/process_runner.hpp"

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

// Discover the vendored svdpi.h include directory from Bazel runfiles.
// Vendored SystemVerilog standard headers live under third_party/systemverilog.
// This helper resolves the svdpi.h include path within that boundary.
// In runfiles: _main/third_party/systemverilog/svdpi.h.
auto FindVendoredSvdpiIncludeDir() -> std::optional<std::filesystem::path> {
  namespace fs = std::filesystem;
  static constexpr auto kRelPath = "_main/third_party/systemverilog";

  const char* test_srcdir = std::getenv("TEST_SRCDIR");
  if (test_srcdir != nullptr) {
    auto candidate = fs::path(test_srcdir) / kRelPath;
    if (fs::exists(candidate / "svdpi.h")) {
      return candidate;
    }
  }

  try {
    auto exe_path = fs::read_symlink("/proc/self/exe");
    auto candidate = fs::path(exe_path.string() + ".runfiles") / kRelPath;
    if (fs::exists(candidate / "svdpi.h")) {
      return candidate;
    }
  } catch (const fs::filesystem_error&) {
  }

  return std::nullopt;
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

  auto svdpi_dir = FindVendoredSvdpiIncludeDir();
  if (!svdpi_dir.has_value()) {
    result.error =
        "Failed to locate vendored svdpi.h for DPI companion compilation";
    return result;
  }

  fs::create_directories(work_dir);

  auto ext = GetSharedLibraryExtension();

  for (const auto& src : dpi_sources) {
    auto src_abs = fs::absolute(src);
    auto out_path = fs::absolute(work_dir / fs::path(src).stem());
    out_path += ext;

    std::vector<std::string> args = {
        "-shared",
        "-fPIC",
        "-o",
        out_path.string(),
        std::format("-I{}", svdpi_dir->string()),
        src_abs.string(),
    };

    auto proc = RunChildProcess(toolchain->cc_path, args);
    if (proc.termination != TerminationKind::kExitedNormally) {
      result.error =
          FormatToolFailure("DPI C compilation", src_abs, out_path, proc);
      return result;
    }
    result.link_inputs.push_back(out_path);
  }

  return result;
}

auto CompileDpiSourcesToObjects(
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

  auto svdpi_dir = FindVendoredSvdpiIncludeDir();
  if (!svdpi_dir.has_value()) {
    result.error =
        "Failed to locate vendored svdpi.h for DPI companion compilation";
    return result;
  }

  fs::create_directories(work_dir);

  for (const auto& src : dpi_sources) {
    auto src_abs = fs::absolute(src);
    auto out_path = fs::absolute(work_dir / fs::path(src).stem());
    out_path += ".o";

    std::vector<std::string> args = {
        "-c",
        "-fPIC",
        "-o",
        out_path.string(),
        std::format("-I{}", svdpi_dir->string()),
        src_abs.string(),
    };

    auto proc = RunChildProcess(toolchain->cc_path, args);
    if (proc.termination != TerminationKind::kExitedNormally) {
      result.error =
          FormatToolFailure("DPI C compilation", src_abs, out_path, proc);
      return result;
    }
    result.link_inputs.push_back(out_path);
  }

  return result;
}

}  // namespace lyra::test
