#include "lyra/driver/cpp_build.hpp"

#include <format>
#include <fstream>
#include <span>
#include <string>
#include <string_view>
#include <system_error>
#include <utility>
#include <vector>

#include "lyra/backend/cpp/api.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/driver/project_layout.hpp"
#include "lyra/driver/runtime_export.hpp"
#include "lyra/support/subprocess.hpp"

namespace lyra::driver {

namespace {

auto IoError(std::string message) {
  return diag::HostError(diag::DiagCode::kHostIoError, std::move(message));
}

auto WriteFile(const std::filesystem::path& path, std::string_view content)
    -> diag::Result<void> {
  std::error_code ec;
  std::filesystem::create_directories(path.parent_path(), ec);
  if (ec) {
    return IoError(
        std::format(
            "failed to create '{}': {}", path.parent_path().string(),
            ec.message()));
  }
  std::ofstream out(path, std::ios::binary);
  out << content;
  out.flush();
  if (!out) {
    return IoError(std::format("failed to write '{}'", path.string()));
  }
  return {};
}

auto RenderBuildScript() -> std::string {
  return std::format(
      "#!/bin/sh\n"
      "# Build this self-contained Lyra C++ project. Override the compiler "
      "with $CXX.\n"
      "set -e\n"
      "\"${{CXX:-clang++}}\" {} -I {} {} {}/{} -o {}\n",
      kCxxStandardFlag, kRuntimeIncludeDir, kMainSource, kRuntimeLibDir,
      kRuntimeLibFile, kProgramName);
}

// Best-effort: reformat the emitted C++ files in place with clang-format if it
// is on PATH. A missing formatter or a non-zero exit is ignored -- the emitted
// code is valid C++ either way, so formatting never gates emission.
void FormatSources(
    std::span<const backend::cpp::CppArtifact> files,
    const std::filesystem::path& dir) {
  auto clang_format = support::FindOnPath("clang-format");
  if (!clang_format) {
    return;
  }
  std::vector<std::string> args = {"-i", "-style=Google"};
  for (const auto& file : files) {
    args.push_back((dir / file.relpath).string());
  }
  (void)support::RunProcessCaptured(*clang_format, args);
}

auto EmitAndWriteSources(
    std::span<const mir::CompilationUnit> units,
    std::span<const backend::cpp::TopInstance> tops,
    const std::filesystem::path& dir, bool format) -> diag::Result<void> {
  auto set_or = backend::cpp::EmitCpp(units, tops);
  if (!set_or) {
    return std::unexpected(std::move(set_or.error()));
  }
  for (const auto& file : set_or->files) {
    if (auto r = WriteFile(dir / file.relpath, file.content); !r) {
      return r;
    }
  }
  if (format) {
    FormatSources(set_or->files, dir);
  }
  return {};
}

auto CompileProgram(
    const std::filesystem::path& main_cpp,
    const std::filesystem::path& include_root, const std::filesystem::path& lib,
    const std::filesystem::path& program) -> diag::Result<void> {
  auto cxx_or = support::ResolveCxxCompiler();
  if (!cxx_or) {
    return IoError(std::move(cxx_or.error()));
  }
  const std::vector<std::string> args = {
      std::string(kCxxStandardFlag),
      "-I",
      include_root.string(),
      main_cpp.string(),
      lib.string(),
      "-o",
      program.string()};
  auto result_or = support::RunProcessCaptured(*cxx_or, args);
  if (!result_or) {
    return IoError(std::move(result_or.error()));
  }
  if (result_or->exit_code != 0) {
    return diag::HostError(
        diag::DiagCode::kHostBuildFailed,
        std::format(
            "C++ compiler exited with {}:\n{}", result_or->exit_code,
            result_or->stderr_text));
  }
  return {};
}

}  // namespace

auto AssembleProject(
    const RuntimeLocation& runtime, std::span<const mir::CompilationUnit> units,
    std::span<const backend::cpp::TopInstance> tops,
    const std::filesystem::path& dir, bool format) -> diag::Result<void> {
  if (auto r = EmitAndWriteSources(units, tops, dir, format); !r) {
    return r;
  }

  const auto script_path = dir / "build.sh";
  if (auto r = WriteFile(script_path, RenderBuildScript()); !r) {
    return r;
  }
  std::error_code ec;
  std::filesystem::permissions(
      script_path,
      std::filesystem::perms::owner_exec | std::filesystem::perms::group_exec |
          std::filesystem::perms::others_exec,
      std::filesystem::perm_options::add, ec);
  if (ec) {
    return IoError(
        std::format(
            "failed to mark '{}' executable: {}", script_path.string(),
            ec.message()));
  }

  if (auto exported = ExportRuntimeTree(runtime, dir); !exported) {
    return IoError(std::move(exported.error()));
  }
  return {};
}

auto BuildProject(const std::filesystem::path& dir)
    -> diag::Result<std::filesystem::path> {
  const auto program = dir / kProgramName;
  if (auto r = CompileProgram(
          dir / kMainSource, dir / kRuntimeIncludeDir,
          dir / kRuntimeLibDir / kRuntimeLibFile, program);
      !r) {
    return std::unexpected(std::move(r.error()));
  }
  return program;
}

auto RunInPlace(
    const RuntimeLocation& runtime, std::span<const mir::CompilationUnit> units,
    std::span<const backend::cpp::TopInstance> tops,
    const std::filesystem::path& work_dir, bool format) -> diag::Result<int> {
  if (auto r = EmitAndWriteSources(units, tops, work_dir, format); !r) {
    return std::unexpected(std::move(r.error()));
  }
  const auto program = work_dir / kProgramName;
  if (auto r = CompileProgram(
          work_dir / kMainSource, runtime.include_root, runtime.lib, program);
      !r) {
    return std::unexpected(std::move(r.error()));
  }
  auto exit_or = support::RunProcessStreaming(program, {});
  if (!exit_or) {
    return IoError(std::move(exit_or.error()));
  }
  return *exit_or;
}

}  // namespace lyra::driver
