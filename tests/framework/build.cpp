#include "build.hpp"

#include <cerrno>
#include <chrono>
#include <cstdlib>
#include <cstring>
#include <expected>
#include <filesystem>
#include <format>
#include <span>
#include <string>
#include <string_view>
#include <unistd.h>
#include <vector>

#include "process.hpp"

namespace lyra::test {

namespace {

auto IsExecutableFile(const std::filesystem::path& path) -> bool {
  std::error_code ec;
  if (!std::filesystem::is_regular_file(path, ec) || ec) {
    return false;
  }
  return access(path.c_str(), X_OK) == 0;
}

auto FindOnPath(std::string_view name)
    -> std::expected<std::filesystem::path, std::string> {
  const std::filesystem::path candidate(name);
  if (candidate.is_absolute() || candidate.has_parent_path()) {
    auto absolute = std::filesystem::absolute(candidate);
    if (IsExecutableFile(absolute)) {
      return absolute;
    }
    return std::unexpected(
        std::format("'{}' is not an executable file", absolute.string()));
  }
  const char* path_env = std::getenv("PATH");
  if (path_env == nullptr) {
    return std::unexpected("PATH is unset");
  }
  std::string_view path(path_env);
  while (!path.empty()) {
    const auto sep = path.find(':');
    const auto entry = path.substr(0, sep);
    if (!entry.empty()) {
      auto full = std::filesystem::path(entry) / candidate;
      if (IsExecutableFile(full)) {
        return full;
      }
    }
    if (sep == std::string_view::npos) {
      break;
    }
    path.remove_prefix(sep + 1);
  }
  return std::unexpected(
      std::format("'{}' not found on PATH", candidate.string()));
}

auto ResolveCxxCompiler() -> std::expected<std::filesystem::path, std::string> {
  const char* env = std::getenv("CXX");
  if (env != nullptr) {
    const std::string_view sv(env);
    if (!sv.empty()) {
      return FindOnPath(sv);
    }
  }
  // Fall back through the common C++ compiler names. CI runners may have
  // either clang++ or g++ in PATH depending on the image; both compile the
  // emitted C++ since it uses only standard C++23 features.
  for (const std::string_view candidate : {"clang++", "g++", "c++"}) {
    auto found = FindOnPath(candidate);
    if (found) {
      return *found;
    }
  }
  return std::unexpected(
      "no C++ compiler found on PATH (tried clang++, g++, c++; set $CXX to "
      "override)");
}

}  // namespace

auto MakeTempCaseDir() -> std::expected<std::filesystem::path, std::string> {
  const auto base = std::filesystem::temp_directory_path() / "lyra-XXXXXX";
  std::string templ = base.string();
  if (mkdtemp(templ.data()) == nullptr) {
    return std::unexpected(
        std::format(
            "mkdtemp('{}') failed: {}", base.string(), std::strerror(errno)));
  }
  return std::filesystem::path(templ);
}

auto BuildAndRunEmittedArtifacts(
    const std::filesystem::path& work_dir,
    const std::filesystem::path& include_root,
    const std::filesystem::path& cpp_runtime,
    std::span<const std::filesystem::path> extra_include_roots)
    -> BuildAndRunOutcome {
  BuildAndRunOutcome outcome;

  auto cxx_or = ResolveCxxCompiler();
  if (!cxx_or) {
    outcome.error =
        std::format("compiler resolution failed: {}", cxx_or.error());
    return outcome;
  }
  const auto& cxx = *cxx_or;

  if (!std::filesystem::exists(cpp_runtime)) {
    outcome.error = std::format(
        "missing prebuilt C++ runtime at '{}'", cpp_runtime.string());
    return outcome;
  }

  const auto main_cpp = work_dir / "main.cpp";
  const auto program = work_dir / "program";

  if (!std::filesystem::exists(main_cpp)) {
    outcome.error =
        std::format("missing emitted main.cpp at '{}'", main_cpp.string());
    return outcome;
  }

  // The C++ runtime must follow main.cpp on the link line so the linker
  // resolves main.cpp's references against runtime members.
  std::vector<std::string> compile_args = {
      "-std=c++23",
      "-O0",
      "-I",
      include_root.string(),
  };
  for (const auto& extra : extra_include_roots) {
    compile_args.emplace_back("-I");
    compile_args.push_back(extra.string());
  }
  compile_args.push_back(main_cpp.string());
  compile_args.push_back(cpp_runtime.string());
  compile_args.emplace_back("-o");
  compile_args.push_back(program.string());

  auto compile = RunChildProcess(cxx, compile_args, std::chrono::seconds{60});
  if (compile.termination != TerminationKind::kExitedNormally) {
    outcome.error = std::format(
        "compile failed (termination={}, exit={}):\nstdout:\n{}\nstderr:\n{}",
        static_cast<int>(compile.termination), compile.exit_code,
        compile.stdout_text, compile.stderr_text);
    return outcome;
  }

  auto run = RunChildProcess(program, {}, std::chrono::seconds{30});
  if (run.termination != TerminationKind::kExitedNormally &&
      run.termination != TerminationKind::kExitedNonZero) {
    outcome.error = std::format(
        "program did not exit normally (termination={}, signal={}):\n"
        "stdout:\n{}\nstderr:\n{}",
        static_cast<int>(run.termination), run.signal_number, run.stdout_text,
        run.stderr_text);
    return outcome;
  }
  outcome.exit_code = run.exit_code;
  outcome.stdout_text = std::move(run.stdout_text);
  outcome.stderr_text = std::move(run.stderr_text);
  return outcome;
}

}  // namespace lyra::test
