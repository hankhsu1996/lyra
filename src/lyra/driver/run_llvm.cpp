#include "run_llvm.hpp"

#include <cerrno>
#include <cstdlib>
#include <cstring>
#include <filesystem>
#include <fstream>
#include <spawn.h>
#include <sys/wait.h>
#include <unistd.h>

#include <fmt/core.h>

#include "lyra/lowering/mir_to_llvm/lower.hpp"
#include "pipeline.hpp"
#include "print.hpp"

namespace lyra::driver {

namespace {

class TempFileGuard {
 public:
  explicit TempFileGuard(std::string path) : path_(std::move(path)) {
  }
  ~TempFileGuard() {
    if (!path_.empty()) {
      std::filesystem::remove(path_);
    }
  }

  TempFileGuard(const TempFileGuard&) = delete;
  auto operator=(const TempFileGuard&) -> TempFileGuard& = delete;
  TempFileGuard(TempFileGuard&&) = delete;
  auto operator=(TempFileGuard&&) -> TempFileGuard& = delete;

  void Release() {
    path_.clear();
  }

 private:
  std::string path_;
};

auto CreateTempFile(const std::string& suffix) -> std::string {
  std::string tmpl = std::filesystem::temp_directory_path() / "lyra_XXXXXX";
  tmpl += suffix;

  std::vector<char> buf(tmpl.begin(), tmpl.end());
  buf.push_back('\0');

  int fd = mkstemps(buf.data(), static_cast<int>(suffix.size()));
  if (fd == -1) {
    return "";
  }
  close(fd);
  return {buf.data()};
}

auto FindRuntimeLibrary(std::vector<std::string>& tried_paths) -> std::string {
  constexpr auto kLibName = "liblyra_runtime.so";

  if (const char* env_path = std::getenv("LYRA_RUNTIME_PATH")) {
    if (std::filesystem::exists(env_path)) {
      return env_path;
    }
    tried_paths.emplace_back(fmt::format("LYRA_RUNTIME_PATH={}", env_path));
  }

  std::filesystem::path exe_path;
  try {
    exe_path = std::filesystem::read_symlink("/proc/self/exe");
  } catch (const std::filesystem::filesystem_error& e) {
    tried_paths.emplace_back(
        fmt::format(
            "/proc/self/exe ({}; non-Linux or sandboxed?)",
            e.code().message()));
  }

  if (!exe_path.empty()) {
    auto runfiles_path =
        std::filesystem::path(exe_path.string() + ".runfiles") / "_main" /
        kLibName;
    tried_paths.push_back(runfiles_path.string());
    if (std::filesystem::exists(runfiles_path)) {
      return runfiles_path.string();
    }

    auto sibling_path = exe_path.parent_path() / kLibName;
    tried_paths.push_back(sibling_path.string());
    if (std::filesystem::exists(sibling_path)) {
      return sibling_path.string();
    }
  }

  auto cwd_path = std::filesystem::current_path() / kLibName;
  tried_paths.push_back(cwd_path.string());
  if (std::filesystem::exists(cwd_path)) {
    return cwd_path.string();
  }

  return "";
}

auto RunLli(const std::string& runtime_path, const std::string& ir_path)
    -> int {
  std::string dlopen_arg = fmt::format("--dlopen={}", runtime_path);

  std::vector<char*> argv;
  std::string lli_cmd = "lli";
  argv.push_back(lli_cmd.data());
  argv.push_back(dlopen_arg.data());
  std::string ir_path_copy = ir_path;
  argv.push_back(ir_path_copy.data());
  argv.push_back(nullptr);

  pid_t pid = 0;
  int spawn_result =
      posix_spawnp(&pid, "lli", nullptr, nullptr, argv.data(), environ);

  if (spawn_result != 0) {
    return -1;
  }

  int status = 0;
  if (waitpid(pid, &status, 0) == -1) {
    return -1;
  }

  if (WIFEXITED(status)) {
    return WEXITSTATUS(status);
  }
  return -1;
}

}  // namespace

auto RunLlvm(const std::vector<std::string>& files) -> int {
  auto compilation = CompileToMir(files);
  if (!compilation) {
    PrintError(compilation.error());
    return 1;
  }

  lowering::mir_to_llvm::LoweringInput llvm_input{
      .design = &compilation->mir.design,
      .mir_arena = compilation->mir.mir_arena.get(),
      .type_arena = compilation->hir.type_arena.get(),
  };

  lowering::mir_to_llvm::LoweringResult llvm_result;
  try {
    llvm_result = lowering::mir_to_llvm::LowerMirToLlvm(llvm_input);
  } catch (const std::exception& e) {
    PrintError(
        fmt::format(
            "LLVM backend error: {}\n"
            "       hint: some features are not yet supported in "
            "--backend=llvm\n"
            "       hint: try --backend=mir instead",
            e.what()));
    return 1;
  }

  std::string ir_path = CreateTempFile(".ll");
  if (ir_path.empty()) {
    PrintError(
        fmt::format("failed to create temp file: {}", std::strerror(errno)));
    return 1;
  }
  TempFileGuard temp_guard(ir_path);

  {
    std::ofstream out(ir_path);
    if (!out) {
      PrintError(fmt::format("failed to write to {}", ir_path));
      return 1;
    }
    out << lowering::mir_to_llvm::DumpLlvmIr(llvm_result);
  }

  std::vector<std::string> tried_paths;
  std::string runtime_path = FindRuntimeLibrary(tried_paths);
  if (runtime_path.empty()) {
    std::string msg = "runtime library not found\n       tried:";
    for (const auto& path : tried_paths) {
      msg += fmt::format("\n         - {}", path);
    }
    msg += "\n       hint: set LYRA_RUNTIME_PATH environment variable";
    PrintError(msg);
    return 1;
  }

  int exit_code = RunLli(runtime_path, ir_path);
  if (exit_code == -1) {
    PrintError(
        fmt::format(
            "failed to execute lli: {}\n"
            "       hint: ensure 'lli' is installed and in PATH",
            std::strerror(errno)));
    return 1;
  }

  return exit_code;
}

}  // namespace lyra::driver
