#include "run_lli.hpp"

#include <cerrno>
#include <cstring>
#include <filesystem>
#include <format>
#include <fstream>
#include <string>
#include <utility>
#include <vector>

// POSIX headers
#include <spawn.h>
#include <sys/wait.h>
#include <unistd.h>

#include "frontend.hpp"
#include "lyra/llvm_backend/lower.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/lowering/origin_map_lookup.hpp"
#include "pipeline.hpp"
#include "print.hpp"
#include "runtime_path.hpp"
#include "verbose_logger.hpp"

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

auto SpawnLli(const std::string& runtime_path, const std::string& ir_path)
    -> int {
  std::string dlopen_arg = std::format("--dlopen={}", runtime_path);

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

auto RunLli(const CompilationInput& input) -> int {
  VerboseLogger vlog(input.verbose);

  auto result = CompileToMir(input, vlog);
  if (!result) {
    result.error().Print();
    return 1;
  }
  auto compilation = std::move(*result);

  // Create diagnostic context for LLVM backend error reporting
  lowering::OriginMapLookup origin_lookup(
      &compilation.mir.origin_map, compilation.hir.hir_arena.get());
  lowering::DiagnosticContext diag_ctx(origin_lookup);

  lowering::mir_to_llvm::LoweringInput llvm_input{
      .design = &compilation.mir.design,
      .mir_arena = compilation.mir.mir_arena.get(),
      .type_arena = compilation.hir.type_arena.get(),
      .diag_ctx = &diag_ctx,
      .fs_base_dir = input.fs_base_dir.string(),
      .plusargs = input.plusargs,
  };

  std::expected<lowering::mir_to_llvm::LoweringResult, Diagnostic> llvm_result;
  {
    PhaseTimer timer(vlog, "lower_llvm");
    llvm_result = lowering::mir_to_llvm::LowerMirToLlvm(llvm_input);
  }
  if (!llvm_result) {
    PrintDiagnostic(llvm_result.error(), *compilation.hir.source_manager);
    return 1;
  }

  std::string ir_path = CreateTempFile(".ll");
  if (ir_path.empty()) {
    PrintError(
        std::format("failed to create temp file: {}", std::strerror(errno)));
    return 1;
  }
  TempFileGuard temp_guard(ir_path);

  {
    std::ofstream out(ir_path);
    if (!out) {
      PrintError(std::format("failed to write to {}", ir_path));
      return 1;
    }
    out << lowering::mir_to_llvm::DumpLlvmIr(*llvm_result);
  }

  std::vector<std::string> tried_paths;
  auto runtime_path = FindRuntimeLibrary(tried_paths);
  if (runtime_path.empty()) {
    std::string msg = "runtime library not found\n       tried:";
    for (const auto& path : tried_paths) {
      msg += std::format("\n         - {}", path);
    }
    msg += "\n       hint: set LYRA_RUNTIME_PATH environment variable";
    PrintError(msg);
    return 1;
  }

  int exit_code;
  {
    PhaseTimer timer(vlog, "lli", true);
    exit_code = SpawnLli(runtime_path.string(), ir_path);
  }
  if (exit_code == -1) {
    PrintError(
        std::format(
            "failed to execute lli: {}\n"
            "       hint: ensure 'lli' is installed and in PATH",
            std::strerror(errno)));
    return 1;
  }

  return exit_code;
}

}  // namespace lyra::driver
