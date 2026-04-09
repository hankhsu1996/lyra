#include "run_lli.hpp"

#include <cerrno>
#include <cstring>
#include <filesystem>
#include <format>
#include <fstream>
#include <span>
#include <string>
#include <utility>
#include <vector>

// POSIX headers
#include <spawn.h>
#include <sys/wait.h>
#include <unistd.h>

#include "compilation_output.hpp"
#include "driver_output_options.hpp"
#include "frontend.hpp"
#include "llvm_stats.hpp"
#include "lyra/llvm_backend/lower.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/lowering/origin_map_lookup.hpp"
#include "lyra/runtime/artifact_names.hpp"
#include "pipeline.hpp"
#include "process_stats.hpp"
#include "runtime_path.hpp"
#include "validated_input.hpp"

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

auto SpawnLli(
    const std::string& runtime_path, const std::string& ir_path,
    std::span<const std::filesystem::path> dpi_link_inputs) -> int {
  // Runtime is loaded via --dlopen so lli can resolve Lyra* symbols.
  std::vector<std::string> dlopen_args;
  dlopen_args.push_back(std::format("--dlopen={}", runtime_path));

  // LLI materialization: each validated DPI link input is passed through
  // --dlopen so the interpreter can resolve foreign symbols at execution time.
  for (const auto& input : dpi_link_inputs) {
    dlopen_args.push_back(std::format("--dlopen={}", input.string()));
  }

  std::vector<char*> argv;
  std::string lli_cmd = "lli";
  argv.push_back(lli_cmd.data());
  for (auto& arg : dlopen_args) {
    argv.push_back(arg.data());
  }
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

auto RunLli(const ValidatedCompilationInput& input) -> int {
  bool emit_stats = input.input.stats_top_n >= 0;
  CompilationOutput output(
      BuildLliDriverOutputOptions(input.input, emit_stats));

  auto result = CompileToMir(input.input, output);
  if (!result) {
    result.error().Render(output);
    output.Flush();
    return 1;
  }
  auto compilation = std::move(*result);

  // Create diagnostic context for LLVM backend error reporting
  lowering::OriginMapLookup origin_lookup(
      &compilation.mir.design_origins, &compilation.hir.design,
      compilation.hir.hir_arena.get());
  lowering::DiagnosticContext diag_ctx(origin_lookup);

  lowering::mir_to_llvm::LoweringInput llvm_input{
      .design = &compilation.mir.design,
      .construction = &compilation.mir.construction,
      .mir_arena = compilation.mir.design_arena.get(),
      .type_arena = compilation.hir.type_arena.get(),
      .diag_ctx = &diag_ctx,
      .source_manager = compilation.hir.source_manager.get(),
      .body_origins = &compilation.mir.body_origins,
      .hir_design = &compilation.hir.design,
      .hir_global_arena = compilation.hir.hir_arena.get(),
      .fs_base_dir = input.input.fs_base_dir.string(),
      .plusargs = input.input.plusargs,
      .feature_flags = 0,
      .signal_trace_path = input.input.trace_signals_output.value_or(""),
      .iteration_limit = input.input.iteration_limit,
      .body_timescales = &compilation.hir.body_timescales,
      .force_two_state = input.input.two_state,
      .collect_forwarding_analysis =
          output.IsEnabled(OutputCategory::kAnalysis),
      .main_abi = lowering::mir_to_llvm::MainAbi::kArgvForwarding,
      .dpi_export_wrappers = &compilation.mir.dpi_export_wrappers,
      .bound_connections = &compilation.mir.bound_connections,
      .expr_connections = &compilation.mir.expr_connections,
  };

  std::expected<lowering::mir_to_llvm::LoweringResult, Diagnostic> llvm_result;
  {
    PhaseTimer timer(output, Phase::kLowerLlvm);
    llvm_result = lowering::mir_to_llvm::LowerMirToLlvm(llvm_input);
  }
  if (!llvm_result) {
    output.PrintDiagnostic(
        llvm_result.error(), *compilation.hir.source_manager);
    output.Flush();
    return 1;
  }

  if (emit_stats) {
    output.PrintMirStats(compilation.mir.stats);
    output.PrintPhaseSummary();
    LlvmStats llvm_stats_data = CollectLlvmStats(*llvm_result->module);
    output.PrintLlvmStats(llvm_stats_data, input.input.stats_top_n);
    auto ps = CollectProcessStats(
        compilation.mir.design, *compilation.mir.design_arena,
        compilation.mir.design_origins, compilation.hir.design,
        *compilation.hir.hir_arena, *compilation.hir.source_manager,
        llvm_stats_data);
    output.PrintProcessStats(ps);
  }

  if (output.IsEnabled(OutputCategory::kAnalysis)) {
    output.PrintForwardingAnalysisReport(
        llvm_result->report.forwarding_analysis);
  }

  std::string ir_path = CreateTempFile(".ll");
  if (ir_path.empty()) {
    output.PrintError(
        std::format("failed to create temp file: {}", std::strerror(errno)));
    output.Flush();
    return 1;
  }
  TempFileGuard temp_guard(ir_path);

  {
    std::ofstream out(ir_path);
    if (!out) {
      output.PrintError(std::format("failed to write to {}", ir_path));
      output.Flush();
      return 1;
    }
    out << lowering::mir_to_llvm::DumpLlvmIr(*llvm_result);
  }

  std::vector<std::string> tried_paths;
  auto runtime_path = FindRuntimeLibrary(runtime::kSharedLibName, tried_paths);
  if (runtime_path.empty()) {
    std::string msg = "runtime library not found\n       tried:";
    for (const auto& path : tried_paths) {
      msg += std::format("\n         - {}", path);
    }
    msg += "\n       hint: set LYRA_RUNTIME_PATH environment variable";
    output.PrintError(msg);
    output.Flush();
    return 1;
  }

  int exit_code = 0;
  {
    PhaseTimer timer(output, Phase::kSim, HeartbeatPolicy::kEnabled);
    exit_code =
        SpawnLli(runtime_path.string(), ir_path, input.input.dpi_link_inputs);
  }
  if (exit_code == -1) {
    output.PrintError(
        std::format(
            "failed to execute lli: {}\n"
            "       hint: ensure 'lli' is installed and in PATH",
            std::strerror(errno)));
    output.Flush();
    return 1;
  }

  output.Flush();
  return exit_code;
}

}  // namespace lyra::driver
