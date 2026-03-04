#include "compile.hpp"

#include <format>
#include <string>
#include <utility>
#include <vector>

#include "frontend.hpp"
#include "llvm_stats.hpp"
#include "lyra/llvm_backend/emit.hpp"
#include "lyra/llvm_backend/lower.hpp"
#include "lyra/llvm_backend/toolchain.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/lowering/origin_map_lookup.hpp"
#include "lyra/runtime/feature_flags.hpp"
#include "pipeline.hpp"
#include "print.hpp"
#include "runtime_path.hpp"
#include "stats_report.hpp"
#include "verbose_logger.hpp"

namespace lyra::driver {

auto Compile(const CompilationInput& input, const CompileOptions& options)
    -> std::expected<std::filesystem::path, int> {
  VerboseLogger vlog(input.verbose);

  auto result = CompileToMir(input, vlog);
  if (!result) {
    result.error().Print();
    return std::unexpected(1);
  }
  auto compilation = std::move(*result);

  lowering::OriginMapLookup origin_lookup(
      &compilation.mir.origin_map, compilation.hir.hir_arena.get());
  lowering::DiagnosticContext diag_ctx(origin_lookup);

  uint32_t feature_flags = 0;
  if (input.enable_trace) {
    feature_flags |= runtime::ToUint32(runtime::FeatureFlag::kEnableTrace);
  }
  if (input.enable_system) {
    feature_flags |= runtime::ToUint32(runtime::FeatureFlag::kEnableSystem);
  }

  lowering::mir_to_llvm::LoweringInput llvm_input{
      .design = &compilation.mir.design,
      .mir_arena = compilation.mir.mir_arena.get(),
      .type_arena = compilation.hir.type_arena.get(),
      .diag_ctx = &diag_ctx,
      .hooks = nullptr,
      .fs_base_dir = input.fs_base_dir.string(),
      .plusargs = {},
      .feature_flags = feature_flags,
      .force_two_state = input.two_state,
      .main_abi = lowering::mir_to_llvm::MainAbi::kArgvForwarding,
  };

  std::expected<lowering::mir_to_llvm::LoweringResult, Diagnostic> llvm_result;
  {
    PhaseTimer timer(vlog, "lower_llvm");
    llvm_result = lowering::mir_to_llvm::LowerMirToLlvm(llvm_input);
  }
  if (!llvm_result) {
    PrintDiagnostic(llvm_result.error(), *compilation.hir.source_manager);
    return std::unexpected(1);
  }

  // Collect LLVM stats before module is consumed
  LlvmStats llvm_stats;
  if (input.stats_out_path) {
    llvm_stats = CollectLlvmStats(*llvm_result->module);
  }

  // Create target machine for object emission
  auto target_machine =
      lowering::mir_to_llvm::CreateHostTargetMachine(input.opt_level);

  // Emit object file to temp location
  auto obj_path = options.output_dir / (options.name + ".o");
  {
    PhaseTimer timer(vlog, "emit_obj");
    std::error_code ec;
    std::filesystem::create_directories(options.output_dir, ec);
    if (ec) {
      PrintError(
          std::format(
              "cannot create output directory '{}': {}",
              options.output_dir.string(), ec.message()));
      return std::unexpected(1);
    }

    auto emit_result = lowering::mir_to_llvm::EmitObjectFile(
        *llvm_result->module, *target_machine, obj_path);
    if (!emit_result) {
      PrintError(
          std::format("object emission failed: {}", emit_result.error()));
      return std::unexpected(1);
    }
  }

  // Find runtime library
  std::vector<std::string> tried_paths;
  auto runtime_path = FindRuntimeLibrary(tried_paths);
  if (runtime_path.empty()) {
    std::string msg = "runtime library not found\n       tried:";
    for (const auto& path : tried_paths) {
      msg += std::format("\n         - {}", path);
    }
    msg += "\n       hint: set LYRA_RUNTIME_PATH environment variable";
    PrintError(msg);
    return std::unexpected(1);
  }

  // Detect toolchain and link
  auto toolchain = lowering::mir_to_llvm::DetectToolchain();
  if (!toolchain) {
    PrintError(
        std::format("toolchain detection failed: {}", toolchain.error()));
    return std::unexpected(1);
  }

  std::expected<std::filesystem::path, lowering::mir_to_llvm::LinkError>
      link_result;
  {
    PhaseTimer timer(vlog, "link");
    link_result = lowering::mir_to_llvm::LinkExecutable(
        *toolchain, obj_path, runtime_path, options.output_dir, options.name);
  }

  // Clean up object file
  std::filesystem::remove(obj_path);

  if (!link_result) {
    const auto& err = link_result.error();
    std::string msg = std::format("link failed: {}", err.message);
    if (!err.stderr.empty()) {
      msg += std::format("\n{}", err.stderr);
    }
    PrintError(msg);
    return std::unexpected(1);
  }

  // Write structured JSON stats
  if (input.stats_out_path) {
    StatsReport report{
        .backend = StatsBackend::kAot,
        .git_sha = ResolveGitSha(),
        .vlog = &vlog,
        .llvm = llvm_stats,
        .mir = compilation.mir.stats,
        .jit = std::nullopt,
    };
    WriteStatsJson(report, *input.stats_out_path);
  }

  return *link_result;
}

}  // namespace lyra::driver
