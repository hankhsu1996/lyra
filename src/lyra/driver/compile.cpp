#include "compile.hpp"

#include <format>
#include <string>
#include <utility>
#include <vector>

#include "compilation_output.hpp"
#include "driver_output_options.hpp"
#include "frontend.hpp"
#include "llvm_stats.hpp"
#include "lyra/llvm_backend/emit.hpp"
#include "lyra/llvm_backend/ir_optimize.hpp"
#include "lyra/llvm_backend/lower.hpp"
#include "lyra/llvm_backend/toolchain.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/lowering/origin_map_lookup.hpp"
#include "lyra/runtime/artifact_names.hpp"
#include "lyra/runtime/feature_flags.hpp"
#include "pipeline.hpp"
#include "runtime_path.hpp"
#include "stats_report.hpp"

namespace lyra::driver {

auto Compile(const CompilationInput& input, const CompileOptions& options)
    -> std::expected<std::filesystem::path, int> {
  CompilationOutput output(BuildCompileDriverOutputOptions(input));

  auto result = CompileToMir(input, output);
  if (!result) {
    result.error().Render(output);
    output.Flush();
    return std::unexpected(1);
  }
  auto compilation = std::move(*result);

  lowering::OriginMapLookup origin_lookup(
      &compilation.mir.design_origins, &compilation.mir.body_origins,
      &compilation.hir.design, compilation.hir.hir_arena.get());
  lowering::DiagnosticContext diag_ctx(origin_lookup);

  uint32_t feature_flags = 0;
  if (input.enable_trace_summary) {
    feature_flags |= runtime::ToUint32(runtime::FeatureFlag::kEnableTrace);
    feature_flags |=
        runtime::ToUint32(runtime::FeatureFlag::kEnableTraceSummary);
  }
  if (input.trace_signals_output.has_value()) {
    feature_flags |= runtime::ToUint32(runtime::FeatureFlag::kEnableTrace);
    feature_flags |=
        runtime::ToUint32(runtime::FeatureFlag::kEnableSignalTrace);
  }
  if (input.enable_system) {
    feature_flags |= runtime::ToUint32(runtime::FeatureFlag::kEnableSystem);
  }
  if (input.trace_activations) {
    feature_flags |=
        runtime::ToUint32(runtime::FeatureFlag::kEnableActivationTrace);
  }
  if (input.verbose >= 2) {
    feature_flags |= runtime::ToUint32(runtime::FeatureFlag::kDumpRuntimeStats);
  }
  if (input.verbose >= 3) {
    feature_flags |= runtime::ToUint32(runtime::FeatureFlag::kDetailedStats);
  }
  lowering::mir_to_llvm::LoweringInput llvm_input{
      .design = &compilation.mir.design,
      .mir_arena = compilation.mir.design_arena.get(),
      .type_arena = compilation.hir.type_arena.get(),
      .diag_ctx = &diag_ctx,
      .source_manager = compilation.hir.source_manager.get(),
      .hooks = nullptr,
      .fs_base_dir = input.fs_base_dir.string(),
      .plusargs = {},
      .feature_flags = feature_flags,
      .signal_trace_path = input.trace_signals_output.value_or(""),
      .iteration_limit = input.iteration_limit,
      .force_two_state = input.two_state,
      .collect_forwarding_analysis =
          output.IsEnabled(OutputCategory::kAnalysis),
      .main_abi = lowering::mir_to_llvm::MainAbi::kArgvForwarding,
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
    return std::unexpected(1);
  }

  if (output.IsEnabled(OutputCategory::kAnalysis)) {
    output.PrintForwardingAnalysisReport(
        llvm_result->report.forwarding_analysis);
  }

  LlvmStats llvm_stats;
  if (input.stats_out_path) {
    llvm_stats = CollectLlvmStats(*llvm_result->module);
  }

  {
    PhaseTimer timer(output, Phase::kOptimizeIr);
    lowering::mir_to_llvm::OptimizeModule(
        *llvm_result->module, input.opt_level);
  }

  auto target_machine =
      lowering::mir_to_llvm::CreateHostTargetMachine(input.opt_level);

  auto obj_path = options.output_dir / (options.name + ".o");
  {
    PhaseTimer timer(output, Phase::kEmitObj);
    std::error_code ec;
    std::filesystem::create_directories(options.output_dir, ec);
    if (ec) {
      output.PrintError(
          std::format(
              "cannot create output directory '{}': {}",
              options.output_dir.string(), ec.message()));
      output.Flush();
      return std::unexpected(1);
    }

    auto emit_result = lowering::mir_to_llvm::EmitObjectFile(
        *llvm_result->module, *target_machine, obj_path);
    if (!emit_result) {
      output.PrintError(
          std::format("object emission failed: {}", emit_result.error()));
      output.Flush();
      return std::unexpected(1);
    }
  }

  std::vector<std::string> tried_paths;
  auto runtime_path = FindRuntimeLibrary(runtime::kStaticLibName, tried_paths);
  if (runtime_path.empty()) {
    std::string msg = "runtime library not found\n       tried:";
    for (const auto& path : tried_paths) {
      msg += std::format("\n         - {}", path);
    }
    msg += "\n       hint: set LYRA_RUNTIME_PATH environment variable";
    output.PrintError(msg);
    output.Flush();
    return std::unexpected(1);
  }

  auto toolchain = lowering::mir_to_llvm::DetectToolchain();
  if (!toolchain) {
    output.PrintError(
        std::format("toolchain detection failed: {}", toolchain.error()));
    output.Flush();
    return std::unexpected(1);
  }

  std::expected<std::filesystem::path, lowering::mir_to_llvm::LinkError>
      link_result;
  {
    PhaseTimer timer(output, Phase::kLink);
    link_result = lowering::mir_to_llvm::LinkExecutable(
        *toolchain, obj_path, runtime_path, options.output_dir, options.name);
  }

  std::filesystem::remove(obj_path);

  if (!link_result) {
    const auto& err = link_result.error();
    std::string msg = std::format("link failed: {}", err.message);
    if (!err.stderr.empty()) {
      msg += std::format("\n{}", err.stderr);
    }
    output.PrintError(msg);
    output.Flush();
    return std::unexpected(1);
  }

  if (input.stats_out_path) {
    StatsReport report{
        .backend = StatsBackend::kAot,
        .git_sha = ResolveGitSha(),
        .phases = output.GetPhaseSummaryData(),
        .llvm = llvm_stats,
        .mir = compilation.mir.stats,
        .jit = std::nullopt,
    };
    WriteStatsJson(report, *input.stats_out_path);
  }

  output.Flush();
  return *link_result;
}

}  // namespace lyra::driver
