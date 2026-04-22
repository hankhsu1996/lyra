#include "render_stats.hpp"

#include <algorithm>
#include <cstdint>
#include <numeric>
#include <string>
#include <string_view>
#include <vector>

#include <fmt/core.h>
#include <fmt/format.h>

#include "llvm_stats.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/llvm_backend/execution.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "process_stats.hpp"
#include "text_sink.hpp"

namespace lyra::driver {

namespace {

auto ProcessStatsKindName(ProcessStatsKind kind) -> std::string_view {
  switch (kind) {
    case ProcessStatsKind::kConnection:
      return "connection";
    case ProcessStatsKind::kInitial:
      return "initial";
    case ProcessStatsKind::kAlways:
      return "always";
    case ProcessStatsKind::kAlwaysComb:
      return "always_comb";
    case ProcessStatsKind::kAlwaysFf:
      return "always_ff";
    case ProcessStatsKind::kAlwaysLatch:
      return "always_latch";
    case ProcessStatsKind::kFinal:
      return "final";
    case ProcessStatsKind::kMirOnce:
      return "once";
    case ProcessStatsKind::kMirLooping:
      return "looping";
    case ProcessStatsKind::kMirFinal:
      return "final";
  }
  throw common::InternalError(
      "ProcessStatsKindName", "invalid process stats kind");
}

}  // namespace

void RenderPhaseSummary(
    TextSink& sink, const PhaseDurationArray& durations,
    const PhaseRecordedArray& recorded) {
  fmt::memory_buffer buf;
  fmt::format_to(std::back_inserter(buf), "[lyra][stats][phase]");
  for (const auto& [phase, name] : kOrderedPhases) {
    auto idx = static_cast<size_t>(phase);
    if (recorded[idx]) {
      fmt::format_to(
          std::back_inserter(buf), " {}={:.2f}s", name, durations[idx]);
    }
  }
  fmt::format_to(std::back_inserter(buf), "\n");
  sink.Write(std::string_view(buf.data(), buf.size()));
}

void RenderMirStats(
    TextSink& sink, const lowering::hir_to_mir::LoweringStats& stats) {
  sink.Write(
      fmt::format(
          "[lyra][stats][mir] place_temps={} value_temps={} "
          "materialize_to_place={} mir_stmts={}\n",
          stats.place_temps, stats.value_temps, stats.materialize_to_place,
          stats.mir_stmts));
}

void RenderLlvmStats(TextSink& sink, const LlvmStats& stats, int top_n) {
  sink.Write(
      fmt::format(
          "[lyra][stats] functions={} globals={} bbs={} insts={}\n",
          stats.defined_functions, stats.global_count, stats.total_bbs,
          stats.total_insts));

  if (!stats.func_stats.empty() && stats.total_insts > 0) {
    const auto& max_func = stats.func_stats[0];
    double percent = 100.0 * static_cast<double>(max_func.instructions) /
                     static_cast<double>(stats.total_insts);
    sink.Write(
        fmt::format(
            "[lyra][stats][max] name={} insts={} percent={:.1f}%\n",
            max_func.name, max_func.instructions, percent));
  }

  int count = std::min(top_n, static_cast<int>(stats.func_stats.size()));
  for (int i = 0; i < count; ++i) {
    const auto& fs = stats.func_stats[i];
    sink.Write(
        fmt::format(
            "[lyra][stats][top] {}) {} insts={} bbs={}\n", i + 1, fs.name,
            fs.instructions, fs.basic_blocks));
  }
}

void RenderProcessStats(TextSink& sink, const ProcessStatsData& data) {
  size_t total_with_kernelized = data.entries.size() + data.num_kernelized;
  sink.Write(
      fmt::format(
          "[lyra][stats][proc] total={} init={} connection={} "
          "kernelized={} module={}\n",
          total_with_kernelized, data.num_init, data.num_connection,
          data.num_kernelized, data.num_module));

  sink.Write(
      fmt::format(
          "[lyra][stats][proc] always_ff={} always_comb={} always={} "
          "always_latch={} initial={} final={} connection={}\n",
          data.count_always_ff, data.count_always_comb, data.count_always,
          data.count_always_latch, data.count_initial, data.count_final,
          data.count_connection));

  if (data.total_llvm_insts > 0) {
    double proc_percent = 100.0 * static_cast<double>(data.total_proc_insts) /
                          static_cast<double>(data.total_llvm_insts);
    sink.Write(
        fmt::format(
            "[lyra][stats][proc] proc_insts={} total_insts={} "
            "proc_share={:.1f}%\n",
            data.total_proc_insts, data.total_llvm_insts, proc_percent));
  }

  std::vector<size_t> sorted_indices(data.entries.size());
  std::iota(sorted_indices.begin(), sorted_indices.end(), 0);
  std::sort(
      sorted_indices.begin(), sorted_indices.end(), [&](size_t a, size_t b) {
        return data.entries[a].llvm_insts > data.entries[b].llvm_insts;
      });

  int top_n = std::min(10, static_cast<int>(data.entries.size()));
  for (int rank = 0; rank < top_n; ++rank) {
    const auto& e = data.entries[sorted_indices[rank]];
    auto kind_str = ProcessStatsKindName(e.kind);

    double percent = (data.total_llvm_insts > 0)
                         ? 100.0 * static_cast<double>(e.llvm_insts) /
                               static_cast<double>(data.total_llvm_insts)
                         : 0.0;

    fmt::memory_buffer buf;
    fmt::format_to(
        std::back_inserter(buf),
        "[lyra][stats][proc][top] {}) process_{} kind={} "
        "llvm_insts={} llvm_bbs={} mir_stmts={} percent={:.1f}%",
        rank + 1, e.layout_index, kind_str, e.llvm_insts, e.llvm_bbs,
        e.mir_stmts, percent);

    if (!e.source_location.empty()) {
      fmt::format_to(std::back_inserter(buf), " src={}", e.source_location);
    }
    fmt::format_to(std::back_inserter(buf), "\n");
    sink.Write(std::string_view(buf.data(), buf.size()));
  }

  sink.Write(
      fmt::format(
          "[lyra][stats][proc] template: groups={} shared_ir_insts={} "
          "wrappers={} nonshared={}\n",
          data.template_groups, data.shared_ir_insts, data.wrapper_count,
          data.nonshared_count));

  if (!data.entries.empty()) {
    sink.Write(
        fmt::format(
            "[lyra][stats][proc] median_insts={} p90_insts={} p99_insts={} "
            "max_insts={}\n",
            data.median_insts, data.p90_insts, data.p99_insts, data.max_insts));
  }
}

void RenderJitTimings(
    TextSink& sink, const lowering::mir_to_llvm::JitCompileTimings& jt) {
  if (!jt.complete) return;

  fmt::memory_buffer buf;
  fmt::format_to(
      std::back_inserter(buf),
      "[lyra][stats][jit] create_jit={:.3f}s load_runtime={:.3f}s "
      "optimize_ir={:.3f}s add_ir={:.3f}s lookup_main={:.3f}s "
      "codegen={:.3f}s linking={:.3f}s",
      jt.create_jit, jt.load_runtime, jt.optimize_ir, jt.add_ir, jt.lookup_main,
      jt.codegen, jt.linking);

  if (jt.has_link_detail) {
    fmt::format_to(
        std::back_inserter(buf),
        " link_graph={:.3f}s link_alloc={:.3f}s"
        " link_fixup={:.3f}s link_finalize={:.3f}s",
        jt.link_graph, jt.link_alloc, jt.link_fixup, jt.link_finalize);
    if (jt.finalize_perm > 0.0 || jt.finalize_overhead > 0.0) {
      fmt::format_to(
          std::back_inserter(buf),
          " finalize_perm={:.3f}s finalize_overhead={:.3f}s", jt.finalize_perm,
          jt.finalize_overhead);
    }
  }
  fmt::format_to(std::back_inserter(buf), "\n");
  sink.Write(std::string_view(buf.data(), buf.size()));
}

void RenderOrcStats(
    TextSink& sink, const lowering::mir_to_llvm::JitOrcStats& orc) {
  fmt::memory_buffer buf;
  fmt::format_to(
      std::back_inserter(buf),
      "[lyra][stats][orc] linker={} objects={} obj_bytes={}",
      orc.linker_backend, orc.object_count, orc.total_object_bytes);
  if (orc.relocation_count > 0) {
    fmt::format_to(
        std::back_inserter(buf), " relocs={} syms={} sections={} blocks={}",
        orc.relocation_count, orc.symbol_count, orc.section_count,
        orc.block_count);
  }
  if (orc.finalize_segments > 0) {
    fmt::format_to(
        std::back_inserter(buf), " finalize_segments={} finalize_bytes={}",
        orc.finalize_segments, orc.finalize_bytes);
  }
  fmt::format_to(std::back_inserter(buf), "\n");
  sink.Write(std::string_view(buf.data(), buf.size()));
}

}  // namespace lyra::driver
