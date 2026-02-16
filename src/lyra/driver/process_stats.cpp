#include "process_stats.hpp"

#include <algorithm>
#include <cstdint>
#include <format>
#include <string>
#include <unordered_map>
#include <variant>
#include <vector>

#include <fmt/core.h>

#include "lyra/common/overloaded.hpp"
#include "lyra/common/source_span.hpp"
#include "lyra/hir/routine.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/module.hpp"
#include "lyra/mir/routine.hpp"

namespace lyra::driver {

namespace {

auto HirProcessKindName(hir::ProcessKind kind) -> const char* {
  switch (kind) {
    case hir::ProcessKind::kInitial:
      return "initial";
    case hir::ProcessKind::kAlways:
      return "always";
    case hir::ProcessKind::kAlwaysComb:
      return "always_comb";
    case hir::ProcessKind::kAlwaysFf:
      return "always_ff";
    case hir::ProcessKind::kAlwaysLatch:
      return "always_latch";
    case hir::ProcessKind::kFinal:
      return "final";
  }
  return "unknown";
}

auto MirProcessKindName(mir::ProcessKind kind) -> const char* {
  switch (kind) {
    case mir::ProcessKind::kOnce:
      return "once";
    case mir::ProcessKind::kLooping:
      return "looping";
    case mir::ProcessKind::kFinal:
      return "final";
  }
  return "unknown";
}

// Resolve a MIR process origin to its HIR ProcessKind.
// Returns nullopt if origin is invalid or doesn't map to a HIR process.
auto ResolveHirKind(
    const mir::Process& process, const lowering::OriginMap& origin_map,
    const hir::Arena& hir_arena) -> std::optional<hir::ProcessKind> {
  if (process.origin == common::OriginId::Invalid()) {
    return std::nullopt;
  }
  auto entry = origin_map.Resolve(process.origin);
  if (!entry) {
    return std::nullopt;
  }
  auto* hir_proc_id = std::get_if<hir::ProcessId>(&entry->hir_source);
  if (hir_proc_id == nullptr) {
    return std::nullopt;
  }
  return hir_arena[*hir_proc_id].kind;
}

// Resolve source location for a MIR process.
auto ResolveSourceLocation(
    const mir::Process& process, const lowering::OriginMap& origin_map,
    const hir::Arena& hir_arena, const SourceManager& source_manager)
    -> std::string {
  if (process.origin == common::OriginId::Invalid()) {
    return "";
  }
  auto entry = origin_map.Resolve(process.origin);
  if (!entry) {
    return "";
  }
  return std::visit(
      common::Overloaded{
          [&](hir::ProcessId proc_id) -> std::string {
            return FormatSourceLocation(
                hir_arena[proc_id].span, source_manager);
          },
          [](const auto&) -> std::string { return ""; },
      },
      entry->hir_source);
}

struct ProcessEntry {
  uint32_t layout_index;  // Index in layout ordering (= process_N suffix)
  mir::ProcessId mir_id;
  mir::ProcessKind mir_kind;
  std::optional<hir::ProcessKind> hir_kind;
  std::string source_location;
  uint64_t llvm_insts = 0;
  uint64_t llvm_bbs = 0;
  uint32_t mir_stmts = 0;
  uint32_t mir_blocks = 0;
};

}  // namespace

void PrintProcessStats(
    const mir::Design& design, const mir::Arena& arena,
    const lowering::OriginMap& origin_map, const hir::Arena& hir_arena,
    const SourceManager& source_manager, const LlvmStats& llvm_stats,
    FILE* sink) {
  // Reconstruct process ordering (same as BuildLayout):
  // 1. init_processes
  // 2. connection_processes
  // 3. module processes (non-final)
  std::vector<mir::ProcessId> process_ids;

  for (mir::ProcessId pid : design.init_processes) {
    process_ids.push_back(pid);
  }
  size_t num_init = process_ids.size();

  for (mir::ProcessId pid : design.connection_processes) {
    process_ids.push_back(pid);
  }
  size_t num_connection = process_ids.size() - num_init;

  for (const auto& element : design.elements) {
    if (!std::holds_alternative<mir::Module>(element)) {
      continue;
    }
    const auto& mir_module = std::get<mir::Module>(element);
    for (mir::ProcessId pid : mir_module.processes) {
      if (arena[pid].kind != mir::ProcessKind::kFinal) {
        process_ids.push_back(pid);
      }
    }
  }
  size_t num_module = process_ids.size() - num_init - num_connection;

  // Build LLVM stats lookup: function name -> stats
  std::unordered_map<std::string, const LlvmFunctionStats*> llvm_lookup;
  for (const auto& fs : llvm_stats.func_stats) {
    llvm_lookup[fs.name] = &fs;
  }

  // Build process entries with all metadata
  std::vector<ProcessEntry> entries;
  entries.reserve(process_ids.size());

  // Kind counters (HIR-level for detailed breakdown)
  uint32_t count_initial = 0;
  uint32_t count_always = 0;
  uint32_t count_always_comb = 0;
  uint32_t count_always_ff = 0;
  uint32_t count_always_latch = 0;
  uint32_t count_final = 0;
  uint32_t count_connection = 0;  // Synthetic (no HIR origin)

  uint64_t total_proc_insts = 0;

  for (size_t i = 0; i < process_ids.size(); ++i) {
    const auto& process = arena[process_ids[i]];

    auto hir_kind = ResolveHirKind(process, origin_map, hir_arena);
    auto source_loc =
        ResolveSourceLocation(process, origin_map, hir_arena, source_manager);

    // Count MIR statements
    uint32_t mir_stmts = 0;
    for (const auto& block : process.blocks) {
      mir_stmts += static_cast<uint32_t>(block.statements.size());
    }

    // Look up LLVM function stats
    std::string func_name = std::format("process_{}", i);
    uint64_t llvm_insts = 0;
    uint64_t llvm_bbs = 0;
    auto it = llvm_lookup.find(func_name);
    if (it != llvm_lookup.end()) {
      llvm_insts = it->second->instructions;
      llvm_bbs = it->second->basic_blocks;
    }

    total_proc_insts += llvm_insts;

    // Count by kind
    if (i >= num_init && i < num_init + num_connection) {
      ++count_connection;
    } else if (hir_kind) {
      switch (*hir_kind) {
        case hir::ProcessKind::kInitial:
          ++count_initial;
          break;
        case hir::ProcessKind::kAlways:
          ++count_always;
          break;
        case hir::ProcessKind::kAlwaysComb:
          ++count_always_comb;
          break;
        case hir::ProcessKind::kAlwaysFf:
          ++count_always_ff;
          break;
        case hir::ProcessKind::kAlwaysLatch:
          ++count_always_latch;
          break;
        case hir::ProcessKind::kFinal:
          ++count_final;
          break;
      }
    }

    entries.push_back(
        ProcessEntry{
            .layout_index = static_cast<uint32_t>(i),
            .mir_id = process_ids[i],
            .mir_kind = process.kind,
            .hir_kind = hir_kind,
            .source_location = std::move(source_loc),
            .llvm_insts = llvm_insts,
            .llvm_bbs = llvm_bbs,
            .mir_stmts = mir_stmts,
            .mir_blocks = static_cast<uint32_t>(process.blocks.size()),
        });
  }

  // Print summary
  fmt::print(
      sink, "[lyra][stats][proc] total={} init={} connection={} module={}\n",
      process_ids.size(), num_init, num_connection, num_module);

  // Print kind breakdown
  fmt::print(
      sink,
      "[lyra][stats][proc] always_ff={} always_comb={} always={} "
      "always_latch={} initial={} final={} connection={}\n",
      count_always_ff, count_always_comb, count_always, count_always_latch,
      count_initial, count_final, count_connection);

  // Print process LLVM IR share
  if (llvm_stats.total_insts > 0) {
    double proc_percent = 100.0 * static_cast<double>(total_proc_insts) /
                          static_cast<double>(llvm_stats.total_insts);
    fmt::print(
        sink,
        "[lyra][stats][proc] proc_insts={} total_insts={} "
        "proc_share={:.1f}%\n",
        total_proc_insts, llvm_stats.total_insts, proc_percent);
  }

  // Sort by LLVM instruction count for top-N display
  std::vector<size_t> sorted_indices(entries.size());
  std::iota(sorted_indices.begin(), sorted_indices.end(), 0);
  std::sort(
      sorted_indices.begin(), sorted_indices.end(), [&](size_t a, size_t b) {
        return entries[a].llvm_insts > entries[b].llvm_insts;
      });

  // Print top 10 largest processes
  int top_n = std::min(10, static_cast<int>(entries.size()));
  for (int rank = 0; rank < top_n; ++rank) {
    const auto& e = entries[sorted_indices[rank]];
    std::string kind_str;
    if (e.layout_index >= num_init &&
        e.layout_index < num_init + num_connection) {
      kind_str = "connection";
    } else if (e.hir_kind) {
      kind_str = HirProcessKindName(*e.hir_kind);
    } else {
      kind_str = MirProcessKindName(e.mir_kind);
    }

    double percent = (llvm_stats.total_insts > 0)
                         ? 100.0 * static_cast<double>(e.llvm_insts) /
                               static_cast<double>(llvm_stats.total_insts)
                         : 0.0;

    fmt::print(
        sink,
        "[lyra][stats][proc][top] {}) process_{} kind={} "
        "llvm_insts={} llvm_bbs={} mir_stmts={} percent={:.1f}%",
        rank + 1, e.layout_index, kind_str, e.llvm_insts, e.llvm_bbs,
        e.mir_stmts, percent);

    if (!e.source_location.empty()) {
      fmt::print(sink, " src={}", e.source_location);
    }
    fmt::print(sink, "\n");
  }

  // Size distribution: median, p90, p99
  if (!entries.empty()) {
    std::vector<uint64_t> sizes;
    sizes.reserve(entries.size());
    for (const auto& e : entries) {
      sizes.push_back(e.llvm_insts);
    }
    std::sort(sizes.begin(), sizes.end());

    auto percentile = [&](double p) -> uint64_t {
      size_t idx = static_cast<size_t>(
          p / 100.0 * static_cast<double>(sizes.size() - 1));
      return sizes[idx];
    };

    fmt::print(
        sink,
        "[lyra][stats][proc] median_insts={} p90_insts={} p99_insts={} "
        "max_insts={}\n",
        percentile(50), percentile(90), percentile(99), sizes.back());
  }

  std::fflush(sink);
}

}  // namespace lyra::driver
