#include "process_stats.hpp"

#include <algorithm>
#include <cstdint>
#include <format>
#include <optional>
#include <string>
#include <unordered_map>
#include <variant>
#include <vector>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/source_span.hpp"
#include "lyra/hir/routine.hpp"
#include "lyra/lowering/origin_map_lookup.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/design.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/module.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/routine.hpp"
#include "lyra/mir/statement.hpp"
#include "lyra/mir/terminator.hpp"

namespace lyra::driver {

namespace {

auto ResolveHirKind(
    const mir::Process& process, const lowering::OriginMap& origin_map,
    const hir::Design& hir_design, const hir::Arena& global_hir_arena)
    -> std::optional<hir::ProcessKind> {
  if (process.origin == common::OriginId::Invalid()) {
    return std::nullopt;
  }
  auto entry = origin_map.Resolve(process.origin);
  if (!entry) {
    return std::nullopt;
  }
  auto* proc_id = std::get_if<hir::ProcessId>(&entry->hir_source);
  if (proc_id == nullptr) {
    return std::nullopt;
  }
  const hir::Arena& arena =
      lowering::ResolveHirArena(hir_design, global_hir_arena, entry->body_id);
  return arena[*proc_id].kind;
}

auto ResolveSourceLocation(
    const mir::Process& process, const lowering::OriginMap& origin_map,
    const hir::Design& hir_design, const hir::Arena& global_hir_arena,
    const SourceManager& source_manager) -> std::string {
  if (process.origin == common::OriginId::Invalid()) {
    return "";
  }
  auto entry = origin_map.Resolve(process.origin);
  if (!entry) {
    return "";
  }
  const hir::Arena& arena =
      lowering::ResolveHirArena(hir_design, global_hir_arena, entry->body_id);
  return std::visit(
      common::Overloaded{
          [&](hir::ProcessId proc_id) -> std::string {
            return FormatSourceLocation(arena[proc_id].span, source_manager);
          },
          [](const auto&) -> std::string { return ""; },
      },
      entry->hir_source);
}

auto ClassifyProcessKind(
    const mir::Process& process, std::optional<hir::ProcessKind> hir_kind,
    bool is_connection) -> ProcessStatsKind {
  if (is_connection) {
    return ProcessStatsKind::kConnection;
  }
  if (hir_kind) {
    switch (*hir_kind) {
      case hir::ProcessKind::kInitial:
        return ProcessStatsKind::kInitial;
      case hir::ProcessKind::kAlways:
        return ProcessStatsKind::kAlways;
      case hir::ProcessKind::kAlwaysComb:
        return ProcessStatsKind::kAlwaysComb;
      case hir::ProcessKind::kAlwaysFf:
        return ProcessStatsKind::kAlwaysFf;
      case hir::ProcessKind::kAlwaysLatch:
        return ProcessStatsKind::kAlwaysLatch;
      case hir::ProcessKind::kFinal:
        return ProcessStatsKind::kFinal;
    }
  }
  switch (process.kind) {
    case mir::ProcessKind::kOnce:
      return ProcessStatsKind::kMirOnce;
    case mir::ProcessKind::kLooping:
      return ProcessStatsKind::kMirLooping;
    case mir::ProcessKind::kFinal:
      return ProcessStatsKind::kMirFinal;
  }
  throw common::InternalError("ClassifyProcessKind", "invalid process kind");
}

}  // namespace

auto CollectProcessStats(
    const mir::Design& design, const mir::Arena& design_arena,
    const lowering::OriginMap& design_origins, const hir::Design& hir_design,
    const hir::Arena& global_hir_arena, const SourceManager& source_manager,
    const LlvmStats& llvm_stats) -> ProcessStatsData {
  struct ProcessRef {
    mir::ProcessId id;
    const mir::Arena* arena;
  };
  std::vector<ProcessRef> process_refs;

  for (mir::ProcessId pid : design.init_processes) {
    process_refs.push_back({pid, &design_arena});
  }
  size_t num_init = process_refs.size();

  // Connection processes are no longer in design MIR (resolved to
  // endpoint-based bindings). Report kernel binding count directly.
  size_t num_kernelized = 0;
  size_t num_connection = 0;

  for (const auto& element : design.elements) {
    if (!std::holds_alternative<mir::Module>(element)) {
      continue;
    }
    const auto& mir_module = std::get<mir::Module>(element);
    const auto& body = mir::GetModuleBody(design, mir_module);
    for (mir::ProcessId pid : body.processes) {
      if (body.arena[pid].kind != mir::ProcessKind::kFinal) {
        process_refs.push_back({pid, &body.arena});
      }
    }
  }
  size_t num_module =
      process_refs.size() - num_init - (num_connection - num_kernelized);

  std::unordered_map<std::string, const LlvmFunctionStats*> llvm_lookup;
  for (const auto& fs : llvm_stats.func_stats) {
    llvm_lookup[fs.name] = &fs;
  }

  ProcessStatsData result;
  result.num_init = num_init;
  result.num_connection = num_connection;
  result.num_kernelized = num_kernelized;
  result.num_module = num_module;
  result.total_llvm_insts = llvm_stats.total_insts;
  result.entries.reserve(process_refs.size());

  size_t num_non_kernelized_conn = num_connection - num_kernelized;

  for (size_t i = 0; i < process_refs.size(); ++i) {
    const auto& process = (*process_refs[i].arena)[process_refs[i].id];

    auto hir_kind =
        ResolveHirKind(process, design_origins, hir_design, global_hir_arena);
    auto source_loc = ResolveSourceLocation(
        process, design_origins, hir_design, global_hir_arena, source_manager);

    uint32_t mir_stmts = 0;
    for (const auto& block : process.blocks) {
      mir_stmts += static_cast<uint32_t>(block.statements.size());
    }

    std::string func_name = std::format("process_{}", i);
    uint64_t llvm_insts = 0;
    uint64_t llvm_bbs = 0;
    auto it = llvm_lookup.find(func_name);
    if (it != llvm_lookup.end()) {
      llvm_insts = it->second->instructions;
      llvm_bbs = it->second->basic_blocks;
    }

    result.total_proc_insts += llvm_insts;

    bool is_connection =
        i >= num_init && i < num_init + num_non_kernelized_conn;
    auto kind = ClassifyProcessKind(process, hir_kind, is_connection);

    switch (kind) {
      case ProcessStatsKind::kInitial:
        ++result.count_initial;
        break;
      case ProcessStatsKind::kAlways:
        ++result.count_always;
        break;
      case ProcessStatsKind::kAlwaysComb:
        ++result.count_always_comb;
        break;
      case ProcessStatsKind::kAlwaysFf:
        ++result.count_always_ff;
        break;
      case ProcessStatsKind::kAlwaysLatch:
        ++result.count_always_latch;
        break;
      case ProcessStatsKind::kFinal:
        ++result.count_final;
        break;
      case ProcessStatsKind::kConnection:
        ++result.count_connection;
        break;
      default:
        break;
    }

    result.entries.push_back(
        ProcessStatsEntry{
            .layout_index = static_cast<uint32_t>(i),
            .kind = kind,
            .storage_class = ProcessStatsStorageClass::kNonSharedBody,
            .source_location = std::move(source_loc),
            .llvm_insts = llvm_insts,
            .llvm_bbs = llvm_bbs,
            .mir_stmts = mir_stmts,
        });
  }

  result.count_connection += static_cast<uint32_t>(num_kernelized);

  // Template summary and storage class classification.
  for (const auto& fs : llvm_stats.func_stats) {
    if (fs.name.starts_with("proc_template_")) {
      ++result.template_groups;
      result.shared_ir_insts += fs.instructions;
    }
  }
  for (size_t idx = 0; idx < result.entries.size(); ++idx) {
    auto& e = result.entries[idx];
    if (e.llvm_insts > 0 && e.llvm_insts <= 10) {
      e.storage_class = ProcessStatsStorageClass::kWrapper;
      ++result.wrapper_count;
    } else if (e.llvm_insts > 10 && idx >= result.num_init) {
      e.storage_class = ProcessStatsStorageClass::kNonSharedBody;
      ++result.nonshared_count;
    }
  }

  // Size distribution.
  if (!result.entries.empty()) {
    std::vector<uint64_t> sizes;
    sizes.reserve(result.entries.size());
    for (const auto& e : result.entries) {
      sizes.push_back(e.llvm_insts);
    }
    std::sort(sizes.begin(), sizes.end());

    auto percentile = [&](double p) -> uint64_t {
      auto idx = static_cast<size_t>(
          p / 100.0 * static_cast<double>(sizes.size() - 1));
      return sizes[idx];
    };

    result.median_insts = percentile(50);
    result.p90_insts = percentile(90);
    result.p99_insts = percentile(99);
    result.max_insts = sizes.back();
  }

  return result;
}

}  // namespace lyra::driver
