#pragma once

#include <cstdint>
#include <format>
#include <span>
#include <string>
#include <utility>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "lyra/common/edge_kind.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/origin_id.hpp"
#include "lyra/common/source_manager.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/mir/module_body.hpp"
#include "lyra/mir/routine.hpp"
#include "lyra/runtime/body_realization_desc.hpp"
#include "lyra/runtime/process_meta.hpp"
#include "lyra/runtime/wait_site.hpp"

namespace lyra::lowering::mir_to_llvm {

inline auto MapProcessKind(mir::ProcessKind kind) -> runtime::ProcessKind {
  switch (kind) {
    case mir::ProcessKind::kOnce:
      return runtime::ProcessKind::kInitial;
    case mir::ProcessKind::kFinal:
      return runtime::ProcessKind::kFinal;
    case mir::ProcessKind::kLooping:
      return runtime::ProcessKind::kAlways;
  }
  return runtime::ProcessKind::kAlways;
}

struct ResolvedLoc {
  std::string file;
  uint32_t line = 0;
  uint32_t col = 0;
};

inline auto ResolveProcessOrigin(
    common::OriginId origin, const lowering::DiagnosticContext* diag_ctx,
    const SourceManager* source_manager) -> ResolvedLoc {
  if (diag_ctx == nullptr || source_manager == nullptr || !origin.IsValid()) {
    return {};
  }

  auto span = diag_ctx->ResolveToSpan(origin);
  if (!span || !span->file_id) {
    return {};
  }

  const FileInfo* file = source_manager->GetFile(span->file_id);
  if (file == nullptr) {
    return {};
  }

  auto [line, col] =
      source_manager->OffsetToLineCol(span->file_id, span->begin);
  if (line == 0) {
    return {};
  }

  return {.file = file->path, .line = line, .col = col};
}

// Body-shaped stage-1 groupability classification.
// A process is groupable when it has static wait shape, all triggers have
// a uniform edge kind, and no trigger has sub-slot observation.
// Operates on template-level data: TriggerTemplateEntry flags carry
// kTriggerTemplateFlagHasObservedPlace for the observation check.
inline auto IsBodyGroupable(
    std::span<const runtime::TriggerTemplateEntry> entries,
    runtime::WaitShapeKind shape) -> bool {
  if (entries.empty()) return false;
  if (shape != runtime::WaitShapeKind::kStatic) return false;

  uint32_t uniform_edge = entries[0].edge;
  for (const auto& e : entries) {
    if (e.flags & runtime::kTriggerTemplateFlagHasObservedPlace) return false;
    if (e.edge != uniform_edge) return false;
  }
  return true;
}

// Canonical mapping between raw body.processes source order and the dense
// non-final ordinal used by all exported layout/codegen/runtime contracts.
// Build once per body; look up in O(1) in both directions.
struct BodyProcessOrdinalMap {
  std::vector<mir::ProcessId> nonfinal_processes;
  absl::flat_hash_map<mir::ProcessId, uint32_t> process_to_nonfinal_ordinal;
};

inline auto BuildBodyProcessOrdinalMap(const mir::ModuleBody& body)
    -> BodyProcessOrdinalMap {
  BodyProcessOrdinalMap map;
  for (mir::ProcessId pid : body.processes) {
    const auto& proc = body.arena[pid];
    if (proc.kind == mir::ProcessKind::kFinal) continue;
    auto ordinal = static_cast<uint32_t>(map.nonfinal_processes.size());
    map.nonfinal_processes.push_back(pid);
    auto [it, inserted] = map.process_to_nonfinal_ordinal.emplace(pid, ordinal);
    if (!inserted) {
      throw common::InternalError(
          "BuildBodyProcessOrdinalMap",
          std::format("duplicate non-final ProcessId {}", pid.value));
    }
  }
  if (map.nonfinal_processes.size() != map.process_to_nonfinal_ordinal.size()) {
    throw common::InternalError(
        "BuildBodyProcessOrdinalMap",
        std::format(
            "non-final process vector size {} != ordinal map size {}",
            map.nonfinal_processes.size(),
            map.process_to_nonfinal_ordinal.size()));
  }
  return map;
}

inline auto CountNonFinalProcesses(const mir::ModuleBody& body) -> uint32_t {
  uint32_t count = 0;
  for (mir::ProcessId pid : body.processes) {
    if (body.arena[pid].kind != mir::ProcessKind::kFinal) ++count;
  }
  return count;
}

inline auto GetProcessIdByNonFinalOrdinal(
    const BodyProcessOrdinalMap& map, uint32_t ordinal) -> mir::ProcessId {
  if (ordinal >= map.nonfinal_processes.size()) {
    throw common::InternalError(
        "GetProcessIdByNonFinalOrdinal",
        std::format(
            "ordinal {} >= nonfinal count {}", ordinal,
            map.nonfinal_processes.size()));
  }
  return map.nonfinal_processes[ordinal];
}

inline auto GetNonFinalOrdinal(
    const BodyProcessOrdinalMap& map, mir::ProcessId pid) -> uint32_t {
  auto it = map.process_to_nonfinal_ordinal.find(pid);
  if (it == map.process_to_nonfinal_ordinal.end()) {
    throw common::InternalError(
        "GetNonFinalOrdinal",
        std::format("ProcessId {} not in non-final ordinal map", pid.value));
  }
  return it->second;
}

// Iterate non-final processes in dense ordinal order, invoking fn with
// each process's canonical ordinal, ProcessId, and Process reference.
// fn signature: void(uint32_t nonfinal_ordinal, mir::ProcessId, const
// mir::Process&)
template <typename Fn>
void ForEachNonFinalProcess(
    const mir::ModuleBody& body, const BodyProcessOrdinalMap& map, Fn&& fn) {
  for (uint32_t i = 0; i < map.nonfinal_processes.size(); ++i) {
    mir::ProcessId pid = map.nonfinal_processes[i];
    std::forward<Fn>(fn)(i, pid, body.arena[pid]);
  }
}

}  // namespace lyra::lowering::mir_to_llvm
