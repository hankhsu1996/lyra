#pragma once

#include <cstdint>
#include <span>
#include <string>

#include "lyra/common/edge_kind.hpp"
#include "lyra/common/origin_id.hpp"
#include "lyra/common/source_manager.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
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

}  // namespace lyra::lowering::mir_to_llvm
