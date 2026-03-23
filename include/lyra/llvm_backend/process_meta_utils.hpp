#pragma once

#include <cstdint>
#include <string>

#include "lyra/common/origin_id.hpp"
#include "lyra/common/source_manager.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/mir/routine.hpp"
#include "lyra/runtime/process_meta.hpp"

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

}  // namespace lyra::lowering::mir_to_llvm
