#pragma once

#include <format>
#include <optional>
#include <span>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/origin_id.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/source_span.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/design.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/lowering/origin_map.hpp"

namespace lyra::lowering {

// Resolve the HIR arena for an origin's body_id.
// Body-local content (valid body_id) resolves from the body unit's arena.
// Design-global content (kInvalidModuleBodyId) resolves from the global arena.
// Throws InternalError if body_id is out of range.
//
// This is the single source of truth for body-local vs design-global HIR
// arena selection. All consumers that need to resolve HIR IDs from origin
// entries should use this helper, not reimplement the logic.
inline auto ResolveHirArena(
    const hir::Design& design, const hir::Arena& global_arena,
    hir::ModuleBodyId body_id) -> const hir::Arena& {
  if (body_id == hir::kInvalidModuleBodyId) {
    return global_arena;
  }
  if (body_id.value >= design.module_bodies.size()) {
    throw common::InternalError(
        "ResolveHirArena", std::format(
                               "body_id {} out of range (num_bodies={})",
                               body_id.value, design.module_bodies.size()));
  }
  return design.module_bodies[body_id.value].arena;
}

// Body-local origin resolver. Resolves OriginIds from a single body's
// origin entries. Satisfies the OriginLookup concept for DiagnosticContext.
// Used during per-body codegen and post-session metadata assembly where
// the active body is known statically.
class BodyLocalOriginResolver {
 public:
  BodyLocalOriginResolver(
      std::span<const OriginEntry> entries, const hir::Design* design,
      const hir::Arena* global_arena)
      : entries_(entries), design_(design), global_arena_(global_arena) {
  }

  [[nodiscard]] auto ResolveToSpan(common::OriginId id) const
      -> std::optional<SourceSpan> {
    if (id.value >= entries_.size()) return std::nullopt;
    const auto& entry = entries_[id.value];
    const hir::Arena& arena =
        ResolveHirArena(*design_, *global_arena_, entry.body_id);
    return std::visit(
        common::Overloaded{
            [&arena](hir::StatementId stmt_id) -> std::optional<SourceSpan> {
              return arena[stmt_id].span;
            },
            [&arena](hir::ExpressionId expr_id) -> std::optional<SourceSpan> {
              return arena[expr_id].span;
            },
            [&arena](hir::FunctionId func_id) -> std::optional<SourceSpan> {
              return arena[func_id].span;
            },
            [&arena](hir::ProcessId proc_id) -> std::optional<SourceSpan> {
              return arena[proc_id].span;
            },
            [&arena](hir::TaskId task_id) -> std::optional<SourceSpan> {
              return arena[task_id].span;
            },
            [&arena](FunctionParamRef ref) -> std::optional<SourceSpan> {
              return arena[ref.func].span;
            },
            [&arena](TaskParamRef ref) -> std::optional<SourceSpan> {
              return arena[ref.task].span;
            },
        },
        entry.hir_source);
  }

 private:
  std::span<const OriginEntry> entries_;
  const hir::Design* design_;
  const hir::Arena* global_arena_;
};

// Design-global origin resolver. Resolves OriginIds from the design-global
// origin map (package init processes, generated functions). Satisfies the
// OriginLookup concept for DiagnosticContext.
//
// Body-local origins are resolved separately via BodyLocalOriginResolver,
// which is installed per-session through Context::DiagnosticScope.
class OriginMapLookup {
 public:
  OriginMapLookup(
      const OriginMap* design_origins, const hir::Design* design,
      const hir::Arena* global_arena)
      : design_origins_(design_origins),
        design_(design),
        global_arena_(global_arena) {
  }

  [[nodiscard]] auto ResolveToSpan(common::OriginId id) const
      -> std::optional<SourceSpan> {
    auto entry = design_origins_->Resolve(id);
    if (!entry) {
      return std::nullopt;
    }

    const hir::Arena& arena =
        ResolveHirArena(*design_, *global_arena_, entry->body_id);

    return std::visit(
        common::Overloaded{
            [&arena](hir::StatementId stmt_id) -> std::optional<SourceSpan> {
              return arena[stmt_id].span;
            },
            [&arena](hir::ExpressionId expr_id) -> std::optional<SourceSpan> {
              return arena[expr_id].span;
            },
            [&arena](hir::FunctionId func_id) -> std::optional<SourceSpan> {
              return arena[func_id].span;
            },
            [&arena](hir::ProcessId proc_id) -> std::optional<SourceSpan> {
              return arena[proc_id].span;
            },
            [&arena](hir::TaskId task_id) -> std::optional<SourceSpan> {
              return arena[task_id].span;
            },
            [&arena](FunctionParamRef ref) -> std::optional<SourceSpan> {
              return arena[ref.func].span;
            },
            [&arena](TaskParamRef ref) -> std::optional<SourceSpan> {
              return arena[ref.task].span;
            },
        },
        entry->hir_source);
  }

 private:
  const OriginMap* design_origins_;
  const hir::Design* design_;
  const hir::Arena* global_arena_;
};

}  // namespace lyra::lowering
