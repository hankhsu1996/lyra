#pragma once

#include <format>
#include <optional>
#include <vector>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/origin_id.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/source_span.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/design.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/lowering/origin_map.hpp"
#include "lyra/mir/handle.hpp"

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

// Generic source-resolution adapter. Resolves OriginId to SourceSpan
// through body-aware arena lookup. Used by DiagnosticContext for error
// reporting.
//
// Supports two resolution scopes:
// - Design-global: resolves from design_origins (for package/init MIR)
// - Body-local: resolves from per-body origin entries (for body MIR)
//
// Use BodyScope guard for per-body resolution scope.
class OriginMapLookup {
 public:
  OriginMapLookup(
      const OriginMap* design_origins,
      const std::vector<std::vector<OriginEntry>>* body_origins,
      const hir::Design* design, const hir::Arena* global_arena)
      : design_origins_(design_origins),
        body_origins_(body_origins),
        design_(design),
        global_arena_(global_arena) {
  }

  // Scoped body resolution guard: sets body scope on construction,
  // restores previous scope on destruction.
  class BodyScope {
   public:
    BodyScope(OriginMapLookup& lookup, mir::ModuleBodyId body_id)
        : lookup_(lookup), saved_(lookup.current_body_id_) {
      lookup_.current_body_id_ = body_id.value;
    }
    ~BodyScope() {
      lookup_.current_body_id_ = saved_;
    }
    BodyScope(const BodyScope&) = delete;
    auto operator=(const BodyScope&) -> BodyScope& = delete;

   private:
    OriginMapLookup& lookup_;
    std::optional<uint32_t> saved_;
  };

  [[nodiscard]] auto ResolveToSpan(common::OriginId id) const
      -> std::optional<SourceSpan> {
    std::optional<OriginEntry> entry;
    if (current_body_id_.has_value()) {
      uint32_t body_idx = *current_body_id_;
      if (body_origins_ != nullptr && body_idx < body_origins_->size() &&
          id.value < (*body_origins_)[body_idx].size()) {
        entry = (*body_origins_)[body_idx][id.value];
      }
    } else {
      entry = design_origins_->Resolve(id);
    }

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
  const std::vector<std::vector<OriginEntry>>* body_origins_;
  const hir::Design* design_;
  const hir::Arena* global_arena_;
  std::optional<uint32_t> current_body_id_;
};

}  // namespace lyra::lowering
