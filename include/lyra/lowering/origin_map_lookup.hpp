#pragma once

#include <format>
#include <optional>

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

// Generic source-resolution adapter. Resolves OriginId to SourceSpan
// through body-aware arena lookup. Used by DiagnosticContext for error
// reporting.
//
// This class is a generic origin-to-span resolver. Domain-specific queries
// (e.g., resolving HIR process kind) belong in their consumers, not here.
class OriginMapLookup {
 public:
  OriginMapLookup(
      const OriginMap* origin_map, const hir::Design* design,
      const hir::Arena* global_arena)
      : origin_map_(origin_map), design_(design), global_arena_(global_arena) {
  }

  [[nodiscard]] auto ResolveToSpan(common::OriginId id) const
      -> std::optional<SourceSpan> {
    auto entry = origin_map_->Resolve(id);
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
            [&arena](FunctionParamRef ref) -> std::optional<SourceSpan> {
              return arena[ref.func].span;
            },
        },
        entry->hir_source);
  }

 private:
  const OriginMap* origin_map_;
  const hir::Design* design_;
  const hir::Arena* global_arena_;
};

}  // namespace lyra::lowering
