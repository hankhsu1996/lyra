#pragma once

#include <optional>
#include <span>
#include <vector>

#include "lyra/common/origin_id.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/source_span.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/design.hpp"
#include "lyra/lowering/origin_map.hpp"

namespace lyra::lowering {

// Pre-resolved per-body origin provenance for backend span resolution.
// Built at the lowering/backend boundary from body-local origin entries
// and their corresponding HIR arenas. The backend never needs raw
// hir::Design -- this table provides everything needed to construct
// body-local origin resolvers.
struct BodyOriginProvenance {
  struct Entry {
    std::span<const OriginEntry> origins;
    const hir::Arena* arena = nullptr;
  };
  std::vector<Entry> bodies;  // indexed by ModuleBodyId::value
};

// Build provenance table from body-local origins and HIR design.
// Each body entry pairs its origin vector with the corresponding
// HIR body arena for direct span resolution.
inline auto BuildBodyOriginProvenance(
    const std::vector<std::vector<OriginEntry>>& body_origins,
    const hir::Design& design) -> BodyOriginProvenance {
  BodyOriginProvenance result;
  result.bodies.reserve(body_origins.size());
  for (size_t i = 0; i < body_origins.size(); ++i) {
    const hir::Arena* arena = (i < design.module_bodies.size())
                                  ? &design.module_bodies[i].arena
                                  : nullptr;
    result.bodies.push_back({.origins = body_origins[i], .arena = arena});
  }
  return result;
}

// Body-local origin resolver. Resolves OriginIds from a single body's
// origin entries against that body's HIR arena. Satisfies the OriginLookup
// concept for DiagnosticContext.
class BodyLocalOriginResolver {
 public:
  BodyLocalOriginResolver(
      std::span<const OriginEntry> entries, const hir::Arena& arena)
      : entries_(entries), arena_(&arena) {
  }

  [[nodiscard]] auto ResolveToSpan(common::OriginId id) const
      -> std::optional<SourceSpan> {
    if (id.value >= entries_.size()) return std::nullopt;
    const auto& entry = entries_[id.value];
    return std::visit(
        common::Overloaded{
            [this](hir::StatementId stmt_id) -> std::optional<SourceSpan> {
              return (*arena_)[stmt_id].span;
            },
            [this](hir::ExpressionId expr_id) -> std::optional<SourceSpan> {
              return (*arena_)[expr_id].span;
            },
            [this](hir::FunctionId func_id) -> std::optional<SourceSpan> {
              return (*arena_)[func_id].span;
            },
            [this](hir::ProcessId proc_id) -> std::optional<SourceSpan> {
              return (*arena_)[proc_id].span;
            },
            [this](hir::TaskId task_id) -> std::optional<SourceSpan> {
              return (*arena_)[task_id].span;
            },
            [this](FunctionParamRef ref) -> std::optional<SourceSpan> {
              return (*arena_)[ref.func].span;
            },
            [this](TaskParamRef ref) -> std::optional<SourceSpan> {
              return (*arena_)[ref.task].span;
            },
        },
        entry.hir_source);
  }

 private:
  std::span<const OriginEntry> entries_;
  const hir::Arena* arena_;
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
      const OriginMap* design_origins, const hir::Arena* global_arena)
      : design_origins_(design_origins), global_arena_(global_arena) {
  }

  [[nodiscard]] auto ResolveToSpan(common::OriginId id) const
      -> std::optional<SourceSpan> {
    auto entry = design_origins_->Resolve(id);
    if (!entry) {
      return std::nullopt;
    }

    return std::visit(
        common::Overloaded{
            [this](hir::StatementId stmt_id) -> std::optional<SourceSpan> {
              return (*global_arena_)[stmt_id].span;
            },
            [this](hir::ExpressionId expr_id) -> std::optional<SourceSpan> {
              return (*global_arena_)[expr_id].span;
            },
            [this](hir::FunctionId func_id) -> std::optional<SourceSpan> {
              return (*global_arena_)[func_id].span;
            },
            [this](hir::ProcessId proc_id) -> std::optional<SourceSpan> {
              return (*global_arena_)[proc_id].span;
            },
            [this](hir::TaskId task_id) -> std::optional<SourceSpan> {
              return (*global_arena_)[task_id].span;
            },
            [this](FunctionParamRef ref) -> std::optional<SourceSpan> {
              return (*global_arena_)[ref.func].span;
            },
            [this](TaskParamRef ref) -> std::optional<SourceSpan> {
              return (*global_arena_)[ref.task].span;
            },
        },
        entry->hir_source);
  }

 private:
  const OriginMap* design_origins_;
  const hir::Arena* global_arena_;
};

}  // namespace lyra::lowering
