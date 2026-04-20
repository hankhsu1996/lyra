#pragma once

#include <optional>
#include <span>
#include <unordered_map>
#include <vector>

#include "lyra/common/origin_id.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/source_span.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/module_body.hpp"
#include "lyra/lowering/origin_map.hpp"
#include "lyra/mir/module_body.hpp"

namespace lyra::lowering {

// Pre-resolved per-body origin provenance for backend span resolution.
// Built at the lowering/backend boundary from body-local origin entries
// and their corresponding HIR arenas. The backend never needs the
// AST->HIR output directly -- this table provides everything needed to
// construct body-local origin resolvers. Keyed by MIR body pointer
// (in-memory identity valid for the lifetime of the owning mir::Design).
struct BodyOriginProvenance {
  struct Entry {
    std::span<const OriginEntry> origins;
    const hir::Arena* arena = nullptr;
  };
  std::unordered_map<const mir::ModuleBody*, Entry> by_body;

  // Look up the origin entry for a body. Returns nullptr if not found.
  [[nodiscard]] auto Find(const mir::ModuleBody* body) const -> const Entry* {
    auto it = by_body.find(body);
    return (it != by_body.end()) ? &it->second : nullptr;
  }
};

// Build provenance table from body-local origins, HIR bodies, and MIR
// module bodies. body_origins[i], hir_bodies[i], and mir_bodies[i] must
// correspond to the same specialization group.
inline auto BuildBodyOriginProvenance(
    const std::vector<std::vector<OriginEntry>>& body_origins,
    std::span<const hir::ModuleBody> hir_bodies,
    const std::vector<mir::ModuleBody>& mir_bodies) -> BodyOriginProvenance {
  BodyOriginProvenance result;
  result.by_body.reserve(body_origins.size());
  for (size_t i = 0; i < body_origins.size(); ++i) {
    const hir::Arena* arena =
        (i < hir_bodies.size()) ? &hir_bodies[i].arena : nullptr;
    if (i < mir_bodies.size()) {
      result.by_body[&mir_bodies[i]] = {
          .origins = body_origins[i], .arena = arena};
    }
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
