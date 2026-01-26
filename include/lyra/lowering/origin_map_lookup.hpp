#pragma once

#include <optional>

#include "lyra/common/origin_id.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/source_span.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/lowering/origin_map.hpp"

namespace lyra::lowering {

// Adapter that implements OriginLookup by resolving through OriginMap and
// HIR Arena. Used by DiagnosticContext to resolve OriginId to SourceSpan.
class OriginMapLookup {
 public:
  OriginMapLookup(const OriginMap* origin_map, const hir::Arena* hir_arena)
      : origin_map_(origin_map), hir_arena_(hir_arena) {
  }

  [[nodiscard]] auto ResolveToSpan(common::OriginId id) const
      -> std::optional<SourceSpan> {
    auto entry = origin_map_->Resolve(id);
    if (!entry) {
      return std::nullopt;
    }

    return std::visit(
        common::Overloaded{
            [this](hir::StatementId stmt_id) -> std::optional<SourceSpan> {
              return (*hir_arena_)[stmt_id].span;
            },
            [this](hir::ExpressionId expr_id) -> std::optional<SourceSpan> {
              return (*hir_arena_)[expr_id].span;
            },
            [this](hir::FunctionId func_id) -> std::optional<SourceSpan> {
              return (*hir_arena_)[func_id].span;
            },
            [this](hir::ProcessId proc_id) -> std::optional<SourceSpan> {
              return (*hir_arena_)[proc_id].span;
            },
            [this](FunctionParamRef ref) -> std::optional<SourceSpan> {
              // Use function span for parameter errors (parameters don't have
              // individual spans in our representation yet).
              return (*hir_arena_)[ref.func].span;
            },
        },
        entry->hir_source);
  }

 private:
  const OriginMap* origin_map_;
  const hir::Arena* hir_arena_;
};

}  // namespace lyra::lowering
