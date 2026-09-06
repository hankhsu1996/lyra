#pragma once

#include <span>

#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/published_target.hpp"
#include "lyra/hir/unit_signature.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace lyra::lowering::ast_to_hir {

// The reference to the part a port stands for: `base`, which reaches the whole
// of the member the declaring unit published, with the descent that unit stated
// applied on top (LRM 23.2.2.2). A port naming a whole declaration descends no
// steps, so what comes back is `base` as it stands. Every coordinate is the one
// the declaring unit wrote and resolves against the value it descends into, so
// nothing here synthesizes a storage position; each step's type is answered out
// of this unit's pool, since the path is read off `signature`.
auto ProjectPublishedPath(
    UnitLowerer& unit_lowerer, WalkFrame frame,
    const hir::UnitSignature& signature,
    std::span<const hir::PublishedSelector> path, hir::Expr base,
    diag::SourceSpan span) -> hir::Expr;

}  // namespace lyra::lowering::ast_to_hir
