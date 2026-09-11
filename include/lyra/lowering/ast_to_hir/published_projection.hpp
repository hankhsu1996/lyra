#pragma once

#include <span>
#include <vector>

#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/published_target.hpp"
#include "lyra/hir/unit_signature.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace lyra::lowering::ast_to_hir {

// The same descent with every type answered out of this unit's pool, for a
// caller reading a path straight off another unit's signature, where the types
// index that signature's storage. A caller reading one out of the record this
// unit took from a signature needs none of this: taking the record is where
// those types were answered.
auto ImportPublishedPath(
    UnitLowerer& unit_lowerer, const hir::UnitSignature& signature,
    std::span<const hir::PublishedSelector> path)
    -> std::vector<hir::PublishedSelector>;

// The reference to the part a connection point stands for: `base`, which
// reaches the whole of the member the declaring unit published, with the
// descent that unit stated applied on top (LRM 23.2.2.2, 25.5.4). A point
// naming a whole declaration descends no steps, so what comes back is `base` as
// it stands. Every coordinate is the one the declaring unit wrote and resolves
// against the value it descends into, so nothing here synthesizes a storage
// position; every type on `path` is already this unit's.
auto ProjectPublishedPath(
    UnitLowerer& unit_lowerer, WalkFrame frame,
    std::span<const hir::PublishedSelector> path, hir::Expr base,
    diag::SourceSpan span) -> hir::Expr;

}  // namespace lyra::lowering::ast_to_hir
