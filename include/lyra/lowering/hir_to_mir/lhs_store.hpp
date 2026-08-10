#pragma once

#include <optional>

#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// Walks through container-access CallExprs and member-ref projections to the
// LHS chain's root primary. A projection whose own type is already a capability
// wrapper is a captured carrier (a closure-record field holding a `Ref`), which
// is itself the root: the walk stops there rather than descending through the
// field access as if the field were a struct member.
[[nodiscard]] auto FindLhsRootId(
    const mir::CompilationUnit& unit, const mir::Block& block,
    mir::ExprId lhs_id) -> mir::ExprId;

// Rebuilds the LHS chain over a different root, keeping every projection the
// chain applies. This is how a write reaches a destination the source did not
// name: a continuous assign spells its target as the net, but a net is only
// driven (LRM 6.5), so the same projections re-root onto the driver's own
// contribution.
[[nodiscard]] auto ReplaceLhsRoot(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId lhs_id,
    mir::ExprId root_id) -> mir::ExprId;

// The place holding the storage an LHS chain designates. A chain rooted in a
// capability wrapper names the wrapper rather than what it represents, so the
// chain re-roots on a dereference of that wrapper and its projections descend
// into the represented storage; a chain rooted anywhere else already names its
// storage and passes through unchanged. Storing into the result writes through
// the wrapper and handing it to a by-reference formal lends that storage, while
// the un-dereferenced chain keeps naming the wrapper itself -- which is what
// separates rebinding a reference from writing the place it refers to. Naming a
// net's own cell is a caller error: a net takes a value only through one of its
// drivers.
[[nodiscard]] auto StoragePlaceOf(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId lhs_id)
    -> mir::ExprId;

// Builds an `lhs op= rhs` (or simple `lhs = rhs`) against the storage the
// destination designates.
[[nodiscard]] auto BuildStoreExpr(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId lhs_id,
    mir::ExprId rhs_id, std::optional<mir::BinaryOp> compound_op,
    mir::TypeId result_type) -> mir::Expr;

}  // namespace lyra::lowering::hir_to_mir
