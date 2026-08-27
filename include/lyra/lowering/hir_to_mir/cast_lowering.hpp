#pragma once

#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

// Materializes "view `operand_id` as type `dst_type`" as a value-layer
// `CallExpr` to the matching `lyra::value` factory: integral-to-integral
// reshape calls `PackedArray::ConvertFrom`; the real-integral bridge nests
// `.ToInt64()` / `.Round()` inside a `Real` ctor or `PackedArray::FromInt`;
// packed bits and unpacked-byte arrays lift to `String::FromPackedArray` /
// `String::FromByteArray`; cross-precision real reshape calls the
// `RealValue<Other>` ctor; identical / same-shape inputs return the operand
// expression unchanged. The (source, destination) type pair fully drives the
// choice; this helper is the one place that makes it. Reinterpreting a
// reference, and converting a machine integer's width, are structurally
// different operations -- each has its own primitive and is materialized
// directly at its producer, not through this helper.
//
// `block` is the destination scope for any intermediate `ExprId` the helper
// interns when the factory call nests an inner one.
[[nodiscard]] auto BuildValueConversion(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId operand_id,
    mir::TypeId dst_type) -> mir::Expr;

// The conversion LRM 11.8.2 puts on a context-determined operand, which differs
// from every other context in what fixes the fill above the operand's width:
// the operand is converted to the propagated type first, so that type's
// signedness decides whether the widening replicates the sign bit, where an
// assignment's right-hand side is extended by its own signedness (LRM 11.8.3).
[[nodiscard]] auto BuildPropagatedConversion(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId operand_id,
    mir::TypeId dst_type) -> mir::Expr;

// The declared representation a packed value-layer call lands its result into,
// carried as an ordinary MIR value of that type -- a default literal -- so the
// representation reaches the runtime through the argument list, not composed by
// the backend from type payload. Its contents are ignored; only its type's
// declared shape matters. A select uses it to state the shape its result takes,
// which a receiver whose own dimensions differ (an aggregate's flat base) can
// never supply.
[[nodiscard]] auto BuildPackedShapePrototype(
    mir::Block& block, const mir::PackedArrayType& dst_pa, mir::TypeId dst_type)
    -> mir::ExprId;

// Returns `operand_id` already at `dst_type`: the operand unchanged when its
// type already matches, otherwise the converted expression interned into
// `block`. This is the single entry every semantic store routes its right-hand
// side through to reach the destination's full declared representation, for
// every value family -- the store boundary never decides per type whether a
// conversion applies.
[[nodiscard]] auto ConvertToType(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId operand_id,
    mir::TypeId dst_type) -> mir::ExprId;

}  // namespace lyra::lowering::hir_to_mir
