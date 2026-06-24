#pragma once

#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

// Materializes "view `operand_id` as type `dst_type`" as a value-layer
// `CallExpr` to the matching `lyra::value` factory: integral-to-integral
// reshape calls `PackedArray::ConvertFrom`; the real-integral bridge nests
// `.ToInt64()` / `.Round()` inside a `Real` ctor or `PackedArray::FromInt`;
// packed bits and unpacked-byte arrays lift to `String::FromPackedArray` /
// `String::FromByteArray`; cross-precision real reshape calls the
// `RealValue<Other>` ctor; identical / same-shape inputs return the operand
// expression unchanged. The (source, destination) type pair fully drives the
// choice; this helper is the one place that makes it. Pointer reinterpret is
// a structurally different operation (`mir::CastExpr`, type-level view change
// only) and is materialized directly at its single producer, not through this
// helper.
//
// `block` is the destination scope for any intermediate `ExprId` the helper
// interns when the factory call nests an inner one.
[[nodiscard]] auto BuildValueConversion(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId operand_id,
    mir::TypeId dst_type) -> mir::Expr;

}  // namespace lyra::lowering::hir_to_mir
