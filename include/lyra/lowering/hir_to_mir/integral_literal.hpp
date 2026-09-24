#pragma once

#include <cstdint>
#include <optional>

#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// A machine integer, written as MIR's own literal. This is the domain a
// runtime entry's count, size, and index operands are in: they are not
// SystemVerilog values, so they carry no declared type and need no factory to
// produce them.
[[nodiscard]] auto BuildMachineIntLiteral(
    const mir::CompilationUnit& unit, mir::Block& block, std::int64_t value)
    -> mir::ExprId;

// The bits of `value` in the form a value of `shape` holds them: one word per
// 64 bits of its width with the bits above it cleared, and no unknown plane
// unless the shape has one. Two spellings of one value come out as the same
// words, and a consumer never reads bits the type does not have.
[[nodiscard]] auto CanonicalIntegralConstant(
    const mir::PackedArrayType& shape, const mir::IntegralConstant& value)
    -> mir::IntegralConstant;

// An integral constant at `type`: the unit holds it once, among the constants
// it was written with, and an occurrence names that entry. The bits are brought
// to the canonical form the pool keys on first, so two spellings of one value
// are one entry.
[[nodiscard]] auto MakeIntegralLiteral(
    const mir::CompilationUnit& unit, mir::TypeId type,
    const mir::IntegralConstant& value) -> mir::Expr;

// The same, added to `block`.
[[nodiscard]] auto BuildIntegralLiteral(
    const mir::CompilationUnit& unit, mir::Block& block, mir::TypeId type,
    const mir::IntegralConstant& value) -> mir::ExprId;

// What a builder states for an operation it has built: the constant the
// operation folds to, where every operand was one, and the operation itself
// otherwise.
[[nodiscard]] auto FoldedOr(
    const mir::CompilationUnit& unit,
    const std::optional<mir::IntegralConstant>& folded, mir::Expr unfolded)
    -> mir::Expr;

// 2-state signed 32-bit constant, typed `int` (LRM 6.11.1).
[[nodiscard]] auto BuildIntLiteral(
    const mir::CompilationUnit& unit, mir::Block& block, std::int64_t value)
    -> mir::ExprId;

// 4-state signed 32-bit constant, typed `integer` (LRM 6.11.1). Used by sites
// that compare against the matched-count return of `$sscanf` / `$fscanf` --
// those return `integer`, so the operand on the other side must match
// state-kind.
[[nodiscard]] auto BuildIntegerLiteral(
    const mir::CompilationUnit& unit, mir::Block& block, std::int64_t value)
    -> mir::ExprId;

// 1-bit unsigned 2-state constant. The value a boolean fold yields when it has
// nothing to fold, and the constant a synthesized flag is seeded with.
[[nodiscard]] auto BuildBit1Literal(
    const mir::CompilationUnit& unit, mir::Block& block, bool value)
    -> mir::ExprId;

}  // namespace lyra::lowering::hir_to_mir
