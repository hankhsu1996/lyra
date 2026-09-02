#pragma once

#include <cstdint>

#include "lyra/mir/compilation_unit.hpp"
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

// An integral constant, as the value-layer call that builds it. A packed value
// is a runtime object with no literal form of its own, so what stands for one
// in MIR is the factory that produces it: `PackedArray::FromInt` where the bits
// fit a machine integer carrier, `PackedArray::FromWords` where they do not --
// a value past 64 bits, or one bearing X or Z. Which factory a constant needs
// follows from its own bits, so this is the one place that reads them; every
// consumer downstream sees an ordinary call.
//
// The result's declared representation reaches the factory as a shape operand,
// so the same call serves any width, signedness, state domain, and rank.
[[nodiscard]] auto BuildIntegralLiteral(
    const mir::CompilationUnit& unit, mir::Block& block, mir::TypeId type,
    const mir::IntegralConstant& value) -> mir::ExprId;

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
