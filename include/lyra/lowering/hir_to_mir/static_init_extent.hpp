#pragma once

#include <vector>

#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

// Appends the call that enters or leaves the extent a static initializer draws
// inside (LRM 18.14.1): `bracket`, taking the engine handle and then `operands`
// -- the instance holding the seeds where a scope names one, nothing where a
// namespace has none. Every bracket entry takes the handle first, so it is
// composed here. `block` is where the entering call's own operands are interned
// too.
void EmitStaticInitBracket(
    const UnitLowerer& unit_lowerer, mir::Block& block,
    support::BuiltinFn bracket, std::vector<mir::ExprId> operands);

// Closes `extent` around `body`. `extent` already holds the call that named the
// container's generator; this puts the body inside it and gives the generator
// back however the body ends, a control effect passing through included.
[[nodiscard]] auto CloseStaticInitExtent(
    const UnitLowerer& unit_lowerer, mir::Block&& extent, mir::Block&& body)
    -> mir::Block;

}  // namespace lyra::lowering::hir_to_mir
