#pragma once

#include <cstddef>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace slang::ast {
class Expression;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

class UnitLowerer;

// How the variables an expression reads answer for its sampled value: how
// many are cells this scope arms, and how many answer with what they hold now.
// An expression counting neither reads only constants.
struct SampledReads {
  std::size_t armed = 0;
  std::size_t current = 0;
};

// Arms every cell `expr` reads, so each answers for its sampled value (LRM
// 16.5.1). The sampled value of an expression is composed from the sampled
// values of the variables it reads, so those variables are exactly what has to
// answer, and they are the same set an event control watches, taken from the
// same analysis. A sampled value function's operand and a Boolean of a
// property are both such expressions. An automatic variable, and a `ref` formal
// that is not `ref static`, answers with its current value and needs nothing
// armed; a constant is its own sampled value and is neither armed nor counted.
//
// The set only ever has to cover them: a cell armed that nothing samples holds
// a value nobody reads, while one left unarmed cannot answer at all. So a
// coarser read set is safe here in a way it is not for a wake-up.
auto ArmSampledReads(
    UnitLowerer& unit_lowerer, const WalkFrame& frame,
    const slang::ast::Expression& expr, diag::SourceSpan span)
    -> diag::Result<SampledReads>;

}  // namespace lyra::lowering::ast_to_hir
