#pragma once

#include "lir/value.hpp"

namespace lyra::mir {
class Expression;
}

namespace lyra::lowering {
class LirBuilder;

// Lowers a MIR Expression into LIR instructions and returns a value
// that holds the result.
auto LowerExpression(const mir::Expression& expression, LirBuilder& builder)
    -> lir::Value;

}  // namespace lyra::lowering
