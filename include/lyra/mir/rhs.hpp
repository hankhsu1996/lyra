#pragma once

#include <variant>

#include "lyra/mir/operand.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::mir {

// RightHandSide: unified source for all assignment-like statements.
// Represents either a simple value (Operand) or a computed expression (Rvalue).
using RightHandSide = std::variant<Operand, Rvalue>;

}  // namespace lyra::mir
