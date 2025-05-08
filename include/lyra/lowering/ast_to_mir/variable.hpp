#pragma once

#include <optional>

#include "lyra/common/variable.hpp"

namespace lyra::mir {
class Variable;
}

namespace lyra::lowering {

// Lowers a slang AST Symbol (specifically VariableSymbol) into a MIR Variable.
// Returns nullptr if the symbol is not a variable.
auto LowerVariable(const slang::ast::Symbol& symbol)
    -> std::optional<common::Variable>;

}  // namespace lyra::lowering
