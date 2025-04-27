#pragma once

#include <optional>

namespace slang::ast {
class Symbol;
}

namespace lyra::mir {
class Variable;
}

namespace lyra::lowering {

// Lowers a slang AST Symbol (specifically VariableSymbol) into a MIR Variable.
// Returns nullptr if the symbol is not a variable.
auto LowerVariable(const slang::ast::Symbol& symbol)
    -> std::optional<mir::Variable>;

}  // namespace lyra::lowering
