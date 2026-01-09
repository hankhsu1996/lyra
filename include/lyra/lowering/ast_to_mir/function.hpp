#pragma once

#include <slang/ast/symbols/SubroutineSymbols.h>

#include "lyra/mir/module.hpp"

namespace lyra::lowering::ast_to_mir {

/// Lower a function definition from slang AST to MIR
auto LowerFunction(const slang::ast::SubroutineSymbol& subroutine)
    -> mir::FunctionDefinition;

}  // namespace lyra::lowering::ast_to_mir
