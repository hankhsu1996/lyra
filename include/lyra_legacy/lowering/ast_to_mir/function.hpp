#pragma once

#include <slang/ast/symbols/SubroutineSymbols.h>

#include "lyra/mir/module.hpp"

namespace lyra::common {
class TypeArena;
}

namespace lyra::lowering::ast_to_mir {

class SymbolRegistrar;

/// Lower a function definition from slang AST to MIR
auto LowerFunction(
    const slang::ast::SubroutineSymbol& subroutine, common::TypeArena& arena,
    SymbolRegistrar& registrar) -> mir::FunctionDefinition;

}  // namespace lyra::lowering::ast_to_mir
