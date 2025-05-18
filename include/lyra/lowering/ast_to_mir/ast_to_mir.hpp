#pragma once

#include <memory>

#include "lyra/mir/module.hpp"

namespace slang::ast {
class RootSymbol;
}

namespace lyra::lowering::ast_to_mir {

// Lowers a slang AST Compilation into a MIR Module.
auto AstToMir(const slang::ast::RootSymbol& root)
    -> std::unique_ptr<mir::Module>;

}  // namespace lyra::lowering::ast_to_mir
