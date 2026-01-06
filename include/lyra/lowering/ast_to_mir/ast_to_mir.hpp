#pragma once

#include <memory>
#include <string>
#include <vector>

#include "lyra/mir/module.hpp"

namespace slang::ast {
class RootSymbol;
}

namespace lyra::lowering::ast_to_mir {

// Lowers AST to MIR Modules.
//
// If `top` is specified: returns modules in hierarchy starting from top.
//   (Currently returns just the top module; will traverse instantiations
//   later.)
// If `top` is empty: returns all top-level instances (for dump command).
//
// Throws DiagnosticException if top module not found.
auto AstToMir(const slang::ast::RootSymbol& root, const std::string& top)
    -> std::vector<std::unique_ptr<mir::Module>>;

}  // namespace lyra::lowering::ast_to_mir
