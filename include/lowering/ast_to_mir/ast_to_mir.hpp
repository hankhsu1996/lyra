#pragma once

#include <memory>

namespace slang::ast {
class RootSymbol;
}

namespace lyra::mir {
class Module;
}

namespace lyra::lowering {

// Lowers a slang AST Compilation into a MIR Module.
auto AstToMir(const slang::ast::RootSymbol& root)
    -> std::unique_ptr<mir::Module>;

}  // namespace lyra::lowering
