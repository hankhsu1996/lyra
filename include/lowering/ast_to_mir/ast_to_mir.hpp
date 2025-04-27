#pragma once

#include <memory>

namespace slang::ast {
class Compilation;
}

namespace lyra::mir {
class Module;
}

namespace lyra::lowering {

// Lowers a slang AST Compilation into a MIR Module.
auto AstToMir(slang::ast::Compilation& compilation)
    -> std::shared_ptr<mir::Module>;

}  // namespace lyra::lowering
