#pragma once

#include <memory>

namespace slang::ast {
class InstanceSymbol;
}

namespace lyra::mir {
class Module;
}

namespace lyra::lowering::ast_to_mir {

// Lowers a slang AST InstanceSymbol into a MIR Module.
auto LowerModule(const slang::ast::InstanceSymbol& instance_symbol)
    -> std::unique_ptr<mir::Module>;

}  // namespace lyra::lowering::ast_to_mir
