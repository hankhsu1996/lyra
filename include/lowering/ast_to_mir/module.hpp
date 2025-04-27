#pragma once

#include <memory>

namespace slang::ast {
class InstanceSymbol;
}

namespace lyra::mir {
class Module;
}

namespace lyra::lowering {

// Lowers a slang AST InstanceSymbol into a MIR Module.
auto LowerModule(const slang::ast::InstanceSymbol& instance_symbol)
    -> std::shared_ptr<mir::Module>;

}  // namespace lyra::lowering
