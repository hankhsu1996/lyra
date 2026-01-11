#pragma once

#include <memory>
#include <string>

namespace slang::ast {
class InstanceSymbol;
}  // namespace slang::ast

namespace lyra::mir {
class Module;
}

namespace lyra::lowering::ast_to_mir {

// Computes a unique signature for a module instance.
// Format: "ModuleName" for non-parameterized, "ModuleName<val1,val2>" for
// parameterized. Used for deduplication during lowering and linking.
auto ComputeModuleSignature(const slang::ast::InstanceSymbol& instance)
    -> std::string;

// Lowers a slang AST InstanceSymbol into a MIR Module.
auto LowerModule(const slang::ast::InstanceSymbol& instance_symbol)
    -> std::unique_ptr<mir::Module>;

}  // namespace lyra::lowering::ast_to_mir
