#pragma once

#include <memory>
#include <string>

namespace slang::ast {
class InstanceSymbol;
}  // namespace slang::ast

namespace lyra::common {
class TypeArena;
}

namespace lyra::mir {
class Module;
}

namespace lyra::lowering::ast_to_mir {

class SymbolRegistrar;

// Computes a unique signature for a module instance.
// Format: "ModuleName" for non-parameterized, "ModuleName<val1,val2>" for
// parameterized. Used for deduplication during lowering and linking.
auto ComputeModuleSignature(const slang::ast::InstanceSymbol& instance)
    -> std::string;

// Lowers a slang AST InstanceSymbol into a MIR Module.
auto LowerModule(
    const slang::ast::InstanceSymbol& instance_symbol, common::TypeArena& arena,
    SymbolRegistrar& registrar) -> std::unique_ptr<mir::Module>;

}  // namespace lyra::lowering::ast_to_mir
