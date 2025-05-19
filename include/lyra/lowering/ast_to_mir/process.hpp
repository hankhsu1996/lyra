#pragma once

#include <memory>

#include "lyra/mir/process.hpp"

namespace slang::ast {
class ProceduralBlockSymbol;
}

namespace lyra::lowering::ast_to_mir {

// Lowers a slang AST ProceduralBlockSymbol into a MIR Process.
auto LowerProcess(const slang::ast::ProceduralBlockSymbol& procedural_block)
    -> std::unique_ptr<mir::Process>;

}  // namespace lyra::lowering::ast_to_mir
