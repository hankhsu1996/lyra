#pragma once

#include <memory>

namespace slang::ast {
class ProceduralBlockSymbol;
}

namespace lyra::mir {
class Process;
}

namespace lyra::lowering {

// Lowers a slang AST ProceduralBlockSymbol into a MIR Process.
auto LowerProcess(const slang::ast::ProceduralBlockSymbol& procedural_block)
    -> std::unique_ptr<mir::Process>;

}  // namespace lyra::lowering
