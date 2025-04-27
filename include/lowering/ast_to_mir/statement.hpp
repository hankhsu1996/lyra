#pragma once

#include <memory>
#include <vector>

namespace slang::ast {
class Statement;
}

namespace lyra::mir {
class Statement;
}

namespace lyra::lowering {

// Lowers a slang AST Statement into a list of MIR Statements.
auto LowerStatement(const slang::ast::Statement& statement)
    -> std::vector<std::shared_ptr<mir::Statement>>;

}  // namespace lyra::lowering
