#pragma once

#include <memory>

namespace slang::ast {
class Statement;
}  // namespace slang::ast

namespace lyra::mir {
class Statement;
}  // namespace lyra::mir

namespace lyra::lowering {

// Lowers a slang AST Statement into a list of MIR Statements.
auto LowerStatement(const slang::ast::Statement& statement)
    -> std::unique_ptr<mir::Statement>;

}  // namespace lyra::lowering
