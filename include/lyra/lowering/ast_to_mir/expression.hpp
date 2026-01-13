#pragma once

#include <memory>

namespace slang::ast {
class Expression;
}

namespace lyra::common {
class TypeArena;
}

namespace lyra::mir {
class Expression;
}

namespace lyra::lowering::ast_to_mir {

class SymbolRegistrar;

// Lowers a slang AST Expression into a MIR Expression.
auto LowerExpression(
    const slang::ast::Expression& expression, common::TypeArena& arena,
    SymbolRegistrar& registrar) -> std::unique_ptr<mir::Expression>;

}  // namespace lyra::lowering::ast_to_mir
