#pragma once

#include <memory>

namespace slang::ast {
class Expression;
}

namespace lyra::mir {
class Expression;
}

namespace lyra::lowering {

// Lowers a slang AST Expression into a MIR Expression.
auto LowerExpression(const slang::ast::Expression& expression)
    -> std::unique_ptr<mir::Expression>;

// Helper to create an Identifier Expression from a signal name.
auto LowerExpressionFromName(const std::string& name)
    -> std::unique_ptr<mir::Expression>;

}  // namespace lyra::lowering
