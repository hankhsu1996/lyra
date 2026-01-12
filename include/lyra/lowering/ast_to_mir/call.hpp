#pragma once

#include <memory>

namespace slang::ast {
class CallExpression;
}

namespace lyra::common {
class TypeArena;
}

namespace lyra::mir {
class Expression;
}

namespace lyra::lowering::ast_to_mir {

// Lowers a slang AST CallExpression into a MIR Expression.
// Handles enum methods, system calls, and user-defined function calls.
auto LowerCall(const slang::ast::CallExpression& call, common::TypeArena& arena)
    -> std::unique_ptr<mir::Expression>;

}  // namespace lyra::lowering::ast_to_mir
