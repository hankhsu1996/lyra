#pragma once

#include <memory>

namespace slang::ast {
class AssignmentExpression;
}

namespace lyra::common {
class TypeArena;
}

namespace lyra::mir {
class Expression;
}

namespace lyra::lowering::ast_to_mir {

// Lowers a slang AST AssignmentExpression into a MIR Expression.
// Handles simple variable, element select, hierarchical, and struct/union field
// assignments.
auto LowerAssignment(
    const slang::ast::AssignmentExpression& assignment,
    common::TypeArena& arena) -> std::unique_ptr<mir::Expression>;

}  // namespace lyra::lowering::ast_to_mir
