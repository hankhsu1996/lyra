#pragma once

#include <memory>

namespace slang::ast {
class Statement;
class Expression;
class VariableSymbol;
}  // namespace slang::ast

namespace lyra::common {
class TypeArena;
}

namespace lyra::mir {
class Statement;
class Expression;
class VariableDeclarationStatement;
class ExpressionStatement;
}  // namespace lyra::mir

namespace lyra::lowering::ast_to_mir {

class SymbolRegistrar;

// Lowers a slang AST Statement into a list of MIR Statements.
auto LowerStatement(
    const slang::ast::Statement& statement, common::TypeArena& arena,
    SymbolRegistrar& registrar) -> std::unique_ptr<mir::Statement>;

// Lowers a VariableSymbol into a MIR VariableDeclarationStatement.
auto LowerVariableDeclaration(
    const slang::ast::VariableSymbol& symbol, common::TypeArena& arena,
    SymbolRegistrar& registrar)
    -> std::unique_ptr<mir::VariableDeclarationStatement>;

// Wraps an expression into a MIR ExpressionStatement.
auto LowerExpressionStatement(
    const slang::ast::Expression& expr, common::TypeArena& arena,
    SymbolRegistrar& registrar) -> std::unique_ptr<mir::ExpressionStatement>;

}  // namespace lyra::lowering::ast_to_mir
