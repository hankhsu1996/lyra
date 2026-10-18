#pragma once

#include <span>
#include <unordered_set>

namespace slang::ast {
class Statement;
class VariableSymbol;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// LRM 6.21: the automatic variables a detached (join_none / join_any) fork
// branch borrows and can outlive. A pre-pass over a procedural body, run before
// the body is lowered, so each declaration learns its status at creation rather
// than having it back-patched at a later reference. A borrow is a reference
// inside a detached branch to an automatic declared before that fork (enclosing
// it); the fork's own block-item locals and a branch's own locals are excluded.
// `declared_ahead` are the variables the body's scope declares before any
// statement of it -- a subroutine's formal arguments and its result -- which
// every fork in the body therefore encloses.
[[nodiscard]] auto CollectLifetimeExtendedVars(
    std::span<const slang::ast::VariableSymbol* const> declared_ahead,
    const slang::ast::Statement& body)
    -> std::unordered_set<const slang::ast::VariableSymbol*>;

}  // namespace lyra::lowering::ast_to_hir
