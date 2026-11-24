#pragma once

#include <span>
#include <unordered_set>

#include <slang/ast/SemanticFacts.h>

namespace slang::ast {
class Statement;
class VariableSymbol;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// Whether a block of `kind` can be left while branches it started still run,
// and so outlive the scope around it: a fork that does not wait for every
// branch (LRM 9.3.2 Table 9-1). Its own block_item_declarations are no branch;
// they run in the forking process before any branch is spawned.
[[nodiscard]] auto BranchesMayOutliveBlock(slang::ast::StatementBlockKind kind)
    -> bool;

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
