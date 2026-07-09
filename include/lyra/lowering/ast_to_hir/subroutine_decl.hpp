#pragma once

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/foreign_import.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace slang::ast {
class SubroutineSymbol;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

class ModuleLowerer;

// Lowers a slang subroutine (LRM 13) into a hir::SubroutineDecl: its result
// type, its formals as body-local procedural vars carrying their direction, the
// implicit result variable of a non-void function (LRM 13.4.1), and its lowered
// body. The same shape serves a structural-scope subroutine and a class method;
// `frame` is the enclosing context the body is lowered against. The caller
// records the result where it belongs (a scope's subroutine arena, a class's
// method list).
auto LowerSubroutineDecl(
    ModuleLowerer& module, const slang::ast::SubroutineSymbol& sym,
    WalkFrame frame) -> diag::Result<hir::SubroutineDecl>;

// Lowers a slang `import "DPI-C"` subroutine (LRM 35.4) into a bodyless
// hir::ForeignImportDecl: the resolved foreign name, the pure property, and the
// ABI projection of its signature. Distinct from LowerSubroutineDecl because a
// DPI import has no SV body; the caller records the result in the scope's
// `foreign_imports` arena, never in its subroutine arena.
auto LowerForeignImport(
    ModuleLowerer& module, const slang::ast::SubroutineSymbol& sym)
    -> diag::Result<hir::ForeignImportDecl>;

}  // namespace lyra::lowering::ast_to_hir
