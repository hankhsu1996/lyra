#pragma once

#include "lyra/diag/diagnostic.hpp"
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

}  // namespace lyra::lowering::ast_to_hir
