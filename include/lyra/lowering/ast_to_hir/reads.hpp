#pragma once

// The cells an evaluation reads that its text fixes (LRM 9.4.2, 9.2.2.2.1): the
// storage elaboration sealed that a waited expression reads, and what a body
// it runs reads and writes outside it. What any of them reaches through a
// handle is the evaluation's to state as it reaches it, and needs nothing
// stated here.

#include <vector>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/timing.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace slang::ast {
class ClassType;
class Expression;
class ProceduralBlockSymbol;
class SubroutineSymbol;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// The storage elaboration sealed that `expr` reads, which a wait on it watches
// for as long as it lasts.
template <typename Lowerer>
auto CellsOfWaitedExpression(
    Lowerer& lowerer, WalkFrame frame, const slang::ast::Expression& expr)
    -> diag::Result<std::vector<hir::SensitivityEntry>>;

// The cells a call of `function` reads and writes, and the calls an implicit
// list follows out of it. `frame` is the frame the body lowered in, after it
// lowered.
auto ReadsOfFunctionBody(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::SubroutineSymbol& function) -> hir::Reads;

// The cells constructing an object of `cls` reads (LRM 9.4.2): its property
// initializers, the body of `constructor`, which is null for the implicit
// `new` (LRM 8.7), and `base_clause`, the arguments an extends specifier hands
// the base (LRM 8.17), null where the constructor's own body hands them or
// none are handed; the base's construction states its own. `frame` is the
// constructor body's frame.
auto ReadsOfConstruction(
    ProcessLowerer& proc, WalkFrame frame, const slang::ast::ClassType& cls,
    const slang::ast::SubroutineSymbol* constructor,
    const slang::ast::Expression* base_clause) -> hir::Reads;

// The implicit list of an `always_comb` / `always_latch` (LRM 9.2.2.2.1): what
// its own text reads and writes, and each function call it makes, which states
// what that function reads and writes once, ahead of the first run.
auto ReadsOfProcedure(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::ProceduralBlockSymbol& procedure)
    -> diag::Result<hir::Reads>;

}  // namespace lyra::lowering::ast_to_hir
