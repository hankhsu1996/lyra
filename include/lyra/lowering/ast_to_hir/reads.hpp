#pragma once

// What an evaluation can read (LRM 9.4.2): the storage elaboration sealed that
// a waited expression reads, and what a call of a function reports to a wait.
// What a waited expression reaches beyond that is its own nodes, found by the
// evaluation that reaches them, and needs nothing stated here.

#include <vector>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/timing.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace slang::ast {
class Expression;
class ProceduralBlockSymbol;
class SubroutineSymbol;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// The storage elaboration sealed that `expr` reads, which a wait on it watches
// for as long as it lasts.
auto CellsOfWaitedExpression(
    ProcessLowerer& proc, WalkFrame frame, const slang::ast::Expression& expr)
    -> diag::Result<std::vector<hir::SensitivityEntry>>;

// What a call of `function` can read, stated so its own report can evaluate
// it: over its formals, the object it runs on, and storage that exists before
// its body runs. Its automatic variables hold nothing yet when it reports, so
// what it reaches through one of them is covered by every object, and a call it
// makes is handed its type's default for an argument that reads one. `frame` is
// the frame the body lowered in, after it lowered.
auto ReadsOfFunctionBody(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::SubroutineSymbol& function, diag::SourceSpan span)
    -> diag::Result<hir::Reads>;

// The implicit list of an `always_comb` / `always_latch` (LRM 9.2.2.2.1): what
// its own text reads and writes, and each function call it makes, which
// reports what that function reads and writes once, ahead of the first run,
// where the call reaches it -- wherever the function is declared.
auto ReadsOfProcedure(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::ProceduralBlockSymbol& procedure, diag::SourceSpan span)
    -> diag::Result<hir::Reads>;

}  // namespace lyra::lowering::ast_to_hir
