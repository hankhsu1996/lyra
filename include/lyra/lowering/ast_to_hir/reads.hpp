#pragma once

// What an evaluation can read (LRM 9.4.2): the leaves a wait on an expression
// watches, and what a call of a function reports to one. Both are stated here,
// by one rule, because a wait learns what a function reads only by asking it.

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/timing.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace slang::ast {
class Expression;
class SubroutineSymbol;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// What a wait on `expr` watches: every cell it reads, every object and
// interface variable it reaches through a handle, and every call of a function
// it makes, which reports what it reads when the wait collects its leaves.
// Everything here is evaluated where the wait stands.
auto ReadsOfWaitedExpression(
    ProcessLowerer& proc, WalkFrame frame, const slang::ast::Expression& expr,
    diag::SourceSpan span) -> diag::Result<hir::Reads>;

// What a call of `function` can read, stated so its own report can evaluate
// it: over its formals, the object it runs on, and storage declared outside
// it. Its own variables hold nothing yet when it reports, so what it reaches
// through one of them is covered by every object, and a call it makes is handed
// its type's default for an argument that reads one. `frame` is the frame the
// body lowered in, after it lowered.
auto ReadsOfFunctionBody(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::SubroutineSymbol& function, diag::SourceSpan span)
    -> diag::Result<hir::Reads>;

}  // namespace lyra::lowering::ast_to_hir
