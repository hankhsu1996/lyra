#pragma once

// Lowering of aggregate value-construction expressions: Concatenation
// (LRM 11.4.12), Replication (LRM 11.4.12.1), AssignmentPattern (LRM 10.9.1
// `'{...}` positional and structured forms), ReplicatedAssignmentPattern
// (LRM 10.9.1 replicated form), and NewArray (LRM 7.5.1 dynamic-array
// constructor). Both procedural and structural contexts where applicable.

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/ast_to_hir/expression/expr_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace slang::ast {
class AssignmentPatternExpressionBase;
class ConcatenationExpression;
class NewArrayExpression;
class NewClassExpression;
class ReplicatedAssignmentPatternExpression;
class ReplicationExpression;
class StructuredAssignmentPatternExpression;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

class ProcessLowerer;

// An aggregate's value-construction is independent of the enclosing scope, so
// one template over the pass class serves both the procedural and structural
// contexts; explicit instantiations live in the implementation file.
template <ExprLowerer Lowerer>
auto LowerConcatExpr(
    Lowerer& lowerer, WalkFrame frame,
    const slang::ast::ConcatenationExpression& cc, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;
template <ExprLowerer Lowerer>
auto LowerAssignmentPatternFromElements(
    Lowerer& lowerer, WalkFrame frame,
    const slang::ast::AssignmentPatternExpressionBase& ap,
    diag::SourceSpan span) -> diag::Result<hir::Expr>;
template <ExprLowerer Lowerer>
auto LowerReplicatedAssignmentPatternExpr(
    Lowerer& lowerer, WalkFrame frame,
    const slang::ast::ReplicatedAssignmentPatternExpression& rp,
    diag::SourceSpan span) -> diag::Result<hir::Expr>;
template <ExprLowerer Lowerer>
auto LowerAssociativeAssignmentPattern(
    Lowerer& lowerer, WalkFrame frame,
    const slang::ast::StructuredAssignmentPatternExpression& ap,
    diag::SourceSpan span) -> diag::Result<hir::Expr>;

// Replication (LRM 11.4.12.1) is an ordinary value expression, legal wherever a
// value is, so it is one template over the pass class like the other aggregate
// families.
template <ExprLowerer Lowerer>
auto LowerReplicationExpr(
    Lowerer& lowerer, WalkFrame frame,
    const slang::ast::ReplicationExpression& rp, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;

// The dynamic-array constructor `new[N]` (LRM 7.5.1) allocates simulation-time
// storage, which a constructor-time structural expression cannot do, so it
// stays a procedural-only handler.
auto LowerNewArrayExprProc(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::NewArrayExpression& na, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;

// The class constructor `new` (LRM 8.5) yields a handle; the construction is
// independent of the enclosing scope, so one template over the pass class
// serves both contexts.
template <ExprLowerer Lowerer>
auto LowerNewClassExpr(
    Lowerer& lowerer, WalkFrame frame, const slang::ast::NewClassExpression& nc,
    diag::SourceSpan span) -> diag::Result<hir::Expr>;

}  // namespace lyra::lowering::ast_to_hir
