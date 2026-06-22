#pragma once

// Lowering of aggregate value-construction expressions: Concatenation
// (LRM 11.4.12), Replication (LRM 11.4.12.1), AssignmentPattern (LRM 10.9.1
// `'{...}` positional and structured forms), ReplicatedAssignmentPattern
// (LRM 10.9.1 replicated form), and NewArray (LRM 7.5.1 dynamic-array
// constructor). Both procedural and structural contexts where applicable.

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/structural_scope_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace slang::ast {
class AssignmentPatternExpressionBase;
class ConcatenationExpression;
class NewArrayExpression;
class ReplicatedAssignmentPatternExpression;
class ReplicationExpression;
class StructuredAssignmentPatternExpression;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// Procedural-context handlers.
auto LowerConcatExprProc(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::ConcatenationExpression& cc, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;
auto LowerReplicationExprProc(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::ReplicationExpression& rp, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;
auto LowerAssignmentPatternFromElementsProc(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::AssignmentPatternExpressionBase& ap,
    diag::SourceSpan span) -> diag::Result<hir::Expr>;
auto LowerReplicatedAssignmentPatternExprProc(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::ReplicatedAssignmentPatternExpression& rp,
    diag::SourceSpan span) -> diag::Result<hir::Expr>;
auto LowerNewArrayExprProc(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::NewArrayExpression& na, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;
auto LowerAssociativeAssignmentPatternProc(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::StructuredAssignmentPatternExpression& ap,
    diag::SourceSpan span) -> diag::Result<hir::Expr>;

// Structural-context handlers.
auto LowerConcatExprStructural(
    StructuralScopeLowerer& scope, WalkFrame frame,
    const slang::ast::ConcatenationExpression& cc, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;
auto LowerAssignmentPatternFromElementsStructural(
    StructuralScopeLowerer& scope, WalkFrame frame,
    const slang::ast::AssignmentPatternExpressionBase& ap,
    diag::SourceSpan span) -> diag::Result<hir::Expr>;
auto LowerReplicatedAssignmentPatternExprStructural(
    StructuralScopeLowerer& scope, WalkFrame frame,
    const slang::ast::ReplicatedAssignmentPatternExpression& rp,
    diag::SourceSpan span) -> diag::Result<hir::Expr>;
auto LowerAssociativeAssignmentPatternStructural(
    StructuralScopeLowerer& scope, WalkFrame frame,
    const slang::ast::StructuredAssignmentPatternExpression& ap,
    diag::SourceSpan span) -> diag::Result<hir::Expr>;

}  // namespace lyra::lowering::ast_to_hir
