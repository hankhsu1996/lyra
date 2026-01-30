#pragma once

#include "lyra/common/source_span.hpp"
#include "lyra/common/type.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/lowering/ast_to_hir/detail/expression_lowering.hpp"

namespace slang::ast {
class StructuredAssignmentPatternExpression;
}

namespace lyra::lowering::ast_to_hir {

// Lower assignment pattern to HIR Pattern
// target_type_id: the HIR type of the container being filled (already lowered)
auto LowerPattern(
    const slang::ast::StructuredAssignmentPatternExpression& pattern,
    TypeId target_type_id, SourceSpan span, ExpressionLoweringView view)
    -> hir::PatternId;

}  // namespace lyra::lowering::ast_to_hir
