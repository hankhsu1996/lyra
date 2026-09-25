#include "lyra/lowering/ast_to_hir/event_handle.hpp"

#include <slang/ast/types/Type.h>

namespace lyra::lowering::ast_to_hir {

auto RefuseGivingAnEventAValue(
    const slang::ast::Type& target, diag::SourceSpan span)
    -> diag::Result<void> {
  if (!target.isEvent()) {
    return {};
  }
  return diag::Fail(
      span, diag::DiagCode::kUnsupportedExpressionForm,
      "giving an event a value (LRM 15.5.5) is not yet supported");
}

auto RefuseReadingAnEventAsAValue(
    const slang::ast::Type& operand, diag::SourceSpan span)
    -> diag::Result<void> {
  if (!operand.isEvent()) {
    return {};
  }
  return diag::Fail(
      span, diag::DiagCode::kUnsupportedExpressionForm,
      "reading an event as a value (LRM 15.5.5.3) is not yet supported");
}

}  // namespace lyra::lowering::ast_to_hir
