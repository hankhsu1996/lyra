#include "lyra/lowering/ast_to_hir/event_handle.hpp"

#include <slang/ast/types/Type.h>

namespace lyra::lowering::ast_to_hir {

auto RefuseGivingAnEventAValue(
    const slang::ast::Type& target, diag::SourceSpan span)
    -> diag::Result<void> {
  if (!target.isEvent()) {
    return {};
  }
  // LRM 15.5.5.
  return diag::Fail(
      span, diag::DiagCode::kUnsupportedExpressionForm,
      "giving an event a value is not yet supported");
}

auto RefuseReadingAnEventAsAValue(
    const slang::ast::Type& operand, diag::SourceSpan span)
    -> diag::Result<void> {
  if (!operand.isEvent()) {
    return {};
  }
  // LRM 15.5.5.3.
  return diag::Fail(
      span, diag::DiagCode::kUnsupportedExpressionForm,
      "reading an event as a value is not yet supported");
}

auto RefuseTakingOverAnEvent(
    const slang::ast::Type& target, diag::SourceSpan span)
    -> diag::Result<void> {
  if (!target.isEvent()) {
    return {};
  }
  // LRM 10.6, 15.5.5.
  return diag::Fail(
      span, diag::DiagCode::kUnsupportedStatementForm,
      "a procedural continuous assignment to an event is not yet supported");
}

}  // namespace lyra::lowering::ast_to_hir
