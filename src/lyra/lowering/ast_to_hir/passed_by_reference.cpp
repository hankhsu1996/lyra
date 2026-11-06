#include "lyra/lowering/ast_to_hir/passed_by_reference.hpp"

#include <slang/ast/Expression.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/expressions/SelectExpressions.h>
#include <slang/ast/types/Type.h>

#include "lyra/diag/diag_code.hpp"
#include "lyra/frontend/slang_source_span.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// Whether `place` is a member of an unpacked union or a part below one,
// followed outward through every member and element select it is written as.
auto ReachesThroughAUnionMember(const slang::ast::Expression& place) -> bool {
  const slang::ast::Expression* at = &place;
  while (true) {
    if (const auto* member = at->as_if<slang::ast::MemberAccessExpression>()) {
      if (member->value().type->isUnpackedUnion()) return true;
      at = &member->value();
    } else if (
        const auto* element =
            at->as_if<slang::ast::ElementSelectExpression>()) {
      at = &element->value();
    } else if (
        const auto* range = at->as_if<slang::ast::RangeSelectExpression>()) {
      at = &range->value();
    } else {
      return false;
    }
  }
}

}  // namespace

auto RefuseUnionPartByReference(const slang::ast::Expression& actual)
    -> diag::Result<void> {
  if (!ReachesThroughAUnionMember(actual)) return {};
  return std::unexpected(
      diag::Make(
          frontend::SpanOf(actual.sourceRange),
          diag::DiagCode::kErrorUnionPartPassedByReference,
          "a member of a union, or a part of one, cannot be passed by "
          "reference")
          .WithNote(
              "pass the whole union, or pass the member to an 'inout' "
              "formal"));
}

}  // namespace lyra::lowering::ast_to_hir
