#include "lyra/lowering/ast_to_hir/constant_value.hpp"

#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/kind.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/lowering/ast_to_hir/integral_constant.hpp"

namespace lyra::lowering::ast_to_hir {

auto MakeConstantValueExpr(
    const slang::ConstantValue& cv, hir::TypeId type, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  if (cv.isInteger()) {
    return MakeIntegralLiteralExpr(cv.integer(), type, span);
  }
  if (cv.isReal()) {
    return hir::Expr{
        .type = type,
        .data = hir::PrimaryExpr{.data = hir::RealLiteral{.value = cv.real()}},
        .span = span,
    };
  }
  if (cv.isShortReal()) {
    return hir::Expr{
        .type = type,
        .data =
            hir::PrimaryExpr{
                .data =
                    hir::RealLiteral{
                        .value = static_cast<double>(cv.shortReal())}},
        .span = span,
    };
  }
  if (cv.isString()) {
    return hir::Expr{
        .type = type,
        .data = hir::PrimaryExpr{.data = hir::StringLiteral{.value = cv.str()}},
        .span = span,
    };
  }
  return diag::Fail(
      span, diag::DiagCode::kUnsupportedExpressionForm,
      "constant of aggregate type is not yet supported");
}

}  // namespace lyra::lowering::ast_to_hir
