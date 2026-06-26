#include "lyra/lowering/ast_to_hir/constant_value.hpp"

#include <utility>
#include <variant>
#include <vector>

#include "lyra/diag/diag_code.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/lowering/ast_to_hir/integral_constant.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// An unpacked array constant folds element-by-element into an
// AssignmentPatternExpr, the same shape a written `'{...}` pattern lowers to,
// so the downstream array-construction path is shared. Every element shares the
// array's element type and recurses, so an array of arrays folds the same way.
auto MakeUnpackedConstantExpr(
    const hir::ModuleUnit& unit, WalkFrame frame,
    const slang::ConstantValue& cv, hir::TypeId type, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  const auto& ty = unit.types.Get(type);
  const auto* arr = std::get_if<hir::UnpackedArrayType>(&ty.data);
  if (arr == nullptr) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "constant of aggregate type is not yet supported");
  }
  const auto elements = cv.elements();
  std::vector<hir::ExprId> element_ids;
  element_ids.reserve(elements.size());
  for (const auto& elem_cv : elements) {
    auto elem =
        MakeConstantValueExpr(unit, frame, elem_cv, arr->element_type, span);
    if (!elem) return std::unexpected(std::move(elem.error()));
    element_ids.push_back(frame.Exprs().Add(*std::move(elem)));
  }
  return hir::Expr{
      .type = type,
      .data = hir::AssignmentPatternExpr{.elements = std::move(element_ids)},
      .span = span,
  };
}

}  // namespace

auto MakeConstantValueExpr(
    const hir::ModuleUnit& unit, WalkFrame frame,
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
  if (cv.isUnpacked()) {
    return MakeUnpackedConstantExpr(unit, frame, cv, type, span);
  }
  return diag::Fail(
      span, diag::DiagCode::kUnsupportedExpressionForm,
      "constant of aggregate type is not yet supported");
}

}  // namespace lyra::lowering::ast_to_hir
