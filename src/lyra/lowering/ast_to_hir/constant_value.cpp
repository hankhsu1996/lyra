#include "lyra/lowering/ast_to_hir/constant_value.hpp"

#include <cstddef>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/lowering/ast_to_hir/integral_constant.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// Materialize a folded constant value as its HIR expression form: a scalar leaf
// becomes a literal; an unpacked aggregate becomes an AssignmentPatternExpr
// (the shape a written `'{...}` pattern lowers to) over recursively
// materialized components, with the type giving each component's type -- a
// struct's field i, an array's shared element. This is the value's realization
// in an expression context; the value itself is folded once, at the sole
// boundary that reads slang's constant representation.
auto MaterializeConstantExpr(
    const hir::CompilationUnit& unit, WalkFrame frame,
    const hir::ConstantValue& value, hir::TypeId type, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  return std::visit(
      Overloaded{
          [&](const hir::IntegralConstant& integral)
              -> diag::Result<hir::Expr> {
            return MakeIntegerLiteralFromConstant(integral, type, span);
          },
          [&](double real) -> diag::Result<hir::Expr> {
            return hir::Expr{
                .type = type,
                .data =
                    hir::PrimaryExpr{.data = hir::RealLiteral{.value = real}},
                .span = span};
          },
          [&](const std::string& text) -> diag::Result<hir::Expr> {
            return hir::Expr{
                .type = type,
                .data =
                    hir::PrimaryExpr{.data = hir::StringLiteral{.value = text}},
                .span = span};
          },
          [&](const std::vector<hir::ConstantValue>& components)
              -> diag::Result<hir::Expr> {
            const auto& ty = unit.types.Get(type);
            const auto* arr = std::get_if<hir::UnpackedArrayType>(&ty.data);
            const auto* st = std::get_if<hir::UnpackedStructType>(&ty.data);
            if (arr == nullptr && st == nullptr) {
              return diag::Fail(
                  span, diag::DiagCode::kUnsupportedExpressionForm,
                  "constant of aggregate type is not yet supported");
            }
            std::vector<hir::ExprId> element_ids;
            element_ids.reserve(components.size());
            for (std::size_t i = 0; i < components.size(); ++i) {
              const hir::TypeId component_type =
                  arr != nullptr ? arr->element_type : st->fields[i].type;
              auto element = MaterializeConstantExpr(
                  unit, frame, components[i], component_type, span);
              if (!element) return std::unexpected(std::move(element.error()));
              element_ids.push_back(frame.Exprs().Add(*std::move(element)));
            }
            return hir::Expr{
                .type = type,
                .data =
                    hir::AssignmentPatternExpr{
                        .elements = std::move(element_ids)},
                .span = span};
          },
      },
      value.data);
}

}  // namespace

auto MakeConstantValue(const slang::ConstantValue& cv, diag::SourceSpan span)
    -> diag::Result<hir::ConstantValue> {
  if (cv.isInteger()) {
    return hir::ConstantValue{
        .data = LowerSVIntToIntegralConstant(cv.integer())};
  }
  if (cv.isReal()) {
    return hir::ConstantValue{.data = cv.real()};
  }
  if (cv.isShortReal()) {
    return hir::ConstantValue{.data = static_cast<double>(cv.shortReal())};
  }
  if (cv.isString()) {
    return hir::ConstantValue{.data = cv.str()};
  }
  if (cv.isUnpacked()) {
    std::vector<hir::ConstantValue> components;
    const auto elements = cv.elements();
    components.reserve(elements.size());
    for (const auto& element : elements) {
      auto component = MakeConstantValue(element, span);
      if (!component) return std::unexpected(std::move(component.error()));
      components.push_back(*std::move(component));
    }
    return hir::ConstantValue{.data = std::move(components)};
  }
  return diag::Fail(
      span, diag::DiagCode::kUnsupportedExpressionForm,
      "constant of this type is not yet supported");
}

auto MakeConstantValueExpr(
    const hir::CompilationUnit& unit, WalkFrame frame,
    const slang::ConstantValue& cv, hir::TypeId type, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto value = MakeConstantValue(cv, span);
  if (!value) return std::unexpected(std::move(value.error()));
  return MaterializeConstantExpr(unit, frame, *value, type, span);
}

}  // namespace lyra::lowering::ast_to_hir
