#include "lyra/lowering/ast_to_hir/published_projection.hpp"

#include <cstdint>
#include <span>
#include <utility>
#include <variant>

#include "lyra/base/overloaded.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/expr_builders.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/published_target.hpp"
#include "lyra/hir/range_bounds.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/hir/unit_signature.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

auto ProjectedTypeOf(const hir::PublishedSelector& step) -> hir::TypeId {
  return std::visit(
      [](const auto& selector) { return selector.projected_type; }, step);
}

}  // namespace

auto ProjectPublishedPath(
    UnitLowerer& unit_lowerer, WalkFrame frame,
    const hir::UnitSignature& signature,
    std::span<const hir::PublishedSelector> path, hir::Expr base,
    diag::SourceSpan span) -> hir::Expr {
  const hir::TypeId int_type = unit_lowerer.Unit().builtins.int_type;
  const auto coordinate = [&](std::int32_t value) {
    return frame.Exprs().Add(hir::MakeIntLiteral(value, int_type, span));
  };
  hir::Expr reached = std::move(base);
  for (const hir::PublishedSelector& step : path) {
    const hir::ExprId under = frame.Exprs().Add(std::move(reached));
    hir::ExprData data = std::visit(
        Overloaded{
            [&](const hir::PublishedElementSelector& element) -> hir::ExprData {
              return hir::ElementSelectExpr{
                  .base_value = under, .index = coordinate(element.index)};
            },
            [&](const hir::PublishedSliceSelector& slice) -> hir::ExprData {
              return hir::RangeSelectExpr{
                  .base_value = under,
                  .bounds = std::visit(
                      Overloaded{
                          [&](const hir::PublishedConstantRange& range)
                              -> hir::RangeBounds {
                            return hir::RangeConstantBounds{
                                .left_bound = coordinate(range.left),
                                .right_bound = coordinate(range.right)};
                          },
                          [&](const hir::PublishedIndexedUpRange& range)
                              -> hir::RangeBounds {
                            return hir::RangeIndexedUpBounds{
                                .base_index = coordinate(range.base),
                                .width = coordinate(range.width)};
                          },
                          [&](const hir::PublishedIndexedDownRange& range)
                              -> hir::RangeBounds {
                            return hir::RangeIndexedDownBounds{
                                .base_index = coordinate(range.base),
                                .width = coordinate(range.width)};
                          }},
                      slice.range)};
            }},
        step);
    reached = hir::Expr{
        .type =
            unit_lowerer.ImportSignatureType(signature, ProjectedTypeOf(step)),
        .data = std::move(data),
        .span = span};
  }
  return reached;
}

}  // namespace lyra::lowering::ast_to_hir
