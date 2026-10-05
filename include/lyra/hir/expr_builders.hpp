#pragma once

#include <cstdint>
#include <utility>
#include <variant>

#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/integral_constant.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/hir/value_ref.hpp"

// Pure builders for the synthetic HIR expressions a lowering reaches for
// whenever it needs a counter, bound, or sentinel -- stateless, unlike the
// lowering passes that own them.
namespace lyra::hir {

// A 2-state signed 32-bit literal, typed `int` (LRM 6.11.1).
[[nodiscard]] inline auto MakeIntLiteral(
    std::int64_t value, TypeId int_type, diag::SourceSpan span) -> Expr {
  return Expr{
      .type = int_type,
      .data =
          PrimaryExpr{
              .data =
                  IntegerLiteral{
                      .value =
                          IntegralConstant{
                              .value_words =
                                  {static_cast<std::uint64_t>(value) &
                                   0xFFFFFFFFULL},
                              .state_words = {},
                              .width = 32,
                              .signedness = Signedness::kSigned,
                              .state_kind = IntegralStateKind::kTwoState}}},
      .span = span};
}

// Wraps a named-value reference primary (a procedural, structural, or loop-var
// reference) as an Expr. The caller builds the specific primary; one builder
// serves every reference family.
[[nodiscard]] inline auto MakeRefExpr(
    Primary ref, TypeId type, diag::SourceSpan span) -> Expr {
  return Expr{
      .type = type, .data = PrimaryExpr{.data = std::move(ref)}, .span = span};
}

// The same for a resolved value target. Every way of reaching a cell -- a route
// through the design hierarchy, a namespace unit's cell named across the
// boundary, a static property's cell -- is a reference primary, so one wrap
// serves them all.
[[nodiscard]] inline auto MakeValueTargetRefExpr(
    const ValueTarget& target, TypeId type, diag::SourceSpan span) -> Expr {
  return std::visit(
      [&](const auto& reached) {
        return MakeRefExpr(Primary{reached}, type, span);
      },
      target);
}

}  // namespace lyra::hir
