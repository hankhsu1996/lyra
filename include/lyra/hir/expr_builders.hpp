#pragma once

#include <cstdint>
#include <utility>

#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/integral_constant.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/hir/type_id.hpp"

// Pure builders for the synthetic HIR expressions a lowering reaches for
// whenever it needs a counter, bound, or sentinel -- stateless, unlike the
// lowering passes that own them. The AST-to-HIR counterpart of
// `mir::MakeInt32Literal`.
namespace lyra::hir {

// A 32-bit signed two-state `int`-typed integer literal for a raw value.
[[nodiscard]] inline auto MakeInt32Literal(
    std::int64_t value, TypeId int32_type, diag::SourceSpan span) -> Expr {
  return Expr{
      .type = int32_type,
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
                              .state_kind = IntegralStateKind::kTwoState},
                      .base = IntegerLiteralBase::kDecimal,
                      .declared_unsized = false}},
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

}  // namespace lyra::hir
