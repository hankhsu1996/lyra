#pragma once

#include <cstdint>

#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/integral_constant.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/hir/value_ref.hpp"

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

// A reference to a procedural variable.
[[nodiscard]] inline auto MakeProcVarRefExpr(
    ProceduralVarId var, TypeId type, diag::SourceSpan span) -> Expr {
  return Expr{
      .type = type,
      .data = PrimaryExpr{.data = ProceduralVarRef{.var = var}},
      .span = span};
}

}  // namespace lyra::hir
