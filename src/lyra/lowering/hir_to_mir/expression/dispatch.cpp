#include <concepts>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/expression/aggregates.hpp"
#include "lyra/lowering/hir_to_mir/expression/assignment.hpp"
#include "lyra/lowering/hir_to_mir/expression/calls.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/expression/inside.hpp"
#include "lyra/lowering/hir_to_mir/expression/operators.hpp"
#include "lyra/lowering/hir_to_mir/expression/references.hpp"
#include "lyra/lowering/hir_to_mir/expression/selects.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// The one value-context expression dispatcher, shared by both pass classes. An
// expression's meaning does not depend on whether a process body or a
// structural scope encloses it, so every context-free kind routes to one shared
// handler listed exactly once -- a kind cannot be wired in one context and
// forgotten in the other. The two real differences are parameterized inline: a
// bare name resolves to different storage per scope, and the kinds LRM allows
// only in procedural code (the assignment expression LRM 10.3, increment /
// decrement LRM 11.4.2, the dynamic-array constructor LRM 7.5.1) cannot appear
// in a structural HIR -- AST-to-HIR has already rejected them there, so the
// structural arm is an unreachable invariant. An observable-cell leaf is
// auto-wrapped in a `Get` call so the result is value-typed.
template <ExprLowerer L>
auto LowerExprImpl(L& lowerer, const hir::Expr& expr, WalkFrame frame)
    -> diag::Result<mir::Expr> {
  constexpr bool kProcedural = std::same_as<L, ProcessLowerer>;
  const mir::TypeId result_type = lowerer.Module().TranslateType(expr.type);
  auto raw_or = std::visit(
      Overloaded{
          [&](const hir::PrimaryExpr& p) -> diag::Result<mir::Expr> {
            if constexpr (kProcedural) {
              return LowerHirPrimaryExprProc(
                  lowerer, frame, p.data, result_type);
            } else {
              return LowerHirPrimaryExprStructural(
                  lowerer, frame, p.data, result_type);
            }
          },
          [&](const hir::UnaryExpr& u) -> diag::Result<mir::Expr> {
            return LowerHirUnaryExpr(lowerer, frame, u, result_type);
          },
          [&](const hir::BinaryExpr& b) -> diag::Result<mir::Expr> {
            return LowerHirBinaryExpr(lowerer, frame, b, result_type);
          },
          [&](const hir::ConditionalExpr& c) -> diag::Result<mir::Expr> {
            return LowerHirConditionalExpr(lowerer, frame, c, result_type);
          },
          [&](const hir::AssignExpr& a) -> diag::Result<mir::Expr> {
            if constexpr (kProcedural) {
              return LowerHirAssignExprProc(
                  lowerer, frame, a, expr.span, result_type);
            } else {
              throw InternalError(
                  "structural expression lowering: HIR AssignExpr is "
                  "unreachable; assignment expressions are rejected in "
                  "structural context at AST-to-HIR (LRM 10.3)");
            }
          },
          [&](const hir::IncDecExpr& inc) -> diag::Result<mir::Expr> {
            if constexpr (kProcedural) {
              return LowerHirIncDecExprProc(lowerer, frame, inc, result_type);
            } else {
              throw InternalError(
                  "structural expression lowering: HIR IncDecExpr is "
                  "unreachable; increment / decrement is rejected in "
                  "structural context at AST-to-HIR (LRM 11.4.2)");
            }
          },
          [&](const hir::ConversionExpr& cv) -> diag::Result<mir::Expr> {
            return LowerHirConversionExpr(lowerer, frame, cv, result_type);
          },
          [&](const hir::CallExpr& c) -> diag::Result<mir::Expr> {
            return LowerHirCallExpr(lowerer, frame, c, expr.span, result_type);
          },
          [&](const hir::InsideExpr& in) -> diag::Result<mir::Expr> {
            return LowerHirInsideExpr(lowerer, frame, in, result_type);
          },
          [&](const hir::ElementSelectExpr& sel) -> diag::Result<mir::Expr> {
            return LowerHirElementSelectExpr(lowerer, frame, sel, result_type);
          },
          [&](const hir::RangeSelectExpr& sel) -> diag::Result<mir::Expr> {
            return LowerHirRangeSelectExpr(lowerer, frame, sel, result_type);
          },
          [&](const hir::MemberAccessExpr& sel) -> diag::Result<mir::Expr> {
            return LowerHirMemberAccessExpr(lowerer, frame, sel, result_type);
          },
          [&](const hir::ConcatExpr& c) -> diag::Result<mir::Expr> {
            return LowerHirConcatExpr(lowerer, frame, c, result_type);
          },
          [&](const hir::ReplicationExpr& r) -> diag::Result<mir::Expr> {
            return LowerHirReplicationExpr(lowerer, frame, r, result_type);
          },
          [&](const hir::AssignmentPatternExpr& a) -> diag::Result<mir::Expr> {
            return LowerHirAssignmentPatternExpr(
                lowerer, frame, a, result_type);
          },
          [&](const hir::AssignmentPatternReplicationExpr& a)
              -> diag::Result<mir::Expr> {
            return LowerHirAssignmentPatternReplicationExpr(
                lowerer, frame, a, result_type);
          },
          [&](const hir::DynamicArrayNewExpr& n) -> diag::Result<mir::Expr> {
            if constexpr (kProcedural) {
              return LowerHirDynamicArrayNewExprProc(
                  lowerer, frame, n, expr.type, result_type);
            } else {
              throw InternalError(
                  "structural expression lowering: HIR DynamicArrayNewExpr is "
                  "unreachable; dynamic-array new[] is rejected in structural "
                  "context at AST-to-HIR (LRM 7.5.1)");
            }
          },
          [&](const hir::AssociativeAssignmentPatternExpr& a)
              -> diag::Result<mir::Expr> {
            return LowerHirAssociativeAssignmentPatternExpr(
                lowerer, frame, a, result_type);
          },
      },
      expr.data);
  if (!raw_or) return raw_or;
  if (mir::IsObservableCellType(
          lowerer.Module().Unit().types.Get(raw_or->type))) {
    const mir::ExprId cell_id =
        frame.current_block->exprs.Add(*std::move(raw_or));
    return mir::MakeObservableGetCallExpr(cell_id, result_type);
  }
  return raw_or;
}

// The LHS-context dispatcher, shared by both pass classes. Addressable kinds
// only, and no `Get` auto-wrap, so an observable-cell leaf flows out as the
// bare cell the assignment target needs. A bare name is the only per-scope arm;
// selector kinds keep their base cell-rooted by recursing through the pass
// class's own LHS entry.
template <ExprLowerer L>
auto LowerLhsExprImpl(L& lowerer, const hir::Expr& expr, WalkFrame frame)
    -> diag::Result<mir::Expr> {
  constexpr bool kProcedural = std::same_as<L, ProcessLowerer>;
  const mir::TypeId result_type = lowerer.Module().TranslateType(expr.type);
  return std::visit(
      Overloaded{
          [&](const hir::PrimaryExpr& p) -> diag::Result<mir::Expr> {
            if constexpr (kProcedural) {
              return LowerHirPrimaryExprProc(
                  lowerer, frame, p.data, result_type);
            } else {
              return LowerHirPrimaryExprStructural(
                  lowerer, frame, p.data, result_type);
            }
          },
          [&](const hir::ElementSelectExpr& sel) -> diag::Result<mir::Expr> {
            return LowerHirElementSelectExprLhs(
                lowerer, frame, sel, result_type);
          },
          [&](const hir::RangeSelectExpr& sel) -> diag::Result<mir::Expr> {
            return LowerHirRangeSelectExprLhs(lowerer, frame, sel, result_type);
          },
          [&](const hir::MemberAccessExpr& sel) -> diag::Result<mir::Expr> {
            return LowerHirMemberAccessExprLhs(
                lowerer, frame, sel, result_type);
          },
          [&](const hir::ConcatExpr& c) -> diag::Result<mir::Expr> {
            return LowerHirConcatExpr(lowerer, frame, c, result_type);
          },
          [](const auto&) -> diag::Result<mir::Expr> {
            throw InternalError(
                "LHS expression lowering: non-addressable HIR expression in "
                "LHS context (assignability validated at AST-to-HIR)");
          },
      },
      expr.data);
}

}  // namespace

auto ProcessLowerer::LowerExpr(const hir::Expr& expr, WalkFrame frame)
    -> diag::Result<mir::Expr> {
  return LowerExprImpl(*this, expr, frame);
}

auto ProcessLowerer::LowerLhsExpr(const hir::Expr& expr, WalkFrame frame)
    -> diag::Result<mir::Expr> {
  return LowerLhsExprImpl(*this, expr, frame);
}

auto StructuralScopeLowerer::LowerExpr(
    const hir::Expr& expr, WalkFrame frame) const -> diag::Result<mir::Expr> {
  return LowerExprImpl(*this, expr, frame);
}

auto StructuralScopeLowerer::LowerLhsExpr(
    const hir::Expr& expr, WalkFrame frame) const -> diag::Result<mir::Expr> {
  return LowerLhsExprImpl(*this, expr, frame);
}

}  // namespace lyra::lowering::hir_to_mir
