#include <algorithm>
#include <concepts>
#include <optional>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/failure_context.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/lowering/hir_to_mir/expression/aggregates.hpp"
#include "lyra/lowering/hir_to_mir/expression/assignment.hpp"
#include "lyra/lowering/hir_to_mir/expression/calls.hpp"
#include "lyra/lowering/hir_to_mir/expression/dynamic_cast.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/expression/inside.hpp"
#include "lyra/lowering/hir_to_mir/expression/operators.hpp"
#include "lyra/lowering/hir_to_mir/expression/references.hpp"
#include "lyra/lowering/hir_to_mir/expression/selects.hpp"
#include "lyra/lowering/hir_to_mir/expression/tagged_union.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// Whether `expr`, read as of the Preponed region, answers with a value its cell
// kept. A variable a body declares with automatic lifetime -- a `ref` formal
// among them -- keeps none, its sampled value being its current one (LRM
// 16.5.1).
template <ExprLowerer L>
auto KeepsSampledValue(const L& lowerer, const hir::Expr& expr) -> bool {
  if constexpr (std::same_as<L, ProcessLowerer>) {
    const auto* primary = std::get_if<hir::PrimaryExpr>(&expr.data);
    const auto* var = primary == nullptr
                          ? nullptr
                          : std::get_if<hir::ProceduralVarRef>(&primary->data);
    if (var != nullptr) {
      return lowerer.HirBody().procedural_vars.Get(var->var).lifetime !=
             hir::VariableLifetime::kAutomatic;
    }
  }
  return true;
}

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
  const diag::FailureContext at(expr.span);
  const mir::TypeId result_type = lowerer.Owner().TranslateType(expr.type);
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
            // Both contexts have a statement stream to put the chain a
            // binding predicate needs in, so neither is special.
            if (DeclaresBindings(c)) {
              return LowerHirBindingConditionalExpr(
                  lowerer, frame, c, result_type);
            }
            return LowerHirConditionalExpr(lowerer, frame, c, result_type);
          },
          [&](const hir::AssignExpr& a) -> diag::Result<mir::Expr> {
            return LowerHirAssignExpr(
                lowerer, frame, a, expr.span, result_type);
          },
          [&](const hir::IncDecExpr& inc) -> diag::Result<mir::Expr> {
            return LowerHirIncDecExpr(lowerer, frame, inc, result_type);
          },
          [&](const hir::ConversionExpr& cv) -> diag::Result<mir::Expr> {
            return LowerHirConversionExpr(lowerer, frame, cv, result_type);
          },
          [&](const hir::CallExpr& c) -> diag::Result<mir::Expr> {
            return LowerHirCallExpr(lowerer, frame, c, expr.span, result_type);
          },
          // LRM 11.4.13: a value range has no value of its own -- it only
          // means something as an operand of a membership test, and the
          // lowering of that test consumes it directly. Reaching the generic
          // dispatcher means it appeared where slang would not have put one.
          [&](const hir::ValueRangeExpr&) -> diag::Result<mir::Expr> {
            throw InternalError(
                "expression lowering: a value range is lowered by the "
                "membership test that consumes it, never on its own");
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
          [&](const hir::ClassPropertyAccessExpr& sel)
              -> diag::Result<mir::Expr> {
            return LowerHirClassPropertyAccessExpr(
                lowerer, frame, sel, result_type);
          },
          [&](const hir::InterfaceMemberAccessExpr& sel)
              -> diag::Result<mir::Expr> {
            return LowerHirInterfaceMemberAccessExpr(lowerer, frame, sel);
          },
          [&](const hir::InterfaceInstanceAccessExpr& sel)
              -> diag::Result<mir::Expr> {
            return LowerHirInterfaceInstanceAccessExpr(
                lowerer, frame, sel, result_type);
          },
          [&](const hir::ConcatExpr& c) -> diag::Result<mir::Expr> {
            return LowerHirConcatExpr(
                lowerer, frame, c, expr.type, result_type);
          },
          [&](const hir::StreamingConcatExpr& s) -> diag::Result<mir::Expr> {
            return LowerHirStreamingConcatExpr(lowerer, frame, s, result_type);
          },
          [&](const hir::ReplicationExpr& r) -> diag::Result<mir::Expr> {
            return LowerHirReplicationExpr(lowerer, frame, r, result_type);
          },
          [&](const hir::AssignmentPatternExpr& a) -> diag::Result<mir::Expr> {
            return LowerHirAssignmentPatternExpr(
                lowerer, frame, a, expr.type, result_type);
          },
          [&](const hir::AssignmentPatternReplicationExpr& a)
              -> diag::Result<mir::Expr> {
            return LowerHirAssignmentPatternReplicationExpr(
                lowerer, frame, a, expr.type, result_type);
          },
          [&](const hir::AssignmentPatternKeyedExpr& k)
              -> diag::Result<mir::Expr> {
            return LowerHirAssignmentPatternKeyedExpr(
                lowerer, frame, k, expr.type, result_type);
          },
          [&](const hir::DynamicArrayNewExpr& n) -> diag::Result<mir::Expr> {
            return LowerHirDynamicArrayNewExpr(
                lowerer, frame, n, expr.type, result_type);
          },
          [&](const hir::AssociativeAssignmentPatternExpr& a)
              -> diag::Result<mir::Expr> {
            return LowerHirAssociativeAssignmentPatternExpr(
                lowerer, frame, a, expr.type, result_type);
          },
          [&](const hir::ClassNewExpr& n) -> diag::Result<mir::Expr> {
            // `new` allocates a managed object and runs its constructor: a
            // construction whose result type (a managed reference) names what
            // to build; the actuals flow to the constructor's formals.
            std::vector<mir::ExprId> args;
            args.reserve(n.arguments.size() + 1);
            // A class declared in a structural scope is a type of that scope's
            // instance (LRM 6.22), so the object records which instance it
            // belongs to and construction is where that arrives -- ahead of
            // the source actuals, the way every construction prefix does.
            if (const std::optional<ImplicitInstanceArgument> instance =
                    ImplicitInstanceArgumentOf(
                        lowerer.Owner().DeclaringInstanceOf(n.class_ref),
                        n.declaring_scope_hops)) {
              args.push_back(BuildImplicitInstanceArgument(
                  frame, lowerer.Owner().Unit(), *instance));
            }
            for (const hir::ExprId arg_hid : n.arguments) {
              auto arg_or =
                  lowerer.LowerExpr(lowerer.HirExprs().Get(arg_hid), frame);
              if (!arg_or) return std::unexpected(std::move(arg_or.error()));
              args.push_back(
                  frame.current_block->exprs.Add(*std::move(arg_or)));
            }
            return mir::Expr{
                .data =
                    mir::CallExpr{
                        .callee = mir::Construct{},
                        .arguments = std::move(args)},
                .type = result_type};
          },
          [&](const hir::TaggedUnionExpr& t) -> diag::Result<mir::Expr> {
            return LowerHirTaggedUnionExpr(
                lowerer, frame, t, expr.type, result_type);
          },
          [&](const hir::DynamicCastExpr& c) -> diag::Result<mir::Expr> {
            return LowerHirDynamicCastExpr(
                lowerer, frame, c, result_type, expr.span);
          },
      },
      expr.data);
  if (!raw_or) return raw_or;
  if (lowerer.Owner().Unit().types.Get(raw_or->type).IsCapabilityWrapper()) {
    const mir::ExprId cell_id =
        frame.current_block->exprs.Add(*std::move(raw_or));
    // The sampled value of an expression is the expression over the sampled
    // values of the variables it reads (LRM 16.5.1), so the whole of what
    // reading one changes is which value each leaf answers with -- here, where
    // every leaf read is built.
    if (frame.reads_as_of == ReadsAsOf::kPreponed &&
        KeepsSampledValue(lowerer, expr)) {
      return mir::MakeCellSampledLoadCallExpr(cell_id, result_type);
    }
    return mir::MakeCellLoadCallExpr(cell_id, result_type);
  }
  return raw_or;
}

// The dispatcher for an expression named as a part rather than read, shared by
// both pass classes. Addressable kinds only, and no `Get` auto-wrap, so an
// observable-cell leaf flows out as the bare cell. It peels rather than
// composes: a kind that reaches a part of a value adds one step to the descent
// and recurses, and every other kind is the place the descent bottoms out in.
// It appends nothing, so what it answers with is only a statement of which
// part is named.
template <ExprLowerer L>
auto LowerAccessPathImpl(L& lowerer, const hir::Expr& expr, WalkFrame frame)
    -> diag::Result<AccessPath> {
  constexpr bool kProcedural = std::same_as<L, ProcessLowerer>;
  const mir::TypeId result_type = lowerer.Owner().TranslateType(expr.type);
  // A kind that reaches no part of a value is the place that owns one, so what
  // it lowers to is a path that descends nowhere.
  const auto as_place =
      [&](diag::Result<mir::Expr> lowered) -> diag::Result<AccessPath> {
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    return AccessPath{
        .owner = frame.current_block->exprs.Add(*std::move(lowered)),
        .descent = {}};
  };
  const auto names_no_storage = []() -> diag::Result<AccessPath> {
    throw InternalError(
        "access path lowering: an expression that names no storage was named "
        "as a part");
  };
  return std::visit(
      Overloaded{
          [&](const hir::PrimaryExpr& p) -> diag::Result<AccessPath> {
            if constexpr (kProcedural) {
              // A property named bare is one of the object the method runs on
              // (LRM 8.4), and a write to it is opened on that object.
              if (const auto* property =
                      std::get_if<hir::ClassPropertyRef>(&p.data)) {
                return PropertyPath(
                    lowerer, frame,
                    frame.current_block->exprs.Add(MakeSelfRefExpr(
                        frame, frame.current_class->self_pointer_type)),
                    property->target, result_type);
              }
              return as_place(
                  LowerHirPrimaryExprProc(lowerer, frame, p.data, result_type));
            } else {
              return as_place(LowerHirPrimaryExprStructural(
                  lowerer, frame, p.data, result_type));
            }
          },
          [&](const hir::ElementSelectExpr& sel) -> diag::Result<AccessPath> {
            return LowerHirElementSelectExprPath(
                lowerer, frame, sel, result_type);
          },
          [&](const hir::RangeSelectExpr& sel) -> diag::Result<AccessPath> {
            return LowerHirRangeSelectExprPath(
                lowerer, frame, sel, result_type);
          },
          [&](const hir::MemberAccessExpr& sel) -> diag::Result<AccessPath> {
            return LowerHirMemberAccessExprPath(
                lowerer, frame, sel, result_type);
          },
          [&](const hir::ClassPropertyAccessExpr& sel)
              -> diag::Result<AccessPath> {
            return LowerHirClassPropertyAccessExprPath(
                lowerer, frame, sel, result_type);
          },
          [&](const hir::InterfaceMemberAccessExpr& sel)
              -> diag::Result<AccessPath> {
            return as_place(
                LowerHirInterfaceMemberAccessExpr(lowerer, frame, sel));
          },
          // A destructuring target is written as a whole: the join stands for
          // the run of destinations the source spelled, and each run reaches
          // its own place from inside it.
          [&](const hir::ConcatExpr& c) -> diag::Result<AccessPath> {
            return as_place(
                LowerHirConcatExpr(lowerer, frame, c, expr.type, result_type));
          },
          // A stream stands for a run of destinations too, but it does not
          // stand for a place: what fills each of them is a share of a
          // sequence of bits rather than a share of a value laid out like the
          // targets, so the assignment that consumes it distributes the shares
          // itself. Every context that can reach one does that; a context that
          // cannot is one where the assignment is not a statement of its own.
          [&](const hir::StreamingConcatExpr&) -> diag::Result<AccessPath> {
            return diag::Fail(
                expr.span, diag::DiagCode::kUnsupportedExpressionForm,
                "a streaming operator is not yet supported as the target of "
                "this kind of assignment (LRM 11.4.14.3)");
          },
          // The front end verifies that an assignment's target is an lvalue
          // whose every element can be assigned to, and refuses the program
          // otherwise, so a form arriving here that reaches no storage means a
          // target was lowered to something the source did not name.
          [&](const hir::UnaryExpr&) { return names_no_storage(); },
          [&](const hir::BinaryExpr&) { return names_no_storage(); },
          [&](const hir::ConditionalExpr&) { return names_no_storage(); },
          [&](const hir::AssignExpr&) { return names_no_storage(); },
          [&](const hir::IncDecExpr&) { return names_no_storage(); },
          [&](const hir::CallExpr&) { return names_no_storage(); },
          [&](const hir::ConversionExpr&) { return names_no_storage(); },
          [&](const hir::InterfaceInstanceAccessExpr&) {
            return names_no_storage();
          },
          [&](const hir::ValueRangeExpr&) { return names_no_storage(); },
          [&](const hir::InsideExpr&) { return names_no_storage(); },
          [&](const hir::ReplicationExpr&) { return names_no_storage(); },
          [&](const hir::AssignmentPatternExpr&) { return names_no_storage(); },
          [&](const hir::AssignmentPatternReplicationExpr&) {
            return names_no_storage();
          },
          [&](const hir::AssignmentPatternKeyedExpr&) {
            return names_no_storage();
          },
          [&](const hir::AssociativeAssignmentPatternExpr&) {
            return names_no_storage();
          },
          [&](const hir::DynamicArrayNewExpr&) { return names_no_storage(); },
          [&](const hir::ClassNewExpr&) { return names_no_storage(); },
          [&](const hir::TaggedUnionExpr&) { return names_no_storage(); },
          [&](const hir::DynamicCastExpr&) { return names_no_storage(); },
      },
      expr.data);
}

// The part `expr` names as the target of a write, shared by both pass classes:
// the path, with the checks the write owes before it lands appended where the
// statement is reached.
template <ExprLowerer L>
auto LowerLhsExprImpl(L& lowerer, const hir::Expr& expr, WalkFrame frame)
    -> diag::Result<AccessPath> {
  auto target = LowerAccessPathImpl(lowerer, expr, frame);
  if (!target) return std::unexpected(std::move(target.error()));
  AppendTagChecks(lowerer.Owner(), frame, *target);
  return target;
}

}  // namespace

auto ProcessLowerer::LowerExpr(const hir::Expr& expr, WalkFrame frame)
    -> diag::Result<mir::Expr> {
  return LowerExprImpl(*this, expr, frame);
}

auto ProcessLowerer::LowerAccessPath(const hir::Expr& expr, WalkFrame frame)
    -> diag::Result<AccessPath> {
  return LowerAccessPathImpl(*this, expr, frame);
}

auto ProcessLowerer::LowerLhsExpr(const hir::Expr& expr, WalkFrame frame)
    -> diag::Result<AccessPath> {
  return LowerLhsExprImpl(*this, expr, frame);
}

auto StructuralScopeLowerer::LowerExpr(
    const hir::Expr& expr, WalkFrame frame) const -> diag::Result<mir::Expr> {
  return LowerExprImpl(*this, expr, frame);
}

auto StructuralScopeLowerer::LowerAccessPath(
    const hir::Expr& expr, WalkFrame frame) const -> diag::Result<AccessPath> {
  return LowerAccessPathImpl(*this, expr, frame);
}

auto StructuralScopeLowerer::LowerLhsExpr(
    const hir::Expr& expr, WalkFrame frame) const -> diag::Result<AccessPath> {
  return LowerLhsExprImpl(*this, expr, frame);
}

}  // namespace lyra::lowering::hir_to_mir
