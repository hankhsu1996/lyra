#include <algorithm>
#include <concepts>
#include <optional>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/failure_context.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/lowering/hir_to_mir/binding_origin.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
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
#include "lyra/lowering/hir_to_mir/sensitivity_wait.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_builders.hpp"

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

// The one expression dispatcher, shared by both pass classes. An expression's
// meaning does not depend on whether a process body or a structural scope
// encloses it, so every context-free kind routes to one shared handler listed
// exactly once -- a kind cannot be wired in one context and forgotten in the
// other. The one real difference is parameterized inline: a bare name resolves
// to different storage per scope. What an observable cell holds is named by
// reading the cell, so the result is value-typed. An expression naming storage
// comes out as that place, unread, as rustc's `as_place` leaves it: an access
// to a part of it reaches the part where it lies.
template <ExprLowerer L>
auto LowerAsPlaceImpl(L& lowerer, const hir::Expr& expr, WalkFrame frame)
    -> diag::Result<mir::Expr> {
  constexpr bool kProcedural = std::same_as<L, ProcessLowerer>;
  const diag::FailureContext at(expr.span);
  const mir::TypeId result_type = lowerer.Owner().TranslateType(expr.type);
  if (IsTagCheckedRead(lowerer, frame, expr)) {
    return LowerTagCheckedRead(lowerer, frame, expr);
  }
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
            return LowerHirAssignExpr(
                lowerer, frame, a, expr.span, result_type);
          },
          [&](const hir::IncDecExpr& inc) -> diag::Result<mir::Expr> {
            return LowerHirIncDecExpr(lowerer, frame, inc, result_type);
          },
          [&](const hir::ConversionExpr& cv) -> diag::Result<mir::Expr> {
            return LowerHirConversionExpr(lowerer, frame, cv, result_type);
          },
          // A settled constant is the value that states it.
          [&](const hir::ConstantExpr& c) -> diag::Result<mir::Expr> {
            return lowerer.LowerExpr(lowerer.HirExprs().Get(c.value), frame);
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
            return LowerHirConcatExpr(lowerer, frame, c, result_type);
          },
          [&](const hir::StreamingConcatExpr& s) -> diag::Result<mir::Expr> {
            return LowerHirStreamingConcatExpr(lowerer, frame, s, result_type);
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
                lowerer, frame, a, result_type);
          },
          [&](const hir::ClassNewExpr& n) -> diag::Result<mir::Expr> {
            // `new` allocates an object and initializes it by its constructor,
            // to which the actuals flow; the handle owns it from there.
            std::vector<mir::ExprId> args;
            args.reserve(n.arguments.size() + 1);
            // A class declared in a structural scope is a type of that scope's
            // instance (LRM 6.22), so the object records which instance it
            // belongs to and construction is where that arrives -- ahead of
            // the source actuals, the way every construction prefix does.
            if (n.declaring_instance.has_value()) {
              args.push_back(BuildImplicitInstanceArgument(
                  lowerer, frame, *n.declaring_instance));
            }
            for (const hir::ExprId arg_hid : n.arguments) {
              auto arg_or =
                  lowerer.LowerExpr(lowerer.HirExprs().Get(arg_hid), frame);
              if (!arg_or) return std::unexpected(std::move(arg_or.error()));
              args.push_back(
                  frame.current_block->exprs.Add(*std::move(arg_or)));
            }
            args.push_back(BuildReportHandedOn(lowerer.Owner().Unit(), frame));
            const mir::TypeId object_type =
                *mir::AllocatedType(lowerer.Owner().Unit().types, result_type);
            return mir::MakeNewExpr(
                frame.current_block->exprs.Add(
                    mir::MakeObjectConstructionExpr(
                        object_type, std::move(args))),
                result_type);
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
  const mir::Type& raw_type = lowerer.Owner().Unit().types.Get(raw_or->type);
  if (raw_type.IsCapabilityWrapper()) {
    // What the wrapper holds is the type of the storage it names, which for a
    // reference to a value of any type is no type a value has, as `*r` of
    // Rust's `&mut dyn Any` is `dyn Any` (LRM 15.4.9).
    const mir::TypeId held = raw_type.WrappedValueType();
    const mir::ExprId cell_id =
        frame.current_block->exprs.Add(*std::move(raw_or));
    // The sampled value of an expression is the expression over the sampled
    // values of the variables it reads (LRM 16.5.1), so the whole of what
    // reading one changes is which value each leaf answers with -- here, where
    // every leaf read is built.
    if (frame.reads_as_of == ReadsAsOf::kPreponed &&
        KeepsSampledValue(lowerer, expr)) {
      return frame.current_block->SampledWrapperContents(
          lowerer.Owner().Unit().types, cell_id, held);
    }
    // Reading what the wrapper holds names the storage it represents, which
    // is then read where a value is wanted.
    return frame.current_block->WrapperContents(
        lowerer.Owner().Unit().types, cell_id, held);
  }
  return raw_or;
}

// The same expression where a value is wanted: a place standing there is read,
// as Sema wraps an lvalue operand in its lvalue-to-rvalue conversion.
template <ExprLowerer L>
auto LowerExprImpl(L& lowerer, const hir::Expr& expr, WalkFrame frame)
    -> diag::Result<mir::Expr> {
  auto lowered = LowerAsPlaceImpl(lowerer, expr, frame);
  if (!lowered) {
    return lowered;
  }
  return frame.current_block->AsValue(
      lowerer.Owner().Unit().types, *std::move(lowered));
}

// The dispatcher for an expression named as a part rather than read, shared by
// both pass classes. Addressable kinds only, and no cell is asked for what it
// holds, so an observable-cell leaf flows out as the bare cell. It peels rather
// than composes: a kind that reaches a part of a value adds one step to the
// descent and recurses, and every other kind is the place the descent bottoms
// out in. What it answers with is only a statement of which part is named: the
// part is neither read nor written here, and the one thing appended is the
// binding of a value the path is reached through where the path takes it more
// than once.
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
                    frame.current_block->exprs.Add(MakeSelfRefExpr(frame)),
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
          // The front end verifies that an assignment's target is an lvalue
          // whose every element can be assigned to, and refuses the program
          // otherwise, so a form arriving here that reaches no storage means a
          // target was lowered to something the source did not name. A join of
          // lvalues (LRM A.8.5) stands for several places and is not one: a
          // write takes it as an lvalue, and what asks for one place here -- a
          // `ref` actual, a receiver -- is something the front end refuses a
          // join for.
          [&](const hir::ConcatExpr&) { return names_no_storage(); },
          [&](const hir::StreamingConcatExpr&) { return names_no_storage(); },
          [&](const hir::UnaryExpr&) { return names_no_storage(); },
          [&](const hir::BinaryExpr&) { return names_no_storage(); },
          [&](const hir::ConditionalExpr&) { return names_no_storage(); },
          [&](const hir::AssignExpr&) { return names_no_storage(); },
          [&](const hir::IncDecExpr&) { return names_no_storage(); },
          [&](const hir::CallExpr&) { return names_no_storage(); },
          [&](const hir::ConversionExpr&) { return names_no_storage(); },
          [&](const hir::ConstantExpr&) { return names_no_storage(); },
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

// An expression whose value nothing reads, shared by both pass classes. A write
// is then the write alone, as clang's `EmitIgnoredExpr` emits an assignment
// without its result; only where the source reads one does the lowering hold
// what it stored. Every other expression is its value, discarded.
template <ExprLowerer L>
auto LowerIgnoredExprImpl(L& lowerer, const hir::Expr& expr, WalkFrame frame)
    -> diag::Result<mir::Expr> {
  const diag::FailureContext at(expr.span);
  if (const auto* assign = std::get_if<hir::AssignExpr>(&expr.data)) {
    return LowerHirAssignWrite(lowerer, frame, *assign, expr.span);
  }
  if (const auto* inc = std::get_if<hir::IncDecExpr>(&expr.data)) {
    return LowerHirIncDecWrite(lowerer, frame, *inc);
  }
  return LowerExprImpl(lowerer, expr, frame);
}

// The part `expr` names as the target of a write, shared by both pass classes:
// the path, with whatever the write names more than once evaluated where the
// statement is reached.
template <ExprLowerer L>
auto LowerLhsExprImpl(L& lowerer, const hir::Expr& expr, WalkFrame frame)
    -> diag::Result<AccessPath> {
  auto target = LowerAccessPathImpl(lowerer, expr, frame);
  if (!target) return std::unexpected(std::move(target.error()));
  SettleForTagChecks(lowerer.Owner(), frame, *target);
  // A write to bits of a character reads the character before putting it back,
  // so the target is settled once for both (LRM 11.5.1, 6.16.2).
  if (WritesBitsOfACharacter(*target)) {
    *target = Settled(lowerer.Owner(), frame, *std::move(target));
  }
  return target;
}

// Whether `expr` is an argument of a call `frame` lowers after its statement,
// which that statement evaluates (LRM 16.4). The lowering is handed the node
// rather than its id, so an operand is recognized by the node's address.
auto IsArgumentEvaluatedEarlier(
    const ProcessLowerer& lowerer, const hir::Expr& expr,
    const WalkFrame& frame) -> bool {
  if (frame.arguments_evaluated_earlier == nullptr) {
    return false;
  }
  return std::ranges::any_of(
      frame.arguments_evaluated_earlier->operands, [&](hir::ExprId operand) {
        return &lowerer.HirBody().exprs.Get(operand) == &expr;
      });
}

// Where the argument `expr` lies as the statement evaluated it: lowered where
// the statement is, held in a local there, and carried into the body the call
// is made in, as a C++ lambda's init-capture is.
auto ArgumentEvaluatedEarlier(
    ProcessLowerer& lowerer, const hir::Expr& expr, const WalkFrame& frame)
    -> diag::Result<mir::Expr> {
  const WalkFrame& statement = *frame.arguments_evaluated_earlier->statement;
  auto value = lowerer.LowerExpr(expr, statement);
  if (!value) return std::unexpected(std::move(value.error()));
  mir::Block& at = *statement.current_block;
  const mir::ExprId held =
      at.AsValue(lowerer.Owner().Unit().types, at.exprs.Add(*std::move(value)));
  const BindingOriginId origin = SynthesizedOrigin{
      .owner_site = lowerer.Owner().NextSynthesizedSite(), .ordinal = 0};
  const mir::LocalId local =
      statement.bindings->Declare(origin, at.exprs.Get(held).type);
  at.AppendStmt(mir::LocalDeclStmt{.target = local, .init = held});
  return frame.bindings->MakeBindingPlace(
      frame.bindings->EnsureCarrier(origin), *frame.current_block);
}

}  // namespace

auto ProcessLowerer::LowerExpr(const hir::Expr& expr, WalkFrame frame)
    -> diag::Result<mir::Expr> {
  if (IsArgumentEvaluatedEarlier(*this, expr, frame)) {
    auto place = ArgumentEvaluatedEarlier(*this, expr, frame);
    if (!place) return std::unexpected(std::move(place.error()));
    return frame.current_block->AsValue(
        Owner().Unit().types, *std::move(place));
  }
  return LowerExprImpl(*this, expr, frame);
}

auto ProcessLowerer::LowerAsPlace(const hir::Expr& expr, WalkFrame frame)
    -> diag::Result<mir::Expr> {
  if (IsArgumentEvaluatedEarlier(*this, expr, frame)) {
    return ArgumentEvaluatedEarlier(*this, expr, frame);
  }
  return LowerAsPlaceImpl(*this, expr, frame);
}

auto ProcessLowerer::LowerIgnoredExpr(const hir::Expr& expr, WalkFrame frame)
    -> diag::Result<mir::Expr> {
  return LowerIgnoredExprImpl(*this, expr, frame);
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

auto StructuralScopeLowerer::LowerAsPlace(
    const hir::Expr& expr, WalkFrame frame) const -> diag::Result<mir::Expr> {
  return LowerAsPlaceImpl(*this, expr, frame);
}

auto StructuralScopeLowerer::LowerIgnoredExpr(
    const hir::Expr& expr, WalkFrame frame) const -> diag::Result<mir::Expr> {
  return LowerIgnoredExprImpl(*this, expr, frame);
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
