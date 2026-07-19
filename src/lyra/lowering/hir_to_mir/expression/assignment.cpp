#include "lyra/lowering/hir_to_mir/expression/assignment.hpp"

#include <algorithm>
#include <array>
#include <expected>
#include <optional>
#include <span>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/lowering/hir_to_mir/closure_builder.hpp"
#include "lyra/lowering/hir_to_mir/expression/operators.hpp"
#include "lyra/lowering/hir_to_mir/lhs_observable.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/services_call.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// Walks the LHS expression to determine whether its addressable root is a
// structural var (vs procedural local). NBA assignment requires a structural
// target; procedural-local NBA is a known gap.
auto IsExprRootedAtStructuralDataObject(
    const mir::Block& block, mir::ExprId expr_id) -> bool {
  const auto& expr = block.exprs.Get(expr_id);
  return std::visit(
      Overloaded{
          [](const mir::FieldAccessExpr&) { return true; },
          [](const mir::LocalRef&) { return false; },
          // A sealed endpoint reaches a structural cell through a borrowed
          // pointer stored on this object: a routed reference (an enclosing,
          // sibling, or cross-unit target) dereferences its slot member. The
          // root is structural when that pointer is, so recurse through it --
          // a `ref` formal, whose pointer roots at a local, stays non-
          // structural.
          [&](const mir::DerefExpr& d) {
            return IsExprRootedAtStructuralDataObject(block, d.pointer);
          },
          [&](const mir::TupleGetExpr& g) {
            return IsExprRootedAtStructuralDataObject(block, g.tuple);
          },
          [&](const mir::UnionGetRefExpr& g) {
            return IsExprRootedAtStructuralDataObject(block, g.union_value);
          },
          [&](const mir::CallExpr& c) {
            if (!mir::IsContainerAccessCallee(c.callee) ||
                c.arguments.empty()) {
              return false;
            }
            return IsExprRootedAtStructuralDataObject(
                block, c.arguments.front());
          },
          [&](const mir::ConcatExpr& c) {
            return std::ranges::all_of(c.operands, [&](mir::ExprId op) {
              return IsExprRootedAtStructuralDataObject(block, op);
            });
          },
          [](const auto&) { return false; },
      },
      expr.data);
}

// Rebuilds the selector layers above an NBA target's root cell onto a body-side
// reference to that cell. The navigation that reaches the cell is evaluated
// once at submit time and captured as `captured_root`; only the selector layers
// above it (element / range / struct-member access) are reproduced here, with
// their index subexpressions snapshotted by value so the body writes the place
// the statement named at submit time (LRM 10.4.2). The recursion bottoms out at
// the cell, which is the captured reference rather than a re-navigation from a
// receiver.
auto CloneLhsSelectorChainOntoRef(
    UnitLowerer& unit_lowerer, const WalkFrame& outer_frame,
    ClosureBuilder& closure, mir::ExprId outer_id, mir::ExprId root_id,
    mir::ExprId captured_root) -> mir::ExprId {
  if (outer_id == root_id) {
    return captured_root;
  }
  const mir::Block& outer_block = *outer_frame.current_block;
  mir::Block& body = closure.Body();
  const auto& outer_expr = outer_block.exprs.Get(outer_id);
  return std::visit(
      Overloaded{
          [&](const mir::CallExpr& c) -> mir::ExprId {
            if (!mir::IsContainerAccessCallee(c.callee) ||
                c.arguments.empty()) {
              throw InternalError(
                  "CloneLhsSelectorChainOntoRef: CallExpr above the NBA target "
                  "root is not a container access");
            }
            // Copy the callee, argument ids, and result type up front: the
            // recursion and `SnapshotIntoClosure` below append to
            // `outer_block`, which can reallocate its expression storage and
            // dangle the `outer_expr` reference `c` is bound to.
            const auto callee = c.callee;
            const mir::TypeId call_type = outer_expr.type;
            const std::vector<mir::ExprId> src_args(
                c.arguments.begin(), c.arguments.end());
            std::vector<mir::ExprId> body_args;
            body_args.reserve(src_args.size());
            body_args.push_back(CloneLhsSelectorChainOntoRef(
                unit_lowerer, outer_frame, closure, src_args.front(), root_id,
                captured_root));
            for (std::size_t i = 1; i < src_args.size(); ++i) {
              body_args.push_back(SnapshotIntoClosure(
                  unit_lowerer, outer_frame, closure, src_args[i],
                  "_lyra_nba_arg"));
            }
            return body.exprs.Add(
                mir::Expr{
                    .data =
                        mir::CallExpr{
                            .callee = callee,
                            .arguments = std::move(body_args)},
                    .type = call_type});
          },
          [&](const mir::TupleGetExpr& g) -> mir::ExprId {
            const auto index = g.index;
            const mir::TypeId type = outer_expr.type;
            const mir::ExprId base = CloneLhsSelectorChainOntoRef(
                unit_lowerer, outer_frame, closure, g.tuple, root_id,
                captured_root);
            return body.exprs.Add(
                mir::Expr{
                    .data = mir::TupleGetExpr{.tuple = base, .index = index},
                    .type = type});
          },
          [&](const mir::UnionGetRefExpr& g) -> mir::ExprId {
            const auto index = g.index;
            const mir::TypeId type = outer_expr.type;
            const mir::ExprId base = CloneLhsSelectorChainOntoRef(
                unit_lowerer, outer_frame, closure, g.union_value, root_id,
                captured_root);
            return body.exprs.Add(
                mir::Expr{
                    .data =
                        mir::UnionGetRefExpr{
                            .union_value = base, .index = index},
                    .type = type});
          },
          [&](const auto&) -> mir::ExprId {
            throw InternalError(
                "CloneLhsSelectorChainOntoRef: unexpected node above the NBA "
                "target root");
          },
      },
      outer_expr.data);
}

// Axis B (timing), deferred half: build the closure the NBA region invokes.
// `effect_fn` is the target's write effect (axis A); this envelope is the only
// part that knows about deferral. The closure is receiver-less: it captures a
// reference to the target cell -- the navigation to the cell is evaluated now,
// in the active region, and frozen into that reference -- plus the operand
// snapshots (the active-region values, LRM 10.4.2), and takes its execution
// context as a `services` parameter the NBA region supplies on invocation.
// `effect_fn` then builds the write against those body-side nodes -- the same
// call it would build for a blocking write, only in the closure body.
template <typename EffectFn>
auto BuildDeferredAssignClosure(
    UnitLowerer& unit_lowerer, WalkFrame frame, mir::ExprId target_in_outer,
    std::span<const mir::ExprId> operands_in_outer, EffectFn effect_fn)
    -> mir::Expr {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Block& outer_block = *frame.current_block;

  ClosureBuilder closure(unit, frame);
  mir::Block& body = closure.Body();

  const mir::LocalId services_binding =
      closure.AddParamAnonymous("services", unit.builtins.services);
  const mir::ExprId services_param = body.exprs.Add(
      mir::MakeLocalRefExpr(services_binding, unit.builtins.services));

  const mir::ExprId root_in_outer =
      FindLhsRootId(unit, outer_block, target_in_outer);
  const mir::ExprId place_ref = BuildReferenceArg(
      unit, outer_block, root_in_outer,
      outer_block.exprs.Get(root_in_outer).type);
  const mir::ExprId captured_root = SnapshotIntoClosure(
      unit_lowerer, frame, closure, place_ref, "_lyra_nba_place");

  const mir::ExprId body_target = CloneLhsSelectorChainOntoRef(
      unit_lowerer, frame, closure, target_in_outer, root_in_outer,
      captured_root);

  std::vector<mir::ExprId> body_operands;
  body_operands.reserve(operands_in_outer.size());
  for (const mir::ExprId op : operands_in_outer) {
    body_operands.push_back(
        SnapshotIntoClosure(unit_lowerer, frame, closure, op, "_lyra_nba_arg"));
  }

  const mir::ExprId effect_id = body.exprs.Add(effect_fn(
      body, body_target, std::span<const mir::ExprId>(body_operands),
      services_param));
  body.AppendStmt(mir::ExprStmt{.expr = effect_id});
  return closure.BuildVoid();
}

// Axis B (timing): apply a target's write effect now (blocking) or deferred to
// the NBA region (nonblocking). `effect_fn(block, target, operands, services)`
// builds the write into `block`; this is the only place the blocking/deferred
// choice lives, so every target shares one timing envelope (LRM 10.4).
template <typename EffectFn>
auto ApplyAssignEffect(
    ProcessLowerer& process, WalkFrame frame, hir::AssignKind kind,
    diag::SourceSpan span, mir::ExprId target_in_outer,
    std::span<const mir::ExprId> operands_in_outer, EffectFn effect_fn)
    -> diag::Result<mir::Expr> {
  auto& block = *frame.current_block;
  // Only an observable-cell write notifies subscribers, so only it reaches a
  // services value; a plain-local write carries none.
  const bool target_is_observable_cell =
      LhsRootIsObservableCell(process.Owner().Unit(), block, target_in_outer);
  // Firing subscribers needs the runtime services, reached through the
  // callable's `self`. A receiver-less callable (a package function or task,
  // LRM 26.3) has no `self`, so it cannot yet write an observable cell -- which
  // for it is a package variable (LRM 26.2). Reading one is fine (a read needs
  // no services); writing awaits threading the services into a receiver-less
  // callable. Reject cleanly here rather than reach for a receiver that is not
  // there.
  if (target_is_observable_cell && frame.current_class == nullptr) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedAssignmentTarget,
        "writing a package variable from a package subroutine is not yet "
        "supported");
  }
  if (kind == hir::AssignKind::kBlocking) {
    const std::optional<mir::ExprId> services_id =
        target_is_observable_cell
            ? std::optional{block.exprs.Add(
                  BuildServicesCallExpr(process.Owner(), frame))}
            : std::nullopt;
    return effect_fn(block, target_in_outer, operands_in_outer, services_id);
  }
  if (!IsExprRootedAtStructuralDataObject(block, target_in_outer)) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedAssignmentTarget,
        "non-blocking assignment to procedural local is not supported yet");
  }
  mir::Expr closure = BuildDeferredAssignClosure(
      process.Owner(), frame, target_in_outer, operands_in_outer, effect_fn);
  const mir::ExprId closure_id = block.exprs.Add(std::move(closure));
  const mir::ExprId services_id =
      block.exprs.Add(BuildServicesCallExpr(process.Owner(), frame));
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Direct{.target = support::BuiltinFn::kSubmitNba},
              .arguments = {services_id, closure_id}},
      .type = process.Owner().Unit().builtins.void_type};
}

// Axis A for an observable / local target: a whole-cell `Set`, a selector-chain
// `Mutate`, or a plain local assign -- `BuildObservableAssignExpr` picks by the
// target's shape. Timing-agnostic: the same effect serves blocking and NBA.
auto LowerObservableAssign(
    ProcessLowerer& process, WalkFrame frame, const hir::AssignExpr& a,
    diag::SourceSpan span, mir::TypeId result_type) -> diag::Result<mir::Expr> {
  const auto& hir_process = process.HirBody();
  auto& block = *frame.current_block;

  auto rhs_or = process.LowerExpr(hir_process.exprs.Get(a.rhs), frame);
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
  const mir::ExprId rhs_id = block.exprs.Add(*std::move(rhs_or));
  auto lhs_or = process.LowerLhsExpr(
      hir_process.exprs.Get(a.lhs), frame.WithLvalueTarget(true));
  if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
  const mir::ExprId lhs_id = block.exprs.Add(*std::move(lhs_or));

  const std::optional<mir::BinaryOp> compound_op =
      a.compound_op.has_value() ? std::optional{LowerBinaryOp(*a.compound_op)}
                                : std::nullopt;
  const mir::TypeId void_type = process.Owner().Unit().builtins.void_type;
  const std::array<mir::ExprId, 1> operands{rhs_id};
  return ApplyAssignEffect(
      process, frame, a.kind, span, lhs_id, operands,
      [&](mir::Block& blk, mir::ExprId target, std::span<const mir::ExprId> ops,
          std::optional<mir::ExprId> services) -> mir::Expr {
        return BuildObservableAssignExpr(
            process.Owner().Unit(), blk, services, target, ops[0], compound_op,
            result_type, void_type);
      });
}

}  // namespace

auto LowerHirAssignExprProc(
    ProcessLowerer& process, WalkFrame frame, const hir::AssignExpr& a,
    diag::SourceSpan span, mir::TypeId result_type) -> diag::Result<mir::Expr> {
  if (a.compound_op.has_value() && a.kind == hir::AssignKind::kNonBlocking) {
    throw InternalError(
        "LowerHirAssignExprProc: compound assignment with non-blocking kind "
        "is not a legal SV form (LRM A.6.2 grammar)");
  }

  // Every target -- whole var, array / string element, struct / union member --
  // lowers to one shape: the LHS is an op=-able write location, and the write
  // is a single `AssignExpr{target, compound_op?, value}`. "Evaluate the
  // left-hand side once" (LRM 11.4.1) is the backend's job on that single
  // target. The blocking vs deferred (NBA) choice is the timing envelope
  // inside.
  return LowerObservableAssign(process, frame, a, span, result_type);
}

// Builds the deferred-write closure for an observable assignment, used by the
// LHS-destructuring desugar where each part is a separate NBA submit. The
// general timing envelope (`ApplyAssignEffect`) does not fit there because
// destructuring submits per part inside its own block, so this exposes just
// the closure-building half over the observable write effect.
auto BuildNbaSubmitClosureExpr(
    UnitLowerer& unit_lowerer, WalkFrame frame, mir::ExprId lhs_in_outer,
    mir::ExprId rhs_id_in_outer, mir::TypeId rhs_type) -> mir::Expr {
  const std::array<mir::ExprId, 1> operands{rhs_id_in_outer};
  return BuildDeferredAssignClosure(
      unit_lowerer, frame, lhs_in_outer, operands,
      [&](mir::Block& blk, mir::ExprId target, std::span<const mir::ExprId> ops,
          std::optional<mir::ExprId> services) -> mir::Expr {
        return BuildObservableAssignExpr(
            unit_lowerer.Unit(), blk, services, target, ops[0], std::nullopt,
            rhs_type, unit_lowerer.Unit().builtins.void_type);
      });
}

}  // namespace lyra::lowering::hir_to_mir
