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
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/closure_builder.hpp"
#include "lyra/lowering/hir_to_mir/expression/operators.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/lowering/hir_to_mir/lhs_store.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// Whether a deferred update may hold a reference to what this target names
// until the update region runs (LRM 10.4.2). The storage a place descends from
// decides it: a member of an object, a type-associated cell, and another
// unit's namespace variable all outlive the stretch that submits the update,
// while a procedural local's does not. Every descent step is transparent -- it
// reaches part of the same storage -- so the answer is the root's.
//
// Every form a target can take answers for itself. A form that is not a place
// cannot be one, so reaching the fallback means the target lowering produced
// something that is not an assignment target at all.
auto TargetOutlivesDeferredUpdate(const mir::Block& block, mir::ExprId expr_id)
    -> bool {
  const auto& expr = block.exprs.Get(expr_id);
  return std::visit(
      Overloaded{
          // A field reached through a pointer is storage of its own and
          // outlives the update. A structural product's component is not: it
          // lives exactly as long as the value holding it, so the question
          // passes to the receiver.
          [&](const mir::FieldAccessExpr& m) {
            if (!std::holds_alternative<mir::ComponentTarget>(m.field)) {
              return true;
            }
            return TargetOutlivesDeferredUpdate(block, m.receiver);
          },
          [](const mir::StaticPropertyRef&) { return true; },
          [](const mir::ExternalStaticPropertyRef&) { return true; },
          [](const mir::ExternalUnitVariableRef&) { return true; },
          [](const mir::LocalRef&) { return false; },
          // A sealed endpoint reaches a structural cell through a borrowed
          // pointer stored on this object: a routed reference (an enclosing,
          // sibling, or cross-unit target) dereferences its slot member. The
          // root is structural when that pointer is, so recurse through it --
          // a `ref` formal, whose pointer roots at a local, stays non-
          // structural.
          [&](const mir::DerefExpr& d) {
            return TargetOutlivesDeferredUpdate(block, d.pointer);
          },
          // A join stands for the destructuring LHS it came from, which writes
          // each run through that run's own root, so the whole outlives the
          // update exactly when every run does. Any other call in target
          // position names a place through its receiver, its first argument.
          [&](const mir::CallExpr& c) {
            if (mir::DirectBuiltinFn(c) == support::BuiltinFn::kConcat) {
              return std::ranges::all_of(c.arguments, [&](mir::ExprId op) {
                return TargetOutlivesDeferredUpdate(block, op);
              });
            }
            return !c.arguments.empty() &&
                   TargetOutlivesDeferredUpdate(block, c.arguments[0]);
          },
          [](const auto&) -> bool {
            throw InternalError(
                "TargetOutlivesDeferredUpdate: the assignment target is not a "
                "place; the target lowering should have produced one -- please "
                "report this as a bug");
          },
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
          // An access above the root: its receiver is rebuilt onto the
          // body-side
          // reference and its coordinates are snapshotted by value, so the body
          // writes the part the statement named at submit time. Copy the call
          // up front -- the recursion and snapshots below append to
          // `outer_block`, which can reallocate and dangle `outer_expr`.
          [&](const mir::CallExpr& c) -> mir::ExprId {
            const mir::TypeId type = outer_expr.type;
            mir::CallExpr rebuilt = c;
            rebuilt.arguments.front() = CloneLhsSelectorChainOntoRef(
                unit_lowerer, outer_frame, closure, rebuilt.arguments.front(),
                root_id, captured_root);
            for (mir::ExprId& coordinate :
                 std::span(rebuilt.arguments).subspan(1)) {
              coordinate = SnapshotIntoClosure(
                  unit_lowerer, outer_frame, closure, coordinate,
                  "_lyra_nba_arg");
            }
            return body.exprs.Add(
                mir::Expr{.data = std::move(rebuilt), .type = type});
          },
          // A field above the root names no coordinates to snapshot, so only
          // its receiver is rebuilt.
          [&](const mir::FieldAccessExpr& m) -> mir::ExprId {
            mir::FieldAccessExpr rebuilt = m;
            const mir::TypeId type = outer_expr.type;
            rebuilt.receiver = CloneLhsSelectorChainOntoRef(
                unit_lowerer, outer_frame, closure, rebuilt.receiver, root_id,
                captured_root);
            return body.exprs.Add(
                mir::Expr{.data = std::move(rebuilt), .type = type});
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
// part that knows about deferral. The closure is receiver-less and takes no
// arguments: it captures a reference to the target cell -- the navigation to
// the cell is evaluated now, in the active region, and frozen into that
// reference -- plus the operand snapshots (the active-region values, LRM
// 10.4.2). `effect_fn` then builds the write against those body-side nodes --
// the same node it would build for a blocking write, only in the closure body.
template <typename EffectFn>
auto BuildDeferredAssignClosure(
    UnitLowerer& unit_lowerer, WalkFrame frame, mir::ExprId target_in_outer,
    std::span<const mir::ExprId> operands_in_outer, EffectFn effect_fn)
    -> mir::Expr {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Block& outer_block = *frame.current_block;

  ClosureBuilder closure(unit, frame);
  mir::Block& body = closure.Body();

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
      body, body_target, std::span<const mir::ExprId>(body_operands)));
  body.AppendStmt(mir::ExprStmt{.expr = effect_id});
  return closure.BuildVoid();
}

// The NBA commit of `closure_id`, into the region of the slot the assignment
// names. Without an intra-assignment delay that is the slot the statement is
// reached in; with one it is the slot that delay reaches, and the amount
// crosses unscaled with its scope's powers because LRM 9.4.1 reads a delay
// expression's own value before any scaling.
auto BuildNbaSubmitCall(
    ProcessLowerer& process, WalkFrame frame,
    const hir::NonBlockingAssign& deferred, mir::ExprId runtime_id,
    mir::ExprId closure_id) -> diag::Result<mir::Expr> {
  auto& unit = process.Owner().Unit();
  auto& block = *frame.current_block;
  if (!deferred.delay.has_value()) {
    return mir::Expr{
        .data =
            mir::CallExpr{
                .callee = mir::Direct{.target = support::BuiltinFn::kSubmitNba},
                .arguments = {runtime_id, closure_id}},
        .type = unit.builtins.void_type};
  }
  auto duration_or =
      process.LowerExpr(process.HirBody().exprs.Get(*deferred.delay), frame);
  if (!duration_or) return std::unexpected(std::move(duration_or.error()));
  mir::ExprId duration_id = block.exprs.Add(*std::move(duration_or));
  const mir::Type& duration_type =
      unit.types.Get(block.exprs.Get(duration_id).type);
  const bool is_real = duration_type.IsRealFamily();
  if (duration_type.Is<mir::ShortRealType>()) {
    // LRM 6.12.1: `real` and `realtime` are one type, and a `shortreal` differs
    // from them only in host precision, so the entry takes the wider.
    duration_id =
        ConvertToType(unit, block, duration_id, unit.builtins.realtime);
  }
  const mir::ExprId unit_power_id = BuildIntLiteral(
      unit, block, static_cast<std::int64_t>(process.Resolution().unit_power));
  const mir::ExprId precision_power_id = BuildIntLiteral(
      unit, block,
      static_cast<std::int64_t>(process.Resolution().precision_power));
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Direct{
                      .target = is_real
                                    ? support::BuiltinFn::kSubmitNbaAfterReal
                                    : support::BuiltinFn::kSubmitNbaAfter},
              .arguments =
                  {runtime_id, duration_id, unit_power_id, precision_power_id,
                   closure_id}},
      .type = unit.builtins.void_type};
}

// Axis B (timing): apply a target's write effect now (blocking) or deferred to
// the NBA region (nonblocking). `effect_fn(block, target, operands)` builds the
// write into `block`; this is the only place the blocking/deferred choice
// lives, so every target shares one timing envelope (LRM 10.4).
template <typename EffectFn>
auto ApplyAssignEffect(
    ProcessLowerer& process, WalkFrame frame, const hir::AssignKind& kind,
    diag::SourceSpan span, mir::ExprId target_in_outer,
    std::span<const mir::ExprId> operands_in_outer, EffectFn effect_fn)
    -> diag::Result<mir::Expr> {
  auto& block = *frame.current_block;
  const auto* deferred = std::get_if<hir::NonBlockingAssign>(&kind);
  if (deferred == nullptr) {
    return effect_fn(block, target_in_outer, operands_in_outer);
  }
  // The update runs after the stretch that submitted it returns, and it holds a
  // reference to the target's storage until then, so that storage has to
  // outlive the stretch. LRM 10.4.2 makes the case that fails this illegal --
  // "It shall be illegal to make nonblocking assignments to automatic
  // variables" -- and the front end rejects it, so this stands behind that
  // rather than in front of it: reaching it means a target was lowered to
  // storage the source did not name.
  if (!TargetOutlivesDeferredUpdate(block, target_in_outer)) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedAssignmentTarget,
        "a nonblocking assignment names storage that does not outlive the "
        "statement submitting it (LRM 10.4.2)");
  }
  mir::Expr closure = BuildDeferredAssignClosure(
      process.Owner(), frame, target_in_outer, operands_in_outer, effect_fn);
  const mir::ExprId closure_id = block.exprs.Add(std::move(closure));
  const mir::ExprId runtime_id =
      block.exprs.Add(BuildCurrentRuntimeCallExpr(process.Owner()));
  return BuildNbaSubmitCall(process, frame, *deferred, runtime_id, closure_id);
}

// Axis A: the store itself, against the storage the target designates.
// Timing-agnostic: the same effect serves blocking and NBA.
auto LowerObservableAssign(
    ProcessLowerer& process, WalkFrame frame, const hir::AssignExpr& a,
    diag::SourceSpan span, mir::TypeId result_type) -> diag::Result<mir::Expr> {
  const auto& hir_process = process.HirBody();
  auto& block = *frame.current_block;

  auto rhs_or = process.LowerExpr(hir_process.exprs.Get(a.rhs), frame);
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
  const mir::ExprId rhs_id = block.exprs.Add(*std::move(rhs_or));
  auto lhs_or = process.LowerLhsExpr(hir_process.exprs.Get(a.lhs), frame);
  if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
  const mir::ExprId lhs_id = block.exprs.Add(*std::move(lhs_or));

  const std::optional<mir::BinaryOp> compound_op =
      a.compound_op.has_value() ? std::optional{LowerBinaryOp(*a.compound_op)}
                                : std::nullopt;
  const std::array<mir::ExprId, 1> operands{rhs_id};
  return ApplyAssignEffect(
      process, frame, a.kind, span, lhs_id, operands,
      [&](mir::Block& blk, mir::ExprId target,
          std::span<const mir::ExprId> ops) -> mir::Expr {
        return BuildStoreExpr(
            process.Owner().Unit(), blk, target, ops[0], compound_op,
            result_type);
      });
}

}  // namespace

auto LowerHirAssignExprProc(
    ProcessLowerer& process, WalkFrame frame, const hir::AssignExpr& a,
    diag::SourceSpan span, mir::TypeId result_type) -> diag::Result<mir::Expr> {
  if (a.compound_op.has_value() &&
      std::holds_alternative<hir::NonBlockingAssign>(a.kind)) {
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
      [&](mir::Block& blk, mir::ExprId target,
          std::span<const mir::ExprId> ops) -> mir::Expr {
        return BuildStoreExpr(
            unit_lowerer.Unit(), blk, target, ops[0], std::nullopt, rhs_type);
      });
}

}  // namespace lyra::lowering::hir_to_mir
