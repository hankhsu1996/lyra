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
#include "lyra/lowering/hir_to_mir/deferred_effect.hpp"
#include "lyra/lowering/hir_to_mir/expression/operators.hpp"
#include "lyra/lowering/hir_to_mir/lhs_store.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
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
          // A field is storage of its own, reached through a pointer, so it
          // outlives the update whatever holds that pointer.
          [](const mir::FieldAccessExpr&) { return true; },
          // A name reaches storage of one of two durations: a body's own
          // binding, which goes away when the stretch that holds it returns,
          // and everything a compilation unit declares once -- a
          // type-associated cell, a namespace variable, a generated descriptor
          // -- which the whole program shares and so outlives any stretch.
          [](const mir::ReferenceExpr& r) {
            return std::visit(
                Overloaded{
                    [](const mir::LocalRef&) { return false; },
                    [](const mir::StaticConstantRef&) { return true; },
                    [](const mir::StaticPropertyRef&) { return true; },
                    [](const mir::PackedTypeRef&) { return true; },
                    [](const mir::FunctionRef&) { return true; },
                    [](const mir::ExternalUnitVariableRef&) { return true; },
                    [](const mir::ExternalStaticPropertyRef&) { return true; },
                },
                r.target);
          },
          // A sealed endpoint reaches a structural cell through a borrowed
          // pointer stored on this object: a routed reference (an enclosing,
          // sibling, or cross-unit target) dereferences its slot member. The
          // root is structural when that pointer is, so recurse through it --
          // a `ref` formal, whose pointer roots at a local, stays non-
          // structural.
          [&](const mir::DerefExpr& d) {
            return TargetOutlivesDeferredUpdate(block, d.pointer);
          },
          // A call in target position names a place through the object it
          // dispatches on. A join stands for the destructuring LHS it came
          // from, which writes each run through that run's own root, so the
          // whole outlives the update exactly when every run does.
          [&](const mir::CallExpr& c) {
            const std::optional<mir::ExprId> receiver =
                mir::CalleeReceiver(c.callee);
            if (!receiver.has_value()) {
              return false;
            }
            if (!TargetOutlivesDeferredUpdate(block, *receiver)) {
              return false;
            }
            if (mir::DirectBuiltinFn(c) != support::BuiltinFn::kConcat) {
              return true;
            }
            return std::ranges::all_of(c.arguments, [&](mir::ExprId op) {
              return TargetOutlivesDeferredUpdate(block, op);
            });
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

// Rebuilds a target's descent onto a body-side reference to its owner. The
// navigation that reaches the owner is evaluated
// once at submit time and captured as `captured_owner`; the descent above it
// is restated over that capture with every coordinate snapshotted by value, so
// the body writes the part the statement named at submit time (LRM 10.4.2). A
// target that designates nothing is the captured reference itself.
auto FreezeTarget(
    UnitLowerer& unit_lowerer, const WalkFrame& outer_frame,
    ClosureBuilder& closure, const WriteTarget& target,
    mir::ExprId captured_owner) -> WriteTarget {
  WriteTarget frozen = target;
  frozen.owner = captured_owner;
  for (DescentStep& step : frozen.descent) {
    for (mir::ExprId& coordinate : step.operands) {
      coordinate = SnapshotIntoClosure(
          unit_lowerer, outer_frame, closure, coordinate, "_lyra_nba_arg");
    }
  }
  return frozen;
}

// What a deferred update writes, and where it writes it, once both are frozen
// into a closure's environment: the navigation to the target's owner is
// evaluated where the statement is reached and captured as a reference, the
// descent above it is restated over that capture with its coordinates
// snapshotted, and each operand is snapshotted. LRM 10.4.2 settles both there,
// however much later the update runs.
struct FrozenAssignment {
  WriteTarget target;
  std::vector<mir::ExprId> operands;
};

auto FreezeAssignmentInto(
    UnitLowerer& unit_lowerer, const WalkFrame& outer_frame,
    ClosureBuilder& closure, const WriteTarget& target_in_outer,
    std::span<const mir::ExprId> operands_in_outer) -> FrozenAssignment {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Block& outer_block = *outer_frame.current_block;

  const mir::ExprId place_ref = BuildReferenceArg(
      unit, outer_block, target_in_outer.owner,
      outer_block.exprs.Get(target_in_outer.owner).type);
  const mir::ExprId captured_owner = SnapshotIntoClosure(
      unit_lowerer, outer_frame, closure, place_ref, "_lyra_nba_place");

  FrozenAssignment frozen{
      .target = FreezeTarget(
          unit_lowerer, outer_frame, closure, target_in_outer, captured_owner),
      .operands = {}};
  frozen.operands.reserve(operands_in_outer.size());
  for (const mir::ExprId op : operands_in_outer) {
    frozen.operands.push_back(SnapshotIntoClosure(
        unit_lowerer, outer_frame, closure, op, "_lyra_nba_arg"));
  }
  return frozen;
}

// The update runs after the stretch that reached the statement returns, and it
// holds a reference to the target's storage until then, so that storage has to
// outlive the stretch. LRM 10.4.2 makes the case that fails this illegal -- "It
// shall be illegal to make nonblocking assignments to automatic variables" --
// and the front end rejects it, so this stands behind that rather than in front
// of it: reaching it means a target was lowered to storage the source did not
// name.
auto CheckTargetOutlivesUpdate(
    const mir::Block& block, mir::ExprId target_in_outer, diag::SourceSpan span)
    -> diag::Result<void> {
  if (!TargetOutlivesDeferredUpdate(block, target_in_outer)) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedAssignmentTarget,
        "a nonblocking assignment names storage that does not outlive the "
        "statement that reached it (LRM 10.4.2)");
  }
  return {};
}

// Axis B (timing): apply a target's write effect where the statement is
// reached, or as an update due later. `effect_fn(block, target, operands)`
// builds the write into `block`, the same node for either timing, so a target
// says nothing about when its write happens (LRM 10.4).
template <typename EffectFn>
auto ApplyAssignEffect(
    ProcessLowerer& process, WalkFrame frame, const hir::EffectTiming& timing,
    diag::SourceSpan span, const WriteTarget& target_in_outer,
    std::span<const mir::ExprId> operands_in_outer, EffectFn effect_fn)
    -> diag::Result<mir::Expr> {
  auto& block = *frame.current_block;
  const auto* deferred = std::get_if<hir::NonBlockingEffect>(&timing);
  if (deferred == nullptr) {
    return effect_fn(block, target_in_outer, operands_in_outer);
  }
  auto outlives = CheckTargetOutlivesUpdate(block, target_in_outer.owner, span);
  if (!outlives) return std::unexpected(std::move(outlives.error()));
  return BuildDeferredEffect(
      process, frame, deferred->control,
      [&](ClosureBuilder& closure) -> diag::Result<FrozenAssignment> {
        return FreezeAssignmentInto(
            process.Owner(), frame, closure, target_in_outer,
            operands_in_outer);
      },
      [&](mir::Block& body, const FrozenAssignment& frozen) {
        body.AppendStmt(
            mir::ExprStmt{
                .expr = body.exprs.Add(effect_fn(
                    body, frozen.target,
                    std::span<const mir::ExprId>(frozen.operands)))});
      });
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

  const std::optional<mir::BinaryOp> compound_op =
      a.compound_op.has_value() ? std::optional{LowerBinaryOp(*a.compound_op)}
                                : std::nullopt;
  const std::array<mir::ExprId, 1> operands{rhs_id};
  return ApplyAssignEffect(
      process, frame, a.timing, span, *lhs_or, operands,
      [&](mir::Block& blk, const WriteTarget& target,
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
      std::holds_alternative<hir::NonBlockingEffect>(a.timing)) {
    throw InternalError(
        "LowerHirAssignExprProc: compound assignment with non-blocking timing "
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

auto BuildDestructuredDeferredAssign(
    ProcessLowerer& process, WalkFrame frame, diag::SourceSpan span,
    const std::optional<hir::DelayOrEventControl>& control,
    std::span<const DestructuredPart> parts) -> diag::Result<mir::Expr> {
  for (const DestructuredPart& part : parts) {
    auto outlives = CheckTargetOutlivesUpdate(
        *frame.current_block, part.target.owner, span);
    if (!outlives) return std::unexpected(std::move(outlives.error()));
  }
  return BuildDeferredEffect(
      process, frame, control,
      [&](ClosureBuilder& closure)
          -> diag::Result<std::vector<FrozenAssignment>> {
        std::vector<FrozenAssignment> frozen;
        frozen.reserve(parts.size());
        for (const DestructuredPart& part : parts) {
          const std::array<mir::ExprId, 1> operands{part.value};
          frozen.push_back(FreezeAssignmentInto(
              process.Owner(), frame, closure, part.target, operands));
        }
        return frozen;
      },
      [&](mir::Block& body, const std::vector<FrozenAssignment>& frozen) {
        for (std::size_t i = 0; i < frozen.size(); ++i) {
          body.AppendStmt(
              mir::ExprStmt{
                  .expr = body.exprs.Add(BuildStoreExpr(
                      process.Owner().Unit(), body, frozen[i].target,
                      frozen[i].operands.front(), std::nullopt,
                      parts[i].type))});
        }
      });
}

}  // namespace lyra::lowering::hir_to_mir
