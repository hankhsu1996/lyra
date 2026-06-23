#include "lyra/lowering/hir_to_mir/expression/assignment.hpp"

#include <algorithm>
#include <array>
#include <cstdint>
#include <expected>
#include <format>
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
#include "lyra/hir/type.hpp"
#include "lyra/lowering/hir_to_mir/closure_builder.hpp"
#include "lyra/lowering/hir_to_mir/expression/operators.hpp"
#include "lyra/lowering/hir_to_mir/lhs_observable.hpp"
#include "lyra/lowering/hir_to_mir/module_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/services_call.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/block_hops.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// Walks the LHS expression to determine whether its addressable root is a
// structural var (vs procedural local). NBA assignment requires a structural
// target; procedural-local NBA is a known gap.
auto IsExprRootedAtStructuralVar(const mir::Block& block, mir::ExprId expr_id)
    -> bool {
  const auto& expr = block.exprs.Get(expr_id);
  return std::visit(
      Overloaded{
          [](const mir::MemberRef&) { return true; },
          [](const mir::MemberAccessExpr&) { return true; },
          [](const mir::LocalRef&) { return false; },
          [&](const mir::CallExpr& c) {
            if (!mir::IsContainerAccessCallee(c.callee) ||
                c.arguments.empty()) {
              return false;
            }
            return IsExprRootedAtStructuralVar(block, c.arguments.front());
          },
          [&](const mir::ConcatExpr& c) {
            return std::ranges::all_of(c.operands, [&](mir::ExprId op) {
              return IsExprRootedAtStructuralVar(block, op);
            });
          },
          [](const auto&) { return false; },
      },
      expr.data);
}

// Recursively clone an LHS-shaped expression into the closure body. The LHS
// structure (PrimaryExpr, container-access CallExpr, ConcatExpr) is
// reproduced as-is; per-layer subexpressions that are NOT part of the LHS
// structure (selector indices, range offsets and count literals) are
// snapshotted by value through the builder so the closure body sees the values
// that were live at submit time.
auto CloneLhsExprForNbaBody(
    ClosureBuilder& closure, const mir::Block& outer_block,
    mir::TypeId self_ptr_type, std::uint32_t& snapshot_counter,
    mir::ExprId outer_id) -> mir::ExprId {
  mir::Block& body = closure.Body();
  const mir::LocalId body_self_id = closure.SelfBinding();
  const auto& outer_expr = outer_block.exprs.Get(outer_id);
  return std::visit(
      Overloaded{
          [&](const mir::CallExpr& c) -> mir::ExprId {
            if (!mir::IsContainerAccessCallee(c.callee) ||
                c.arguments.empty()) {
              throw InternalError(
                  "CloneLhsExprForNbaBody: CallExpr is not a container-access "
                  "form in NBA LHS clone walk");
            }
            std::vector<mir::ExprId> body_args;
            body_args.reserve(c.arguments.size());
            body_args.push_back(CloneLhsExprForNbaBody(
                closure, outer_block, self_ptr_type, snapshot_counter,
                c.arguments.front()));
            for (std::size_t i = 1; i < c.arguments.size(); ++i) {
              body_args.push_back(closure.CaptureByValue(
                  c.arguments[i],
                  std::format("_lyra_nba_arg{}", snapshot_counter++)));
            }
            return body.exprs.Add(
                mir::Expr{
                    .data =
                        mir::CallExpr{
                            .callee = c.callee,
                            .arguments = std::move(body_args)},
                    .type = outer_expr.type});
          },
          [&](const mir::ConcatExpr& c) -> mir::ExprId {
            std::vector<mir::ExprId> body_operands;
            body_operands.reserve(c.operands.size());
            for (const mir::ExprId op_id : c.operands) {
              body_operands.push_back(CloneLhsExprForNbaBody(
                  closure, outer_block, self_ptr_type, snapshot_counter,
                  op_id));
            }
            return body.exprs.Add(
                mir::Expr{
                    .data =
                        mir::ConcatExpr{.operands = std::move(body_operands)},
                    .type = outer_expr.type});
          },
          [&](const mir::MemberRef&) -> mir::ExprId {
            return body.exprs.Add(outer_expr);
          },
          [&](const mir::MemberAccessExpr& m) -> mir::ExprId {
            const mir::ExprId body_receiver = body.exprs.Add(
                mir::Expr{
                    .data =
                        mir::LocalRef{
                            .hops = mir::BlockHops{.value = 0},
                            .var = body_self_id},
                    .type = self_ptr_type});
            return body.exprs.Add(
                mir::Expr{
                    .data =
                        mir::MemberAccessExpr{
                            .receiver = body_receiver, .member = m.member},
                    .type = outer_expr.type});
          },
          [&](const mir::LocalRef&) -> mir::ExprId {
            return body.exprs.Add(outer_expr);
          },
          [&](const auto&) -> mir::ExprId {
            throw InternalError(
                "CloneLhsExprForNbaBody: non-addressable expression form in "
                "NBA LHS clone walk");
          },
      },
      outer_expr.data);
}

auto MakeStringMethodCallExpr(
    support::BuiltinFn fn, std::vector<mir::ExprId> args, mir::TypeId type)
    -> mir::Expr {
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::BuiltinFnCallee{.id = fn},
              .arguments = std::move(args)},
      .type = type};
}

// Axis B (timing), deferred half: build the closure the NBA region invokes.
// `effect_fn` is the target's write effect (axis A); this envelope is the only
// part that knows about deferral. The target is cloned through captured `self`
// (so it reaches the cell at NBA time) and the operands are snapshotted by
// value (the active-region values, LRM 10.4.2); `effect_fn` then builds the
// write against those body-side nodes -- the same call it would build for a
// blocking write, only in the closure body.
template <typename EffectFn>
auto BuildDeferredAssignClosure(
    ModuleLowerer& module, WalkFrame frame, mir::ExprId target_in_outer,
    std::span<const mir::ExprId> operands_in_outer, EffectFn effect_fn)
    -> mir::Expr {
  const auto& outer_block = *frame.current_block;
  ClosureBuilder closure(module.Unit(), frame);
  mir::Block& body = closure.Body();
  const mir::TypeId self_ptr_type = frame.current_class->self_pointer_type;

  std::uint32_t snapshot_counter = 0;
  const mir::ExprId body_target = CloneLhsExprForNbaBody(
      closure, outer_block, self_ptr_type, snapshot_counter, target_in_outer);
  std::vector<mir::ExprId> body_operands;
  body_operands.reserve(operands_in_outer.size());
  for (const mir::ExprId op : operands_in_outer) {
    body_operands.push_back(closure.CaptureByValue(
        op, std::format("_lyra_nba_arg{}", snapshot_counter++)));
  }

  const mir::ExprId body_self_ref = body.exprs.Add(
      mir::Expr{
          .data =
              mir::LocalRef{
                  .hops = mir::BlockHops{.value = 0},
                  .var = closure.SelfBinding()},
          .type = self_ptr_type});
  const mir::ExprId body_services = body.exprs.Add(
      mir::MakeServicesCallExpr(
          body_self_ref, module.Unit().builtins.services));
  const mir::ExprId effect_id = body.exprs.Add(effect_fn(
      body, body_target, std::span<const mir::ExprId>(body_operands),
      body_services));
  body.AppendStmt(
      mir::Stmt{
          .label = std::nullopt, .data = mir::ExprStmt{.expr = effect_id}});
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
  if (kind == hir::AssignKind::kBlocking) {
    const mir::ExprId services_id =
        block.exprs.Add(BuildServicesCallExpr(process, frame));
    return effect_fn(block, target_in_outer, operands_in_outer, services_id);
  }
  if (!IsExprRootedAtStructuralVar(block, target_in_outer)) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedAssignmentTarget,
        "non-blocking assignment to procedural local is not supported yet",
        diag::UnsupportedCategory::kFeature);
  }
  mir::Expr closure = BuildDeferredAssignClosure(
      process.Module(), frame, target_in_outer, operands_in_outer, effect_fn);
  const mir::ExprId closure_id = block.exprs.Add(std::move(closure));
  const mir::ExprId services_id =
      block.exprs.Add(BuildServicesCallExpr(process, frame));
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::BuiltinFnCallee{.id = support::BuiltinFn::kSubmitNba},
              .arguments = {services_id, closure_id}},
      .type = process.Module().Unit().builtins.void_type};
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
  const mir::TypeId void_type = process.Module().Unit().builtins.void_type;
  const std::array<mir::ExprId, 1> operands{rhs_id};
  return ApplyAssignEffect(
      process, frame, a.kind, span, lhs_id, operands,
      [&](mir::Block& blk, mir::ExprId target, std::span<const mir::ExprId> ops,
          mir::ExprId services) -> mir::Expr {
        return BuildObservableAssignExpr(
            process.Module().Unit(), blk, services, target, ops[0], compound_op,
            result_type, void_type);
      });
}

// Axis A for a string element target: putc, the symmetric counterpart of the
// getc index read, because a string exposes no element lvalue (LRM 6.16.2). A
// compound `s[i] op= v` is desugared here to a getc read-modify so the effect
// always carries the final byte; the receiver routes through `Var<String>::
// Mutate` so a deferred write still wakes the cell's subscribers.
auto LowerStringElementAssign(
    ProcessLowerer& process, WalkFrame frame, const hir::AssignExpr& a,
    const hir::ElementSelectExpr& sel, diag::SourceSpan span,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  const auto& hir_process = process.HirBody();
  const auto& unit = process.Module().Unit();
  auto& block = *frame.current_block;

  const auto& base_hir = hir_process.exprs.Get(sel.base_value);
  auto cell_or = process.LowerLhsExpr(base_hir, frame.WithLvalueTarget(true));
  if (!cell_or) return std::unexpected(std::move(cell_or.error()));
  const mir::ExprId cell_id = block.exprs.Add(*std::move(cell_or));

  auto idx_or = process.LowerExpr(hir_process.exprs.Get(sel.index), frame);
  if (!idx_or) return std::unexpected(std::move(idx_or.error()));
  const mir::ExprId idx_id = block.exprs.Add(*std::move(idx_or));

  auto rhs_or = process.LowerExpr(hir_process.exprs.Get(a.rhs), frame);
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
  mir::ExprId value_id = block.exprs.Add(*std::move(rhs_or));

  // A compound write only ever reaches here blocking (compound + NBA is not a
  // legal SV form, rejected upstream), so the read-modify lands in the active
  // region before the effect runs.
  if (a.compound_op.has_value()) {
    auto base_read_or = process.LowerExpr(base_hir, frame);
    if (!base_read_or) return std::unexpected(std::move(base_read_or.error()));
    const mir::ExprId base_read_id = block.exprs.Add(*std::move(base_read_or));
    const mir::ExprId cur_id = block.exprs.Add(MakeStringMethodCallExpr(
        support::BuiltinFn::kGetc, {base_read_id, idx_id}, result_type));
    value_id = block.exprs.Add(
        mir::Expr{
            .data =
                mir::BinaryExpr{
                    .op = LowerBinaryOp(*a.compound_op),
                    .lhs = cur_id,
                    .rhs = value_id},
            .type = result_type});
  }

  const std::array<mir::ExprId, 2> operands{idx_id, value_id};
  return ApplyAssignEffect(
      process, frame, a.kind, span, cell_id, operands,
      [&unit](
          mir::Block& blk, mir::ExprId target, std::span<const mir::ExprId> ops,
          mir::ExprId services) -> mir::Expr {
        mir::ExprId recv = target;
        const mir::ExprId root = FindLhsRootId(blk, target);
        if (mir::IsObservableCellType(unit.GetType(blk.exprs.Get(root).type))) {
          recv = RewriteLhsRootWithMutate(unit, blk, target, services);
        }
        return MakeStringMethodCallExpr(
            support::BuiltinFn::kPutc, {recv, ops[0], ops[1]},
            unit.builtins.void_type);
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

  // Axis A: pick the target's write effect. A string element has no lvalue, so
  // it realizes as putc (LRM 6.16.2); every other target is an observable cell
  // or a local. Axis B (blocking vs deferred) is handled uniformly inside each.
  const auto& hir_process = process.HirBody();
  const hir::Expr& hir_lhs = hir_process.exprs.Get(a.lhs);
  if (const auto* sel = std::get_if<hir::ElementSelectExpr>(&hir_lhs.data)) {
    const hir::Type& base_ty = process.Module().Hir().GetType(
        hir_process.exprs.Get(sel->base_value).type);
    if (base_ty.Kind() == hir::TypeKind::kString) {
      return LowerStringElementAssign(
          process, frame, a, *sel, span, result_type);
    }
  }
  return LowerObservableAssign(process, frame, a, span, result_type);
}

// Builds the deferred-write closure for an observable assignment, used by the
// LHS-destructuring desugar where each part is a separate NBA submit. The
// general timing envelope (`ApplyAssignEffect`) does not fit there because
// destructuring submits per part inside its own block, so this exposes just
// the closure-building half over the observable write effect.
auto BuildNbaSubmitClosureExpr(
    ModuleLowerer& module, WalkFrame frame, mir::ExprId lhs_in_outer,
    mir::ExprId rhs_id_in_outer, mir::TypeId rhs_type) -> mir::Expr {
  const std::array<mir::ExprId, 1> operands{rhs_id_in_outer};
  return BuildDeferredAssignClosure(
      module, frame, lhs_in_outer, operands,
      [&module, rhs_type](
          mir::Block& blk, mir::ExprId target, std::span<const mir::ExprId> ops,
          mir::ExprId services) -> mir::Expr {
        return BuildObservableAssignExpr(
            module.Unit(), blk, services, target, ops[0], std::nullopt,
            rhs_type, module.Unit().builtins.void_type);
      });
}

}  // namespace lyra::lowering::hir_to_mir
