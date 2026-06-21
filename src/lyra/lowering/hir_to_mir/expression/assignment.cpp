#include "lyra/lowering/hir_to_mir/expression/assignment.hpp"

#include <algorithm>
#include <cstdint>
#include <expected>
#include <format>
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
#include "lyra/lowering/hir_to_mir/module_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/services_call.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/block_hops.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/runtime_submit.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// Walks the LHS expression to determine whether its addressable root is a
// structural var (vs procedural local). NBA assignment requires a structural
// target; procedural-local NBA is a known gap.
auto IsExprRootedAtStructuralVar(const mir::Block& block, mir::ExprId expr_id)
    -> bool {
  const auto& expr = block.GetExpr(expr_id);
  return std::visit(
      Overloaded{
          [](const mir::MemberRef&) { return true; },
          [](const mir::MemberAccessExpr&) { return true; },
          [](const mir::LocalRef&) { return false; },
          [&](const mir::CallExpr& c) {
            const auto* callee =
                std::get_if<mir::BuiltinMethodCallee>(&c.callee);
            if (callee == nullptr || !mir::IsContainerAccessCall(*callee) ||
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
  const auto& outer_expr = outer_block.GetExpr(outer_id);
  return std::visit(
      Overloaded{
          [&](const mir::CallExpr& c) -> mir::ExprId {
            const auto* callee =
                std::get_if<mir::BuiltinMethodCallee>(&c.callee);
            if (callee == nullptr || !mir::IsContainerAccessCall(*callee) ||
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
            return body.AddExpr(
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
            return body.AddExpr(
                mir::Expr{
                    .data =
                        mir::ConcatExpr{.operands = std::move(body_operands)},
                    .type = outer_expr.type});
          },
          [&](const mir::MemberRef&) -> mir::ExprId {
            return body.AddExpr(outer_expr);
          },
          [&](const mir::MemberAccessExpr& m) -> mir::ExprId {
            const mir::ExprId body_receiver = body.AddExpr(
                mir::Expr{
                    .data =
                        mir::LocalRef{
                            .hops = mir::BlockHops{.value = 0},
                            .var = body_self_id},
                    .type = self_ptr_type});
            return body.AddExpr(
                mir::Expr{
                    .data =
                        mir::MemberAccessExpr{
                            .receiver = body_receiver, .member = m.member},
                    .type = outer_expr.type});
          },
          [&](const mir::LocalRef&) -> mir::ExprId {
            return body.AddExpr(outer_expr);
          },
          [&](const auto&) -> mir::ExprId {
            throw InternalError(
                "CloneLhsExprForNbaBody: non-addressable expression form in "
                "NBA LHS clone walk");
          },
      },
      outer_expr.data);
}

}  // namespace

auto LowerHirAssignExprProc(
    ProcessLowerer& process, WalkFrame frame, const hir::AssignExpr& a,
    diag::SourceSpan span, mir::TypeId result_type) -> diag::Result<mir::Expr> {
  const auto& hir_process = process.HirBody();
  auto& block = *frame.current_block;
  auto rhs_or = process.LowerExpr(hir_process.exprs.at(a.rhs.value), frame);
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
  const mir::TypeId rhs_type = (*rhs_or).type;
  const mir::ExprId rhs_id = block.AddExpr(*std::move(rhs_or));
  auto lhs_or = process.LowerLhsExpr(
      hir_process.exprs.at(a.lhs.value), frame.WithLvalueTarget(true));
  if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
  const mir::ExprId lhs_id = block.AddExpr(*std::move(lhs_or));

  if (a.kind == hir::AssignKind::kBlocking) {
    const std::optional<mir::BinaryOp> compound_op =
        a.compound_op.has_value() ? std::optional{LowerBinaryOp(*a.compound_op)}
                                  : std::nullopt;
    const mir::ExprId services_id =
        block.AddExpr(BuildServicesCallExpr(process, frame));
    return BuildObservableAssignExpr(
        process.Module().Unit(), block, services_id, lhs_id, rhs_id,
        compound_op, result_type, process.Module().Unit().builtins.void_type);
  }

  if (a.compound_op.has_value()) {
    throw InternalError(
        "LowerHirAssignExprProc: compound assignment with non-blocking kind "
        "is not a legal SV form (LRM A.6.2 grammar)");
  }

  if (!IsExprRootedAtStructuralVar(block, lhs_id)) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedAssignmentTarget,
        "non-blocking assignment to procedural local is not supported yet",
        diag::UnsupportedCategory::kFeature);
  }

  mir::Expr closure_expr = BuildNbaSubmitClosureExpr(
      process.Module(), frame, lhs_id, rhs_id, rhs_type);
  const mir::ExprId closure_id = block.AddExpr(std::move(closure_expr));
  return mir::Expr{
      .data =
          mir::RuntimeCallExpr{
              .call = mir::RuntimeSubmitNbaCall{.closure = closure_id}},
      .type = process.Module().Unit().builtins.void_type};
}

// Builds the deferred-write closure that snapshots the RHS by value into the
// body and writes the snapshot to the LHS. It is the vehicle the NBA region
// invokes. The returned Expr has type `void` -- the closure value flows only
// into RuntimeSubmitNbaCall. `lhs_in_outer` must be an addressable expression.
auto BuildNbaSubmitClosureExpr(
    ModuleLowerer& module, WalkFrame frame, mir::ExprId lhs_in_outer,
    mir::ExprId rhs_id_in_outer, mir::TypeId rhs_type) -> mir::Expr {
  auto& outer_block = *frame.current_block;
  ClosureBuilder closure(module.Unit(), frame);
  mir::Block& body = closure.Body();
  const mir::TypeId self_ptr_type = frame.current_class->self_pointer_type;
  const mir::LocalId self_id = closure.SelfBinding();

  const mir::ExprId rhs_ref_id =
      closure.CaptureByValue(rhs_id_in_outer, "_lyra_nba_rhs");

  std::uint32_t snapshot_counter = 0;
  const mir::ExprId body_lhs_id = CloneLhsExprForNbaBody(
      closure, outer_block, self_ptr_type, snapshot_counter, lhs_in_outer);

  const mir::ExprId body_self_ref = body.AddExpr(
      mir::Expr{
          .data =
              mir::LocalRef{.hops = mir::BlockHops{.value = 0}, .var = self_id},
          .type = self_ptr_type});
  const mir::ExprId body_services_id = body.AddExpr(
      mir::MakeServicesCallExpr(
          body_self_ref, module.Unit().builtins.services));

  const mir::Expr assign_expr = BuildObservableAssignExpr(
      module.Unit(), body, body_services_id, body_lhs_id, rhs_ref_id,
      std::nullopt, rhs_type, module.Unit().builtins.void_type);
  const mir::ExprId assign_id = body.AddExpr(assign_expr);
  body.AppendStmt(
      mir::Stmt{
          .label = std::nullopt, .data = mir::ExprStmt{.expr = assign_id}});

  return closure.BuildVoid();
}

}  // namespace lyra::lowering::hir_to_mir
