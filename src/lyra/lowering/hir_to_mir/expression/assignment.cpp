#include "lyra/lowering/hir_to_mir/expression/assignment.hpp"

#include <algorithm>
#include <cstdint>
#include <expected>
#include <format>
#include <memory>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/lowering/hir_to_mir/expression/operators.hpp"
#include "lyra/lowering/hir_to_mir/lhs_observable.hpp"
#include "lyra/lowering/hir_to_mir/module_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/services_call.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/closure.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/procedural_hops.hpp"
#include "lyra/mir/runtime_submit.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// Walks the LHS expression to determine whether its addressable root is a
// structural var (vs procedural local). NBA assignment requires a structural
// target; procedural-local NBA is a known gap.
auto IsExprRootedAtStructuralVar(
    const mir::ProceduralScope& proc_scope, mir::ExprId expr_id) -> bool {
  const auto& expr = proc_scope.GetExpr(expr_id);
  return std::visit(
      Overloaded{
          [](const mir::StructuralVarRef&) { return true; },
          [](const mir::MemberAccessExpr&) { return true; },
          [](const mir::ProceduralVarRef&) { return false; },
          [&](const mir::CallExpr& c) {
            const auto* callee =
                std::get_if<mir::BuiltinMethodCallee>(&c.callee);
            if (callee == nullptr || !mir::IsContainerAccessCall(*callee) ||
                c.arguments.empty()) {
              return false;
            }
            return IsExprRootedAtStructuralVar(proc_scope, c.arguments.front());
          },
          [&](const mir::ConcatExpr& c) {
            return std::ranges::all_of(c.operands, [&](mir::ExprId op) {
              return IsExprRootedAtStructuralVar(proc_scope, op);
            });
          },
          [](const auto&) { return false; },
      },
      expr.data);
}

// Snapshot a single outer-scope subexpression into the closure body. Literal
// values clone verbatim so the body still sees an IntegerLiteral (some
// downstream code paths extract constants from literal-shape exprs). Other
// expressions are captured by value into a fresh procedural var so the body
// reads the value as it stood at submit time. `name_prefix` distinguishes
// the caller so the synthesized binding names stay readable when MIR dumps
// are inspected.
auto SnapshotNonLhsSubexpr(
    const mir::ProceduralScope& outer_scope, mir::ProceduralScope& body,
    std::vector<mir::Capture>& captures, std::uint32_t& snapshot_counter,
    mir::ExprId outer_id, std::string_view name_prefix) -> mir::ExprId {
  const auto& outer_expr = outer_scope.GetExpr(outer_id);
  if (std::holds_alternative<mir::IntegerLiteral>(outer_expr.data)) {
    return body.AddExpr(outer_expr);
  }
  const auto binding = body.AddProceduralVar(
      mir::ProceduralVarDecl{
          .name =
              std::format("_lyra_{}_arg{}", name_prefix, snapshot_counter++),
          .type = outer_expr.type});
  captures.emplace_back(mir::Capture{.value = outer_id, .binding = binding});
  return body.AddExpr(
      mir::Expr{
          .data =
              mir::ProceduralVarRef{
                  .hops = mir::ProceduralHops{.value = 0}, .var = binding},
          .type = outer_expr.type});
}

// Recursively clone an LHS-shaped expression into the closure body. The LHS
// structure (PrimaryExpr, container-access CallExpr, ConcatExpr) is
// reproduced as-is; per-layer subexpressions that are NOT part of the LHS
// structure (selector indices, range offsets and count literals) are
// snapshotted by value so the closure body sees the values that were live
// at submit time.
auto CloneLhsExprForNbaBody(
    const mir::ProceduralScope& outer_scope, mir::ProceduralScope& body,
    mir::TypeId self_ptr_type, mir::ProceduralVarId body_self_id,
    std::vector<mir::Capture>& captures, std::uint32_t& snapshot_counter,
    mir::ExprId outer_id) -> mir::ExprId {
  const auto& outer_expr = outer_scope.GetExpr(outer_id);
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
                outer_scope, body, self_ptr_type, body_self_id, captures,
                snapshot_counter, c.arguments.front()));
            for (std::size_t i = 1; i < c.arguments.size(); ++i) {
              body_args.push_back(SnapshotNonLhsSubexpr(
                  outer_scope, body, captures, snapshot_counter, c.arguments[i],
                  "nba"));
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
                  outer_scope, body, self_ptr_type, body_self_id, captures,
                  snapshot_counter, op_id));
            }
            return body.AddExpr(
                mir::Expr{
                    .data =
                        mir::ConcatExpr{.operands = std::move(body_operands)},
                    .type = outer_expr.type});
          },
          [&](const mir::StructuralVarRef&) -> mir::ExprId {
            return body.AddExpr(outer_expr);
          },
          [&](const mir::MemberAccessExpr& m) -> mir::ExprId {
            const mir::ExprId body_receiver = body.AddExpr(
                mir::Expr{
                    .data =
                        mir::ProceduralVarRef{
                            .hops = mir::ProceduralHops{.value = 0},
                            .var = body_self_id},
                    .type = self_ptr_type});
            return body.AddExpr(
                mir::Expr{
                    .data =
                        mir::MemberAccessExpr{
                            .receiver = body_receiver, .member = m.member},
                    .type = outer_expr.type});
          },
          [&](const mir::ProceduralVarRef&) -> mir::ExprId {
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
  auto& proc_scope = *frame.current_procedural_scope;
  auto rhs_or = process.LowerExpr(hir_process.exprs.at(a.rhs.value), frame);
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
  const mir::TypeId rhs_type = (*rhs_or).type;
  const mir::ExprId rhs_id = proc_scope.AddExpr(*std::move(rhs_or));
  auto lhs_or = process.LowerLhsExpr(
      hir_process.exprs.at(a.lhs.value), frame.WithLvalueTarget(true));
  if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
  const mir::ExprId lhs_id = proc_scope.AddExpr(*std::move(lhs_or));

  if (a.kind == hir::AssignKind::kBlocking) {
    const std::optional<mir::BinaryOp> compound_op =
        a.compound_op.has_value() ? std::optional{LowerBinaryOp(*a.compound_op)}
                                  : std::nullopt;
    const mir::ExprId services_id =
        proc_scope.AddExpr(BuildServicesCallExpr(process, frame));
    return BuildObservableAssignExpr(
        process.Module().Unit(), proc_scope, services_id, lhs_id, rhs_id,
        compound_op, result_type, process.Module().Unit().builtins.void_type);
  }

  if (a.compound_op.has_value()) {
    throw InternalError(
        "LowerHirAssignExprProc: compound assignment with non-blocking kind "
        "is not a legal SV form (LRM A.6.2 grammar)");
  }

  if (!IsExprRootedAtStructuralVar(proc_scope, lhs_id)) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedAssignmentTarget,
        "non-blocking assignment to procedural local is not supported yet",
        diag::UnsupportedCategory::kFeature);
  }

  mir::Expr closure_expr = BuildNbaSubmitClosureExpr(
      process.Module(), frame, lhs_id, rhs_id, rhs_type);
  const mir::ExprId closure_id = proc_scope.AddExpr(std::move(closure_expr));
  return mir::Expr{
      .data =
          mir::RuntimeCallExpr{
              .call = mir::RuntimeSubmitNbaCall{.closure = closure_id}},
      .type = process.Module().Unit().builtins.void_type};
}

// Builds a ClosureExpr that snapshots the RHS by value into the body and
// writes the snapshot to the LHS. The closure is the deferred-write vehicle
// the NBA region invokes. The returned Expr has type `void` -- the closure
// value flows only into RuntimeSubmitNbaCall. `lhs_in_outer` must be an
// addressable expression.
auto BuildNbaSubmitClosureExpr(
    const ModuleLowerer& module, WalkFrame frame, mir::ExprId lhs_in_outer,
    mir::ExprId rhs_id_in_outer, mir::TypeId rhs_type) -> mir::Expr {
  auto& outer_scope = *frame.current_procedural_scope;
  mir::ProceduralScope body;
  std::vector<mir::Capture> captures;

  const mir::TypeId self_ptr_type =
      frame.current_structural_scope->self_pointer_type;
  const mir::ProceduralVarId self_id = body.AddProceduralVar(
      mir::ProceduralVarDecl{.name = "self", .type = self_ptr_type});
  const mir::ExprId outer_self_read =
      outer_scope.AddExpr(BuildSelfRefExpr(frame, self_ptr_type));
  captures.emplace_back(
      mir::Capture{.value = outer_self_read, .binding = self_id});

  const mir::ProceduralVarId rhs_binding = body.AddProceduralVar(
      mir::ProceduralVarDecl{.name = "_lyra_nba_rhs", .type = rhs_type});
  captures.emplace_back(
      mir::Capture{.value = rhs_id_in_outer, .binding = rhs_binding});
  const mir::ExprId rhs_ref_id = body.AddExpr(
      mir::Expr{
          .data =
              mir::ProceduralVarRef{
                  .hops = mir::ProceduralHops{.value = 0}, .var = rhs_binding},
          .type = rhs_type});

  std::uint32_t snapshot_counter = 0;
  const mir::ExprId body_lhs_id = CloneLhsExprForNbaBody(
      outer_scope, body, self_ptr_type, self_id, captures, snapshot_counter,
      lhs_in_outer);

  // Body-side `self.Services()` for the observable write. Built only inside
  // the closure body because the outer scope's `self_binding` and depths do
  // not reach into the body -- the body captures `self` by value at depth 0.
  const mir::ExprId body_self_ref = body.AddExpr(
      mir::Expr{
          .data =
              mir::ProceduralVarRef{
                  .hops = mir::ProceduralHops{.value = 0}, .var = self_id},
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

  mir::ClosureExpr closure;
  closure.captures = std::move(captures);
  closure.body = std::make_unique<mir::ProceduralScope>(std::move(body));

  return mir::Expr{
      .data = std::move(closure), .type = module.Unit().builtins.void_type};
}

}  // namespace lyra::lowering::hir_to_mir
