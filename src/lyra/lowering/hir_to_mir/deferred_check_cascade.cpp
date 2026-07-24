#include "lyra/lowering/hir_to_mir/deferred_check_cascade.hpp"

#include <cstdint>
#include <expected>
#include <format>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/binding_origin.hpp"
#include "lyra/lowering/hir_to_mir/closure_builder.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/print_items.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/lowering/hir_to_mir/statement/blocks.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/deferred_check_site.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/runtime_print.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

struct UniqueIfCascade {
  std::vector<hir::ExprId> conditions;
  std::vector<hir::StmtId> bodies;
  std::optional<hir::StmtId> tail_else;
};

auto FlattenUniqueCascade(
    const hir::ProceduralBody& proc, const hir::IfStmt& root)
    -> UniqueIfCascade {
  UniqueIfCascade out;
  out.conditions.push_back(root.condition);
  out.bodies.push_back(root.then_stmt);
  std::optional<hir::StmtId> cur_else = root.else_stmt;
  while (cur_else.has_value()) {
    const hir::Stmt& s = proc.stmts.Get(*cur_else);
    const auto* nested = std::get_if<hir::IfStmt>(&s.data);
    if (nested == nullptr || nested->check.has_value()) {
      out.tail_else = cur_else;
      break;
    }
    out.conditions.push_back(nested->condition);
    out.bodies.push_back(nested->then_stmt);
    cur_else = nested->else_stmt;
  }
  return out;
}

struct CheckVerdict {
  mir::BinaryOp violation_op;
  std::uint64_t expected;
  std::string prefix_text;
  bool include_count_value;
  std::string suffix_text;
};

auto VerdictFor(hir::UniquePriorityCheck check, std::size_t branch_count)
    -> CheckVerdict {
  switch (check) {
    case hir::UniquePriorityCheck::kUnique:
      return {
          .violation_op = mir::BinaryOp::kInequality,
          .expected = 1,
          .prefix_text = "unique check failed: matched ",
          .include_count_value = true,
          .suffix_text = std::format(" of {} conditions", branch_count)};
    case hir::UniquePriorityCheck::kUnique0:
      return {
          .violation_op = mir::BinaryOp::kGreaterThan,
          .expected = 1,
          .prefix_text = "unique0 check failed: matched ",
          .include_count_value = true,
          .suffix_text = std::format(" of {} conditions", branch_count)};
    case hir::UniquePriorityCheck::kPriority:
      return {
          .violation_op = mir::BinaryOp::kEquality,
          .expected = 0,
          .prefix_text = "priority check failed: no condition matched",
          .include_count_value = false,
          .suffix_text = ""};
  }
  throw InternalError("VerdictFor: unknown HIR UniquePriorityCheck");
}

// A predicate snapshot bound for capture: the wrapper local holding the value
// frozen at check time, plus the synthesized origin a deferred closure forwards
// it through.
struct SnapshotBinding {
  mir::LocalId local;
  BindingOriginId origin;
};

auto SnapshotPredicate(
    UnitLowerer& unit_lowerer, WalkFrame frame, mir::Block& wrapper,
    std::size_t index, mir::TypeId predicate_type,
    mir::ExprId predicate_expr_id) -> SnapshotBinding {
  const BindingOriginId origin =
      BindingOriginId::Synthesized(unit_lowerer.NextSynthesizedSite(), 0);
  const mir::LocalId local = SnapshotExprToLocal(
      unit_lowerer, frame, wrapper, std::format("_lyra_unique_cond_{}", index),
      predicate_type, predicate_expr_id, origin);
  return {.local = local, .origin = origin};
}

auto BuildDiagnosticThenScope(
    mir::CompilationUnit& unit, mir::LocalId count_var,
    const CheckVerdict& verdict, std::string origin) -> mir::Block {
  mir::Block block;
  const mir::TypeId int_type = unit.builtins.int_type;

  std::vector<mir::RuntimePrintItem> items;
  items.emplace_back(mir::RuntimePrintLiteral{.text = verdict.prefix_text});
  if (verdict.include_count_value) {
    const mir::ExprId count_read_id =
        block.exprs.Add(mir::MakeLocalRefExpr(count_var, int_type));
    items.emplace_back(
        mir::RuntimePrintValue(
            count_read_id, int_type,
            mir::FormatSpec(
                value::FormatKind::kDecimal, mir::FormatModifiers{})));
    items.emplace_back(mir::RuntimePrintLiteral{.text = verdict.suffix_text});
  }

  // The verdict text is fixed-format decimal, so no %t directive is possible
  // and the time-unit power is unread.
  const mir::ExprId items_array =
      block.exprs.Add(BuildPrintItemsArray(unit, block, items, 0));

  const mir::ExprId runtime_id =
      block.exprs.Add(mir::MakeCurrentRuntimeCallExpr(unit.builtins.effects));

  // Format the verdict text, then route through the diagnostic broker's
  // EmitWarning method (LRM 20.10): unique / priority deferred-check failures
  // are warning-severity by spec. The origin tag is the qualified statement's
  // source location, threaded in from the cascade entry.
  const mir::ExprId text_id = block.exprs.Add(
      BuildFormatCallExpr(unit, block, runtime_id, items_array));
  const mir::ExprId diagnostic_id = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{.target = support::BuiltinFn::kDiagnostic},
                  .arguments = {runtime_id}},
          .type = unit.builtins.diagnostic});
  const mir::ExprId origin_lit = block.exprs.Add(
      mir::Expr{
          .data = mir::StringLiteral{.value = std::move(origin)},
          .type = unit.builtins.string});
  const mir::ExprId origin_id = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Construct{}, .arguments = {origin_lit}},
          .type = unit.builtins.string});
  const mir::ExprId emit_call_id = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{.target = support::BuiltinFn::kEmitWarning},
                  .arguments = {diagnostic_id, origin_id, text_id}},
          .type = unit.builtins.void_type});
  block.AppendStmt(mir::ExprStmt{.expr = emit_call_id});
  return block;
}

auto BuildUniqueCheckClosure(
    UnitLowerer& unit_lowerer, const WalkFrame& wrapper_frame,
    hir::UniquePriorityCheck check,
    const std::vector<SnapshotBinding>& snapshot_vars, std::string origin)
    -> mir::Expr {
  ClosureBuilder closure(unit_lowerer.Unit(), wrapper_frame);
  mir::Block& body = closure.Body();

  const mir::TypeId int_type = unit_lowerer.Unit().builtins.int_type;
  const mir::TypeId bit1_type = unit_lowerer.Unit().builtins.bit1;

  std::vector<mir::ExprId> inner_reads;
  inner_reads.reserve(snapshot_vars.size());
  for (const SnapshotBinding& snap : snapshot_vars) {
    const BodyBindingRef ref = closure.Bindings().EnsureCarrier(snap.origin);
    inner_reads.push_back(
        body.exprs.Add(closure.Bindings().MakeReadExpr(ref, body)));
  }

  const mir::LocalId count_var = closure.Bindings().DeclareAnonymous(
      mir::LocalDecl{.name = "_lyra_unique_count", .type = int_type});

  const mir::ExprId zero_init_id =
      body.exprs.Add(mir::MakeIntLiteral(int_type, 0));
  body.AppendStmt(
      mir::LocalDeclStmt{.target = count_var, .init = zero_init_id});

  for (const mir::ExprId bit_read : inner_reads) {
    const mir::ExprId one_lit =
        body.exprs.Add(mir::MakeIntLiteral(int_type, 1));
    const mir::ExprId zero_lit =
        body.exprs.Add(mir::MakeIntLiteral(int_type, 0));
    const mir::ExprId cond_value = body.exprs.Add(
        mir::Expr{
            .data =
                mir::ConditionalExpr{
                    .condition = ReduceToCondition(body, bit_read, bit1_type),
                    .then_value = one_lit,
                    .else_value = zero_lit},
            .type = int_type});
    const mir::ExprId count_read =
        body.exprs.Add(mir::MakeLocalRefExpr(count_var, int_type));
    const mir::ExprId added = body.exprs.Add(
        mir::Expr{
            .data =
                mir::BinaryExpr{
                    .op = mir::BinaryOp::kAdd,
                    .lhs = count_read,
                    .rhs = cond_value},
            .type = int_type});
    const mir::ExprId count_target =
        body.exprs.Add(mir::MakeLocalRefExpr(count_var, int_type));
    const mir::ExprId assign = body.exprs.Add(
        mir::Expr{
            .data = mir::AssignExpr{.target = count_target, .value = added},
            .type = int_type});
    body.AppendStmt(mir::ExprStmt{.expr = assign});
  }

  const CheckVerdict verdict = VerdictFor(check, snapshot_vars.size());

  const mir::ExprId final_count_read =
      body.exprs.Add(mir::MakeLocalRefExpr(count_var, int_type));
  const mir::ExprId expected_lit = body.exprs.Add(
      mir::MakeIntLiteral(
          int_type, static_cast<std::int64_t>(verdict.expected)));
  const mir::ExprId predicate_id = body.exprs.Add(
      mir::Expr{
          .data =
              mir::BinaryExpr{
                  .op = verdict.violation_op,
                  .lhs = final_count_read,
                  .rhs = expected_lit},
          .type = int_type});

  mir::Block diag_block = BuildDiagnosticThenScope(
      unit_lowerer.Unit(), count_var, verdict, std::move(origin));

  const mir::BlockId diag_scope_id =
      body.child_scopes.Add(std::move(diag_block));

  body.AppendStmt(
      mir::IfStmt{
          .condition = ReduceToCondition(body, predicate_id, bit1_type),
          .then_scope = diag_scope_id,
          .else_scope = std::nullopt});

  return closure.BuildVoid();
}

}  // namespace

auto BuildDeferredCheckCascade(
    UnitLowerer& unit_lowerer, WalkFrame frame, mir::Block wrapper,
    std::vector<DeferredCheckBranch> branches,
    std::optional<mir::Block> tail_else, hir::UniquePriorityCheck check,
    std::optional<std::string> outer_label, diag::SourceSpan span)
    -> mir::Stmt {
  const mir::TypeId void_type = unit_lowerer.Unit().builtins.void_type;
  const mir::TypeId int_type = unit_lowerer.Unit().builtins.int_type;
  const mir::TypeId bit1_type = unit_lowerer.Unit().builtins.bit1;

  // SnapshotPredicate / BuildDefaultValueExpr append to wrapper, so route
  // through a wrapper-local frame; the cascade levels each derive their own
  // local frames below. The snapshot vars are body-locals of this callable, so
  // every read names them directly with no nesting bookkeeping.
  const WalkFrame wrapper_frame = frame.WithBlock(&wrapper);

  std::vector<SnapshotBinding> snapshot_vars;
  snapshot_vars.reserve(branches.size());
  for (std::size_t i = 0; i < branches.size(); ++i) {
    const mir::TypeId predicate_type =
        wrapper.exprs.Get(branches[i].predicate).type;
    snapshot_vars.push_back(SnapshotPredicate(
        unit_lowerer, wrapper_frame, wrapper, i, predicate_type,
        branches[i].predicate));
  }

  mir::Expr closure = BuildUniqueCheckClosure(
      unit_lowerer, wrapper_frame, check, snapshot_vars,
      FormatRuntimeOriginString(span, unit_lowerer.SourceManager()));
  const mir::ExprId closure_expr_id = wrapper.exprs.Add(std::move(closure));

  const mir::DeferredCheckSiteId site_id =
      unit_lowerer.Unit().AllocateDeferredCheckSiteId();
  const mir::ExprId runtime_id =
      wrapper.exprs.Add(BuildCurrentRuntimeCallExpr(unit_lowerer));
  const mir::ExprId site_id_expr = wrapper.exprs.Add(
      mir::MakeIntLiteral(int_type, static_cast<std::int64_t>(site_id.value)));
  const mir::ExprId submit_expr_id = wrapper.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = support::BuiltinFn::kSubmitObserved},
                  .arguments = {runtime_id, site_id_expr, closure_expr_id}},
          .type = void_type});
  wrapper.AppendStmt(mir::ExprStmt{.expr = submit_expr_id});

  // The snapshot vars are body-locals; each cascade level names its predicate
  // snapshot directly.
  std::optional<mir::Block> tail = std::move(tail_else);
  for (std::size_t i = branches.size(); i-- > 1;) {
    mir::Block level_block;
    const mir::ExprId cond_read = level_block.exprs.Add(
        mir::MakeLocalRefExpr(snapshot_vars[i].local, int_type));

    const mir::BlockId body_scope_id =
        level_block.child_scopes.Add(std::move(branches[i].body));
    std::optional<mir::BlockId> else_scope_id;
    if (tail.has_value()) {
      else_scope_id = level_block.child_scopes.Add(std::move(*tail));
    }

    level_block.AppendStmt(
        mir::IfStmt{
            .condition = ReduceToCondition(level_block, cond_read, bit1_type),
            .then_scope = body_scope_id,
            .else_scope = else_scope_id});
    tail = std::move(level_block);
  }

  if (!branches.empty()) {
    const mir::ExprId cond_read0 = wrapper.exprs.Add(
        mir::MakeLocalRefExpr(snapshot_vars[0].local, int_type));

    const mir::BlockId body0_id =
        wrapper.child_scopes.Add(std::move(branches[0].body));
    std::optional<mir::BlockId> else0_id;
    if (tail.has_value()) {
      else0_id = wrapper.child_scopes.Add(std::move(*tail));
    }

    wrapper.AppendStmt(
        mir::IfStmt{
            .condition = ReduceToCondition(wrapper, cond_read0, bit1_type),
            .then_scope = body0_id,
            .else_scope = else0_id});
  }

  const mir::BlockId wrapper_scope_id =
      frame.current_block->child_scopes.Add(std::move(wrapper));

  return mir::Stmt{
      .label = std::move(outer_label),
      .data = mir::BlockStmt{.scope = wrapper_scope_id}};
}

auto LowerUniqueIfStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::IfStmt& root, diag::SourceSpan span) -> diag::Result<mir::Stmt> {
  const auto& hir_proc = process.HirBody();
  const auto cascade = FlattenUniqueCascade(hir_proc, root);

  mir::Block wrapper;
  const WalkFrame wrapper_frame = frame.WithBlock(&wrapper);

  // Lower each branch condition into wrapper; these are the predicates
  // that BuildDeferredCheckCascade will snapshot.
  std::vector<mir::ExprId> predicates;
  predicates.reserve(cascade.conditions.size());
  for (const hir::ExprId hir_cond : cascade.conditions) {
    auto cond_or =
        process.LowerExpr(hir_proc.exprs.Get(hir_cond), wrapper_frame);
    if (!cond_or) return std::unexpected(std::move(cond_or.error()));
    predicates.push_back(wrapper.exprs.Add(*std::move(cond_or)));
  }

  // Lower each body into its own child scope.
  std::vector<DeferredCheckBranch> branches;
  branches.reserve(cascade.bodies.size());
  for (std::size_t i = 0; i < cascade.bodies.size(); ++i) {
    auto body_or =
        LowerStmtIntoChildScope(process, wrapper_frame, cascade.bodies[i]);
    if (!body_or) return std::unexpected(std::move(body_or.error()));
    branches.push_back(
        DeferredCheckBranch{
            .predicate = predicates[i], .body = std::move(*body_or)});
  }

  std::optional<mir::Block> tail_scope;
  if (cascade.tail_else.has_value()) {
    auto tail_or =
        LowerStmtIntoChildScope(process, wrapper_frame, *cascade.tail_else);
    if (!tail_or) return std::unexpected(std::move(tail_or.error()));
    tail_scope = std::move(*tail_or);
  }

  return BuildDeferredCheckCascade(
      process.Owner(), frame, std::move(wrapper), std::move(branches),
      std::move(tail_scope), *root.check, std::move(label), span);
}

}  // namespace lyra::lowering::hir_to_mir
