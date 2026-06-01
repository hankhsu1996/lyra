#include "lyra/lowering/hir_to_mir/lower_stmt.hpp"

#include <expected>
#include <memory>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/integral_constant.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/case_cascade.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/delay_time_resolver.hpp"
#include "lyra/lowering/hir_to_mir/inside_predicate.hpp"
#include "lyra/lowering/hir_to_mir/lower_deferred_check.hpp"
#include "lyra/lowering/hir_to_mir/lower_expr.hpp"
#include "lyra/lowering/hir_to_mir/procedural_scope_helpers.hpp"
#include "lyra/lowering/hir_to_mir/sensitivity_wait.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/procedural_hops.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto IntegralConstantToInt64(const hir::IntegralConstant& c) -> std::int64_t {
  if (c.state_kind == hir::IntegralStateKind::kFourState) {
    throw InternalError(
        "IntegralConstantToInt64: 4-state literal in integer-delay context");
  }
  if (c.value_words.empty()) {
    return 0;
  }
  const std::uint64_t raw = c.value_words[0];
  if (c.signedness == hir::Signedness::kSigned && c.width > 0U &&
      c.width < 64U) {
    const std::uint64_t sign_bit = std::uint64_t{1} << (c.width - 1U);
    if ((raw & sign_bit) != 0U) {
      const std::uint64_t fill =
          ~((std::uint64_t{1} << c.width) - std::uint64_t{1});
      return static_cast<std::int64_t>(raw | fill);
    }
  }
  return static_cast<std::int64_t>(raw);
}

auto ResolveDelayDuration(
    const DelayTimeResolver& resolver, const hir::Expr& duration)
    -> diag::Result<SimDuration> {
  if (const auto* primary = std::get_if<hir::PrimaryExpr>(&duration.data)) {
    if (const auto* int_lit =
            std::get_if<hir::IntegerLiteral>(&primary->data)) {
      return resolver.ResolveIntegerDelay(
          IntegralConstantToInt64(int_lit->value), duration.span);
    }
    if (const auto* time_lit = std::get_if<hir::TimeLiteral>(&primary->data)) {
      return resolver.ResolveTimeLiteral(
          time_lit->value, time_lit->scale, duration.span);
    }
  }
  return diag::Unsupported(
      duration.span, diag::DiagCode::kUnsupportedDelayExpressionForm,
      "delay durations beyond an integer or time literal are not yet supported",
      diag::UnsupportedCategory::kFeature);
}

auto ResolveDelayTicks(
    const ProcessLoweringState& proc_state, const hir::ProceduralBody& hir_proc,
    const hir::DelayControl& d) -> diag::Result<SimDuration> {
  const DelayTimeResolver resolver{proc_state.Resolution()};
  return ResolveDelayDuration(resolver, hir_proc.exprs.at(d.duration.value));
}

auto LowerEmptyStmt(const hir::Stmt& stmt) -> diag::Result<mir::Stmt> {
  return mir::Stmt{
      .label = stmt.label,
      .data = mir::EmptyStmt{},
      .child_procedural_scopes = {}};
}

auto LowerVarDeclStmt(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::ProceduralBody& hir_proc, const hir::Stmt& stmt,
    const hir::VarDeclStmt& v) -> diag::Result<mir::Stmt> {
  const auto& hir_local = hir_proc.procedural_vars.at(v.var.value);
  const mir::TypeId type = unit_state.TranslateType(hir_local.type);
  const mir::ProceduralVarId local_id = proc_scope_state.AddProceduralVar(
      mir::ProceduralVarDecl{.name = hir_local.name, .type = type});
  proc_state.MapProceduralVar(
      v.var,
      ProceduralVarBinding{
          .declaration_procedural_depth = proc_state.CurrentProceduralDepth(),
          .var = local_id});
  mir::ExprId init_id{};
  if (v.init.has_value()) {
    auto init_or = LowerExpr(
        unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
        hir_proc.exprs.at(v.init->value));
    if (!init_or) {
      return std::unexpected(std::move(init_or.error()));
    }
    init_id = proc_scope_state.AddExpr(*std::move(init_or));
  } else {
    init_id = SynthesizeDefaultValueExpr(unit_state, proc_scope_state, type);
  }
  return mir::Stmt{
      .label = stmt.label,
      .data =
          mir::ProceduralVarDeclStmt{
              .target =
                  mir::ProceduralVarRef{
                      .hops = mir::ProceduralHops{.value = 0}, .var = local_id},
              .init = init_id},
      .child_procedural_scopes = {}};
}

// LRM 11.4.12 LHS destructuring desugar. Triggered when an ExprStmt wraps an
// AssignExpr whose LHS is a ConcatExpr -- the only context in which
// destructuring is grammatically legal. Emits:
//
//   Block {
//     ProceduralVarDeclStmt _t : PackedArray(total_w, unsigned, four_state)
//     ExprStmt(AssignExpr _t = rhs)            -- RHS evaluated once
//     ExprStmt(AssignExpr part[0] = _t.Slice(off_0, w_0))   -- MSB-most part
//     ...
//     ExprStmt(AssignExpr part[N-1] = _t.Slice(0, w_last))  -- LSB-most part
//   }
//
// For NBA (`kind == kNonBlocking`), each per-part assignment goes through the
// NBA closure machinery instead.
auto LowerDestructuringAssign(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    ProcessLoweringState& proc_state, const hir::ProceduralBody& hir_proc,
    const hir::Stmt& stmt, const hir::AssignExpr& assign,
    const hir::ConcatExpr& lhs_concat) -> diag::Result<mir::Stmt> {
  ProceduralScopeLoweringState wrapper_state;
  ProceduralDepthGuard wrapper_depth_guard{proc_state};

  std::vector<std::uint64_t> part_widths;
  part_widths.reserve(lhs_concat.operands.size());
  bool any_four_state = false;
  std::uint64_t total_width = 0;
  for (const hir::ExprId op_id : lhs_concat.operands) {
    const hir::Expr& op = hir_proc.exprs.at(op_id.value);
    const hir::Type& op_ty = unit_state.GetHirType(op.type);
    if (!op_ty.IsPackedArray()) {
      throw InternalError(
          "LowerDestructuringAssign: destructuring operand is not "
          "a packed integral type");
    }
    const auto& packed = op_ty.AsPackedArray();
    const std::uint64_t w = packed.BitWidth();
    part_widths.push_back(w);
    total_width += w;
    any_four_state = any_four_state || packed.IsFourState();
  }
  if (total_width == 0) {
    throw InternalError(
        "LowerDestructuringAssign: destructuring total width must be positive");
  }

  const mir::TypeId temp_type = unit_state.AddType(
      mir::TypeData{mir::PackedArrayType{
          .atom = any_four_state ? mir::BitAtom::kLogic : mir::BitAtom::kBit,
          .signedness = mir::Signedness::kUnsigned,
          .dims = {mir::PackedRange{
              .left = static_cast<std::int64_t>(total_width) - 1, .right = 0}},
          .form = mir::PackedArrayForm::kExplicit}});

  const mir::ProceduralVarId temp_var = wrapper_state.AddProceduralVar(
      mir::ProceduralVarDecl{.name = "_lyra_destruct_rhs", .type = temp_type});
  const mir::ExprId temp_default_init =
      SynthesizeDefaultValueExpr(unit_state, wrapper_state, temp_type);
  const mir::StmtId temp_decl_id = wrapper_state.AddStmt(
      mir::Stmt{
          .label = std::nullopt,
          .data =
              mir::ProceduralVarDeclStmt{
                  .target =
                      mir::ProceduralVarRef{
                          .hops = mir::ProceduralHops{.value = 0},
                          .var = temp_var},
                  .init = temp_default_init},
          .child_procedural_scopes = {}});
  wrapper_state.AddRootStmt(temp_decl_id);

  // RHS is evaluated once; the snapshot temp is what gets distributed,
  // which is what makes `{a, b} = {b, a}` swap correctly.
  auto rhs_or = LowerExpr(
      unit_state, scope_state, proc_state, wrapper_state, hir_proc,
      hir_proc.exprs.at(assign.rhs.value));
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
  mir::ExprId rhs_id = wrapper_state.AddExpr(*std::move(rhs_or));
  if (wrapper_state.GetExpr(rhs_id).type != temp_type) {
    rhs_id = wrapper_state.AddExpr(
        mir::Expr{
            .data =
                mir::ConversionExpr{
                    .operand = rhs_id, .kind = mir::ConversionKind::kImplicit},
            .type = temp_type});
  }

  const mir::ExprId temp_assign_target = wrapper_state.AddExpr(
      mir::Expr{
          .data =
              mir::ProceduralVarRef{
                  .hops = mir::ProceduralHops{.value = 0}, .var = temp_var},
          .type = temp_type});
  const mir::ExprId temp_assign_id = wrapper_state.AddExpr(
      mir::Expr{
          .data =
              mir::AssignExpr{.target = temp_assign_target, .value = rhs_id},
          .type = temp_type});
  wrapper_state.AddRootStmt(wrapper_state.AddStmt(
      mir::Stmt{
          .label = std::nullopt,
          .data = mir::ExprStmt{.expr = temp_assign_id},
          .child_procedural_scopes = {}}));

  // MSB-first per LRM 11.4.12: operands[0] occupies the high bits of the
  // snapshot, operands.back() the low bits.
  std::uint64_t offset = total_width;
  for (std::size_t i = 0; i < lhs_concat.operands.size(); ++i) {
    const std::uint64_t w = part_widths[i];
    offset -= w;

    auto part_lhs_or = LowerExpr(
        unit_state, scope_state, proc_state, wrapper_state, hir_proc,
        hir_proc.exprs.at(lhs_concat.operands[i].value));
    if (!part_lhs_or) {
      return std::unexpected(std::move(part_lhs_or.error()));
    }
    const mir::TypeId part_mir_type = (*part_lhs_or).type;
    const mir::ExprId part_lhs_id =
        wrapper_state.AddExpr(*std::move(part_lhs_or));

    const mir::ExprId base_index = wrapper_state.AddExpr(
        unit_state.MakeInt32LiteralExpr(static_cast<std::int64_t>(offset)));
    const mir::ExprId width_lit = wrapper_state.AddExpr(
        unit_state.MakeInt32LiteralExpr(static_cast<std::int64_t>(w)));
    const mir::ExprId temp_ref = wrapper_state.AddExpr(
        mir::Expr{
            .data =
                mir::ProceduralVarRef{
                    .hops = mir::ProceduralHops{.value = 0}, .var = temp_var},
            .type = temp_type});
    const mir::TypeId slice_type = unit_state.AddType(
        mir::TypeData{mir::PackedArrayType{
            .atom = any_four_state ? mir::BitAtom::kLogic : mir::BitAtom::kBit,
            .signedness = mir::Signedness::kUnsigned,
            .dims = {mir::PackedRange{
                .left = static_cast<std::int64_t>(w) - 1, .right = 0}},
            .form = mir::PackedArrayForm::kExplicit}});
    const mir::ExprId slice_id = wrapper_state.AddExpr(
        mir::Expr{
            .data =
                mir::RangeSelectExpr{
                    .base_value = temp_ref,
                    .bounds =
                        mir::RangeIndexedUpBounds{
                            .base_index = base_index, .width = width_lit}},
            .type = slice_type});
    mir::ExprId rhs_for_part = slice_id;
    if (part_mir_type != slice_type) {
      rhs_for_part = wrapper_state.AddExpr(
          mir::Expr{
              .data =
                  mir::ConversionExpr{
                      .operand = slice_id,
                      .kind = mir::ConversionKind::kImplicit},
              .type = part_mir_type});
    }

    mir::ExprId per_part_expr_id{};
    if (assign.kind == hir::AssignKind::kBlocking) {
      per_part_expr_id = wrapper_state.AddExpr(
          mir::Expr{
              .data =
                  mir::AssignExpr{.target = part_lhs_id, .value = rhs_for_part},
              .type = part_mir_type});
    } else {
      mir::Expr closure_expr = BuildNbaSubmitClosureExpr(
          unit_state, wrapper_state, part_lhs_id, rhs_for_part, part_mir_type);
      const mir::ExprId closure_id =
          wrapper_state.AddExpr(std::move(closure_expr));
      per_part_expr_id = wrapper_state.AddExpr(
          mir::Expr{
              .data =
                  mir::RuntimeCallExpr{
                      .call = mir::RuntimeSubmitNbaCall{.closure = closure_id}},
              .type = unit_state.Builtins().void_type});
    }
    wrapper_state.AddRootStmt(wrapper_state.AddStmt(
        mir::Stmt{
            .label = std::nullopt,
            .data = mir::ExprStmt{.expr = per_part_expr_id},
            .child_procedural_scopes = {}}));
  }

  std::vector<mir::ProceduralScope> child_scopes;
  const mir::ProceduralScopeId wrapper_scope_id =
      AddChildProceduralScope(child_scopes, wrapper_state.Finish());

  return mir::Stmt{
      .label = stmt.label,
      .data = mir::BlockStmt{.scope = wrapper_scope_id},
      .child_procedural_scopes = std::move(child_scopes)};
}

auto LowerExprStmt(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::ProceduralBody& hir_proc, const hir::Stmt& stmt,
    const hir::ExprStmt& e) -> diag::Result<mir::Stmt> {
  // LRM 11.4.12 LHS destructuring: detect AssignExpr-with-ConcatExpr-LHS
  // and dispatch to the snapshot+distribute desugar. Destructuring is
  // grammatically statement-only (LRM A.6.2), so this is the only context
  // it can appear in. Compound destructuring is rejected by slang at
  // parsing.
  const hir::Expr& inner = hir_proc.exprs.at(e.expr.value);
  if (const auto* assign = std::get_if<hir::AssignExpr>(&inner.data)) {
    const hir::Expr& lhs = hir_proc.exprs.at(assign->lhs.value);
    if (const auto* concat = std::get_if<hir::ConcatExpr>(&lhs.data)) {
      if (assign->compound_op.has_value()) {
        throw InternalError(
            "LowerExprStmt: compound assignment with concatenation lvalue "
            "is not a legal SV form (LRM A.6.2 grammar)");
      }
      return LowerDestructuringAssign(
          unit_state, scope_state, proc_state, hir_proc, stmt, *assign,
          *concat);
    }
  }

  auto expr_or = LowerExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
      hir_proc.exprs.at(e.expr.value));
  if (!expr_or) {
    return std::unexpected(std::move(expr_or.error()));
  }
  return mir::Stmt{
      .label = stmt.label,
      .data =
          mir::ExprStmt{.expr = proc_scope_state.AddExpr(*std::move(expr_or))},
      .child_procedural_scopes = {}};
}

auto LowerReturnStmt(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::ProceduralBody& hir_proc, const hir::Stmt& stmt,
    const hir::ReturnStmt& r) -> diag::Result<mir::Stmt> {
  std::optional<mir::ExprId> value;
  if (r.value.has_value()) {
    auto value_or = LowerExpr(
        unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
        hir_proc.exprs.at(r.value->value));
    if (!value_or) return std::unexpected(std::move(value_or.error()));
    value = proc_scope_state.AddExpr(*std::move(value_or));
  }
  return mir::Stmt{
      .label = stmt.label,
      .data = mir::ReturnStmt{.value = value},
      .child_procedural_scopes = {}};
}

auto LowerBlockStmt(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    ProcessLoweringState& proc_state, const hir::ProceduralBody& hir_proc,
    const hir::Stmt& stmt, const hir::BlockStmt& b) -> diag::Result<mir::Stmt> {
  ProceduralScopeLoweringState child_proc_scope_state;
  ProceduralDepthGuard depth_guard{proc_state};
  for (const hir::StmtId child_hir_id : b.statements) {
    const hir::Stmt& child = hir_proc.stmts.at(child_hir_id.value);
    auto lowered = LowerStmt(
        unit_state, scope_state, proc_state, child_proc_scope_state, hir_proc,
        child);
    if (!lowered) {
      return std::unexpected(std::move(lowered.error()));
    }
    const mir::StmtId child_id =
        child_proc_scope_state.AddStmt(*std::move(lowered));
    child_proc_scope_state.AddRootStmt(child_id);
  }
  std::vector<mir::ProceduralScope> child_scopes;
  const mir::ProceduralScopeId scope_id =
      AddChildProceduralScope(child_scopes, child_proc_scope_state.Finish());
  return mir::Stmt{
      .label = stmt.label,
      .data = mir::BlockStmt{.scope = scope_id},
      .child_procedural_scopes = std::move(child_scopes)};
}

auto LowerIfStmt(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::ProceduralBody& hir_proc, const hir::Stmt& stmt,
    const hir::IfStmt& i) -> diag::Result<mir::Stmt> {
  if (i.check.has_value()) {
    return LowerUniqueIfStmt(
        unit_state, scope_state, proc_state, hir_proc, stmt, i);
  }
  auto cond_expr_or = LowerExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
      hir_proc.exprs.at(i.condition.value));
  if (!cond_expr_or) {
    return std::unexpected(std::move(cond_expr_or.error()));
  }
  const mir::ExprId cond_id =
      proc_scope_state.AddExpr(*std::move(cond_expr_or));

  std::vector<mir::ProceduralScope> child_scopes;
  auto then_or = LowerStmtIntoChildScope(
      unit_state, scope_state, proc_state, hir_proc, i.then_stmt);
  if (!then_or) return std::unexpected(std::move(then_or.error()));
  const mir::ProceduralScopeId then_scope_id =
      AddChildProceduralScope(child_scopes, std::move(*then_or));

  std::optional<mir::ProceduralScopeId> else_scope_id;
  if (i.else_stmt.has_value()) {
    auto else_or = LowerStmtIntoChildScope(
        unit_state, scope_state, proc_state, hir_proc, *i.else_stmt);
    if (!else_or) return std::unexpected(std::move(else_or.error()));
    else_scope_id = AddChildProceduralScope(child_scopes, std::move(*else_or));
  }

  return mir::Stmt{
      .label = stmt.label,
      .data =
          mir::IfStmt{
              .condition = cond_id,
              .then_scope = then_scope_id,
              .else_scope = else_scope_id},
      .child_procedural_scopes = std::move(child_scopes)};
}

auto LowerCaseStmt(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    ProcessLoweringState& proc_state, const hir::ProceduralBody& hir_proc,
    const hir::Stmt& stmt, const hir::CaseStmt& c) -> diag::Result<mir::Stmt> {
  const mir::TypeId bit_type = unit_state.Builtins().bit1;
  const mir::BinaryOp compare_op = [&] {
    switch (c.condition_kind) {
      case hir::CaseCondition::kNormal:
        return mir::BinaryOp::kEquality;
      case hir::CaseCondition::kWildcardJustZ:
        return mir::BinaryOp::kCasezEquality;
      case hir::CaseCondition::kWildcardXOrZ:
        return mir::BinaryOp::kCasexEquality;
    }
    throw InternalError("LowerCaseStmt: unknown hir::CaseCondition");
  }();

  ProceduralScopeLoweringState wrapper_state;
  ProceduralDepthGuard wrapper_depth_guard{proc_state};

  auto cond_or = LowerExpr(
      unit_state, scope_state, proc_state, wrapper_state, hir_proc,
      hir_proc.exprs.at(c.condition.value));
  if (!cond_or) {
    return std::unexpected(std::move(cond_or.error()));
  }
  mir::ExprId cond_expr_id = wrapper_state.AddExpr(*std::move(cond_or));

  // Slang's case-context type unification may wrap the selector in a
  // ConversionExpr widening from a built-in (form=int) type to a form=explicit
  // packed type. The cpp backend cannot yet emit an assignment from a form=int
  // source into a form=explicit lvalue, and the cascade builds its own
  // equality comparisons regardless of the snapshot's form. Peel the outer
  // conversion so the snapshot var matches the unwrapped source type --
  // but only when state-kind is preserved, otherwise the snapshot ends up
  // 2-state while wildcard labels are 4-state and PackedArray::ExpectSameShape
  // rejects the per-label compare.
  auto packed_state = [&](mir::TypeId tid) -> std::optional<bool> {
    const auto& ty = unit_state.GetType(tid);
    if (const auto* pa = std::get_if<mir::PackedArrayType>(&ty.data)) {
      return pa->IsFourState();
    }
    if (const auto* en = std::get_if<mir::EnumType>(&ty.data)) {
      return en->base.IsFourState();
    }
    return std::nullopt;
  };
  if (const auto* cv = std::get_if<mir::ConversionExpr>(
          &wrapper_state.GetExpr(cond_expr_id).data)) {
    const mir::TypeId dst_tid = wrapper_state.GetExpr(cond_expr_id).type;
    const mir::TypeId src_tid = wrapper_state.GetExpr(cv->operand).type;
    const auto dst_state = packed_state(dst_tid);
    const auto src_state = packed_state(src_tid);
    if (dst_state.has_value() && src_state.has_value() &&
        *dst_state == *src_state) {
      cond_expr_id = cv->operand;
    }
  }

  const CaseSnapshotRefs snapshot =
      AppendCaseSnapshot(unit_state, wrapper_state, cond_expr_id);

  // Each cascade level k > 0 sits one MIR scope deeper than the previous
  // level (it lives inside the prior level's else_scope). Bump proc_state by k
  // extra guards before lowering the contents of level k so procvar refs
  // receive the correct hops.
  auto with_extra_depth = [&](std::size_t extras, auto fn) {
    std::vector<std::unique_ptr<ProceduralDepthGuard>> guards;
    guards.reserve(extras);
    for (std::size_t i = 0; i < extras; ++i) {
      guards.push_back(std::make_unique<ProceduralDepthGuard>(proc_state));
    }
    return fn();
  };

  std::vector<mir::ProceduralScope> body_scopes;
  body_scopes.reserve(c.items.size());
  for (std::size_t i = 0; i < c.items.size(); ++i) {
    auto body_or = with_extra_depth(i, [&] {
      return LowerStmtIntoChildScope(
          unit_state, scope_state, proc_state, hir_proc, c.items[i].stmt);
    });
    if (!body_or) {
      return std::unexpected(std::move(body_or.error()));
    }
    body_scopes.push_back(std::move(*body_or));
  }

  std::optional<mir::ProceduralScope> default_scope;
  if (c.default_stmt.has_value()) {
    auto def_or = with_extra_depth(c.items.size(), [&] {
      return LowerStmtIntoChildScope(
          unit_state, scope_state, proc_state, hir_proc, *c.default_stmt);
    });
    if (!def_or) {
      return std::unexpected(std::move(def_or.error()));
    }
    default_scope = std::move(*def_or);
  }

  if (c.check.has_value()) {
    // All predicates live at wrapper depth (sel_hops=0); bodies were lowered
    // with the per-level depth guards above.
    std::vector<mir::ExprId> predicates;
    predicates.reserve(c.items.size());
    for (const auto& item : c.items) {
      auto pred_or = BuildEqualityChain(
          wrapper_state, snapshot, bit_type, compare_op, 0, item.labels.size(),
          [&](ProceduralScopeLoweringState& es,
              std::size_t li) -> diag::Result<mir::ExprId> {
            auto lab_or = LowerExpr(
                unit_state, scope_state, proc_state, es, hir_proc,
                hir_proc.exprs.at(item.labels[li].value));
            if (!lab_or) {
              return std::unexpected(std::move(lab_or.error()));
            }
            return es.AddExpr(*std::move(lab_or));
          });
      if (!pred_or) return std::unexpected(std::move(pred_or.error()));
      predicates.push_back(*pred_or);
    }

    std::vector<DeferredCheckBranch> branches;
    branches.reserve(c.items.size());
    for (std::size_t i = 0; i < c.items.size(); ++i) {
      branches.push_back(
          DeferredCheckBranch{
              .predicate = predicates[i], .body = std::move(body_scopes[i])});
    }
    return BuildDeferredCheckCascade(
        unit_state, std::move(wrapper_state), std::move(branches),
        std::move(default_scope), *c.check, stmt.label);
  }

  auto build_predicate = [&](ProceduralScopeLoweringState& enc,
                             std::size_t item_idx, std::uint32_t sel_hops) {
    return with_extra_depth(item_idx, [&] {
      return BuildEqualityChain(
          enc, snapshot, bit_type, compare_op, sel_hops,
          c.items[item_idx].labels.size(),
          [&](ProceduralScopeLoweringState& es,
              std::size_t li) -> diag::Result<mir::ExprId> {
            auto lab_or = LowerExpr(
                unit_state, scope_state, proc_state, es, hir_proc,
                hir_proc.exprs.at(c.items[item_idx].labels[li].value));
            if (!lab_or) {
              return std::unexpected(std::move(lab_or.error()));
            }
            return es.AddExpr(*std::move(lab_or));
          });
    });
  };

  return BuildCaseCascade(
      std::move(wrapper_state), stmt.label, c.items.size(),
      std::move(body_scopes), std::move(default_scope), build_predicate);
}

auto LowerCaseInsideStmt(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    ProcessLoweringState& proc_state, const hir::ProceduralBody& hir_proc,
    const hir::Stmt& stmt, const hir::CaseInsideStmt& c)
    -> diag::Result<mir::Stmt> {
  const mir::TypeId bit_type = unit_state.Builtins().bit1;

  ProceduralScopeLoweringState wrapper_state;
  ProceduralDepthGuard wrapper_depth_guard{proc_state};

  auto cond_or = LowerExpr(
      unit_state, scope_state, proc_state, wrapper_state, hir_proc,
      hir_proc.exprs.at(c.condition.value));
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  mir::ExprId cond_expr_id = wrapper_state.AddExpr(*std::move(cond_or));

  // Peel an outer ConversionExpr from a case-context-widened selector (same
  // reason as the plain case form: cpp backend cannot init form=explicit from
  // form=int, and the per-item inside predicate compares against the
  // unwrapped source type directly).
  if (const auto* cv = std::get_if<mir::ConversionExpr>(
          &wrapper_state.GetExpr(cond_expr_id).data)) {
    cond_expr_id = cv->operand;
  }

  const CaseSnapshotRefs snapshot =
      AppendCaseSnapshot(unit_state, wrapper_state, cond_expr_id);

  auto with_extra_depth = [&](std::size_t extras, auto fn) {
    std::vector<std::unique_ptr<ProceduralDepthGuard>> guards;
    guards.reserve(extras);
    for (std::size_t i = 0; i < extras; ++i) {
      guards.push_back(std::make_unique<ProceduralDepthGuard>(proc_state));
    }
    return fn();
  };

  std::vector<mir::ProceduralScope> body_scopes;
  body_scopes.reserve(c.items.size());
  for (std::size_t i = 0; i < c.items.size(); ++i) {
    auto body_or = with_extra_depth(i, [&] {
      return LowerStmtIntoChildScope(
          unit_state, scope_state, proc_state, hir_proc, c.items[i].stmt);
    });
    if (!body_or) {
      return std::unexpected(std::move(body_or.error()));
    }
    body_scopes.push_back(std::move(*body_or));
  }

  std::optional<mir::ProceduralScope> default_scope;
  if (c.default_stmt.has_value()) {
    auto def_or = with_extra_depth(c.items.size(), [&] {
      return LowerStmtIntoChildScope(
          unit_state, scope_state, proc_state, hir_proc, *c.default_stmt);
    });
    if (!def_or) {
      return std::unexpected(std::move(def_or.error()));
    }
    default_scope = std::move(*def_or);
  }

  // Builds the per-item inside-membership predicate against the snapshot ref,
  // matching the LRM 11.4.13 OR-reduction semantics. Per LRM 12.5.4 the
  // cascade's boolean-context test on this predicate already excludes 1'bx
  // (`explicit operator bool` returns false for any X bit), so no explicit
  // 2-state clamp is needed.
  auto build_item_predicate =
      [&](ProceduralScopeLoweringState& enc, std::size_t item_idx,
          std::uint32_t sel_hops) -> diag::Result<mir::ExprId> {
    return with_extra_depth(item_idx, [&] -> diag::Result<mir::ExprId> {
      const mir::ExprId sel_ref = enc.AddExpr(
          mir::Expr{
              .data =
                  mir::ProceduralVarRef{
                      .hops = mir::ProceduralHops{.value = sel_hops},
                      .var = snapshot.sel_var},
              .type = snapshot.sel_type});
      const auto& item = c.items[item_idx];
      if (item.items.empty()) {
        throw InternalError(
            "LowerCaseInsideStmt: case-inside item has empty range_list");
      }
      std::optional<mir::ExprId> acc;
      for (const auto& inside_item : item.items) {
        auto pred_or = BuildHirInsideItemPredicate(
            unit_state, scope_state, proc_state, enc, hir_proc, sel_ref,
            inside_item, bit_type);
        if (!pred_or) return std::unexpected(std::move(pred_or.error()));
        if (acc.has_value()) {
          acc = enc.AddExpr(
              mir::Expr{
                  .data =
                      mir::BinaryExpr{
                          .op = mir::BinaryOp::kLogicalOr,
                          .lhs = *acc,
                          .rhs = *pred_or},
                  .type = bit_type});
        } else {
          acc = *pred_or;
        }
      }
      return *acc;
    });
  };

  if (c.check.has_value()) {
    std::vector<mir::ExprId> predicates;
    predicates.reserve(c.items.size());
    for (std::size_t i = 0; i < c.items.size(); ++i) {
      auto pred_or = build_item_predicate(wrapper_state, i, 0);
      if (!pred_or) return std::unexpected(std::move(pred_or.error()));
      predicates.push_back(*pred_or);
    }

    std::vector<DeferredCheckBranch> branches;
    branches.reserve(c.items.size());
    for (std::size_t i = 0; i < c.items.size(); ++i) {
      branches.push_back(
          DeferredCheckBranch{
              .predicate = predicates[i], .body = std::move(body_scopes[i])});
    }
    return BuildDeferredCheckCascade(
        unit_state, std::move(wrapper_state), std::move(branches),
        std::move(default_scope), *c.check, stmt.label);
  }

  return BuildCaseCascade(
      std::move(wrapper_state), stmt.label, c.items.size(),
      std::move(body_scopes), std::move(default_scope), build_item_predicate);
}

auto LowerForStmt(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::ProceduralBody& hir_proc, const hir::Stmt& stmt,
    const hir::ForStmt& f) -> diag::Result<mir::Stmt> {
  std::vector<mir::ForInit> mir_init;
  mir_init.reserve(f.init.size());
  for (const auto& hinit : f.init) {
    auto item_or = std::visit(
        Overloaded{
            [&](const hir::ForInitDecl& d) -> diag::Result<mir::ForInit> {
              const auto& hir_local = hir_proc.procedural_vars.at(d.var.value);
              const mir::TypeId type = unit_state.TranslateType(hir_local.type);
              const mir::ProceduralVarId local_id =
                  proc_scope_state.AddProceduralVar(
                      mir::ProceduralVarDecl{
                          .name = hir_local.name, .type = type});
              proc_state.MapProceduralVar(
                  d.var, ProceduralVarBinding{
                             .declaration_procedural_depth =
                                 proc_state.CurrentProceduralDepth(),
                             .var = local_id});
              mir::ExprId init_id{};
              if (d.init.has_value()) {
                auto init_or = LowerExpr(
                    unit_state, scope_state, proc_state, proc_scope_state,
                    hir_proc, hir_proc.exprs.at(d.init->value));
                if (!init_or) {
                  return std::unexpected(std::move(init_or.error()));
                }
                init_id = proc_scope_state.AddExpr(*std::move(init_or));
              } else {
                init_id = SynthesizeDefaultValueExpr(
                    unit_state, proc_scope_state, type);
              }
              return mir::ForInit{mir::ForInitDecl{
                  .induction_var =
                      mir::ProceduralVarRef{
                          .hops = mir::ProceduralHops{.value = 0},
                          .var = local_id},
                  .init = init_id}};
            },
            [&](const hir::ForInitExpr& e) -> diag::Result<mir::ForInit> {
              auto expr_or = LowerExpr(
                  unit_state, scope_state, proc_state, proc_scope_state,
                  hir_proc, hir_proc.exprs.at(e.expr.value));
              if (!expr_or) {
                return std::unexpected(std::move(expr_or.error()));
              }
              return mir::ForInit{mir::ForInitExpr{
                  .expr = proc_scope_state.AddExpr(*std::move(expr_or))}};
            },
        },
        hinit);
    if (!item_or) return std::unexpected(std::move(item_or.error()));
    mir_init.push_back(*std::move(item_or));
  }

  std::optional<mir::ExprId> cond_id;
  if (f.condition.has_value()) {
    auto cond_or = LowerExpr(
        unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
        hir_proc.exprs.at(f.condition->value));
    if (!cond_or) {
      return std::unexpected(std::move(cond_or.error()));
    }
    cond_id = proc_scope_state.AddExpr(*std::move(cond_or));
  }

  std::vector<mir::ExprId> step_ids;
  step_ids.reserve(f.step.size());
  for (const hir::ExprId step_hid : f.step) {
    auto step_or = LowerExpr(
        unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
        hir_proc.exprs.at(step_hid.value));
    if (!step_or) {
      return std::unexpected(std::move(step_or.error()));
    }
    step_ids.push_back(proc_scope_state.AddExpr(*std::move(step_or)));
  }

  auto body_or = LowerStmtIntoChildScope(
      unit_state, scope_state, proc_state, hir_proc, f.body);
  if (!body_or) {
    return std::unexpected(std::move(body_or.error()));
  }

  std::vector<mir::ProceduralScope> child_scopes;
  const mir::ProceduralScopeId body_scope_id =
      AddChildProceduralScope(child_scopes, std::move(*body_or));

  return mir::Stmt{
      .label = stmt.label,
      .data =
          mir::ForStmt{
              .init = std::move(mir_init),
              .condition = cond_id,
              .step = std::move(step_ids),
              .scope = body_scope_id},
      .child_procedural_scopes = std::move(child_scopes)};
}

auto LowerWhileStmt(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::ProceduralBody& hir_proc, const hir::Stmt& stmt,
    const hir::WhileStmt& w) -> diag::Result<mir::Stmt> {
  auto cond_or = LowerExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
      hir_proc.exprs.at(w.condition.value));
  if (!cond_or) {
    return std::unexpected(std::move(cond_or.error()));
  }
  const mir::ExprId cond_id = proc_scope_state.AddExpr(*std::move(cond_or));

  auto body_or = LowerStmtIntoChildScope(
      unit_state, scope_state, proc_state, hir_proc, w.body);
  if (!body_or) {
    return std::unexpected(std::move(body_or.error()));
  }

  std::vector<mir::ProceduralScope> child_scopes;
  const mir::ProceduralScopeId body_scope_id =
      AddChildProceduralScope(child_scopes, std::move(*body_or));

  return mir::Stmt{
      .label = stmt.label,
      .data = mir::WhileStmt{.condition = cond_id, .scope = body_scope_id},
      .child_procedural_scopes = std::move(child_scopes)};
}

auto LowerRepeatStmt(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    ProcessLoweringState& proc_state, const hir::ProceduralBody& hir_proc,
    const hir::Stmt& stmt, const hir::RepeatStmt& r)
    -> diag::Result<mir::Stmt> {
  const mir::TypeId int_type = unit_state.Builtins().int32;
  const mir::TypeId bit_type = unit_state.Builtins().bit1;

  ProceduralScopeLoweringState wrapper_state;
  ProceduralDepthGuard wrapper_depth_guard{proc_state};

  auto count_or = LowerExpr(
      unit_state, scope_state, proc_state, wrapper_state, hir_proc,
      hir_proc.exprs.at(r.count.value));
  if (!count_or) {
    return std::unexpected(std::move(count_or.error()));
  }
  mir::ExprId count_expr_id = wrapper_state.AddExpr(*std::move(count_or));
  if (wrapper_state.GetExpr(count_expr_id).type != int_type) {
    count_expr_id = wrapper_state.AddExpr(
        mir::Expr{
            .data =
                mir::ConversionExpr{
                    .operand = count_expr_id,
                    .kind = mir::ConversionKind::kImplicit},
            .type = int_type});
  }

  const mir::ProceduralVarId count_var = wrapper_state.AddProceduralVar(
      mir::ProceduralVarDecl{.name = "_lyra_repeat_count", .type = int_type});

  const mir::StmtId count_decl_id = wrapper_state.AddStmt(
      mir::Stmt{
          .label = std::nullopt,
          .data =
              mir::ProceduralVarDeclStmt{
                  .target =
                      mir::ProceduralVarRef{
                          .hops = mir::ProceduralHops{.value = 0},
                          .var = count_var},
                  .init = count_expr_id},
          .child_procedural_scopes = {}});
  wrapper_state.AddRootStmt(count_decl_id);

  const mir::ProceduralVarId idx_var = wrapper_state.AddProceduralVar(
      mir::ProceduralVarDecl{.name = "_lyra_repeat_index", .type = int_type});

  const mir::ExprId zero_id =
      wrapper_state.AddExpr(unit_state.MakeInt32LiteralExpr(0));
  const mir::ExprId one_id =
      wrapper_state.AddExpr(unit_state.MakeInt32LiteralExpr(1));

  const mir::ExprId idx_ref_cond = wrapper_state.AddExpr(
      mir::Expr{
          .data =
              mir::ProceduralVarRef{
                  .hops = mir::ProceduralHops{.value = 0}, .var = idx_var},
          .type = int_type});
  const mir::ExprId count_ref_cond = wrapper_state.AddExpr(
      mir::Expr{
          .data =
              mir::ProceduralVarRef{
                  .hops = mir::ProceduralHops{.value = 0}, .var = count_var},
          .type = int_type});
  const mir::ExprId cond_id = wrapper_state.AddExpr(
      mir::Expr{
          .data =
              mir::BinaryExpr{
                  .op = mir::BinaryOp::kLessThan,
                  .lhs = idx_ref_cond,
                  .rhs = count_ref_cond},
          .type = bit_type});

  const mir::ExprId idx_ref_step = wrapper_state.AddExpr(
      mir::Expr{
          .data =
              mir::ProceduralVarRef{
                  .hops = mir::ProceduralHops{.value = 0}, .var = idx_var},
          .type = int_type});
  const mir::ExprId add_id = wrapper_state.AddExpr(
      mir::Expr{
          .data =
              mir::BinaryExpr{
                  .op = mir::BinaryOp::kAdd,
                  .lhs = idx_ref_step,
                  .rhs = one_id},
          .type = int_type});
  const mir::ExprId step_target_id = wrapper_state.AddExpr(
      mir::Expr{
          .data =
              mir::ProceduralVarRef{
                  .hops = mir::ProceduralHops{.value = 0}, .var = idx_var},
          .type = int_type});
  const mir::ExprId step_id = wrapper_state.AddExpr(
      mir::Expr{
          .data = mir::AssignExpr{.target = step_target_id, .value = add_id},
          .type = int_type});

  auto body_or = LowerStmtIntoChildScope(
      unit_state, scope_state, proc_state, hir_proc, r.body);
  if (!body_or) {
    return std::unexpected(std::move(body_or.error()));
  }

  std::vector<mir::ProceduralScope> for_child_scopes;
  const mir::ProceduralScopeId body_scope_id =
      AddChildProceduralScope(for_child_scopes, std::move(*body_or));

  std::vector<mir::ForInit> for_init;
  for_init.emplace_back(
      mir::ForInitDecl{
          .induction_var =
              mir::ProceduralVarRef{
                  .hops = mir::ProceduralHops{.value = 0}, .var = idx_var},
          .init = zero_id});

  const mir::StmtId for_stmt_id = wrapper_state.AddStmt(
      mir::Stmt{
          .label = std::nullopt,
          .data =
              mir::ForStmt{
                  .init = std::move(for_init),
                  .condition = cond_id,
                  .step = {step_id},
                  .scope = body_scope_id},
          .child_procedural_scopes = std::move(for_child_scopes)});
  wrapper_state.AddRootStmt(for_stmt_id);

  std::vector<mir::ProceduralScope> child_scopes;
  const mir::ProceduralScopeId wrapper_scope_id =
      AddChildProceduralScope(child_scopes, wrapper_state.Finish());

  return mir::Stmt{
      .label = stmt.label,
      .data = mir::BlockStmt{.scope = wrapper_scope_id},
      .child_procedural_scopes = std::move(child_scopes)};
}

auto LowerDoWhileStmt(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::ProceduralBody& hir_proc, const hir::Stmt& stmt,
    const hir::DoWhileStmt& d) -> diag::Result<mir::Stmt> {
  auto body_or = LowerStmtIntoChildScope(
      unit_state, scope_state, proc_state, hir_proc, d.body);
  if (!body_or) {
    return std::unexpected(std::move(body_or.error()));
  }

  auto cond_or = LowerExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
      hir_proc.exprs.at(d.condition.value));
  if (!cond_or) {
    return std::unexpected(std::move(cond_or.error()));
  }
  const mir::ExprId cond_id = proc_scope_state.AddExpr(*std::move(cond_or));

  std::vector<mir::ProceduralScope> child_scopes;
  const mir::ProceduralScopeId body_scope_id =
      AddChildProceduralScope(child_scopes, std::move(*body_or));

  return mir::Stmt{
      .label = stmt.label,
      .data = mir::DoWhileStmt{.condition = cond_id, .scope = body_scope_id},
      .child_procedural_scopes = std::move(child_scopes)};
}

auto LowerForeverStmt(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    ProcessLoweringState& proc_state, const hir::ProceduralBody& hir_proc,
    const hir::Stmt& stmt, const hir::ForeverStmt& f)
    -> diag::Result<mir::Stmt> {
  auto body_or = LowerStmtIntoChildScope(
      unit_state, scope_state, proc_state, hir_proc, f.body);
  if (!body_or) {
    return std::unexpected(std::move(body_or.error()));
  }

  std::vector<mir::ProceduralScope> child_scopes;
  const mir::ProceduralScopeId body_scope_id =
      AddChildProceduralScope(child_scopes, std::move(*body_or));

  return mir::Stmt{
      .label = stmt.label,
      .data =
          mir::ForStmt{
              .init = {},
              .condition = std::nullopt,
              .step = {},
              .scope = body_scope_id},
      .child_procedural_scopes = std::move(child_scopes)};
}

auto LowerBreakStmt(const hir::Stmt& stmt) -> diag::Result<mir::Stmt> {
  return mir::Stmt{
      .label = stmt.label,
      .data = mir::BreakStmt{},
      .child_procedural_scopes = {}};
}

auto LowerContinueStmt(const hir::Stmt& stmt) -> diag::Result<mir::Stmt> {
  return mir::Stmt{
      .label = stmt.label,
      .data = mir::ContinueStmt{},
      .child_procedural_scopes = {}};
}

// LRM 15.5.1 `-> e;`. HIR -> MIR collapses the trigger statement onto a plain
// method-call expression on the named event. The runtime's
// `NamedEvent::Trigger(services)` does not suspend, so no `co_await` wrap is
// emitted -- backend dispatches on `mir::IsSuspending(EventMethodInfo)`.
auto LowerEventTriggerStmt(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::ProceduralBody& hir_proc, const hir::Stmt& stmt,
    const hir::EventTriggerStmt& et) -> diag::Result<mir::Stmt> {
  auto receiver_or = LowerExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
      hir_proc.exprs.at(et.event.value));
  if (!receiver_or) return std::unexpected(std::move(receiver_or.error()));
  const mir::ExprId receiver_id =
      proc_scope_state.AddExpr(*std::move(receiver_or));
  mir::Expr call{
      .data =
          mir::CallExpr{
              .callee =
                  mir::BuiltinMethodCallee{
                      .method =
                          mir::EventMethodInfo{
                              .kind = mir::EventMethodKind::kTrigger}},
              .arguments = {receiver_id},
          },
      .type = unit_state.Builtins().void_type};
  const mir::ExprId call_id = proc_scope_state.AddExpr(std::move(call));
  return mir::Stmt{
      .label = stmt.label,
      .data = mir::ExprStmt{.expr = call_id},
      .child_procedural_scopes = {}};
}

// LRM 15.5.2 `@e body;`. HIR -> MIR expands the timed statement into a
// Block { ExprStmt(MethodCall(Await)); body; } inside a fresh child scope --
// the same shape used for `@*` (LRM 9.4.2.2), substituting a suspending
// method call in place of SensitivityWaitStmt.
auto LowerNamedEventTimedStmt(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    ProcessLoweringState& proc_state, const hir::ProceduralBody& hir_proc,
    const hir::Stmt& stmt, const hir::TimedStmt& t,
    const hir::NamedEventControl& nec) -> diag::Result<mir::Stmt> {
  ProceduralScopeLoweringState child_proc_scope_state;
  ProceduralDepthGuard depth_guard{proc_state};
  auto receiver_or = LowerExpr(
      unit_state, scope_state, proc_state, child_proc_scope_state, hir_proc,
      hir_proc.exprs.at(nec.event.value));
  if (!receiver_or) return std::unexpected(std::move(receiver_or.error()));
  const mir::ExprId receiver_id =
      child_proc_scope_state.AddExpr(*std::move(receiver_or));
  mir::Expr await_call{
      .data =
          mir::CallExpr{
              .callee =
                  mir::BuiltinMethodCallee{
                      .method =
                          mir::EventMethodInfo{
                              .kind = mir::EventMethodKind::kAwait}},
              .arguments = {receiver_id},
          },
      .type = unit_state.Builtins().void_type};
  const mir::ExprId await_id =
      child_proc_scope_state.AddExpr(std::move(await_call));
  child_proc_scope_state.AddRootStmt(child_proc_scope_state.AddStmt(
      mir::Stmt{
          .label = std::nullopt,
          .data = mir::ExprStmt{.expr = await_id},
          .child_procedural_scopes = {}}));
  const hir::Stmt& inner_hir = hir_proc.stmts.at(t.stmt.value);
  auto inner_or = LowerStmt(
      unit_state, scope_state, proc_state, child_proc_scope_state, hir_proc,
      inner_hir);
  if (!inner_or) {
    return std::unexpected(std::move(inner_or.error()));
  }
  child_proc_scope_state.AddRootStmt(
      child_proc_scope_state.AddStmt(*std::move(inner_or)));
  std::vector<mir::ProceduralScope> child_scopes;
  const mir::ProceduralScopeId scope_id =
      AddChildProceduralScope(child_scopes, child_proc_scope_state.Finish());
  return mir::Stmt{
      .label = stmt.label,
      .data = mir::BlockStmt{.scope = scope_id},
      .child_procedural_scopes = std::move(child_scopes)};
}

// LRM 9.4.2 `@(...) body` (explicit event control). Each `hir::EventTrigger`
// carries its event_expression and the DFA-computed leaf set. For the
// single-leaf case ("the expression is a simple selector chain on a packed
// var"), the leaf already carries the right edge polarity (or LSB-reduced
// for edge-qualified) and we lower directly to:
//
//   child_scope {
//     SensitivityWaitStmt(union of all triggers' single leaves)
//     body
//   }
//
// Multi-leaf event expressions (concatenation, arithmetic, cross-var, dynamic
// index) require a snapshot + re-eval wrapper to enforce LRM 9.4.2 "fire only
// when the expression's result changes" -- not yet implemented; reject with
// diagnostic.
auto LowerEventTimedStmt(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    ProcessLoweringState& proc_state, const hir::ProceduralBody& hir_proc,
    const hir::Stmt& stmt, const hir::TimedStmt& t, const hir::EventControl& ec)
    -> diag::Result<mir::Stmt> {
  std::vector<hir::SensitivityEntry> union_reads;
  union_reads.reserve(ec.triggers.size());
  for (const auto& trigger : ec.triggers) {
    // Compound (multi-leaf) requires a snapshot + re-eval wrapper to enforce
    // LRM 9.4.2 "fire only when the expression's result changes" -- not yet
    // implemented; reject. An empty leaf set falls through harmlessly (loop
    // iterates zero times, mirroring SensitivityWaitStmt's "no reads = wait
    // forever" convention).
    if (trigger.sensitivity_list.size() > 1) {
      return diag::Unsupported(
          stmt.span, diag::DiagCode::kUnsupportedEventTriggerForm,
          "compound event expressions (concatenation, arithmetic, dynamic "
          "index) are not yet supported",
          diag::UnsupportedCategory::kFeature);
    }
    for (auto leaf : trigger.sensitivity_list) {
      // LRM 9.4.2 LSB-reduce: an edge event monitors only the LSB of the
      // expression. The leaf's `bit_range` follows slang's `(lo, hi)`;
      // collapsing to `(lo, lo)` keeps the LSB only.
      if (leaf.edge_kind != hir::EventEdge::kAnyChange) {
        leaf.bit_range = {leaf.bit_range.first, leaf.bit_range.first};
      }
      union_reads.push_back(leaf);
    }
  }

  ProceduralScopeLoweringState child_proc_scope_state;
  ProceduralDepthGuard depth_guard{proc_state};
  child_proc_scope_state.AddRootStmt(child_proc_scope_state.AddStmt(
      BuildSensitivityWaitStmt(scope_state, union_reads)));
  const hir::Stmt& inner_hir = hir_proc.stmts.at(t.stmt.value);
  auto inner_or = LowerStmt(
      unit_state, scope_state, proc_state, child_proc_scope_state, hir_proc,
      inner_hir);
  if (!inner_or) {
    return std::unexpected(std::move(inner_or.error()));
  }
  child_proc_scope_state.AddRootStmt(
      child_proc_scope_state.AddStmt(*std::move(inner_or)));
  std::vector<mir::ProceduralScope> child_scopes;
  const mir::ProceduralScopeId scope_id =
      AddChildProceduralScope(child_scopes, child_proc_scope_state.Finish());
  return mir::Stmt{
      .label = stmt.label,
      .data = mir::BlockStmt{.scope = scope_id},
      .child_procedural_scopes = std::move(child_scopes)};
}

// LRM 9.4.2.2 `@* body` expands to:
//
//   child_scope {
//     SensitivityWaitStmt(ie.sensitivity_list)
//     body
//   }
auto LowerImplicitEventTimedStmt(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    ProcessLoweringState& proc_state, const hir::ProceduralBody& hir_proc,
    const hir::Stmt& stmt, const hir::TimedStmt& t,
    const hir::ImplicitEventControl& ie) -> diag::Result<mir::Stmt> {
  ProceduralScopeLoweringState child_proc_scope_state;
  ProceduralDepthGuard depth_guard{proc_state};
  child_proc_scope_state.AddRootStmt(child_proc_scope_state.AddStmt(
      BuildSensitivityWaitStmt(scope_state, ie.sensitivity_list)));
  const hir::Stmt& inner_hir = hir_proc.stmts.at(t.stmt.value);
  auto inner_or = LowerStmt(
      unit_state, scope_state, proc_state, child_proc_scope_state, hir_proc,
      inner_hir);
  if (!inner_or) {
    return std::unexpected(std::move(inner_or.error()));
  }
  child_proc_scope_state.AddRootStmt(
      child_proc_scope_state.AddStmt(*std::move(inner_or)));
  std::vector<mir::ProceduralScope> child_scopes;
  const mir::ProceduralScopeId scope_id =
      AddChildProceduralScope(child_scopes, child_proc_scope_state.Finish());
  return mir::Stmt{
      .label = stmt.label,
      .data = mir::BlockStmt{.scope = scope_id},
      .child_procedural_scopes = std::move(child_scopes)};
}

// LRM 9.4.3 `wait (cond) body` expands to:
//
//   wrapper_scope {
//     while (!cond) {
//       inner_scope { SensitivityWaitStmt(reads(cond)) }
//     }
//     body
//   }
//
// The level-sensitive "skip suspend if cond is already true" semantic falls
// out of the while-loop: an entry-true cond never enters the body.
auto LowerWaitStmt(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    ProcessLoweringState& proc_state, const hir::ProceduralBody& hir_proc,
    const hir::Stmt& stmt, const hir::WaitStmt& w) -> diag::Result<mir::Stmt> {
  ProceduralScopeLoweringState wrapper_state;
  ProceduralDepthGuard wrapper_depth_guard{proc_state};

  const hir::Expr& hir_cond = hir_proc.exprs.at(w.cond.value);
  auto cond_or = LowerExpr(
      unit_state, scope_state, proc_state, wrapper_state, hir_proc, hir_cond);
  if (!cond_or) {
    return std::unexpected(std::move(cond_or.error()));
  }
  const mir::ExprId cond_id = wrapper_state.AddExpr(*std::move(cond_or));
  const mir::TypeId cond_type = wrapper_state.GetExpr(cond_id).type;

  const mir::ExprId not_cond_id = wrapper_state.AddExpr(
      mir::Expr{
          .data =
              mir::UnaryExpr{
                  .op = mir::UnaryOp::kLogicalNot, .operand = cond_id},
          .type = cond_type});

  const auto& reads = w.sensitivity_list;

  ProceduralScopeLoweringState inner_state;
  ProceduralDepthGuard inner_depth_guard{proc_state};
  inner_state.AddRootStmt(
      inner_state.AddStmt(BuildSensitivityWaitStmt(scope_state, reads)));

  std::vector<mir::ProceduralScope> wrapper_child_scopes;
  const mir::ProceduralScopeId inner_scope_id =
      AddChildProceduralScope(wrapper_child_scopes, inner_state.Finish());

  wrapper_state.AddRootStmt(wrapper_state.AddStmt(
      mir::Stmt{
          .label = std::nullopt,
          .data =
              mir::WhileStmt{.condition = not_cond_id, .scope = inner_scope_id},
          .child_procedural_scopes = std::move(wrapper_child_scopes)}));

  const hir::Stmt& body_hir = hir_proc.stmts.at(w.body.value);
  auto body_or = LowerStmt(
      unit_state, scope_state, proc_state, wrapper_state, hir_proc, body_hir);
  if (!body_or) {
    return std::unexpected(std::move(body_or.error()));
  }
  wrapper_state.AddRootStmt(wrapper_state.AddStmt(*std::move(body_or)));

  std::vector<mir::ProceduralScope> outer_child_scopes;
  const mir::ProceduralScopeId wrapper_scope_id =
      AddChildProceduralScope(outer_child_scopes, wrapper_state.Finish());

  return mir::Stmt{
      .label = stmt.label,
      .data = mir::BlockStmt{.scope = wrapper_scope_id},
      .child_procedural_scopes = std::move(outer_child_scopes)};
}

// LRM 9.4.1 `#N body` expands to:
//
//   child_scope {
//     DelayStmt(ticks)
//     body
//   }
//
// Parallel to the SensitivityWaitStmt / NamedEvent expansions, except the
// suspension is anchored to simulation time rather than a value-change or
// named-event subscription.
auto LowerDelayTimedStmt(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    ProcessLoweringState& proc_state, const hir::ProceduralBody& hir_proc,
    const hir::Stmt& stmt, const hir::TimedStmt& t, const hir::DelayControl& d)
    -> diag::Result<mir::Stmt> {
  auto ticks_or = ResolveDelayTicks(proc_state, hir_proc, d);
  if (!ticks_or) {
    return std::unexpected(std::move(ticks_or.error()));
  }
  ProceduralScopeLoweringState child_proc_scope_state;
  ProceduralDepthGuard depth_guard{proc_state};
  child_proc_scope_state.AddRootStmt(child_proc_scope_state.AddStmt(
      mir::Stmt{
          .label = std::nullopt,
          .data = mir::DelayStmt{.duration = *ticks_or},
          .child_procedural_scopes = {}}));
  const hir::Stmt& inner_hir = hir_proc.stmts.at(t.stmt.value);
  auto inner_or = LowerStmt(
      unit_state, scope_state, proc_state, child_proc_scope_state, hir_proc,
      inner_hir);
  if (!inner_or) {
    return std::unexpected(std::move(inner_or.error()));
  }
  child_proc_scope_state.AddRootStmt(
      child_proc_scope_state.AddStmt(*std::move(inner_or)));
  std::vector<mir::ProceduralScope> child_scopes;
  const mir::ProceduralScopeId scope_id =
      AddChildProceduralScope(child_scopes, child_proc_scope_state.Finish());
  return mir::Stmt{
      .label = stmt.label,
      .data = mir::BlockStmt{.scope = scope_id},
      .child_procedural_scopes = std::move(child_scopes)};
}

auto LowerTimedStmt(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    ProcessLoweringState& proc_state, const hir::ProceduralBody& hir_proc,
    const hir::Stmt& stmt, const hir::TimedStmt& t) -> diag::Result<mir::Stmt> {
  if (const auto* ie = std::get_if<hir::ImplicitEventControl>(&t.timing)) {
    return LowerImplicitEventTimedStmt(
        unit_state, scope_state, proc_state, hir_proc, stmt, t, *ie);
  }
  if (const auto* nec = std::get_if<hir::NamedEventControl>(&t.timing)) {
    return LowerNamedEventTimedStmt(
        unit_state, scope_state, proc_state, hir_proc, stmt, t, *nec);
  }
  if (const auto* ec = std::get_if<hir::EventControl>(&t.timing)) {
    return LowerEventTimedStmt(
        unit_state, scope_state, proc_state, hir_proc, stmt, t, *ec);
  }
  const auto* d = std::get_if<hir::DelayControl>(&t.timing);
  if (d == nullptr) {
    throw InternalError("LowerTimedStmt: unknown hir::TimingControl variant");
  }
  return LowerDelayTimedStmt(
      unit_state, scope_state, proc_state, hir_proc, stmt, t, *d);
}

}  // namespace

auto LowerStmtIntoChildScope(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    ProcessLoweringState& proc_state, const hir::ProceduralBody& hir_proc,
    hir::StmtId hir_stmt_id) -> diag::Result<mir::ProceduralScope> {
  ProceduralScopeLoweringState child_state;
  ProceduralDepthGuard depth_guard{proc_state};
  const hir::Stmt& hir_stmt = hir_proc.stmts.at(hir_stmt_id.value);
  auto lowered = LowerStmt(
      unit_state, scope_state, proc_state, child_state, hir_proc, hir_stmt);
  if (!lowered) {
    return std::unexpected(std::move(lowered.error()));
  }
  const mir::StmtId stmt_id = child_state.AddStmt(*std::move(lowered));
  child_state.AddRootStmt(stmt_id);
  return child_state.Finish();
}

auto LowerStmt(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::ProceduralBody& hir_proc, const hir::Stmt& stmt)
    -> diag::Result<mir::Stmt> {
  return std::visit(
      Overloaded{
          [&](const hir::EmptyStmt&) { return LowerEmptyStmt(stmt); },
          [&](const hir::VarDeclStmt& v) {
            return LowerVarDeclStmt(
                unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
                stmt, v);
          },
          [&](const hir::ExprStmt& e) {
            return LowerExprStmt(
                unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
                stmt, e);
          },
          [&](const hir::BlockStmt& b) {
            return LowerBlockStmt(
                unit_state, scope_state, proc_state, hir_proc, stmt, b);
          },
          [&](const hir::IfStmt& i) {
            return LowerIfStmt(
                unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
                stmt, i);
          },
          [&](const hir::CaseStmt& c) {
            return LowerCaseStmt(
                unit_state, scope_state, proc_state, hir_proc, stmt, c);
          },
          [&](const hir::CaseInsideStmt& c) {
            return LowerCaseInsideStmt(
                unit_state, scope_state, proc_state, hir_proc, stmt, c);
          },
          [&](const hir::ForStmt& f) {
            return LowerForStmt(
                unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
                stmt, f);
          },
          [&](const hir::WhileStmt& w) {
            return LowerWhileStmt(
                unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
                stmt, w);
          },
          [&](const hir::RepeatStmt& r) {
            return LowerRepeatStmt(
                unit_state, scope_state, proc_state, hir_proc, stmt, r);
          },
          [&](const hir::DoWhileStmt& d) {
            return LowerDoWhileStmt(
                unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
                stmt, d);
          },
          [&](const hir::ForeverStmt& f) {
            return LowerForeverStmt(
                unit_state, scope_state, proc_state, hir_proc, stmt, f);
          },
          [&](const hir::BreakStmt&) { return LowerBreakStmt(stmt); },
          [&](const hir::ContinueStmt&) { return LowerContinueStmt(stmt); },
          [&](const hir::ReturnStmt& r) {
            return LowerReturnStmt(
                unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
                stmt, r);
          },
          [&](const hir::TimedStmt& t) {
            return LowerTimedStmt(
                unit_state, scope_state, proc_state, hir_proc, stmt, t);
          },
          [&](const hir::EventTriggerStmt& et) {
            return LowerEventTriggerStmt(
                unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
                stmt, et);
          },
          [&](const hir::WaitStmt& w) {
            return LowerWaitStmt(
                unit_state, scope_state, proc_state, hir_proc, stmt, w);
          },
      },
      stmt.data);
}

}  // namespace lyra::lowering::hir_to_mir
