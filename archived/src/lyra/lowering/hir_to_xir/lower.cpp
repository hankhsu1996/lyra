#include "lyra/lowering/hir_to_xir/lower.hpp"

#include <format>
#include <optional>
#include <unordered_map>
#include <variant>

#include "lyra/common/operators.hpp"
#include "lyra/common/symbol_table.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/expression.hpp"
#include "lyra/hir/module_body.hpp"
#include "lyra/hir/routine.hpp"
#include "lyra/hir/statement.hpp"

namespace lyra::lowering::hir_to_xir {

namespace {

auto Unsupported(std::string_view reason) -> lyra::Result<xir::StmtId> {
  return std::unexpected(
      Diagnostic::HostError(std::format("XIR prototype: {}", reason)));
}

auto UnsupportedExpr(std::string_view reason) -> lyra::Result<xir::ExprId> {
  return std::unexpected(
      Diagnostic::HostError(std::format("XIR prototype: {}", reason)));
}

auto TranslateSupportedUnaryOp(hir::UnaryOp hir_op)
    -> std::optional<common::UnaryOp> {
  switch (hir_op) {
    case hir::UnaryOp::kPlus:
      return common::UnaryOp::kPlus;
    case hir::UnaryOp::kMinus:
      return common::UnaryOp::kMinus;
    case hir::UnaryOp::kLogicalNot:
      return common::UnaryOp::kLogicalNot;
    case hir::UnaryOp::kBitwiseNot:
      return common::UnaryOp::kBitwiseNot;
    default:
      return std::nullopt;
  }
}

auto TranslateSupportedBinaryOp(hir::BinaryOp hir_op)
    -> std::optional<common::BinaryOp> {
  switch (hir_op) {
    case hir::BinaryOp::kAdd:
      return common::BinaryOp::kAdd;
    case hir::BinaryOp::kSubtract:
      return common::BinaryOp::kSubtract;
    case hir::BinaryOp::kMultiply:
      return common::BinaryOp::kMultiply;
    case hir::BinaryOp::kEqual:
      return common::BinaryOp::kEqual;
    case hir::BinaryOp::kNotEqual:
      return common::BinaryOp::kNotEqual;
    case hir::BinaryOp::kLogicalAnd:
      return common::BinaryOp::kLogicalAnd;
    case hir::BinaryOp::kLogicalOr:
      return common::BinaryOp::kLogicalOr;
    default:
      return std::nullopt;
  }
}

// First-slice value-like variable type gate.
// Accepts only integral types <= 32 bits that project cleanly to int32_t.
// Rejects everything else: wide integers, packed arrays, enums, reals, etc.
auto IsProjectableFieldType(const Type& type) -> bool {
  if (type.Kind() != TypeKind::kIntegral) return false;
  return type.AsIntegral().bit_width <= 32;
}

class Lowerer {
 public:
  Lowerer(
      const hir::Arena* hir_arena,
      const std::unordered_map<SymbolId, xir::VariableId, SymbolIdHash>*
          sym_to_var,
      xir::Arena* arena)
      : hir_arena_(hir_arena), sym_to_var_(sym_to_var), arena_(arena) {
  }

  auto LowerStmt(hir::StatementId hir_stmt_id) -> lyra::Result<xir::StmtId> {
    const auto& stmt = (*hir_arena_)[hir_stmt_id];
    return std::visit(
        [&](const auto& data) { return LowerStmtData(stmt.span, data); },
        stmt.data);
  }

  auto LowerExpr(hir::ExpressionId hir_expr_id) -> lyra::Result<xir::ExprId> {
    const auto& expr = (*hir_arena_)[hir_expr_id];
    return std::visit(
        [&](const auto& data) {
          return LowerExprData(expr.type, expr.span, data);
        },
        expr.data);
  }

 private:
  auto LowerStmtData(SourceSpan span, const hir::BlockStatementData& data)
      -> lyra::Result<xir::StmtId> {
    std::vector<xir::StmtId> children;
    children.reserve(data.statements.size());
    for (auto child_id : data.statements) {
      auto result = LowerStmt(child_id);
      if (!result) return std::unexpected(result.error());
      children.push_back(*result);
    }
    return arena_->Add(
        xir::Statement{.span = span, .data = xir::Block{std::move(children)}});
  }

  auto LowerStmtData(SourceSpan span, const hir::AssignmentStatementData& data)
      -> lyra::Result<xir::StmtId> {
    return LowerAssignmentTarget(span, data.target, data.value);
  }

  auto LowerStmtData(SourceSpan span, const hir::ConditionalStatementData& data)
      -> lyra::Result<xir::StmtId> {
    auto cond = LowerExpr(data.condition);
    if (!cond) return std::unexpected(cond.error());
    auto then_br = LowerStmt(data.then_branch);
    if (!then_br) return std::unexpected(then_br.error());
    std::optional<xir::StmtId> else_br;
    if (data.else_branch.has_value()) {
      auto result = LowerStmt(*data.else_branch);
      if (!result) return std::unexpected(result.error());
      else_br = *result;
    }
    return arena_->Add(
        xir::Statement{
            .span = span,
            .data = xir::IfThenElse{
                .condition = *cond,
                .then_branch = *then_br,
                .else_branch = else_br}});
  }

  auto LowerStmtData(SourceSpan span, const hir::ExpressionStatementData& data)
      -> lyra::Result<xir::StmtId> {
    const auto& expr = (*hir_arena_)[data.expression];
    if (expr.kind == hir::ExpressionKind::kAssignment) {
      const auto& assign = std::get<hir::AssignmentExpressionData>(expr.data);
      return LowerAssignmentTarget(span, assign.target, assign.value);
    }
    return Unsupported("unsupported expression statement");
  }

  // Catch-all: unsupported statement kinds
  template <typename T>
  auto LowerStmtData(SourceSpan /*span*/, const T& /*data*/)
      -> lyra::Result<xir::StmtId> {
    return Unsupported("unsupported statement kind");
  }

  // Shared helper for assignment lowering (both kAssignment stmt and
  // kExpression stmt wrapping kAssignment expr use the same path).
  auto LowerAssignmentTarget(
      SourceSpan span, hir::ExpressionId target_id, hir::ExpressionId value_id)
      -> lyra::Result<xir::StmtId> {
    const auto& target_expr = (*hir_arena_)[target_id];
    if (target_expr.kind != hir::ExpressionKind::kNameRef) {
      return Unsupported("assignment target must be direct variable ref");
    }
    const auto& name_ref =
        std::get<hir::NameRefExpressionData>(target_expr.data);
    auto it = sym_to_var_->find(name_ref.symbol);
    if (it == sym_to_var_->end()) {
      return Unsupported("assignment target is not a CU variable");
    }
    auto value = LowerExpr(value_id);
    if (!value) return std::unexpected(value.error());
    return arena_->Add(
        xir::Statement{
            .span = span,
            .data = xir::WriteVariable{.target = it->second, .value = *value}});
  }

  auto LowerExprData(
      TypeId type, SourceSpan span, const hir::ConstantExpressionData& data)
      -> lyra::Result<xir::ExprId> {
    return arena_->Add(
        xir::Expression{
            .type = type, .span = span, .data = xir::ConstInt{data.constant}});
  }

  auto LowerExprData(
      TypeId type, SourceSpan span, const hir::NameRefExpressionData& data)
      -> lyra::Result<xir::ExprId> {
    auto it = sym_to_var_->find(data.symbol);
    if (it == sym_to_var_->end()) {
      return UnsupportedExpr("name ref is not a CU variable");
    }
    return arena_->Add(
        xir::Expression{
            .type = type,
            .span = span,
            .data = xir::ReadVariable{.var = it->second}});
  }

  auto LowerExprData(
      TypeId type, SourceSpan span, const hir::UnaryExpressionData& data)
      -> lyra::Result<xir::ExprId> {
    auto op = TranslateSupportedUnaryOp(data.op);
    if (!op) {
      return UnsupportedExpr(
          std::format("unsupported unary op: {}", hir::ToString(data.op)));
    }
    auto operand = LowerExpr(data.operand);
    if (!operand) return std::unexpected(operand.error());
    return arena_->Add(
        xir::Expression{
            .type = type,
            .span = span,
            .data = xir::UnaryExpr{.op = *op, .operand = *operand}});
  }

  auto LowerExprData(
      TypeId type, SourceSpan span, const hir::BinaryExpressionData& data)
      -> lyra::Result<xir::ExprId> {
    auto op = TranslateSupportedBinaryOp(data.op);
    if (!op) {
      return UnsupportedExpr(
          std::format("unsupported binary op: {}", hir::ToString(data.op)));
    }
    auto lhs = LowerExpr(data.lhs);
    if (!lhs) return std::unexpected(lhs.error());
    auto rhs = LowerExpr(data.rhs);
    if (!rhs) return std::unexpected(rhs.error());
    return arena_->Add(
        xir::Expression{
            .type = type,
            .span = span,
            .data = xir::BinaryExpr{.op = *op, .lhs = *lhs, .rhs = *rhs}});
  }

  static auto LowerExprData(
      TypeId /*type*/, SourceSpan /*span*/,
      const hir::CastExpressionData& /*data*/) -> lyra::Result<xir::ExprId> {
    // First slice: reject all casts. Even implicit casts may change
    // width or signedness in ways the prototype does not handle.
    return UnsupportedExpr("casts not supported in prototype");
  }

  // Catch-all: unsupported expression kinds
  template <typename T>
  auto LowerExprData(TypeId /*type*/, SourceSpan /*span*/, const T& /*data*/)
      -> lyra::Result<xir::ExprId> {
    return UnsupportedExpr("unsupported expression kind");
  }

  const hir::Arena* hir_arena_;
  const std::unordered_map<SymbolId, xir::VariableId, SymbolIdHash>*
      sym_to_var_;
  xir::Arena* arena_;
};

}  // namespace

auto LowerToXir(const CompilationUnitInput& input)
    -> lyra::Result<xir::CompilationUnit> {
  // Validate required inputs.
  if (input.body == nullptr || input.hir_arena == nullptr ||
      input.type_arena == nullptr || input.symbol_table == nullptr ||
      input.constant_arena == nullptr) {
    return std::unexpected(
        Diagnostic::HostError(
            "XIR prototype: null required input in CompilationUnitInput"));
  }

  xir::CompilationUnit cu;
  cu.name = input.compilation_unit_name;

  // Build variable table + SymbolId-to-VariableId map.
  // First slice: value-like variable-only path. Only integral types <= 32
  // bits are accepted (project cleanly to int32_t in C++).
  std::unordered_map<SymbolId, xir::VariableId, SymbolIdHash> sym_to_var;
  for (uint32_t i = 0; i < input.variable_symbols.size(); ++i) {
    SymbolId sym_id = input.variable_symbols[i];
    const auto& sym = (*input.symbol_table)[sym_id];
    const auto& type = (*input.type_arena)[sym.type];
    if (!IsProjectableFieldType(type)) {
      return std::unexpected(
          Diagnostic::HostError(
              std::format(
                  "XIR prototype: variable '{}' has type not projectable in "
                  "this "
                  "slice (requires integral <= 32 bits)",
                  sym.name)));
    }
    cu.variables.push_back(
        xir::Variable{
            .name = sym.name, .type = sym.type, .span = sym.definition_span});
    sym_to_var[sym_id] = xir::VariableId{i};
  }

  // Validate: exactly one process, kInitial.
  if (input.body->processes.size() != 1) {
    return std::unexpected(
        Diagnostic::HostError("XIR prototype: requires exactly one process"));
  }
  const auto& hir_proc = (*input.hir_arena)[input.body->processes[0]];
  if (hir_proc.kind != hir::ProcessKind::kInitial) {
    return std::unexpected(
        Diagnostic::HostError("XIR prototype: requires kInitial process"));
  }

  // Lower process body
  Lowerer lowerer(input.hir_arena, &sym_to_var, &cu.arena);
  auto body_result = lowerer.LowerStmt(hir_proc.body);
  if (!body_result) return std::unexpected(body_result.error());

  // First-slice invariant: exactly one process, projected name is fixed to
  // "initial_0". This is a deliberate prototype convention, not a general
  // naming rule. When multiple processes are supported, process naming will
  // derive from frontend-generated stable identity.
  cu.processes.push_back(
      xir::ProcessEntry{
          .name = "initial_0",
          .entry_kind = xir::ProcessEntryKind::kInitial,
          .body = *body_result,
      });

  return cu;
}

}  // namespace lyra::lowering::hir_to_xir
