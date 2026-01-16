#include "lyra/lowering/hir_to_mir/statement.hpp"

#include <variant>
#include <vector>

#include "lyra/common/internal_error.hpp"
#include "lyra/hir/expression.hpp"
#include "lyra/hir/statement.hpp"
#include "lyra/hir/system_call.hpp"
#include "lyra/lowering/hir_to_mir/builder.hpp"
#include "lyra/lowering/hir_to_mir/expression.hpp"
#include "lyra/mir/effect.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

void LowerBlock(const hir::BlockStatementData& data, MirBuilder& builder) {
  for (hir::StatementId stmt_id : data.statements) {
    LowerStatement(stmt_id, builder);
  }
}

void LowerVariableDeclaration(
    const hir::VariableDeclarationStatementData& data, MirBuilder& builder) {
  Context& ctx = builder.GetContext();
  const Symbol& sym = (*ctx.symbol_table)[data.symbol];
  mir::PlaceId place_id = ctx.AllocLocal(data.symbol, sym.type);

  if (data.init != hir::kInvalidExpressionId) {
    mir::Operand value = LowerExpression(data.init, builder);
    builder.EmitAssign(place_id, std::move(value));
  }
}

auto LowerLvalue(hir::ExpressionId expr_id, MirBuilder& builder)
    -> mir::PlaceId {
  Context& ctx = builder.GetContext();
  const hir::Expression& expr = (*ctx.hir_arena)[expr_id];

  return std::visit(
      [&](const auto& data) -> mir::PlaceId {
        using T = std::decay_t<decltype(data)>;
        if constexpr (std::is_same_v<T, hir::NameRefExpressionData>) {
          return ctx.LookupPlace(data.symbol);
        } else {
          throw common::InternalError(
              "LowerLvalue", "unsupported lvalue expression");
        }
      },
      expr.data);
}

void LowerAssignment(
    const hir::AssignmentStatementData& data, MirBuilder& builder) {
  mir::PlaceId target = LowerLvalue(data.target, builder);
  mir::Operand value = LowerExpression(data.value, builder);
  builder.EmitAssign(target, std::move(value));
}

void LowerDisplayEffect(
    const hir::DisplaySystemCallData& data, MirBuilder& builder) {
  std::vector<mir::Operand> operands;
  operands.reserve(data.args.size());
  for (hir::ExpressionId arg_id : data.args) {
    operands.push_back(LowerExpression(arg_id, builder));
  }

  mir::DisplayEffect display{
      .radix = data.radix,
      .append_newline = data.append_newline,
      .args = std::move(operands),
  };
  builder.EmitEffect(std::move(display));
}

void LowerExpressionStatement(
    const hir::ExpressionStatementData& data, MirBuilder& builder) {
  Context& ctx = builder.GetContext();
  const hir::Expression& expr = (*ctx.hir_arena)[data.expression];

  // Check if this is a system call that should be an Effect instruction
  if (const auto* syscall =
          std::get_if<hir::SystemCallExpressionData>(&expr.data)) {
    std::visit(
        [&](const auto& call_data) {
          using T = std::decay_t<decltype(call_data)>;
          if constexpr (std::is_same_v<T, hir::DisplaySystemCallData>) {
            LowerDisplayEffect(call_data, builder);
          } else {
            throw common::InternalError(
                "LowerExpressionStatement", "unhandled system call kind");
          }
        },
        *syscall);
    return;
  }

  // Regular expression statement - evaluate for side effects, discard result
  LowerExpression(data.expression, builder);
}

void LowerConditional(
    const hir::ConditionalStatementData& data, MirBuilder& builder) {
  mir::Operand cond = LowerExpression(data.condition, builder);

  BlockIndex then_bb = builder.CreateBlock();
  BlockIndex merge_bb = builder.CreateBlock();

  if (data.else_branch.has_value()) {
    BlockIndex else_bb = builder.CreateBlock();
    builder.EmitBranch(cond, then_bb, else_bb);

    builder.SetCurrentBlock(then_bb);
    LowerStatement(data.then_branch, builder);
    builder.EmitJump(merge_bb);

    builder.SetCurrentBlock(else_bb);
    LowerStatement(*data.else_branch, builder);
    builder.EmitJump(merge_bb);
  } else {
    builder.EmitBranch(cond, then_bb, merge_bb);

    builder.SetCurrentBlock(then_bb);
    LowerStatement(data.then_branch, builder);
    builder.EmitJump(merge_bb);
  }

  builder.SetCurrentBlock(merge_bb);
}

}  // namespace

void LowerStatement(hir::StatementId stmt_id, MirBuilder& builder) {
  const hir::Statement& stmt = (*builder.GetContext().hir_arena)[stmt_id];

  std::visit(
      [&](const auto& data) {
        using T = std::decay_t<decltype(data)>;
        if constexpr (std::is_same_v<T, hir::BlockStatementData>) {
          LowerBlock(data, builder);
        } else if constexpr (std::is_same_v<
                                 T, hir::VariableDeclarationStatementData>) {
          LowerVariableDeclaration(data, builder);
        } else if constexpr (std::is_same_v<T, hir::AssignmentStatementData>) {
          LowerAssignment(data, builder);
        } else if constexpr (std::is_same_v<T, hir::ExpressionStatementData>) {
          LowerExpressionStatement(data, builder);
        } else if constexpr (std::is_same_v<T, hir::ConditionalStatementData>) {
          LowerConditional(data, builder);
        } else {
          throw common::InternalError(
              "LowerStatement", "unhandled statement kind");
        }
      },
      stmt.data);
}

}  // namespace lyra::lowering::hir_to_mir
