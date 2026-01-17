#include "lyra/lowering/hir_to_mir/statement.hpp"

#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/symbol.hpp"
#include "lyra/hir/expression.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/hir/statement.hpp"
#include "lyra/hir/system_call.hpp"
#include "lyra/lowering/hir_to_mir/builder.hpp"
#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/lowering/hir_to_mir/expression.hpp"
#include "lyra/lowering/hir_to_mir/lvalue.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/operator.hpp"
#include "lyra/mir/rvalue.hpp"

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
  Context& ctx = builder.GetContext();
  mir::Operand cond = LowerExpression(data.condition, builder);

  // EmitBranch requires a Use operand (PlaceId reference). If condition is
  // a constant, materialize it to a temp first.
  if (cond.kind == mir::Operand::Kind::kConst) {
    const hir::Expression& cond_expr = (*ctx.hir_arena)[data.condition];
    mir::PlaceId temp = ctx.AllocTemp(cond_expr.type);
    builder.EmitAssign(temp, std::move(cond));
    cond = mir::Operand::Use(temp);
  }

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

void LowerCase(const hir::CaseStatementData& data, MirBuilder& builder) {
  Context& ctx = builder.GetContext();

  // Slang invariant: each case item has at least one expression.
  for (const auto& item : data.items) {
    if (item.expressions.empty()) {
      throw common::InternalError(
          "LowerCase",
          "case item has no expressions (Slang invariant violated)");
    }
  }

  // 1. Lower and materialize selector to a place (evaluated once)
  mir::Operand sel = LowerExpression(data.selector, builder);
  mir::PlaceId sel_place;
  if (sel.kind == mir::Operand::Kind::kUse) {
    sel_place = std::get<mir::PlaceId>(sel.payload);
  } else {
    const hir::Expression& sel_expr = (*ctx.hir_arena)[data.selector];
    sel_place = ctx.AllocTemp(sel_expr.type);
    builder.EmitAssign(sel_place, std::move(sel));
  }

  // 2. Create stable block references
  BlockIndex merge_bb = builder.CreateBlock();
  BlockIndex default_bb = builder.CreateBlock();

  // Pre-create all item body blocks and first-check blocks
  std::vector<BlockIndex> item_body_blocks;
  std::vector<BlockIndex> item_first_check_blocks;
  for (size_t i = 0; i < data.items.size(); ++i) {
    item_body_blocks.push_back(builder.CreateBlock());
    item_first_check_blocks.push_back(builder.CreateBlock());
  }

  // Jump from entry to first item's check (or default if no items)
  if (!data.items.empty()) {
    builder.EmitJump(item_first_check_blocks[0]);
  } else {
    builder.EmitJump(default_bb);
  }

  // 3. Process each case item
  for (size_t i = 0; i < data.items.size(); ++i) {
    const hir::CaseItem& item = data.items[i];
    BlockIndex body_bb = item_body_blocks[i];

    // Where to go after all expressions of this item fail
    BlockIndex next_item_bb = (i + 1 < data.items.size())
                                  ? item_first_check_blocks[i + 1]
                                  : default_bb;

    // Start at this item's first check block
    builder.SetCurrentBlock(item_first_check_blocks[i]);

    // Chain of expression checks
    for (size_t j = 0; j < item.expressions.size(); ++j) {
      mir::Operand val = LowerExpression(item.expressions[j], builder);

      // Compare: sel_place == val (result is 1-bit 4-state, may be X)
      TypeId cmp_type = ctx.GetBitType();
      mir::Rvalue cmp_rvalue{
          .kind = mir::RvalueKind::kBinary,
          .op = static_cast<int>(mir::BinaryOp::kEqual),
          .operands = {mir::Operand::Use(sel_place), std::move(val)},
          .info = {},
      };
      mir::PlaceId cmp_result =
          builder.EmitTemp(cmp_type, std::move(cmp_rvalue));

      // Where to go on no-match for this expression
      bool is_last_expr = (j + 1 == item.expressions.size());
      BlockIndex nomatch_bb =
          is_last_expr ? next_item_bb : builder.CreateBlock();

      // Branch: match -> body, no match -> next check
      builder.EmitBranch(mir::Operand::Use(cmp_result), body_bb, nomatch_bb);

      if (!is_last_expr) {
        builder.SetCurrentBlock(nomatch_bb);
      }
    }

    // Emit body block
    builder.SetCurrentBlock(body_bb);
    LowerStatement(item.statement, builder);
    builder.EmitJump(merge_bb);
  }

  // 4. Default clause
  builder.SetCurrentBlock(default_bb);
  if (data.default_statement.has_value()) {
    LowerStatement(*data.default_statement, builder);
  }
  builder.EmitJump(merge_bb);

  // 5. Continue after case
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
        } else if constexpr (std::is_same_v<T, hir::CaseStatementData>) {
          LowerCase(data, builder);
        } else {
          throw common::InternalError(
              "LowerStatement", "unhandled statement kind");
        }
      },
      stmt.data);
}

}  // namespace lyra::lowering::hir_to_mir
