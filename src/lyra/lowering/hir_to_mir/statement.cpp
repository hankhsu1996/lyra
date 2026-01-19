#include "lyra/lowering/hir_to_mir/statement.hpp"

#include <cstddef>
#include <cstdint>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/common/constant.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/symbol.hpp"
#include "lyra/common/type.hpp"
#include "lyra/hir/expression.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/hir/statement.hpp"
#include "lyra/hir/system_call.hpp"
#include "lyra/lowering/hir_to_mir/builder.hpp"
#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/lowering/hir_to_mir/expression.hpp"
#include "lyra/lowering/hir_to_mir/lvalue.hpp"
#include "lyra/mir/builtin.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/operator.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

class LoopGuard {
 public:
  LoopGuard(MirBuilder& builder, LoopContext ctx) : builder_(builder) {
    builder_.PushLoop(ctx);
  }
  ~LoopGuard() {
    builder_.PopLoop();
  }
  LoopGuard(const LoopGuard&) = delete;
  LoopGuard(LoopGuard&&) = delete;
  auto operator=(const LoopGuard&) -> LoopGuard& = delete;
  auto operator=(LoopGuard&&) -> LoopGuard& = delete;

 private:
  MirBuilder& builder_;
};

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

  // Check if this is a builtin method call (e.g., arr.delete(), q.push_back())
  if (const auto* builtin =
          std::get_if<hir::BuiltinMethodCallExpressionData>(&expr.data)) {
    Context& ctx = builder.GetContext();
    const hir::Expression& receiver_expr = (*ctx.hir_arena)[builtin->receiver];
    const Type& receiver_type = (*ctx.type_arena)[receiver_expr.type];
    bool is_queue = receiver_type.Kind() == TypeKind::kQueue;

    // Helper to lower args to operands
    auto lower_args = [&]() -> std::vector<mir::Operand> {
      std::vector<mir::Operand> args;
      for (hir::ExpressionId arg_id : builtin->args) {
        args.push_back(LowerExpression(arg_id, builder));
      }
      return args;
    };

    switch (builtin->method) {
      case hir::BuiltinMethod::kDelete: {
        mir::PlaceId receiver_place = LowerLvalue(builtin->receiver, builder);
        if (is_queue) {
          // Queue delete: no args = clear all, one arg = delete at index
          mir::BuiltinMethod method = builtin->args.empty()
                                          ? mir::BuiltinMethod::kQueueDelete
                                          : mir::BuiltinMethod::kQueueDeleteAt;
          mir::BuiltinCallEffect effect{
              .method = method,
              .receiver = receiver_place,
              .args = lower_args(),
          };
          builder.EmitEffect(std::move(effect));
        } else {
          // Dynamic array delete
          mir::BuiltinCallEffect effect{
              .method = mir::BuiltinMethod::kArrayDelete,
              .receiver = receiver_place,
              .args = {},
          };
          builder.EmitEffect(std::move(effect));
        }
        return;
      }

      case hir::BuiltinMethod::kPushBack: {
        mir::PlaceId receiver_place = LowerLvalue(builtin->receiver, builder);
        mir::BuiltinCallEffect effect{
            .method = mir::BuiltinMethod::kQueuePushBack,
            .receiver = receiver_place,
            .args = lower_args(),
        };
        builder.EmitEffect(std::move(effect));
        return;
      }

      case hir::BuiltinMethod::kPushFront: {
        mir::PlaceId receiver_place = LowerLvalue(builtin->receiver, builder);
        mir::BuiltinCallEffect effect{
            .method = mir::BuiltinMethod::kQueuePushFront,
            .receiver = receiver_place,
            .args = lower_args(),
        };
        builder.EmitEffect(std::move(effect));
        return;
      }

      case hir::BuiltinMethod::kInsert: {
        mir::PlaceId receiver_place = LowerLvalue(builtin->receiver, builder);
        mir::BuiltinCallEffect effect{
            .method = mir::BuiltinMethod::kQueueInsert,
            .receiver = receiver_place,
            .args = lower_args(),
        };
        builder.EmitEffect(std::move(effect));
        return;
      }

      case hir::BuiltinMethod::kSize:
      case hir::BuiltinMethod::kPopBack:
      case hir::BuiltinMethod::kPopFront:
        // Value-returning methods - fall through to regular expression lowering
        break;
    }
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

  // Select comparison operator based on case condition
  mir::BinaryOp cmp_op = mir::BinaryOp::kEqual;
  switch (data.condition) {
    case hir::CaseCondition::kNormal:
      cmp_op = mir::BinaryOp::kEqual;
      break;
    case hir::CaseCondition::kCaseZ:
      cmp_op = mir::BinaryOp::kCaseZMatch;
      break;
    case hir::CaseCondition::kCaseX:
      cmp_op = mir::BinaryOp::kCaseXMatch;
      break;
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

      // Compare selector with case item (result is 1-bit 4-state, may be X)
      TypeId cmp_type = ctx.GetBitType();
      mir::Rvalue cmp_rvalue{
          .kind = mir::RvalueKind::kBinary,
          .op = static_cast<int>(cmp_op),
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

void LowerForLoop(const hir::ForLoopStatementData& data, MirBuilder& builder) {
  Context& ctx = builder.GetContext();

  // 1. Execute variable declarations in current block
  for (hir::StatementId var_decl : data.var_decls) {
    LowerStatement(var_decl, builder);
  }

  // 2. Evaluate init expressions (for side effects, result discarded)
  for (hir::ExpressionId init_expr : data.init_exprs) {
    LowerExpression(init_expr, builder);
  }

  // 3. Create blocks
  BlockIndex cond_bb = builder.CreateBlock();
  BlockIndex body_bb = builder.CreateBlock();
  BlockIndex step_bb = builder.CreateBlock();
  BlockIndex exit_bb = builder.CreateBlock();

  // 4. Jump to condition block
  builder.EmitJump(cond_bb);

  // 5. Condition block
  builder.SetCurrentBlock(cond_bb);
  if (data.condition.has_value()) {
    mir::Operand cond = LowerExpression(*data.condition, builder);

    // Materialize constant to temp if needed (EmitBranch requires Use operand)
    if (cond.kind == mir::Operand::Kind::kConst) {
      const hir::Expression& cond_expr = (*ctx.hir_arena)[*data.condition];
      mir::PlaceId temp = ctx.AllocTemp(cond_expr.type);
      builder.EmitAssign(temp, std::move(cond));
      cond = mir::Operand::Use(temp);
    }

    builder.EmitBranch(cond, body_bb, exit_bb);
  } else {
    // No condition = infinite loop
    builder.EmitJump(body_bb);
  }

  // 6. Body block
  builder.SetCurrentBlock(body_bb);
  {
    LoopGuard guard(
        builder, {.exit_block = exit_bb, .continue_block = step_bb});
    LowerStatement(data.body, builder);
  }
  builder.EmitJump(step_bb);

  // 7. Step block - evaluate step expressions (for side effects)
  builder.SetCurrentBlock(step_bb);
  for (hir::ExpressionId step_expr : data.steps) {
    LowerExpression(step_expr, builder);
  }
  builder.EmitJump(cond_bb);

  // 8. Continue in exit block
  builder.SetCurrentBlock(exit_bb);
}

void LowerWhileLoop(
    const hir::WhileLoopStatementData& data, MirBuilder& builder) {
  Context& ctx = builder.GetContext();

  BlockIndex cond_bb = builder.CreateBlock();
  BlockIndex body_bb = builder.CreateBlock();
  BlockIndex exit_bb = builder.CreateBlock();

  builder.EmitJump(cond_bb);

  // Condition block
  builder.SetCurrentBlock(cond_bb);
  mir::Operand cond = LowerExpression(data.condition, builder);
  if (cond.kind == mir::Operand::Kind::kConst) {
    const hir::Expression& cond_expr = (*ctx.hir_arena)[data.condition];
    mir::PlaceId temp = ctx.AllocTemp(cond_expr.type);
    builder.EmitAssign(temp, std::move(cond));
    cond = mir::Operand::Use(temp);
  }
  builder.EmitBranch(cond, body_bb, exit_bb);

  // Body block
  builder.SetCurrentBlock(body_bb);
  {
    LoopGuard guard(
        builder, {.exit_block = exit_bb, .continue_block = cond_bb});
    LowerStatement(data.body, builder);
  }
  builder.EmitJump(cond_bb);

  builder.SetCurrentBlock(exit_bb);
}

void LowerDoWhileLoop(
    const hir::DoWhileLoopStatementData& data, MirBuilder& builder) {
  Context& ctx = builder.GetContext();

  BlockIndex body_bb = builder.CreateBlock();
  BlockIndex cond_bb = builder.CreateBlock();
  BlockIndex exit_bb = builder.CreateBlock();

  builder.EmitJump(body_bb);

  // Body block (executes first)
  builder.SetCurrentBlock(body_bb);
  {
    LoopGuard guard(
        builder, {.exit_block = exit_bb, .continue_block = cond_bb});
    LowerStatement(data.body, builder);
  }
  builder.EmitJump(cond_bb);

  // Condition block
  builder.SetCurrentBlock(cond_bb);
  mir::Operand cond = LowerExpression(data.condition, builder);
  if (cond.kind == mir::Operand::Kind::kConst) {
    const hir::Expression& cond_expr = (*ctx.hir_arena)[data.condition];
    mir::PlaceId temp = ctx.AllocTemp(cond_expr.type);
    builder.EmitAssign(temp, std::move(cond));
    cond = mir::Operand::Use(temp);
  }
  builder.EmitBranch(cond, body_bb, exit_bb);

  builder.SetCurrentBlock(exit_bb);
}

auto MakeIntConstant(uint64_t value, TypeId type) -> Constant {
  IntegralConstant ic;
  ic.value.push_back(value);
  return Constant{.type = type, .value = std::move(ic)};
}

void LowerRepeatLoop(
    const hir::RepeatLoopStatementData& data, MirBuilder& builder) {
  Context& ctx = builder.GetContext();

  // 1. Evaluate count once and store in a temp
  mir::Operand count_val = LowerExpression(data.count, builder);
  const hir::Expression& count_expr = (*ctx.hir_arena)[data.count];
  mir::PlaceId counter = ctx.AllocTemp(count_expr.type);
  builder.EmitAssign(counter, std::move(count_val));

  // 2. Create blocks
  BlockIndex cond_bb = builder.CreateBlock();
  BlockIndex body_bb = builder.CreateBlock();
  BlockIndex step_bb = builder.CreateBlock();
  BlockIndex exit_bb = builder.CreateBlock();

  builder.EmitJump(cond_bb);

  // 3. Condition block: counter > 0
  builder.SetCurrentBlock(cond_bb);
  TypeId cond_type = ctx.GetBitType();
  Constant zero = MakeIntConstant(0, count_expr.type);

  // Choose signed or unsigned comparison based on count expression type
  const Type& count_type = (*ctx.type_arena)[count_expr.type];
  mir::BinaryOp cmp_op = mir::BinaryOp::kGreaterThan;
  if (count_type.Kind() == TypeKind::kIntegral &&
      count_type.AsIntegral().is_signed) {
    cmp_op = mir::BinaryOp::kGreaterThanSigned;
  }

  mir::Rvalue cmp_rvalue{
      .kind = mir::RvalueKind::kBinary,
      .op = static_cast<int>(cmp_op),
      .operands = {mir::Operand::Use(counter), mir::Operand::Const(zero)},
      .info = {},
  };
  mir::PlaceId cmp_result = builder.EmitTemp(cond_type, std::move(cmp_rvalue));
  builder.EmitBranch(mir::Operand::Use(cmp_result), body_bb, exit_bb);

  // 4. Body block
  builder.SetCurrentBlock(body_bb);
  {
    LoopGuard guard(
        builder, {.exit_block = exit_bb, .continue_block = step_bb});
    LowerStatement(data.body, builder);
  }
  builder.EmitJump(step_bb);

  // 5. Step block: decrement counter
  builder.SetCurrentBlock(step_bb);
  Constant one = MakeIntConstant(1, count_expr.type);
  mir::Rvalue dec_rvalue{
      .kind = mir::RvalueKind::kBinary,
      .op = static_cast<int>(mir::BinaryOp::kSubtract),
      .operands = {mir::Operand::Use(counter), mir::Operand::Const(one)},
      .info = {},
  };
  mir::PlaceId new_count =
      builder.EmitTemp(count_expr.type, std::move(dec_rvalue));
  builder.EmitAssign(counter, mir::Operand::Use(new_count));
  builder.EmitJump(cond_bb);

  // 6. Exit
  builder.SetCurrentBlock(exit_bb);
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
        } else if constexpr (std::is_same_v<T, hir::ForLoopStatementData>) {
          LowerForLoop(data, builder);
        } else if constexpr (std::is_same_v<T, hir::WhileLoopStatementData>) {
          LowerWhileLoop(data, builder);
        } else if constexpr (std::is_same_v<T, hir::DoWhileLoopStatementData>) {
          LowerDoWhileLoop(data, builder);
        } else if constexpr (std::is_same_v<T, hir::RepeatLoopStatementData>) {
          LowerRepeatLoop(data, builder);
        } else if constexpr (std::is_same_v<T, hir::BreakStatementData>) {
          const auto* loop = builder.CurrentLoop();
          if (loop == nullptr) {
            throw common::InternalError(
                "LowerStatement", "break statement outside loop");
          }
          builder.EmitJump(loop->exit_block);
          // Create unreachable block for any code after break
          BlockIndex dead_bb = builder.CreateBlock();
          builder.SetCurrentBlock(dead_bb);
        } else if constexpr (std::is_same_v<T, hir::ContinueStatementData>) {
          const auto* loop = builder.CurrentLoop();
          if (loop == nullptr) {
            throw common::InternalError(
                "LowerStatement", "continue statement outside loop");
          }
          builder.EmitJump(loop->continue_block);
          // Create unreachable block for any code after continue
          BlockIndex dead_bb = builder.CreateBlock();
          builder.SetCurrentBlock(dead_bb);
        } else if constexpr (std::is_same_v<T, hir::TerminateStatementData>) {
          // Map HIR TerminationKind to MIR TerminationKind
          mir::TerminationKind mir_kind = mir::TerminationKind::kFinish;
          switch (data.kind) {
            case hir::TerminationKind::kFinish:
              mir_kind = mir::TerminationKind::kFinish;
              break;
            case hir::TerminationKind::kStop:
              mir_kind = mir::TerminationKind::kStop;
              break;
            case hir::TerminationKind::kExit:
              mir_kind = mir::TerminationKind::kExit;
              break;
          }
          builder.EmitTerminate(
              mir::TerminationInfo{.kind = mir_kind, .level = data.level});
          // Create unreachable block for any code after terminate
          BlockIndex dead_bb = builder.CreateBlock();
          builder.SetCurrentBlock(dead_bb);
        } else if constexpr (std::is_same_v<T, hir::ReturnStatementData>) {
          Context& ctx = builder.GetContext();

          // If there's a return value, assign to local 0 (return place)
          if (data.value != hir::kInvalidExpressionId) {
            mir::Operand value = LowerExpression(data.value, builder);
            if (ctx.return_place != mir::kInvalidPlaceId) {
              builder.EmitAssign(ctx.return_place, std::move(value));
            }
          }

          builder.EmitReturn();

          // Create dead block for unreachable code after return
          BlockIndex dead_bb = builder.CreateBlock();
          builder.SetCurrentBlock(dead_bb);
        } else {
          throw common::InternalError(
              "LowerStatement", "unhandled statement kind");
        }
      },
      stmt.data);
}

}  // namespace lyra::lowering::hir_to_mir
