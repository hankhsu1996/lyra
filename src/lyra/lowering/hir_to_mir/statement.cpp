#include "lyra/lowering/hir_to_mir/statement.hpp"

#include <cstddef>
#include <cstdint>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/common/constant.hpp"
#include "lyra/common/format.hpp"
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
#include "lyra/mir/effect.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/operator.hpp"
#include "lyra/mir/rvalue.hpp"
#include "lyra/mir/terminator.hpp"

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
  LvalueResult target = LowerLvalue(data.target, builder);
  mir::Operand value = LowerExpression(data.value, builder);

  if (target.IsAlwaysValid()) {
    builder.EmitAssign(target.place, std::move(value));
    return;
  }

  // Guarded store: only write if validity is true (OOB/X/Z = no-op)
  Context& ctx = builder.GetContext();
  mir::Operand cond = target.validity;

  // EmitBranch requires Use operand - materialize constant if needed
  if (cond.kind == mir::Operand::Kind::kConst) {
    mir::PlaceId temp = ctx.AllocTemp(ctx.GetBitType());
    builder.EmitAssign(temp, std::move(cond));
    cond = mir::Operand::Use(temp);
  }

  BlockIndex store_bb = builder.CreateBlock();
  BlockIndex merge_bb = builder.CreateBlock();
  builder.EmitBranch(cond, store_bb, merge_bb);

  builder.SetCurrentBlock(store_bb);
  builder.EmitAssign(target.place, std::move(value));
  builder.EmitJump(merge_bb);

  builder.SetCurrentBlock(merge_bb);
}

void LowerDisplayEffect(
    const hir::DisplaySystemCallData& data, MirBuilder& builder) {
  Context& ctx = builder.GetContext();

  std::vector<mir::FormatOp> mir_ops;
  mir_ops.reserve(data.ops.size());

  for (const auto& hir_op : data.ops) {
    if (hir_op.kind == FormatKind::kLiteral) {
      mir_ops.push_back(
          mir::FormatOp{
              .kind = FormatKind::kLiteral,
              .value = std::nullopt,
              .literal = hir_op.literal,
              .type = TypeId{},
              .mods = {}});
    } else {
      // Lower the value expression and get its type
      mir::Operand operand = LowerExpression(*hir_op.value, builder);
      const hir::Expression& expr = (*ctx.hir_arena)[*hir_op.value];
      mir_ops.push_back(
          mir::FormatOp{
              .kind = hir_op.kind,
              .value = std::move(operand),
              .literal = {},
              .type = expr.type,
              .mods = hir_op.mods});
    }
  }

  mir::DisplayEffect display{
      .print_kind = data.print_kind,
      .ops = std::move(mir_ops),
  };
  builder.EmitEffect(std::move(display));
}

void LowerSeverityEffect(
    const hir::SeveritySystemCallData& data, MirBuilder& builder) {
  std::vector<mir::Operand> operands;
  operands.reserve(data.args.size());
  for (hir::ExpressionId arg_id : data.args) {
    operands.push_back(LowerExpression(arg_id, builder));
  }

  mir::SeverityEffect severity{
      .level = data.level,
      .args = std::move(operands),
  };
  builder.EmitEffect(std::move(severity));
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
          } else if constexpr (std::is_same_v<T, hir::SeveritySystemCallData>) {
            LowerSeverityEffect(call_data, builder);
          } else {
            throw common::InternalError(
                "LowerExpressionStatement", "unhandled system call kind");
          }
        },
        *syscall);
    return;
  }

  // Regular expression statement - evaluate for side effects, discard result
  // All builtin methods (including void methods like push_back, delete)
  // are handled uniformly through LowerExpression.
  LowerExpression(data.expression, builder);
}

// Represents a flattened condition-body pair from an if-else chain.
struct ConditionBodyPair {
  hir::ExpressionId condition;
  hir::StatementId body;
};

// Result of flattening an if-else chain.
struct FlattenResult {
  std::vector<ConditionBodyPair> pairs;
  std::optional<hir::StatementId> else_body;  // Final else statement, if any
};

// Flatten an if-else chain into a list of (condition, body) pairs.
// Chains else-if's that have no qualifier (kNone) or the same qualifier.
// If an else-if has a DIFFERENT qualifier, it's treated as the final else body
// (preserving its own semantics).
auto FlattenIfElseChain(
    const hir::ConditionalStatementData& data, const hir::Arena& hir_arena,
    hir::UniquePriorityCheck expected_check) -> FlattenResult {
  FlattenResult result;
  result.pairs.push_back({data.condition, data.then_branch});

  if (!data.else_branch.has_value()) {
    return result;  // No else clause
  }

  // Check if else branch is another conditional (else-if chain)
  const hir::Statement& else_stmt = hir_arena[*data.else_branch];
  if (const auto* else_cond =
          std::get_if<hir::ConditionalStatementData>(&else_stmt.data)) {
    // Chain if the else-if has the same qualifier OR no qualifier (kNone).
    // A plain "else if" is part of the outer qualified construct.
    // Only "else <different-qualifier> if" starts a new construct.
    if (else_cond->check == expected_check ||
        else_cond->check == hir::UniquePriorityCheck::kNone) {
      FlattenResult rest =
          FlattenIfElseChain(*else_cond, hir_arena, expected_check);
      result.pairs.insert(
          result.pairs.end(), rest.pairs.begin(), rest.pairs.end());
      result.else_body = rest.else_body;
      return result;
    }
    // Different qualifier - treat the else-if as the final else body
    // It will be lowered with its own qualifier semantics
  }

  // Final else clause (non-conditional, or conditional with different
  // qualifier)
  result.else_body = *data.else_branch;
  return result;
}

// Materialize condition to a PlaceId (for EmitBranch/QualifiedDispatch).
auto MaterializeCondition(hir::ExpressionId cond_expr_id, MirBuilder& builder)
    -> mir::PlaceId {
  Context& ctx = builder.GetContext();
  mir::Operand cond = LowerExpression(cond_expr_id, builder);

  if (cond.kind == mir::Operand::Kind::kUse) {
    return std::get<mir::PlaceId>(cond.payload);
  }
  const hir::Expression& cond_expr = (*ctx.hir_arena)[cond_expr_id];
  mir::PlaceId temp = ctx.AllocTemp(cond_expr.type);
  builder.EmitAssign(temp, std::move(cond));
  return temp;
}

// Standard if-else lowering without qualifiers.
void LowerConditionalNone(
    const hir::ConditionalStatementData& data, MirBuilder& builder) {
  mir::PlaceId cond_place = MaterializeCondition(data.condition, builder);
  mir::Operand cond = mir::Operand::Use(cond_place);

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

// Priority if: branch cascade + warning at end if no else.
void LowerConditionalPriority(
    const hir::ConditionalStatementData& data, MirBuilder& builder) {
  Context& ctx = builder.GetContext();

  // Flatten the if-else chain (only chain same-qualifier else-if's)
  FlattenResult flat = FlattenIfElseChain(
      data, *ctx.hir_arena, hir::UniquePriorityCheck::kPriority);
  const auto& pairs = flat.pairs;

  BlockIndex merge_bb = builder.CreateBlock();

  // Create body blocks for each condition
  std::vector<BlockIndex> body_blocks;
  for (size_t i = 0; i < pairs.size(); ++i) {
    body_blocks.push_back(builder.CreateBlock());
  }

  // Else block (if exists) or warning block (if no else)
  BlockIndex else_target = builder.CreateBlock();

  // Generate branch cascade
  for (size_t i = 0; i < pairs.size(); ++i) {
    mir::PlaceId cond_place = MaterializeCondition(pairs[i].condition, builder);
    BlockIndex next_check =
        (i + 1 < pairs.size()) ? builder.CreateBlock() : else_target;
    builder.EmitBranch(
        mir::Operand::Use(cond_place), body_blocks[i], next_check);

    if (i + 1 < pairs.size()) {
      builder.SetCurrentBlock(next_check);
    }
  }

  // Emit body blocks
  for (size_t i = 0; i < pairs.size(); ++i) {
    builder.SetCurrentBlock(body_blocks[i]);
    LowerStatement(pairs[i].body, builder);
    builder.EmitJump(merge_bb);
  }

  // Handle else or warning (else_target serves both purposes)
  builder.SetCurrentBlock(else_target);
  if (flat.else_body.has_value()) {
    LowerStatement(*flat.else_body, builder);
  } else {
    // No else clause: emit warning
    mir::DisplayEffect display{
        .print_kind = PrintKind::kDisplay,
        .ops = {mir::FormatOp{
            .kind = FormatKind::kLiteral,
            .value = std::nullopt,
            .literal = "warning: no condition matched in priority if",
            .type = TypeId{},
            .mods = {}}},
    };
    builder.EmitEffect(std::move(display));
  }
  builder.EmitJump(merge_bb);

  builder.SetCurrentBlock(merge_bb);
}

// Unique/Unique0 if: eval all conditions, emit QualifiedDispatch.
void LowerConditionalUnique(
    const hir::ConditionalStatementData& data, MirBuilder& builder,
    hir::UniquePriorityCheck check) {
  Context& ctx = builder.GetContext();

  // Flatten the if-else chain (only chain same-qualifier else-if's)
  FlattenResult flat = FlattenIfElseChain(data, *ctx.hir_arena, check);
  const auto& pairs = flat.pairs;
  bool has_else = flat.else_body.has_value();

  // Evaluate all conditions upfront
  std::vector<mir::PlaceId> condition_places;
  for (const auto& pair : pairs) {
    condition_places.push_back(MaterializeCondition(pair.condition, builder));
  }

  // Create blocks: one per body + optional else + merge
  BlockIndex merge_bb = builder.CreateBlock();
  std::vector<BlockIndex> body_blocks;
  for (size_t i = 0; i < pairs.size(); ++i) {
    body_blocks.push_back(builder.CreateBlock());
  }
  BlockIndex else_bb = builder.CreateBlock();

  // Build targets: [body0, body1, ..., bodyN, else]
  std::vector<BlockIndex> targets = body_blocks;
  targets.push_back(else_bb);

  // Determine qualifier
  mir::DispatchQualifier qualifier =
      (check == hir::UniquePriorityCheck::kUnique)
          ? mir::DispatchQualifier::kUnique
          : mir::DispatchQualifier::kUnique0;

  builder.EmitQualifiedDispatch(
      qualifier, mir::DispatchStatementKind::kIf, condition_places, targets,
      has_else);

  // Emit body blocks
  for (size_t i = 0; i < pairs.size(); ++i) {
    builder.SetCurrentBlock(body_blocks[i]);
    LowerStatement(pairs[i].body, builder);
    builder.EmitJump(merge_bb);
  }

  // Emit else block
  builder.SetCurrentBlock(else_bb);
  if (flat.else_body.has_value()) {
    LowerStatement(*flat.else_body, builder);
  }
  builder.EmitJump(merge_bb);

  builder.SetCurrentBlock(merge_bb);
}

void LowerConditional(
    const hir::ConditionalStatementData& data, MirBuilder& builder) {
  switch (data.check) {
    case hir::UniquePriorityCheck::kNone:
      LowerConditionalNone(data, builder);
      break;
    case hir::UniquePriorityCheck::kPriority:
      LowerConditionalPriority(data, builder);
      break;
    case hir::UniquePriorityCheck::kUnique:
    case hir::UniquePriorityCheck::kUnique0:
      LowerConditionalUnique(data, builder, data.check);
      break;
  }
}

// Helper: get comparison operator based on case condition type.
auto GetCaseComparisonOp(hir::CaseCondition condition) -> mir::BinaryOp {
  switch (condition) {
    case hir::CaseCondition::kNormal:
      return mir::BinaryOp::kEqual;
    case hir::CaseCondition::kCaseZ:
      return mir::BinaryOp::kCaseZMatch;
    case hir::CaseCondition::kCaseX:
      return mir::BinaryOp::kCaseXMatch;
  }
  return mir::BinaryOp::kEqual;
}

// Helper: materialize selector to a place.
auto MaterializeSelector(hir::ExpressionId selector_id, MirBuilder& builder)
    -> mir::PlaceId {
  Context& ctx = builder.GetContext();
  mir::Operand sel = LowerExpression(selector_id, builder);
  if (sel.kind == mir::Operand::Kind::kUse) {
    return std::get<mir::PlaceId>(sel.payload);
  }
  const hir::Expression& sel_expr = (*ctx.hir_arena)[selector_id];
  mir::PlaceId sel_place = ctx.AllocTemp(sel_expr.type);
  builder.EmitAssign(sel_place, std::move(sel));
  return sel_place;
}

// Standard case lowering without qualifiers.
void LowerCaseNone(const hir::CaseStatementData& data, MirBuilder& builder) {
  Context& ctx = builder.GetContext();
  mir::BinaryOp cmp_op = GetCaseComparisonOp(data.condition);
  mir::PlaceId sel_place = MaterializeSelector(data.selector, builder);

  BlockIndex merge_bb = builder.CreateBlock();
  BlockIndex default_bb = builder.CreateBlock();

  std::vector<BlockIndex> item_body_blocks;
  std::vector<BlockIndex> item_first_check_blocks;
  for (size_t i = 0; i < data.items.size(); ++i) {
    item_body_blocks.push_back(builder.CreateBlock());
    item_first_check_blocks.push_back(builder.CreateBlock());
  }

  if (!data.items.empty()) {
    builder.EmitJump(item_first_check_blocks[0]);
  } else {
    builder.EmitJump(default_bb);
  }

  for (size_t i = 0; i < data.items.size(); ++i) {
    const hir::CaseItem& item = data.items[i];
    BlockIndex body_bb = item_body_blocks[i];
    BlockIndex next_item_bb = (i + 1 < data.items.size())
                                  ? item_first_check_blocks[i + 1]
                                  : default_bb;

    builder.SetCurrentBlock(item_first_check_blocks[i]);

    for (size_t j = 0; j < item.expressions.size(); ++j) {
      mir::Operand val = LowerExpression(item.expressions[j], builder);
      TypeId cmp_type = ctx.GetBitType();
      mir::Rvalue cmp_rvalue{
          .operands = {mir::Operand::Use(sel_place), std::move(val)},
          .info = mir::BinaryRvalueInfo{.op = cmp_op},
      };
      mir::PlaceId cmp_result =
          builder.EmitTemp(cmp_type, std::move(cmp_rvalue));

      bool is_last_expr = (j + 1 == item.expressions.size());
      BlockIndex nomatch_bb =
          is_last_expr ? next_item_bb : builder.CreateBlock();

      builder.EmitBranch(mir::Operand::Use(cmp_result), body_bb, nomatch_bb);

      if (!is_last_expr) {
        builder.SetCurrentBlock(nomatch_bb);
      }
    }

    builder.SetCurrentBlock(body_bb);
    LowerStatement(item.statement, builder);
    builder.EmitJump(merge_bb);
  }

  builder.SetCurrentBlock(default_bb);
  if (data.default_statement.has_value()) {
    LowerStatement(*data.default_statement, builder);
  }
  builder.EmitJump(merge_bb);

  builder.SetCurrentBlock(merge_bb);
}

// Priority case: comparison cascade + warning at end if no default.
void LowerCasePriority(
    const hir::CaseStatementData& data, MirBuilder& builder) {
  Context& ctx = builder.GetContext();
  mir::BinaryOp cmp_op = GetCaseComparisonOp(data.condition);
  mir::PlaceId sel_place = MaterializeSelector(data.selector, builder);
  bool has_default = data.default_statement.has_value();

  BlockIndex merge_bb = builder.CreateBlock();
  // fallthrough_bb serves as default block (if has_default) or warning block
  BlockIndex fallthrough_bb = builder.CreateBlock();

  std::vector<BlockIndex> item_body_blocks;
  std::vector<BlockIndex> item_first_check_blocks;
  for (size_t i = 0; i < data.items.size(); ++i) {
    item_body_blocks.push_back(builder.CreateBlock());
    item_first_check_blocks.push_back(builder.CreateBlock());
  }

  if (!data.items.empty()) {
    builder.EmitJump(item_first_check_blocks[0]);
  } else {
    builder.EmitJump(fallthrough_bb);
  }

  for (size_t i = 0; i < data.items.size(); ++i) {
    const hir::CaseItem& item = data.items[i];
    BlockIndex body_bb = item_body_blocks[i];
    BlockIndex next_item_bb = (i + 1 < data.items.size())
                                  ? item_first_check_blocks[i + 1]
                                  : fallthrough_bb;

    builder.SetCurrentBlock(item_first_check_blocks[i]);

    for (size_t j = 0; j < item.expressions.size(); ++j) {
      mir::Operand val = LowerExpression(item.expressions[j], builder);
      TypeId cmp_type = ctx.GetBitType();
      mir::Rvalue cmp_rvalue{
          .operands = {mir::Operand::Use(sel_place), std::move(val)},
          .info = mir::BinaryRvalueInfo{.op = cmp_op},
      };
      mir::PlaceId cmp_result =
          builder.EmitTemp(cmp_type, std::move(cmp_rvalue));

      bool is_last_expr = (j + 1 == item.expressions.size());
      BlockIndex nomatch_bb =
          is_last_expr ? next_item_bb : builder.CreateBlock();

      builder.EmitBranch(mir::Operand::Use(cmp_result), body_bb, nomatch_bb);

      if (!is_last_expr) {
        builder.SetCurrentBlock(nomatch_bb);
      }
    }

    builder.SetCurrentBlock(body_bb);
    LowerStatement(item.statement, builder);
    builder.EmitJump(merge_bb);
  }

  // Handle default or warning (fallthrough_bb serves both purposes)
  builder.SetCurrentBlock(fallthrough_bb);
  if (has_default) {
    LowerStatement(*data.default_statement, builder);
  } else {
    mir::DisplayEffect display{
        .print_kind = PrintKind::kDisplay,
        .ops = {mir::FormatOp{
            .kind = FormatKind::kLiteral,
            .value = std::nullopt,
            .literal = "warning: no matching case item in priority case",
            .type = TypeId{},
            .mods = {}}},
    };
    builder.EmitEffect(std::move(display));
  }
  builder.EmitJump(merge_bb);

  builder.SetCurrentBlock(merge_bb);
}

// Unique/Unique0 case: eval all matches, emit QualifiedDispatch.
void LowerCaseUnique(
    const hir::CaseStatementData& data, MirBuilder& builder,
    hir::UniquePriorityCheck check) {
  Context& ctx = builder.GetContext();
  mir::BinaryOp cmp_op = GetCaseComparisonOp(data.condition);
  mir::PlaceId sel_place = MaterializeSelector(data.selector, builder);
  bool has_default = data.default_statement.has_value();

  // Evaluate all item matches upfront.
  // For each item, OR together all its expressions to get a single condition.
  std::vector<mir::PlaceId> item_conditions;
  for (const auto& item : data.items) {
    mir::PlaceId item_match = mir::kInvalidPlaceId;

    for (const auto& expr : item.expressions) {
      mir::Operand val = LowerExpression(expr, builder);
      TypeId cmp_type = ctx.GetBitType();
      mir::Rvalue cmp_rvalue{
          .operands = {mir::Operand::Use(sel_place), std::move(val)},
          .info = mir::BinaryRvalueInfo{.op = cmp_op},
      };
      mir::PlaceId cmp_result =
          builder.EmitTemp(cmp_type, std::move(cmp_rvalue));

      if (item_match == mir::kInvalidPlaceId) {
        item_match = cmp_result;
      } else {
        // OR with previous matches (any expression matching means item matches)
        mir::Rvalue or_rvalue{
            .operands =
                {mir::Operand::Use(item_match), mir::Operand::Use(cmp_result)},
            .info = mir::BinaryRvalueInfo{.op = mir::BinaryOp::kBitwiseOr},
        };
        item_match = builder.EmitTemp(cmp_type, std::move(or_rvalue));
      }
    }
    item_conditions.push_back(item_match);
  }

  // Create blocks
  BlockIndex merge_bb = builder.CreateBlock();
  std::vector<BlockIndex> body_blocks;
  for (size_t i = 0; i < data.items.size(); ++i) {
    body_blocks.push_back(builder.CreateBlock());
  }
  BlockIndex default_bb = builder.CreateBlock();

  // Build targets: [body0, body1, ..., bodyN, default]
  std::vector<BlockIndex> targets = body_blocks;
  targets.push_back(default_bb);

  mir::DispatchQualifier qualifier =
      (check == hir::UniquePriorityCheck::kUnique)
          ? mir::DispatchQualifier::kUnique
          : mir::DispatchQualifier::kUnique0;

  builder.EmitQualifiedDispatch(
      qualifier, mir::DispatchStatementKind::kCase, item_conditions, targets,
      has_default);

  // Emit body blocks
  for (size_t i = 0; i < data.items.size(); ++i) {
    builder.SetCurrentBlock(body_blocks[i]);
    LowerStatement(data.items[i].statement, builder);
    builder.EmitJump(merge_bb);
  }

  // Default block
  builder.SetCurrentBlock(default_bb);
  if (has_default) {
    LowerStatement(*data.default_statement, builder);
  }
  builder.EmitJump(merge_bb);

  builder.SetCurrentBlock(merge_bb);
}

void LowerCase(const hir::CaseStatementData& data, MirBuilder& builder) {
  // Slang invariant: each case item has at least one expression.
  for (const auto& item : data.items) {
    if (item.expressions.empty()) {
      throw common::InternalError(
          "LowerCase",
          "case item has no expressions (Slang invariant violated)");
    }
  }

  switch (data.check) {
    case hir::UniquePriorityCheck::kNone:
      LowerCaseNone(data, builder);
      break;
    case hir::UniquePriorityCheck::kPriority:
      LowerCasePriority(data, builder);
      break;
    case hir::UniquePriorityCheck::kUnique:
    case hir::UniquePriorityCheck::kUnique0:
      LowerCaseUnique(data, builder, data.check);
      break;
  }
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
  if (IsPacked(count_type) && IsPackedSigned(count_type, *ctx.type_arena)) {
    cmp_op = mir::BinaryOp::kGreaterThanSigned;
  }

  mir::Rvalue cmp_rvalue{
      .operands = {mir::Operand::Use(counter), mir::Operand::Const(zero)},
      .info = mir::BinaryRvalueInfo{.op = cmp_op},
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
      .operands = {mir::Operand::Use(counter), mir::Operand::Const(one)},
      .info = mir::BinaryRvalueInfo{.op = mir::BinaryOp::kSubtract},
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
  // Record origin for error tracking (maps MIR back to HIR source)
  builder.RecordStatementOrigin(stmt_id);

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
            case hir::TerminationKind::kFatal:
              mir_kind = mir::TerminationKind::kFatal;
              break;
          }

          // Lower message_args for $fatal
          std::vector<mir::Operand> message_operands;
          message_operands.reserve(data.message_args.size());
          for (hir::ExpressionId arg_id : data.message_args) {
            message_operands.push_back(LowerExpression(arg_id, builder));
          }

          builder.EmitTerminate(
              mir::Finish{
                  .kind = mir_kind,
                  .level = data.level,
                  .message_args = std::move(message_operands)});
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
