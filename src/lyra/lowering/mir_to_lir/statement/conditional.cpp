#include <cassert>
#include <cstddef>
#include <cstdint>
#include <string>
#include <vector>

#include "lyra/common/literal.hpp"
#include "lyra/common/type.hpp"
#include "lyra/lir/instruction.hpp"
#include "lyra/lir/operand.hpp"
#include "lyra/lowering/mir_to_lir/context.hpp"
#include "lyra/lowering/mir_to_lir/expression/expression.hpp"
#include "lyra/lowering/mir_to_lir/lir_builder.hpp"
#include "lyra/lowering/mir_to_lir/statement/internal.hpp"
#include "lyra/lowering/mir_to_lir/statement/statement.hpp"
#include "lyra/mir/statement.hpp"

namespace lyra::lowering::mir_to_lir {

using Type = common::Type;
using Literal = common::Literal;
using Operand = lir::Operand;
using Instruction = lir::Instruction;
using IK = lir::InstructionKind;
using TempRef = lir::TempRef;

namespace {

void EmitWarning(const char* message, LirBuilder& builder) {
  auto warning_instr =
      Instruction::SystemCall("$warning", std::vector<Operand>{});
  auto format_literal = builder.InternLiteral(Literal::String(message));
  auto format_temp = builder.AllocateTemp("warning.fmt", Type::String());
  builder.AddInstruction(
      Instruction::Basic(
          IK::kLiteral, format_temp, Operand::Literal(format_literal)));
  warning_instr.format_operand = Operand::Temp(format_temp);
  warning_instr.format_string_is_literal = true;
  builder.AddInstruction(std::move(warning_instr));
}

// Evaluates a case item's match condition against a pre-evaluated case
// selector. Handles multiple expressions per item (e.g., `0, 1, 2: stmt`) and
// casez masks. Returns a temp containing the match result (0 or 1).
auto EvalCaseItemMatch(
    const mir::CaseItem& item, TempRef cond_temp, LirBuilder& builder)
    -> TempRef {
  TempRef match_result{};
  for (size_t j = 0; j < item.expressions.size(); ++j) {
    auto expr_value = LowerExpression(*item.expressions[j], builder);
    int64_t mask = item.masks[j];

    TempRef cmp_lhs{};
    if (mask == -1) {
      cmp_lhs = cond_temp;
    } else {
      auto mask_literal =
          builder.InternLiteral(Literal::UInt(static_cast<uint32_t>(mask)));
      auto mask_temp = builder.AllocateTemp("case.mask", Type::UInt());
      builder.AddInstruction(
          Instruction::Basic(
              IK::kLiteral, mask_temp, Operand::Literal(mask_literal)));
      auto masked_cond = builder.AllocateTemp("case.masked_cond", Type::UInt());
      builder.AddInstruction(
          Instruction::Basic(
              IK::kBinaryBitwiseAnd, masked_cond,
              std::vector<Operand>{
                  Operand::Temp(cond_temp), Operand::Temp(mask_temp)}));
      cmp_lhs = masked_cond;
    }

    auto cmp_temp = builder.AllocateTemp("case.cmp", Type::Int());
    builder.AddInstruction(
        Instruction::Basic(
            IK::kBinaryEqual, cmp_temp,
            std::vector<Operand>{
                Operand::Temp(cmp_lhs), Operand::Temp(expr_value)}));

    if (j == 0) {
      match_result = cmp_temp;
    } else {
      auto or_temp = builder.AllocateTemp("case.or", Type::Int());
      builder.AddInstruction(
          Instruction::Basic(
              IK::kBinaryLogicalOr, or_temp,
              std::vector<Operand>{
                  Operand::Temp(match_result), Operand::Temp(cmp_temp)}));
      match_result = or_temp;
    }
  }
  return match_result;
}

// Emits overlap check: warns if count > 1.
// count_temp: temp holding number of matching conditions
// label_prefix: "case" or "if" for label naming
void EmitOverlapCheck(
    TempRef count_temp, const char* warning_msg, const char* label_prefix,
    LirBuilder& builder) {
  auto one_lit = builder.InternLiteral(Literal::Int(1));
  auto one_temp =
      builder.AllocateTemp(std::string(label_prefix) + ".one", Type::Int());
  builder.AddInstruction(
      Instruction::Basic(IK::kLiteral, one_temp, Operand::Literal(one_lit)));

  auto overlap_cond =
      builder.AllocateTemp(std::string(label_prefix) + ".overlap", Type::Int());
  builder.AddInstruction(
      Instruction::Basic(
          IK::kBinaryGreaterThan, overlap_cond,
          std::vector<Operand>{
              Operand::Temp(count_temp), Operand::Temp(one_temp)}));

  auto overlap_warn_label =
      builder.MakeLabel(std::string(label_prefix) + ".overlap_warn");
  auto after_overlap_label =
      builder.MakeLabel(std::string(label_prefix) + ".after_overlap");
  builder.AddInstruction(
      Instruction::Branch(
          overlap_cond, overlap_warn_label, after_overlap_label));
  builder.EndBlock();

  builder.StartBlock(overlap_warn_label);
  EmitWarning(warning_msg, builder);
  builder.AddInstruction(Instruction::Jump(after_overlap_label));
  builder.EndBlock();

  builder.StartBlock(after_overlap_label);
}

// Emits no-match check: warns if count == 0.
// count_temp: temp holding number of matching conditions
// zero_lit: pre-interned zero literal (for reuse)
// label_prefix: "case" or "if" for label naming
void EmitNoMatchCheck(
    TempRef count_temp, lir::LiteralRef zero_lit, const char* warning_msg,
    const char* label_prefix, LirBuilder& builder) {
  auto zero_temp =
      builder.AllocateTemp(std::string(label_prefix) + ".zero", Type::Int());
  builder.AddInstruction(
      Instruction::Basic(IK::kLiteral, zero_temp, Operand::Literal(zero_lit)));

  auto no_match_cond = builder.AllocateTemp(
      std::string(label_prefix) + ".no_match", Type::Int());
  builder.AddInstruction(
      Instruction::Basic(
          IK::kBinaryEqual, no_match_cond,
          std::vector<Operand>{
              Operand::Temp(count_temp), Operand::Temp(zero_temp)}));

  auto no_match_warn_label =
      builder.MakeLabel(std::string(label_prefix) + ".no_match_warn");
  auto after_no_match_label =
      builder.MakeLabel(std::string(label_prefix) + ".after_no_match");
  builder.AddInstruction(
      Instruction::Branch(
          no_match_cond, no_match_warn_label, after_no_match_label));
  builder.EndBlock();

  builder.StartBlock(no_match_warn_label);
  EmitWarning(warning_msg, builder);
  builder.AddInstruction(Instruction::Jump(after_no_match_label));
  builder.EndBlock();

  builder.StartBlock(after_no_match_label);
}

// Counts matching conditions by summing bool temps (0 or 1 each).
// Returns a temp holding the total count.
auto CountMatches(
    const std::vector<TempRef>& match_temps, const char* label_prefix,
    LirBuilder& builder) -> TempRef {
  auto zero_lit = builder.InternLiteral(Literal::Int(0));
  auto count_temp =
      builder.AllocateTemp(std::string(label_prefix) + ".count", Type::Int());
  builder.AddInstruction(
      Instruction::Basic(IK::kLiteral, count_temp, Operand::Literal(zero_lit)));

  for (auto match : match_temps) {
    auto match_as_int = builder.AllocateTemp(
        std::string(label_prefix) + ".match_int", Type::Int());
    builder.AddInstruction(
        Instruction::WithType(
            IK::kConversion, match_as_int, match, Type::Int()));
    auto new_count =
        builder.AllocateTemp(std::string(label_prefix) + ".count", Type::Int());
    builder.AddInstruction(
        Instruction::Basic(
            IK::kBinaryAdd, new_count,
            std::vector<Operand>{
                Operand::Temp(count_temp), Operand::Temp(match_as_int)}));
    count_temp = new_count;
  }
  return count_temp;
}

}  // namespace

void LowerConditionalStatement(
    const mir::ConditionalStatement& if_stmt, LirBuilder& builder,
    LoweringContext& lowering_context) {
  assert(if_stmt.condition);
  assert(if_stmt.then_branch);

  bool needs_overlap_check =
      if_stmt.check == mir::UniquePriorityCheck::kUnique ||
      if_stmt.check == mir::UniquePriorityCheck::kUnique0;
  bool needs_priority_check =
      if_stmt.check == mir::UniquePriorityCheck::kPriority;

  auto end_label = builder.MakeLabel("if.end");

  if (needs_overlap_check) {
    // unique/unique0 if: Evaluate ALL conditions first, then check overlap
    auto [conditions, bodies, final_else] = mir::CollectIfChain(if_stmt);

    // Evaluate ALL conditions upfront
    std::vector<TempRef> cond_temps;
    for (const auto* cond : conditions) {
      cond_temps.push_back(LowerExpression(*cond, builder));
    }

    auto count_temp = CountMatches(cond_temps, "if", builder);

    EmitOverlapCheck(
        count_temp, "multiple conditions true in unique if", "if", builder);

    bool needs_no_match = if_stmt.check == mir::UniquePriorityCheck::kUnique &&
                          final_else == nullptr;
    if (needs_no_match) {
      auto zero_lit = builder.InternLiteral(Literal::Int(0));
      EmitNoMatchCheck(
          count_temp, zero_lit, "no condition matched in unique if", "if",
          builder);
    }

    // Dispatch to first true condition
    lir::LabelRef else_label =
        final_else != nullptr ? builder.MakeLabel("if.else") : end_label;

    for (size_t i = 0; i < cond_temps.size(); ++i) {
      auto body_label = builder.MakeLabel("if.body");
      auto next_check = (i + 1 < cond_temps.size())
                            ? builder.MakeLabel("if.dispatch")
                            : else_label;

      builder.AddInstruction(
          Instruction::Branch(cond_temps[i], body_label, next_check));
      builder.EndBlock();

      builder.StartBlock(body_label);
      if (bodies[i] != nullptr) {
        LowerStatement(*bodies[i], builder, lowering_context);
      }
      builder.AddInstruction(Instruction::Jump(end_label));
      builder.EndBlock();

      if (i + 1 < cond_temps.size()) {
        builder.StartBlock(next_check);
      }
    }

    if (final_else != nullptr) {
      builder.StartBlock(else_label);
      LowerStatement(*final_else, builder, lowering_context);
      builder.AddInstruction(Instruction::Jump(end_label));
      builder.EndBlock();
    }

    builder.StartBlock(end_label);
  } else if (needs_priority_check) {
    // priority if: Short-circuit with no-match check at end
    auto [conditions, bodies, final_else] = mir::CollectIfChain(if_stmt);

    // Track if any branch was taken
    auto zero_lit = builder.InternLiteral(Literal::Int(0));
    auto matched_temp = builder.AllocateTemp("if.matched", Type::Int());
    builder.AddInstruction(
        Instruction::Basic(
            IK::kLiteral, matched_temp, Operand::Literal(zero_lit)));

    auto one_lit = builder.InternLiteral(Literal::Int(1));

    lir::LabelRef else_label = final_else != nullptr
                                   ? builder.MakeLabel("if.else")
                                   : builder.MakeLabel("if.no_match_check");

    for (size_t i = 0; i < conditions.size(); ++i) {
      auto cond_value = LowerExpression(*conditions[i], builder);

      auto body_label = builder.MakeLabel("if.body");
      auto next_check = (i + 1 < conditions.size())
                            ? builder.MakeLabel("if.check")
                            : else_label;

      builder.AddInstruction(
          Instruction::Branch(cond_value, body_label, next_check));
      builder.EndBlock();

      builder.StartBlock(body_label);
      // Set matched = 1
      auto one_temp = builder.AllocateTemp("if.one", Type::Int());
      builder.AddInstruction(
          Instruction::Basic(
              IK::kLiteral, one_temp, Operand::Literal(one_lit)));
      builder.AddInstruction(
          Instruction::Basic(IK::kMove, matched_temp, one_temp));

      if (bodies[i] != nullptr) {
        LowerStatement(*bodies[i], builder, lowering_context);
      }
      builder.AddInstruction(Instruction::Jump(end_label));
      builder.EndBlock();

      if (i + 1 < conditions.size()) {
        builder.StartBlock(next_check);
      }
    }

    // Final else or no-match check
    if (final_else != nullptr) {
      builder.StartBlock(else_label);
      // Set matched = 1 (else counts as a match)
      auto one_temp = builder.AllocateTemp("if.one", Type::Int());
      builder.AddInstruction(
          Instruction::Basic(
              IK::kLiteral, one_temp, Operand::Literal(one_lit)));
      builder.AddInstruction(
          Instruction::Basic(IK::kMove, matched_temp, one_temp));
      LowerStatement(*final_else, builder, lowering_context);
      builder.AddInstruction(Instruction::Jump(end_label));
      builder.EndBlock();
    } else {
      // No-match check: if (matched == 0) emit warning
      builder.StartBlock(else_label);

      auto zero_temp = builder.AllocateTemp("if.zero", Type::Int());
      builder.AddInstruction(
          Instruction::Basic(
              IK::kLiteral, zero_temp, Operand::Literal(zero_lit)));

      auto no_match_cond = builder.AllocateTemp("if.no_match", Type::Int());
      builder.AddInstruction(
          Instruction::Basic(
              IK::kBinaryEqual, no_match_cond,
              std::vector<Operand>{
                  Operand::Temp(matched_temp), Operand::Temp(zero_temp)}));

      auto warn_label = builder.MakeLabel("if.warn");
      builder.AddInstruction(
          Instruction::Branch(no_match_cond, warn_label, end_label));
      builder.EndBlock();

      builder.StartBlock(warn_label);
      EmitWarning("no condition matched in priority if", builder);
      builder.AddInstruction(Instruction::Jump(end_label));
      builder.EndBlock();
    }

    builder.StartBlock(end_label);
  } else {
    // Regular if: existing short-circuit logic
    auto then_label = builder.MakeLabel("if.then");
    auto else_label = builder.MakeLabel("if.else");

    auto condition_value = LowerExpression(*if_stmt.condition, builder);

    if (if_stmt.else_branch) {
      auto branch =
          Instruction::Branch(condition_value, then_label, else_label);
      builder.AddInstruction(std::move(branch));

      builder.StartBlock(then_label);
      LowerStatement(*if_stmt.then_branch, builder, lowering_context);
      builder.AddInstruction(Instruction::Jump(end_label));
      builder.EndBlock();

      builder.StartBlock(else_label);
      LowerStatement(*if_stmt.else_branch, builder, lowering_context);
      builder.AddInstruction(Instruction::Jump(end_label));
      builder.EndBlock();
    } else {
      auto branch = Instruction::Branch(condition_value, then_label, end_label);
      builder.AddInstruction(std::move(branch));

      builder.StartBlock(then_label);
      LowerStatement(*if_stmt.then_branch, builder, lowering_context);
      builder.AddInstruction(Instruction::Jump(end_label));
      builder.EndBlock();
    }

    builder.StartBlock(end_label);
  }
}

void LowerCaseStatement(
    const mir::CaseStatement& case_stmt, LirBuilder& builder,
    LoweringContext& lowering_context) {
  assert(case_stmt.condition);

  // Evaluate case condition once and store in a temp
  auto cond_value = LowerExpression(*case_stmt.condition, builder);
  auto cond_temp = builder.AllocateTemp("case.cond", Type::Int());
  builder.AddInstruction(Instruction::Basic(IK::kMove, cond_temp, cond_value));

  auto end_label = builder.MakeLabel("case.end");

  bool needs_overlap_check =
      case_stmt.check == mir::UniquePriorityCheck::kUnique ||
      case_stmt.check == mir::UniquePriorityCheck::kUnique0;

  if (needs_overlap_check) {
    // unique/unique0: Evaluate ALL conditions first, then check overlap
    std::vector<TempRef> match_temps;
    for (const auto& item : case_stmt.items) {
      match_temps.push_back(EvalCaseItemMatch(item, cond_temp, builder));
    }

    auto count_temp = CountMatches(match_temps, "case", builder);

    EmitOverlapCheck(count_temp, "multiple case items match", "case", builder);

    bool needs_no_match =
        case_stmt.check == mir::UniquePriorityCheck::kUnique &&
        !case_stmt.default_case;
    if (needs_no_match) {
      auto zero_lit = builder.InternLiteral(Literal::Int(0));
      EmitNoMatchCheck(
          count_temp, zero_lit, "no matching case item", "case", builder);
    }

    // Dispatch to first matching item
    lir::LabelRef default_label =
        case_stmt.default_case ? builder.MakeLabel("case.default") : end_label;

    for (size_t i = 0; i < match_temps.size(); ++i) {
      auto item_label = builder.MakeLabel("case.item");
      auto next_check = (i + 1 < match_temps.size())
                            ? builder.MakeLabel("case.dispatch")
                            : default_label;

      builder.AddInstruction(
          Instruction::Branch(match_temps[i], item_label, next_check));
      builder.EndBlock();

      builder.StartBlock(item_label);
      if (case_stmt.items[i].statement) {
        LowerStatement(
            *case_stmt.items[i].statement, builder, lowering_context);
      }
      builder.AddInstruction(Instruction::Jump(end_label));
      builder.EndBlock();

      if (i + 1 < match_temps.size()) {
        builder.StartBlock(next_check);
      }
    }

    if (case_stmt.default_case) {
      builder.StartBlock(default_label);
      LowerStatement(*case_stmt.default_case, builder, lowering_context);
      builder.AddInstruction(Instruction::Jump(end_label));
      builder.EndBlock();
    }

    builder.StartBlock(end_label);
  } else {
    // Regular or priority case: short-circuit evaluation
    std::vector<lir::LabelRef> item_labels;
    std::vector<lir::LabelRef> check_labels;
    for (size_t i = 0; i < case_stmt.items.size(); ++i) {
      item_labels.push_back(builder.MakeLabel("case.item"));
      check_labels.push_back(builder.MakeLabel("case.check"));
    }

    lir::LabelRef default_label =
        case_stmt.default_case ? builder.MakeLabel("case.default") : end_label;

    bool needs_no_match_warning =
        case_stmt.check == mir::UniquePriorityCheck::kPriority &&
        !case_stmt.default_case;
    auto no_match_label =
        needs_no_match_warning ? builder.MakeLabel("case.no_match") : end_label;

    if (needs_no_match_warning) {
      default_label = no_match_label;
    }

    if (!case_stmt.items.empty()) {
      builder.AddInstruction(Instruction::Jump(check_labels[0]));
    } else if (case_stmt.default_case) {
      builder.AddInstruction(Instruction::Jump(default_label));
    } else {
      builder.AddInstruction(Instruction::Jump(end_label));
    }

    for (size_t i = 0; i < case_stmt.items.size(); ++i) {
      const auto& item = case_stmt.items[i];
      auto next_label = (i + 1 < case_stmt.items.size()) ? check_labels[i + 1]
                                                         : default_label;

      builder.StartBlock(check_labels[i]);
      auto match_result = EvalCaseItemMatch(item, cond_temp, builder);
      builder.AddInstruction(
          Instruction::Branch(match_result, item_labels[i], next_label));
      builder.EndBlock();

      builder.StartBlock(item_labels[i]);
      if (item.statement) {
        LowerStatement(*item.statement, builder, lowering_context);
      }
      builder.AddInstruction(Instruction::Jump(end_label));
      builder.EndBlock();
    }

    if (case_stmt.default_case) {
      builder.StartBlock(default_label);
      LowerStatement(*case_stmt.default_case, builder, lowering_context);
      builder.AddInstruction(Instruction::Jump(end_label));
      builder.EndBlock();
    }

    if (needs_no_match_warning) {
      builder.StartBlock(no_match_label);
      EmitWarning("no matching case item", builder);
      builder.AddInstruction(Instruction::Jump(end_label));
      builder.EndBlock();
    }

    builder.StartBlock(end_label);
  }
}

}  // namespace lyra::lowering::mir_to_lir
