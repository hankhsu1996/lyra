#include "executor/lir_instruction_executor.hpp"

#include <fmt/core.h>

#include "core/arithmetic_ops.hpp"

namespace lyra::lir {

LIRInstructionExecutor::LIRInstructionExecutor(ExecutionContext& context)
    : ctx_(context) {
}

// Execute a single instruction in the given context
auto LIRInstructionExecutor::ExecuteInstruction(const Instruction& instr)
    -> LIRInstructionResult {
  switch (instr.kind) {
    case InstructionKind::kLiteralBit: {
      bool val = std::get<bool>(instr.operands[0].data);
      ctx_.get().temp_table.Write(instr.result, RuntimeValue::FromBit(val));
      return LIRInstructionResult::Continue();
    }

    case InstructionKind::kLiteralInt: {
      int32_t val = std::get<int32_t>(instr.operands[0].data);
      ctx_.get().temp_table.Write(instr.result, RuntimeValue::FromInt(val));
      return LIRInstructionResult::Continue();
    }

    case InstructionKind::kLiteralLongInt: {
      int64_t val = std::get<int64_t>(instr.operands[0].data);
      ctx_.get().temp_table.Write(instr.result, RuntimeValue::FromLongInt(val));
      return LIRInstructionResult::Continue();
    }

    case InstructionKind::kLiteralString: {
      const auto& val = std::get<std::string>(instr.operands[0].data);
      ctx_.get().temp_table.Write(instr.result, RuntimeValue::FromString(val));
      return LIRInstructionResult::Continue();
    }

    case InstructionKind::kBinaryAdd: {
      auto lhs_name = std::get<std::string>(instr.operands[0].data);
      auto rhs_name = std::get<std::string>(instr.operands[1].data);
      const auto& lhs_val = ctx_.get().temp_table.Read(lhs_name);
      const auto& rhs_val = ctx_.get().temp_table.Read(rhs_name);

      auto result = lyra::kAddOp(lhs_val, rhs_val);
      ctx_.get().temp_table.Write(instr.result, result);

      return LIRInstructionResult::Continue();
    }

    case InstructionKind::kBinarySubtract: {
      auto lhs = std::get<std::string>(instr.operands[0].data);
      auto rhs = std::get<std::string>(instr.operands[1].data);
      const auto& lhs_val = ctx_.get().temp_table.Read(lhs);
      const auto& rhs_val = ctx_.get().temp_table.Read(rhs);

      auto result = lyra::kSubOp(lhs_val, rhs_val);
      ctx_.get().temp_table.Write(instr.result, result);

      return LIRInstructionResult::Continue();
    }

    case InstructionKind::kBinaryMultiply: {
      auto lhs = std::get<std::string>(instr.operands[0].data);
      auto rhs = std::get<std::string>(instr.operands[1].data);
      const auto& lhs_val = ctx_.get().temp_table.Read(lhs);
      const auto& rhs_val = ctx_.get().temp_table.Read(rhs);

      auto result = lyra::kMulOp(lhs_val, rhs_val);
      ctx_.get().temp_table.Write(instr.result, result);

      return LIRInstructionResult::Continue();
    }

    case InstructionKind::kBinaryDivide: {
      auto lhs = std::get<std::string>(instr.operands[0].data);
      auto rhs = std::get<std::string>(instr.operands[1].data);
      const auto& lhs_val = ctx_.get().temp_table.Read(lhs);
      const auto& rhs_val = ctx_.get().temp_table.Read(rhs);

      auto result = lyra::kDivOp(lhs_val, rhs_val);
      ctx_.get().temp_table.Write(instr.result, result);

      return LIRInstructionResult::Continue();
    }

    case InstructionKind::kBinaryModulo: {
      auto lhs = std::get<std::string>(instr.operands[0].data);
      auto rhs = std::get<std::string>(instr.operands[1].data);
      const auto& lhs_val = ctx_.get().temp_table.Read(lhs);
      const auto& rhs_val = ctx_.get().temp_table.Read(rhs);

      auto result = lyra::kModOp(lhs_val, rhs_val);
      ctx_.get().temp_table.Write(instr.result, result);

      return LIRInstructionResult::Continue();
    }

    case InstructionKind::kBinaryEqualInt: {
      auto lhs = std::get<std::string>(instr.operands[0].data);
      auto rhs = std::get<std::string>(instr.operands[1].data);
      int64_t v1 = ctx_.get().temp_table.Read(lhs).AsInt();
      int64_t v2 = ctx_.get().temp_table.Read(rhs).AsInt();
      ctx_.get().temp_table.Write(
          instr.result, RuntimeValue::FromBit(v1 == v2));
      return LIRInstructionResult::Continue();
    }

    case InstructionKind::kBinaryEqualString: {
      auto lhs = std::get<std::string>(instr.operands[0].data);
      auto rhs = std::get<std::string>(instr.operands[1].data);
      std::string v1 = ctx_.get().temp_table.Read(lhs).AsString();
      std::string v2 = ctx_.get().temp_table.Read(rhs).AsString();
      ctx_.get().temp_table.Write(
          instr.result, RuntimeValue::FromBit(v1 == v2));
      return LIRInstructionResult::Continue();
    }

    case InstructionKind::kBinaryNotEqualInt: {
      auto lhs = std::get<std::string>(instr.operands[0].data);
      auto rhs = std::get<std::string>(instr.operands[1].data);
      int64_t v1 = ctx_.get().temp_table.Read(lhs).AsInt();
      int64_t v2 = ctx_.get().temp_table.Read(rhs).AsInt();
      ctx_.get().temp_table.Write(
          instr.result, RuntimeValue::FromBit(v1 != v2));
      return LIRInstructionResult::Continue();
    }

    case InstructionKind::kBinaryNotEqualString: {
      auto lhs = std::get<std::string>(instr.operands[0].data);
      auto rhs = std::get<std::string>(instr.operands[1].data);
      std::string v1 = ctx_.get().temp_table.Read(lhs).AsString();
      std::string v2 = ctx_.get().temp_table.Read(rhs).AsString();
      ctx_.get().temp_table.Write(
          instr.result, RuntimeValue::FromBit(v1 != v2));
      return LIRInstructionResult::Continue();
    }

    case InstructionKind::kBinaryLessThan: {
      auto lhs = std::get<std::string>(instr.operands[0].data);
      auto rhs = std::get<std::string>(instr.operands[1].data);
      int64_t v1 = ctx_.get().temp_table.Read(lhs).AsInt();
      int64_t v2 = ctx_.get().temp_table.Read(rhs).AsInt();
      ctx_.get().temp_table.Write(instr.result, RuntimeValue::FromBit(v1 < v2));
      return LIRInstructionResult::Continue();
    }

    case InstructionKind::kBinaryLessThanEqual: {
      auto lhs = std::get<std::string>(instr.operands[0].data);
      auto rhs = std::get<std::string>(instr.operands[1].data);
      int64_t v1 = ctx_.get().temp_table.Read(lhs).AsInt();
      int64_t v2 = ctx_.get().temp_table.Read(rhs).AsInt();
      ctx_.get().temp_table.Write(
          instr.result, RuntimeValue::FromBit(v1 <= v2));
      return LIRInstructionResult::Continue();
    }

    case InstructionKind::kBinaryGreaterThan: {
      auto lhs = std::get<std::string>(instr.operands[0].data);
      auto rhs = std::get<std::string>(instr.operands[1].data);
      int64_t v1 = ctx_.get().temp_table.Read(lhs).AsInt();
      int64_t v2 = ctx_.get().temp_table.Read(rhs).AsInt();
      ctx_.get().temp_table.Write(instr.result, RuntimeValue::FromBit(v1 > v2));
      return LIRInstructionResult::Continue();
    }

    case InstructionKind::kBinaryGreaterThanEqual: {
      auto lhs = std::get<std::string>(instr.operands[0].data);
      auto rhs = std::get<std::string>(instr.operands[1].data);
      int64_t v1 = ctx_.get().temp_table.Read(lhs).AsInt();
      int64_t v2 = ctx_.get().temp_table.Read(rhs).AsInt();
      ctx_.get().temp_table.Write(
          instr.result, RuntimeValue::FromBit(v1 >= v2));
      return LIRInstructionResult::Continue();
    }

    case InstructionKind::kLoadVariable: {
      auto src_variable = std::get<std::string>(instr.operands[0].data);
      auto value = ctx_.get().variable_table.Read(src_variable);
      ctx_.get().temp_table.Write(instr.result, value);
      return LIRInstructionResult::Continue();
    }

    case InstructionKind::kStoreVariable: {
      auto dst_variable = std::get<std::string>(instr.operands[0].data);
      auto src_val = std::get<std::string>(instr.operands[1].data);
      auto value = ctx_.get().temp_table.Read(src_val);
      ctx_.get().variable_table.Write(dst_variable, value);
      return LIRInstructionResult::Continue(dst_variable);
    }

    case InstructionKind::kDelay: {
      auto delay_amount = std::get<int64_t>(instr.operands[0].data);
      return LIRInstructionResult::Delay(delay_amount);
    }

    case InstructionKind::kSystemCall: {
      if (instr.system_call_name == "$finish") {
        // If there's an argument, we could use it to determine
        // the level of diagnostic info to print (future enhancement)
        // 0 = no info, 1 = minimal info, 2 = full stats

        // For now, we just terminate the simulation
        return LIRInstructionResult::Finish();
      }

      throw std::runtime_error(
          fmt::format("Unsupported system call: {}", instr.system_call_name));
    }

    case InstructionKind::kJump: {
      if (instr.operands.size() != 1) {
        throw std::runtime_error(
            "Jump instruction must have exactly one operand");
      }

      // The operand should be a string literal representing the target label
      if (instr.operands[0].kind != Value::Kind::kLiteralString) {
        throw std::runtime_error("Jump target must be a string literal");
      }

      std::string target = std::get<std::string>(instr.operands[0].data);
      return LIRInstructionResult::Jump(target);
    }

    case InstructionKind::kBranch: {
      if (instr.operands.size() != 3) {
        throw std::runtime_error(
            "Branch instruction must have exactly three operands");
      }

      // The first operand is the condition (a temporary value)
      if (instr.operands[0].kind != Value::Kind::kTemp) {
        throw std::runtime_error("Branch condition must be a temporary value");
      }

      // The second and third operands are the true and false target labels
      if (instr.operands[1].kind != Value::Kind::kLiteralString ||
          instr.operands[2].kind != Value::Kind::kLiteralString) {
        throw std::runtime_error("Branch targets must be string literals");
      }

      std::string condition = std::get<std::string>(instr.operands[0].data);
      std::string true_target = std::get<std::string>(instr.operands[1].data);
      std::string false_target = std::get<std::string>(instr.operands[2].data);

      // Evaluate the condition immediately in the executor
      RuntimeValue condition_value = ctx_.get().temp_table.Read(condition);
      bool condition_result = condition_value.AsInt() != 0;

      // Select the appropriate target based on condition result
      std::string next_label = condition_result ? true_target : false_target;

      // Return a simple jump with the decided target
      return LIRInstructionResult::Jump(next_label);
    }
  }
}

}  // namespace lyra::lir
