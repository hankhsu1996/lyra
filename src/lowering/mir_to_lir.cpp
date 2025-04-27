#include "lowering/mir_to_lir.hpp"

#include <stdexcept>

#include "lowering/lir_builder.hpp"
#include "mir/expression.hpp"

namespace lyra::lowering {

auto MirToLir(const mir::Module& module) -> lir::Module {
  if (module.name.empty()) {
    throw std::runtime_error("Module has empty name");
  }

  LirBuilder builder(module.name);

  // Add signals
  for (const auto& variable : module.variables) {
    if (variable.name.empty()) {
      throw std::runtime_error("Variable has empty name");
    }
    builder.AddSignal(variable.name);
  }

  // Process each process
  for (const auto& process : module.processes) {
    if (!process) {
      throw std::runtime_error("Null process pointer");
    }

    builder.BeginProcess(static_cast<lir::ProcessKind>(process->process_kind));

    // Process each statement in the process
    for (const auto& statement : process->body) {
      if (!statement || statement->kind != mir::Statement::Kind::kAssign) {
        continue;
      }

      const auto& target = statement->target;
      if (target.empty()) {
        throw std::runtime_error("Statement has empty target");
      }

      const auto& expression = statement->value;
      if (!expression) {
        throw std::runtime_error("Statement has null expression");
      }

      // Handle different expression types
      switch (expression->kind) {
        case mir::Expression::Kind::kLiteral: {
          const auto* literal =
              static_cast<const mir::LiteralExpression*>(expression.get());

          auto tmp = builder.MakeTemp("lit");
          builder.AddInstruction(
              lir::InstructionKind::kLiteralInt, tmp,
              {lir::Value::MakeLiteralInt(literal->value)});
          builder.AddInstruction(
              lir::InstructionKind::kStoreSignal, "",
              {lir::Value::MakeSignal(target), lir::Value::MakeTemp(tmp)});
          break;
        }

        case mir::Expression::Kind::kIdentifier: {
          const auto* identifier =
              static_cast<const mir::IdentifierExpression*>(expression.get());

          if (identifier->name.empty()) {
            throw std::runtime_error("Identifier has empty name");
          }

          auto tmp = builder.MakeTemp("load");
          builder.AddInstruction(
              lir::InstructionKind::kLoadSignal, tmp,
              {lir::Value::MakeSignal(identifier->name)});
          builder.AddInstruction(
              lir::InstructionKind::kStoreSignal, "",
              {lir::Value::MakeSignal(target), lir::Value::MakeTemp(tmp)});
          break;
        }

        case mir::Expression::Kind::kBinary: {
          const auto* binary =
              static_cast<const mir::BinaryExpression*>(expression.get());

          if (binary->op != mir::BinaryExpression::Operator::kAdd) {
            throw std::runtime_error("Unsupported binary operator");
          }

          // Handle left operand
          if (!binary->left ||
              binary->left->kind != mir::Expression::Kind::kIdentifier) {
            throw std::runtime_error(
                "Binary left operand must be an identifier");
          }

          const auto* lhs_id =
              static_cast<const mir::IdentifierExpression*>(binary->left.get());
          if (lhs_id->name.empty()) {
            throw std::runtime_error("Left identifier has empty name");
          }

          auto lhs_tmp = builder.MakeTemp("load");
          builder.AddInstruction(
              lir::InstructionKind::kLoadSignal, lhs_tmp,
              {lir::Value::MakeSignal(lhs_id->name)});

          // Handle right operand
          if (!binary->right ||
              binary->right->kind != mir::Expression::Kind::kIdentifier) {
            throw std::runtime_error(
                "Binary right operand must be an identifier");
          }

          const auto* rhs_id = static_cast<const mir::IdentifierExpression*>(
              binary->right.get());
          if (rhs_id->name.empty()) {
            throw std::runtime_error("Right identifier has empty name");
          }

          auto rhs_tmp = builder.MakeTemp("load");
          builder.AddInstruction(
              lir::InstructionKind::kLoadSignal, rhs_tmp,
              {lir::Value::MakeSignal(rhs_id->name)});

          // Perform the addition
          auto sum_tmp = builder.MakeTemp("add");
          builder.AddInstruction(
              lir::InstructionKind::kBinaryAdd, sum_tmp,
              {lir::Value::MakeTemp(lhs_tmp), lir::Value::MakeTemp(rhs_tmp)});

          // Store the result
          builder.AddInstruction(
              lir::InstructionKind::kStoreSignal, "",
              {lir::Value::MakeSignal(target), lir::Value::MakeTemp(sum_tmp)});
          break;
        }

        default:
          throw std::runtime_error("Unsupported expression kind");
      }
    }

    builder.EndProcess();
  }

  return builder.Build();
}

}  // namespace lyra::lowering
