#include "lowering/mir_to_lir.hpp"

#include "lowering/lir_builder.hpp"

namespace volans::lowering {

auto MirToLir(const mir::Module& mod) -> lir::Module {
  LirBuilder builder(mod.name);

  for (const auto& var : mod.variables) {
    builder.AddSignal(var.name);
  }

  for (const auto& process : mod.processes) {
    builder.BeginProcess(static_cast<lir::ProcessKind>(process->kind));

    for (const auto& stmt : process->body) {
      if (stmt->kind != mir::Statement::Kind::kAssign) {
        continue;
      }

      const auto& target = stmt->target;
      const auto& expr = stmt->value;

      if (expr->kind == mir::Expression::Kind::kAdd) {
        const auto& [lhs, rhs] = std::get<std::pair<
            std::shared_ptr<mir::Expression>,
            std::shared_ptr<mir::Expression>>>(expr->value);

        auto lhs_name = std::get<std::string>(lhs->value);
        auto rhs_name = std::get<std::string>(rhs->value);

        auto lhs_tmp = builder.MakeTemp("load");
        builder.AddInstruction(
            lir::InstructionKind::kLoadSignal, lhs_tmp,
            {lir::Value::MakeSignal(lhs_name)});

        auto rhs_tmp = builder.MakeTemp("load");
        builder.AddInstruction(
            lir::InstructionKind::kLoadSignal, rhs_tmp,
            {lir::Value::MakeSignal(rhs_name)});

        auto sum_tmp = builder.MakeTemp("add");
        builder.AddInstruction(
            lir::InstructionKind::kBinaryAdd, sum_tmp,
            {lir::Value::MakeTemp(lhs_tmp), lir::Value::MakeTemp(rhs_tmp)});

        builder.AddInstruction(
            lir::InstructionKind::kStoreSignal, "",
            {lir::Value::MakeSignal(target), lir::Value::MakeTemp(sum_tmp)});
      }

      else if (expr->kind == mir::Expression::Kind::kLiteralInt) {
        int value = std::get<int>(expr->value);
        auto lit_tmp = builder.MakeTemp("lit");
        builder.AddInstruction(
            lir::InstructionKind::kLiteralInt, lit_tmp,
            {lir::Value::MakeLiteralInt(value)});

        builder.AddInstruction(
            lir::InstructionKind::kStoreSignal, "",
            {lir::Value::MakeSignal(target), lir::Value::MakeTemp(lit_tmp)});
      }

      else if (expr->kind == mir::Expression::Kind::kIdentifier) {
        std::string name = std::get<std::string>(expr->value);
        auto id_tmp = builder.MakeTemp("load");
        builder.AddInstruction(
            lir::InstructionKind::kLoadSignal, id_tmp,
            {lir::Value::MakeSignal(name)});

        builder.AddInstruction(
            lir::InstructionKind::kStoreSignal, "",
            {lir::Value::MakeSignal(target), lir::Value::MakeTemp(id_tmp)});
      }

      else {
        throw std::runtime_error("Unsupported assign expression kind");
      }
    }

    builder.EndProcess();
  }

  return builder.Build();
}

}  // namespace volans::lowering
