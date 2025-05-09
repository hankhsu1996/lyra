#include "lyra/lowering/ast_to_mir/process.hpp"

#include <stdexcept>

#include <fmt/format.h>
#include <slang/ast/Statement.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/statements/MiscStatements.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <spdlog/spdlog.h>

#include "lyra/lowering/ast_to_mir/collect_sensitivity.hpp"
#include "lyra/lowering/ast_to_mir/statement.hpp"
#include "lyra/mir/process.hpp"

namespace lyra::lowering {

auto LowerProcess(const slang::ast::ProceduralBlockSymbol& procedural_block)
    -> std::unique_ptr<mir::Process> {
  using ProceduralBlockKind = slang::ast::ProceduralBlockKind;

  auto process = std::make_unique<mir::Process>();

  static std::size_t process_counter = 0;
  if (procedural_block.name.empty()) {
    process->name = fmt::format("proc_{}", process_counter++);
  } else {
    process->name = procedural_block.name;
  }

  switch (procedural_block.procedureKind) {
    case ProceduralBlockKind::Initial: {
      process->process_kind = mir::ProcessKind::kInitial;

      const auto& slang_statement = procedural_block.getBody();
      auto statement = LowerStatement(slang_statement);
      process->body = std::move(statement);
      break;
    }

    case ProceduralBlockKind::AlwaysLatch:
    case ProceduralBlockKind::AlwaysComb: {
      process->process_kind = mir::ProcessKind::kInitial;

      const auto& slang_statement = procedural_block.getBody();
      auto main_body = LowerStatement(slang_statement);
      auto variables = CollectSensitivityList(*main_body);

      std::vector<common::Trigger> triggers;
      for (const auto& variable : variables) {
        triggers.emplace_back(common::Trigger::AnyChange(variable));
      }

      // Build the body for the loop: wait for triggers, then execute the logic
      auto loop_block = std::make_unique<mir::BlockStatement>();
      loop_block->statements.push_back(
          std::make_unique<mir::WaitEventStatement>(std::move(triggers)));
      loop_block->statements.push_back(LowerStatement(slang_statement));

      // while (true) { wait_event; body; }
      auto condition =
          std::make_unique<mir::LiteralExpression>(common::Literal::Bool(true));
      auto loop = std::make_unique<mir::WhileStatement>(
          std::move(condition), std::move(loop_block));

      // Insert an initial execution of the body before entering the loop.
      // This models the SystemVerilog always_comb behavior that executes once
      // at time 0.
      auto full_block = std::make_unique<mir::BlockStatement>();
      full_block->statements.push_back(std::move(main_body));
      full_block->statements.push_back(std::move(loop));

      process->body = std::move(full_block);
      break;
    }

    case ProceduralBlockKind::Always:
    case ProceduralBlockKind::AlwaysFF: {
      process->process_kind = mir::ProcessKind::kInitial;

      // Lower the user's body, which should contain WaitEventStatement itself
      auto loop_block = LowerStatement(procedural_block.getBody());

      // Simply wrap in while (true) { ... }
      auto condition =
          std::make_unique<mir::LiteralExpression>(common::Literal::Bool(true));
      auto loop = std::make_unique<mir::WhileStatement>(
          std::move(condition), std::move(loop_block));

      process->body = std::move(loop);
      break;
    }

    case ProceduralBlockKind::Final:
      throw std::runtime_error(fmt::format(
          "Unsupported procedural block kind {} in AST to MIR LowerProcess",
          slang::ast::toString(procedural_block.procedureKind)));
  }

  return process;
}

}  // namespace lyra::lowering
