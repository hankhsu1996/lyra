#include "lowering/ast_to_mir/process.hpp"

#include <stdexcept>

#include <fmt/format.h>
#include <lowering/ast_to_mir/variable.hpp>
#include <slang/ast/Statement.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/statements/MiscStatements.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <spdlog/spdlog.h>

#include "lowering/ast_to_mir/collect_sensitivity.hpp"
#include "lowering/ast_to_mir/statement.hpp"
#include "mir/process.hpp"

namespace lyra::lowering {

auto LowerProcess(const slang::ast::ProceduralBlockSymbol& procedural_block)
    -> std::unique_ptr<mir::Process> {
  using ProceduralBlockKind = slang::ast::ProceduralBlockKind;

  auto process = std::make_unique<mir::Process>();

  switch (procedural_block.procedureKind) {
    case ProceduralBlockKind::Initial: {
      process->process_kind = mir::ProcessKind::kInitial;

      const auto& slang_statement = procedural_block.getBody();
      auto statement = LowerStatement(slang_statement);
      process->body = std::move(statement);
      break;
    }

    case ProceduralBlockKind::AlwaysComb: {
      process->process_kind = mir::ProcessKind::kInitial;

      const auto& slang_statement = procedural_block.getBody();
      auto main_body = LowerStatement(slang_statement);
      auto variables = CollectSensitivityList(*main_body);

      std::vector<common::Trigger<std::string>> triggers;
      for (const auto& variable : variables) {
        triggers.emplace_back(
            common::Trigger<std::string>::AnyChange(variable));
      }

      // Build the body for the loop: wait for triggers, then execute the logic
      auto loop_block = std::make_unique<mir::BlockStatement>();
      loop_block->statements.push_back(
          std::make_unique<mir::WaitEventStatement>(std::move(triggers)));
      loop_block->statements.push_back(LowerStatement(slang_statement));

      // while (true) { wait_event; body; }
      auto loop = std::make_unique<mir::WhileStatement>(
          std::make_unique<mir::LiteralExpression>(common::Literal::Bool(true)),
          std::move(loop_block));

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
    case ProceduralBlockKind::AlwaysLatch:
    case ProceduralBlockKind::AlwaysFF:
    case ProceduralBlockKind::Final:
      throw std::runtime_error(fmt::format(
          "Unsupported procedural block kind {} in AST to MIR LowerProcess",
          slang::ast::toString(procedural_block.procedureKind)));
  }

  return process;
}

}  // namespace lyra::lowering
