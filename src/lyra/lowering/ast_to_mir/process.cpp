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

namespace lyra::lowering::ast_to_mir {

namespace {
// Counters per procedure kind for descriptive naming
std::size_t initial_counter = 0;
std::size_t always_counter = 0;
std::size_t always_comb_counter = 0;
std::size_t always_latch_counter = 0;
std::size_t always_ff_counter = 0;
}  // namespace

auto LowerProcess(const slang::ast::ProceduralBlockSymbol& procedural_block)
    -> std::unique_ptr<mir::Process> {
  using ProceduralBlockKind = slang::ast::ProceduralBlockKind;

  auto process = std::make_unique<mir::Process>();

  // Generate descriptive name based on procedure kind
  if (!procedural_block.name.empty()) {
    process->name = procedural_block.name;
  } else {
    switch (procedural_block.procedureKind) {
      case ProceduralBlockKind::Initial:
        process->name = fmt::format("initial_{}", initial_counter++);
        break;
      case ProceduralBlockKind::Always:
        process->name = fmt::format("always_{}", always_counter++);
        break;
      case ProceduralBlockKind::AlwaysComb:
        process->name = fmt::format("always_comb_{}", always_comb_counter++);
        break;
      case ProceduralBlockKind::AlwaysLatch:
        process->name = fmt::format("always_latch_{}", always_latch_counter++);
        break;
      case ProceduralBlockKind::AlwaysFF:
        process->name = fmt::format("always_ff_{}", always_ff_counter++);
        break;
      case ProceduralBlockKind::Final:
        process->name = "final";
        break;
    }
  }

  switch (procedural_block.procedureKind) {
    case ProceduralBlockKind::Initial: {
      const auto& slang_statement = procedural_block.getBody();
      auto statement = LowerStatement(slang_statement);
      process->body = std::move(statement);
      break;
    }

    case ProceduralBlockKind::AlwaysLatch:
    case ProceduralBlockKind::AlwaysComb: {
      const auto& slang_statement = procedural_block.getBody();
      auto body = LowerStatement(slang_statement);
      auto variables = CollectSensitivityList(*body);

      std::vector<common::Trigger> triggers;
      for (const auto& variable : variables) {
        triggers.emplace_back(common::Trigger::AnyChange(variable));
      }

      // Build loop: body first, then wait for triggers
      // This achieves always_comb semantics:
      // - Body executes at time 0 (first iteration before wait)
      // - Body re-executes on any input change (after wait)
      auto loop_block = std::make_unique<mir::BlockStatement>();
      loop_block->statements.push_back(std::move(body));
      loop_block->statements.push_back(
          std::make_unique<mir::WaitEventStatement>(std::move(triggers)));

      // while (true) { body; wait_event; }
      auto condition =
          std::make_unique<mir::LiteralExpression>(common::Literal::Bool(true));
      process->body = std::make_unique<mir::WhileStatement>(
          std::move(condition), std::move(loop_block));
      break;
    }

    case ProceduralBlockKind::Always:
    case ProceduralBlockKind::AlwaysFF: {
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
      throw std::runtime_error(
          fmt::format(
              "Unsupported procedural block kind {} in AST to MIR LowerProcess",
              slang::ast::toString(procedural_block.procedureKind)));
  }

  return process;
}

}  // namespace lyra::lowering::ast_to_mir
