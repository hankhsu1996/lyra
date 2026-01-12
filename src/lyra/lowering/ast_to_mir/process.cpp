#include "lyra/lowering/ast_to_mir/process.hpp"

#include <memory>
#include <utility>
#include <vector>

#include <fmt/format.h>
#include <slang/ast/Statement.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/statements/MiscStatements.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <spdlog/spdlog.h>

#include "lyra/common/constant.hpp"
#include "lyra/common/diagnostic.hpp"
#include "lyra/common/trigger.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/lowering/ast_to_mir/collect_sensitivity.hpp"
#include "lyra/lowering/ast_to_mir/statement.hpp"
#include "lyra/mir/expression.hpp"
#include "lyra/mir/process.hpp"
#include "lyra/mir/statement.hpp"

namespace lyra::lowering::ast_to_mir {

auto LowerProcess(
    const slang::ast::ProceduralBlockSymbol& procedural_block,
    ProcessCounters& counters, common::TypeArena& arena)
    -> std::unique_ptr<mir::Process> {
  using ProceduralBlockKind = slang::ast::ProceduralBlockKind;

  auto process = std::make_unique<mir::Process>();

  // Generate descriptive name based on procedure kind
  if (!procedural_block.name.empty()) {
    process->name = procedural_block.name;
  } else {
    switch (procedural_block.procedureKind) {
      case ProceduralBlockKind::Initial:
        process->name = fmt::format("initial_{}", counters.initial++);
        break;
      case ProceduralBlockKind::Always:
        process->name = fmt::format("always_{}", counters.always++);
        break;
      case ProceduralBlockKind::AlwaysComb:
        process->name = fmt::format("always_comb_{}", counters.always_comb++);
        break;
      case ProceduralBlockKind::AlwaysLatch:
        process->name = fmt::format("always_latch_{}", counters.always_latch++);
        break;
      case ProceduralBlockKind::AlwaysFF:
        process->name = fmt::format("always_ff_{}", counters.always_ff++);
        break;
      case ProceduralBlockKind::Final:
        process->name = "final";
        break;
    }
  }

  switch (procedural_block.procedureKind) {
    case ProceduralBlockKind::Initial: {
      const auto& slang_statement = procedural_block.getBody();
      auto statement = LowerStatement(slang_statement, arena);
      process->body = std::move(statement);
      break;
    }

    case ProceduralBlockKind::AlwaysLatch:
    case ProceduralBlockKind::AlwaysComb: {
      const auto& slang_statement = procedural_block.getBody();
      auto body = LowerStatement(slang_statement, arena);
      auto sensitivity_items = CollectSensitivityList(*body);

      std::vector<common::Trigger> triggers;
      for (const auto& item : sensitivity_items) {
        triggers.emplace_back(
            common::Trigger::AnyChange(item.symbol, item.instance_path));
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
      auto condition = std::make_unique<mir::ConstantExpression>(
          common::Constant::Bool(true));
      process->body = std::make_unique<mir::WhileStatement>(
          std::move(condition), std::move(loop_block));
      break;
    }

    case ProceduralBlockKind::Always:
    case ProceduralBlockKind::AlwaysFF: {
      // Lower the user's body, which should contain WaitEventStatement itself
      auto loop_block = LowerStatement(procedural_block.getBody(), arena);

      // Simply wrap in while (true) { ... }
      auto condition = std::make_unique<mir::ConstantExpression>(
          common::Constant::Bool(true));
      auto loop = std::make_unique<mir::WhileStatement>(
          std::move(condition), std::move(loop_block));

      process->body = std::move(loop);
      break;
    }

    case ProceduralBlockKind::Final: {
      slang::SourceRange source_range(
          procedural_block.location, procedural_block.location);
      throw DiagnosticException(
          Diagnostic::Error(
              source_range,
              fmt::format(
                  "unsupported procedural block kind '{}'",
                  slang::ast::toString(procedural_block.procedureKind))));
    }
  }

  return process;
}

}  // namespace lyra::lowering::ast_to_mir
