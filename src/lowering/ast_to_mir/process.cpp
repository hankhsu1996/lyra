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
      process->process_kind = mir::ProcessKind::kAlways;

      const auto& slang_statement = procedural_block.getBody();

      auto statement = LowerStatement(slang_statement);
      auto variables = CollectSensitivityList(*statement);
      process->body = std::move(statement);

      for (const auto& variable : variables) {
        auto trigger = mir::Trigger::AnyEdge(variable);
        process->trigger_list.push_back(std::move(trigger));
      }
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
