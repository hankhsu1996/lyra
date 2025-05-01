#include "lowering/ast_to_mir/process.hpp"

#include <stdexcept>

#include <fmt/format.h>
#include <lowering/ast_to_mir/variable.hpp>
#include <slang/ast/Statement.h>
#include <slang/ast/Symbol.h>
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

      const auto& statement = procedural_block.getBody();
      auto statements = LowerStatement(statement);
      for (auto& stmt : statements) {
        process->body.push_back(std::move(stmt));
      }
      break;
    }

    case ProceduralBlockKind::AlwaysComb: {
      process->process_kind = mir::ProcessKind::kAlwaysComb;

      const auto& statement = procedural_block.getBody();
      auto symbols = CollectSensitivityList(statement);

      for (const auto* symbol : symbols) {
        auto variable_opt = LowerVariable(*symbol);
        if (!variable_opt) {
          throw std::runtime_error(
              "Unexpected non-variable in sensitivity list");
        }

        auto trigger = common::Trigger{
            .edge_kind = common::EdgeKind::kAnyEdge,
            .variable = variable_opt.value()};

        process->trigger_list.push_back(std::move(trigger));
      }

      auto statements = LowerStatement(statement);
      for (auto& stmt : statements) {
        process->body.push_back(std::move(stmt));
      }
      break;
    }

    case ProceduralBlockKind::Always:
    case ProceduralBlockKind::AlwaysLatch:
    case ProceduralBlockKind::AlwaysFF:
    case ProceduralBlockKind::Final:
      throw std::runtime_error(fmt::format(
          "Unsupported procedural block kind {} in LowerProcess",
          slang::ast::toString(procedural_block.procedureKind)));
  }

  return process;
}

}  // namespace lyra::lowering
